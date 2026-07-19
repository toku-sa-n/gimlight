module

public import Gimlight.Loop
import Gimlight.Loop.GameHost
import Gimlight.Terminal.FFI
import Gimlight.Terminal.View

namespace Gimlight

namespace TerminalRawCode

-- Keep these UInt32 values synchronized with packages/terminal/native/terminal/src/lib.rs.
private def ok : UInt32 := 0

private def inputLeft : UInt32 := 1
private def inputRight : UInt32 := 2
private def inputUp : UInt32 := 3
private def inputDown : UInt32 := 4
private def inputQuit : UInt32 := 5
private def inputUnknown : UInt32 := 6
private def inputError : UInt32 := 7

private def rawModeLockPoisoned : UInt32 := 1000
private def rawModeTerminalIO : UInt32 := 1001

end TerminalRawCode

private inductive TerminalError where
  | setRawModeLockPoisoned
  | setRawModeTerminalIO
  | setRawModeOsError (code : UInt32)
  | restoreLockPoisoned
  | restoreTerminalIO
  | restoreOsError (code : UInt32)
  | renderFailed (code : UInt32)
  | readInputFailed (code : UInt32)
deriving DecidableEq, Repr

private inductive TerminalInputCode where
  | moveLeft
  | moveRight
  | moveUp
  | moveDown
  | quit
  | unknown
  | readError
  | invalid (code : UInt32)

private def TerminalError.ofSetRawModeCode (code : UInt32) : Option TerminalError :=
  if code == TerminalRawCode.ok then
    none
  else if code == TerminalRawCode.rawModeLockPoisoned then
    some .setRawModeLockPoisoned
  else if code == TerminalRawCode.rawModeTerminalIO then
    some .setRawModeTerminalIO
  else
    some <| .setRawModeOsError code

private def TerminalError.ofRestoreCode (code : UInt32) : Option TerminalError :=
  if code == TerminalRawCode.ok then
    none
  else if code == TerminalRawCode.rawModeLockPoisoned then
    some .restoreLockPoisoned
  else if code == TerminalRawCode.rawModeTerminalIO then
    some .restoreTerminalIO
  else
    some <| .restoreOsError code

private def TerminalError.toIOError (error : TerminalError) : IO.Error :=
  IO.userError <| match error with
    | .setRawModeLockPoisoned =>
        "set raw mode failed: terminal state lock was poisoned (code 1000)"
    | .setRawModeTerminalIO =>
        "set raw mode failed: terminal IO error without an OS error code (code 1001)"
    | .setRawModeOsError code =>
        s!"set raw mode failed: terminal returned OS error code {code}"
    | .restoreLockPoisoned =>
        "restore terminal failed: terminal state lock was poisoned (code 1000)"
    | .restoreTerminalIO =>
        "restore terminal failed: terminal IO error without an OS error code (code 1001)"
    | .restoreOsError code =>
        s!"restore terminal failed: terminal returned OS error code {code}"
    | .renderFailed code =>
        s!"render failed: terminal returned error code {code}"
    | .readInputFailed code =>
        s!"read input failed: terminal returned error code {code}"

private def TerminalInputCode.decode (code : UInt32) : TerminalInputCode :=
  if code == TerminalRawCode.inputLeft then
    .moveLeft
  else if code == TerminalRawCode.inputRight then
    .moveRight
  else if code == TerminalRawCode.inputUp then
    .moveUp
  else if code == TerminalRawCode.inputDown then
    .moveDown
  else if code == TerminalRawCode.inputQuit then
    .quit
  else if code == TerminalRawCode.inputUnknown then
    .unknown
  else if code == TerminalRawCode.inputError then
    .readError
  else
    .invalid code

private def TerminalInputCode.toGameInput : TerminalInputCode → Except TerminalError (Option GameInput)
  | .moveLeft => .ok <| some (.move .left)
  | .moveRight => .ok <| some (.move .right)
  | .moveUp => .ok <| some (.move .up)
  | .moveDown => .ok <| some (.move .down)
  | .quit => .ok <| some .quit
  | .unknown => .ok none
  | .readError => .error <| .readInputFailed TerminalRawCode.inputError
  | .invalid code => .error <| .readInputFailed code

private def setupRawMode : IO Unit := do
  if let some error := TerminalError.ofSetRawModeCode (← Terminal.FFI.setRawModeCode) then
    throw <| error.toIOError

private def restoreTerminal : IO Unit := do
  if let some error := TerminalError.ofRestoreCode (← Terminal.FFI.restoreTerminalCode) then
    throw <| error.toIOError

private def renderText (text : String) : IO Unit := do
  let code ← Terminal.FFI.renderTextCode text
  unless code == TerminalRawCode.ok do
    throw <| (TerminalError.renderFailed code).toIOError

private partial def readKnownGameInput : IO GameInput := do
  match (TerminalInputCode.decode (← Terminal.FFI.readInputCode)).toGameInput with
  | .ok (some input) => return input
  | .ok none => readKnownGameInput
  | .error error => throw <| error.toIOError

public structure TerminalHost where
  private mk ::

public abbrev TerminalM := ReaderT TerminalHost IO

private def TerminalHost.new : IO TerminalHost := do
  setupRawMode
  return TerminalHost.mk

public def TerminalHost.run (action : TerminalM α) : IO α := do
  let host ← TerminalHost.new
  let actionResult : Except IO.Error α ←
    try
      pure <| Except.ok (← action.run host)
    catch error =>
      pure <| Except.error error
  let restoreResult : Except IO.Error Unit ←
    try
      pure <| Except.ok (← restoreTerminal)
    catch error =>
      pure <| Except.error error
  match actionResult, restoreResult with
  | .ok result, .ok () => return result
  | .ok _, .error restoreError => throw restoreError
  | .error actionError, .ok () => throw actionError
  | .error actionError, .error restoreError =>
      throw <| IO.userError s!"terminal action failed: {actionError}; additionally, {restoreError}"

instance : GameHost TerminalM where
  render view _ := renderText (terminalViewText view)
  readInput _ := readKnownGameInput

end Gimlight
