import Gimlight.Loop
import Gimlight.Loop.GameHost
import Gimlight.Terminal

open Gimlight

def main : IO Unit := do
  let state ← initialState
  TerminalHost.run (GameHost.gameLoop state)
