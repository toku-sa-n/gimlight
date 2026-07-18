module

namespace Gimlight.Terminal.FFI

@[extern "gimlight_set_raw_mode"]
public opaque setRawModeCode : IO UInt32

@[extern "gimlight_restore_terminal"]
public opaque restoreTerminalCode : IO UInt32

@[extern "gimlight_render_text"]
public opaque renderTextCode (text : @& String) : IO UInt32

@[extern "gimlight_read_input"]
public opaque readInputCode : IO UInt32

end Gimlight.Terminal.FFI
