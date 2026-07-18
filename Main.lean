import Gimlight.Loop
import Gimlight.Terminal

open Gimlight

def main : IO Unit :=
  TerminalHost.run GameHost.gameLoop
