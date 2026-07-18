module

public import Gimlight.Loop

namespace Gimlight

public def terminalViewText (view : GameView) : String :=
  Id.run do
    let mut text := ""
    for y in [0:view.height] do
      for x in [0:view.width] do
        text := text.push (if x == view.playerX && y == view.playerY then '@' else '.')
      text := (text.push '\r').push '\n'
    text := (text.push '\r').push '\n'
    return text ++ "h/j/k/l, arrow keys: move   q: quit"

end Gimlight
