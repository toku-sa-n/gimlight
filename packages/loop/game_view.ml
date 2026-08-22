type t = {
  width : int;
  height : int;
  player_x : int;
  player_y : int;
}

let of_state state =
  let map = Gimlight_logic.GameState.map state in
  let dimensions = Gimlight_logic.Map.dimensions map in
  let player = Gimlight_logic.GameState.player state in
  {
    width = Gimlight_logic.Dimensions.width_nat dimensions;
    height = Gimlight_logic.Dimensions.height_nat dimensions;
    player_x = Gimlight_logic.Position.x_value map player;
    player_y = Gimlight_logic.Position.y_value map player;
  }
