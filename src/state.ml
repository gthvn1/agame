type t = {
  player : Player.t;
  window_width : int;
  window_height : int;
  background : Raylib.Color.t;
  foreground : Raylib.Color.t;
}

let update_player (s : t) dx dy =
  let new_p =
    Player.update_player s.player dx dy s.window_width s.window_height
  in
  { s with player = new_p }
