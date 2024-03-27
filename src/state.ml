type t = {
  player : Player.t;
  window_width : int;
  window_height : int;
  background : Raylib.Color.t;
  foreground : Raylib.Color.t;
  acceleration : float;
}

let update_player (s : t) dx dy =
  let new_p =
    Player.update_player s.player dx dy s.window_width s.window_height
  in
  { s with player = new_p }

let increment_acceleration (s : t) v =
  (* we limite the acceleration to 1000.0 *)
  let max_acc = 1000.0 in
  let new_acceleration = s.acceleration +. v in
  {
    s with
    acceleration =
      (if new_acceleration > max_acc then max_acc else new_acceleration);
  }
