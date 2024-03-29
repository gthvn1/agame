module Ball = struct
  type t = { pos_x : int; pos_y : int; radius : float; color : Raylib.Color.t }
end

module Player = struct
  type t = {
    pos_x : int;
    pos_y : int;
    width : int;
    height : int;
    color : Raylib.Color.t;
  }

  let update_player (p : t) (dx : int) (dy : int) (maxx : int) (maxy : int) =
    (* Check x boundary *)
    let new_x = p.pos_x + dx in
    let upper_x = maxx - p.width in
    let new_x =
      if new_x > upper_x then upper_x else if new_x < 0 then 0 else new_x
    in
    (* check y boundary *)
    let new_y = p.pos_y + dy in
    let upper_y = maxy - p.height in
    let new_y =
      if new_y > upper_y then upper_y else if new_y < 0 then 0 else new_y
    in
    { p with pos_x = new_x; pos_y = new_y }
end

type t = {
  player1 : Player.t;
  player2 : Player.t;
  ball : Ball.t;
  window_width : int;
  window_height : int;
  background : Raylib.Color.t;
  acceleration : float;
}

let update_player1 (s : t) dx dy =
  let new_p =
    Player.update_player s.player1 dx dy s.window_width s.window_height
  in
  { s with player1 = new_p }

let update_player2 (s : t) dx dy =
  let new_p =
    Player.update_player s.player2 dx dy s.window_width s.window_height
  in
  { s with player2 = new_p }

let increment_acceleration (s : t) v =
  (* we limite the acceleration to 1000.0 *)
  let max_acc = 1000.0 in
  let new_acceleration = s.acceleration +. v in
  {
    s with
    acceleration =
      (if new_acceleration > max_acc then max_acc else new_acceleration);
  }
