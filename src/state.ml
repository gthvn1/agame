module R = Raylib

type ball = {
  pos_x : int;
  pos_y : int;
  radius : float;
  color : R.Color.t;
  served : bool;
}

type player = {
  pos_x : int;
  pos_y : int;
  width : int;
  height : int;
  color : R.Color.t;
}

type window = {
  width : int;
  height : int;
  margin : int; (* Left/Right margin used to check player limits *)
  background : R.Color.t;
}

type t = {
  pleft : player;
  pright : player;
  ball : ball;
  window : window;
  speed : float;
}
(** structure of State.ml *)

(** [update_player state left delta_x delta_y] update the position of the player
    using [delta_x] and [delta_y]. It will check the boundaries depending he is
    to the left or right of the tennis court. It returns the new state. *)
let update_player (s : t) (left : bool) dx dy =
  let m = s.window.margin in
  let min_x, max_x =
    if left then (m, (s.window.width / 2) - m - s.pleft.width)
    else ((s.window.width / 2) + m, s.window.width - m)
  in
  let min_y, max_y = (0, s.window.height - s.pleft.height) in
  (* players has the same height *)
  let pos_x, pos_y =
    if left then (s.pleft.pos_x, s.pleft.pos_y)
    else (s.pright.pos_x, s.pright.pos_y)
  in
  let new_x = pos_x + dx in
  let new_y = pos_y + dy in
  (* check boundaries *)
  let new_x =
    if new_x < min_x then min_x else if new_x > max_x then max_x else new_x
  in
  let new_y =
    if new_y < min_y then min_y else if new_y > max_y then max_y else new_y
  in
  (* can now update the player *)
  if left then { s with pleft = { s.pleft with pos_x = new_x; pos_y = new_y } }
  else { s with pright = { s.pright with pos_x = new_x; pos_y = new_y } }

let update_pleft (s : t) dx dy = update_player s true dx dy
let update_pright (s : t) dx dy = update_player s false dx dy

(** [update_ball state] update the position of the ball and return the new state. *)
let update_ball (s : t) = s

(** [update_speed state velocity] add the [velocity] to the state.
    We can not reach a velocity greated than 1000.0 and we can not go below
    a velocity of 10.0 and return the new state *)
let update_speed (s : t) v =
  (* we limite the speed to 1000.0 *)
  let max_speed = 1000.0 in
  let min_speed = 10.0 in
  let new_speed = s.speed +. v in
  {
    s with
    speed =
      (if new_speed > max_speed then max_speed
       else if new_speed < min_speed then min_speed
       else new_speed);
  }
