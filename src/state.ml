module R = Raylib

module Ball = struct
  type t = { pos_x : int; pos_y : int; radius : float; color : R.Color.t }
end

module Player = struct
  type t = {
    pos_x : int;
    pos_y : int;
    width : int;
    height : int;
    color : R.Color.t;
  }
end

module Window = struct
  type t = {
    width : int;
    height : int;
    margin : int; (* Left/Right margin used to check player limits *)
    background : R.Color.t;
  }
end

type t = {
  pleft : Player.t;
  pright : Player.t;
  ball : Ball.t;
  window : Window.t;
  acceleration : float;
}
(** Main structure of State.ml *)

(** [update_player state left delta_x delta_y] update the position of the player
    using [delta_x] and [delta_y]. It will check the boundaries depending he is
    to the left or right of the tennis court. *)
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

let increment_acceleration (s : t) v =
  (* we limite the acceleration to 1000.0 *)
  let max_acc = 1000.0 in
  let new_acceleration = s.acceleration +. v in
  {
    s with
    acceleration =
      (if new_acceleration > max_acc then max_acc else new_acceleration);
  }
