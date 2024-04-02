module R = Raylib

module Player = struct
  type t = { pos : R.Vector2.t; size : R.Vector2.t; color : R.Color.t }

  let width (p : t) = R.Vector2.x p.size
  let height (p : t) = R.Vector2.y p.size
end

module Ball = struct
  type t = {
    pos : R.Vector2.t;
    speed : R.Vector2.t;
    radius : float;
    color : R.Color.t;
  }
end

module Window = struct
  type t = {
    size : R.Vector2.t;
    margin : float; (* Left/Right margin used to check player limits *)
    background : R.Color.t;
  }

  let width (w : t) = R.Vector2.x w.size
  let height (w : t) = R.Vector2.y w.size
end

type t = {
  pleft : Player.t;
  pright : Player.t;
  ball : Ball.t;
  window : Window.t;
  speed : float;
}
(** structure of State.ml *)

(** [update_player left dv state] update the position of the player
    using [dv]. It will check the boundaries depending he is
    to the left or right of the tennis court. It returns the new state. *)
let update_player (left : bool) (dv : R.Vector2.t) (s : t) =
  let m = s.window.margin in
  let win_width = Window.width s.window in
  let win_height = Window.height s.window in
  let pl_width = Player.width s.pleft in
  let pl_height = Player.height s.pleft in
  let pr_width = Player.width s.pright in

  (* set boundaries minx, miny, max, maxy for the given player *)
  let min_x = if left then m else (win_width /. 2.0) +. m in
  let max_x =
    if left then (win_width /. 2.0) -. m -. pl_width
    else win_width -. m -. pr_width
  in
  let min_y = 0.0 in
  (* players has the same height *)
  let max_y = win_height -. pl_height in

  let current_pos = if left then s.pleft.pos else s.pright.pos in
  let new_pos = R.Vector2.add current_pos dv in

  (* check boundaries *)
  let new_x = R.Vector2.x new_pos in
  let new_y = R.Vector2.y new_pos in
  let _ =
    R.Vector2.set_x new_pos
    @@ if new_x < min_x then min_x else if new_x > max_x then max_x else new_x;
    R.Vector2.set_y new_pos
    @@ if new_y < min_y then min_y else if new_y > max_y then max_y else new_y
  in
  (* can now update the player *)
  if left then { s with pleft = { s.pleft with pos = new_pos } }
  else { s with pright = { s.pright with pos = new_pos } }

let update_pleft dv (s : t) = update_player true dv s
let update_pright dv (s : t) = update_player false dv s

(** [update_ball state] update the position of the ball and return the new state. *)
let update_ball (s : t) =
  let b = s.ball in
  let pl_width = Player.width s.pleft in
  let pl_height = Player.height s.pleft in
  if R.Vector2.x b.speed = 0.0 && R.Vector2.y b.speed = 0.0 then
    (* we need to follow the left player until he serves *)
    let delta = R.Vector2.create pl_width (pl_height /. 2.0) in
    { s with ball = { b with pos = R.Vector2.add s.pleft.pos delta } }
  else s (* update ball when moving *)

(** [update_speed velocity state] add the [velocity] to the state.
    We can not reach a velocity greated than 1000.0 and we can not go below
    a velocity of 10.0 and return the new state *)
let update_speed v (s : t) =
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
