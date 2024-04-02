module R = Raylib

module V = struct
  let string_of_v (v : R.Vector2.t) =
    let x = string_of_float @@ R.Vector2.x v in
    let y = string_of_float @@ R.Vector2.y v in
    "( " ^ x ^ ", " ^ y ^ ")"

  let equal (v1 : R.Vector2.t) (v2 : R.Vector2.t) =
    R.Vector2.x v1 = R.Vector2.x v2 && R.Vector2.y v1 = R.Vector2.y v2

  let get_coord (v : R.Vector2.t) = (R.Vector2.x v, R.Vector2.y v)

  let multf (v1 : R.Vector2.t) (v2 : R.Vector2.t) =
    let x1, y1 = get_coord v1 in
    let x2, y2 = get_coord v2 in
    R.Vector2.create (x1 *. x2) (y1 *. y2)

  let reverse_x = multf (R.Vector2.create (-1.0) 1.0)
  let reverse_y = multf (R.Vector2.create 1.0 (-1.0))
  let reverse = multf (R.Vector2.create (-1.0) (-1.0))
end

module Player = struct
  type t = { pos : R.Vector2.t; size : R.Vector2.t; color : R.Color.t }

  let string_of_pos (p : t) = V.string_of_v p.pos
  let get_pos (p : t) = V.get_coord p.pos
  let get_size (p : t) = V.get_coord p.size
end

module Ball = struct
  type t = {
    pos : R.Vector2.t;
    speed : R.Vector2.t;
    radius : float;
    color : R.Color.t;
  }

  let string_of_pos (b : t) = V.string_of_v b.pos
  let get_pos (b : t) = V.get_coord b.pos
end

module Window = struct
  type t = {
    size : R.Vector2.t;
    margin : float; (* Left/Right margin used to check player limits *)
    background : R.Color.t;
  }

  let get_size (w : t) = V.get_coord w.size
end

type t = {
  pleft : Player.t;
  pright : Player.t;
  ball : Ball.t;
  window : Window.t;
  speed : float;
  debug : bool;
}
(** structure of State.ml *)

(** [update_player left dv state] update the position of the player
    using [dv]. It will check the boundaries depending he is
    to the left or right of the tennis court. It returns the new state. *)
let update_player (left : bool) (dv : R.Vector2.t) (s : t) =
  let m = s.window.margin in
  let win_width, win_height = Window.get_size s.window in
  let pl_width, pl_height = Player.get_size s.pleft in
  let pr_width, _ = Player.get_size s.pright in

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
let update_ball (serving : bool) (s : t) =
  let b = s.ball in
  let pl_width, pl_height = Player.get_size s.pleft in
  let not_inplay = V.equal b.speed (R.Vector2.zero ()) in
  if not_inplay then (
    (* we need to follow the left player until he serves *)
    let delta = R.Vector2.create pl_width (pl_height /. 2.0) in
    let new_speed = 2.0 in
    (* if serves then update the speed x axis to 5.0 *)
    if serving then R.Vector2.set_x b.speed new_speed else ();
    if serving then R.Vector2.set_y b.speed new_speed else ();
    { s with ball = { b with pos = R.Vector2.add s.pleft.pos delta } })
  else
    (* we need to check if the new position is hitting a player *)
    let new_pos_x, new_pos_y = V.get_coord @@ R.Vector2.add b.pos b.speed in
    let new_pos = R.Vector2.create new_pos_x new_pos_y in
    let old_pos_x, old_pos_y = Ball.get_pos b in
    let pleft_x, pleft_y = Player.get_pos s.pleft in
    let pright_x, pright_y = Player.get_pos s.pright in
    (* Both players have the same size *)
    let _, player_height = Player.get_size s.pright in
    let _, win_height = Window.get_size s.window in
    if old_pos_x > pleft_x && new_pos_x < pleft_x then
      (* Hit player left *)
      if
        (old_pos_y < pleft_y && new_pos_y < pleft_y)
        || old_pos_y > pleft_y +. player_height
           && new_pos_y > pleft_y +. player_height
      then (* we missed the ball *)
        { s with ball = { b with pos = new_pos } }
      else
        (* we hit the ball *)
        { s with ball = { b with pos = new_pos; speed = V.reverse_x b.speed } }
    else if old_pos_x < pright_x && new_pos_x > pright_x then
      (* Hit player right *)
      if
        (old_pos_y < pright_y && new_pos_y < pright_y)
        || old_pos_y > pright_y +. player_height
           && new_pos_y > pright_y +. player_height
      then { (* we missed the ball *)
             s with ball = { b with pos = new_pos } }
      else
        {
          (* we hit the ball *)
          s with
          ball = { b with pos = new_pos; speed = V.reverse_x b.speed };
        }
    else if new_pos_y <= 0.0 || new_pos_y >= win_height then
      {
        (* we hit the top or bottom *)
        s with
        ball = { b with pos = new_pos; speed = V.reverse_y b.speed };
      }
    else { s with ball = { b with pos = new_pos } }

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
