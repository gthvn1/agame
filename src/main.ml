module R = Raylib
module S = State

(** [setup] sets state [s], init the window and return [s]. *)
let setup () =
  let w_width, w_height = (800, 450) in
  let p_width, p_height = (8, 64) in
  let margin = 10 in
  let s : S.t =
    {
      (* it is the upper left corner that is center... *)
      pleft =
        {
          pos_x = margin;
          pos_y = (w_height - p_height) / 2;
          width = p_width;
          height = p_height;
          color = R.Color.create 0xf5 0x68 0x83 0xff;
        };
      pright =
        {
          pos_x = w_width - margin - p_width;
          pos_y = (w_height - p_height) / 2;
          width = p_width;
          height = p_height;
          color = R.Color.create 0x83 0xf5 0x68 0xff;
        };
      ball =
        {
          (* At the beginning of the game it is the left player
             that will serve. So put the ball on him. *)
          pos =
            R.Vector2.create
              (float_of_int @@ (margin + p_width + 5))
              (float_of_int @@ (w_height / 2));
          speed = R.Vector2.create 0.0 0.0;
          radius = 5.0;
          color = R.Color.black;
        };
      window =
        {
          width = w_width;
          height = w_height;
          margin;
          background = R.Color.create 0x68 0x83 0xf5 0xff;
        };
      speed = 100.0;
    }
  in
  R.init_window w_width w_height "OCaml Pong";
  R.set_target_fps 60;
  s

(** [update state] updates and returns a new state.
 *   pleft:
 *       a  --> move left   --> will be Q for french kb
 *       s  --> move down
 *       d  --> move up
 *       f  --> move right
 *   pright:
 *       j  --> move left
 *       k  --> move down
 *       l  --> move up
 *       semicolon  --> move right  --> will be m for french kb
 *)
let update (s : S.t) =
  let frate : float = R.get_frame_time () in
  let velocity = int_of_float @@ (frate *. s.speed) in
  let delta key_up key_down =
    if R.is_key_down key_down then velocity
    else if R.is_key_down key_up then -1 * velocity
    else 0
  in
  (* Update left player according to key pressed *)
  S.update_pleft (delta R.Key.A R.Key.F) (delta R.Key.D R.Key.S) s
  (* Update right player according to key pressed *)
  |> S.update_pright (delta R.Key.J R.Key.Semicolon) (delta R.Key.K R.Key.L)
  |> S.update_ball
  (* and return the state after incrementing a little bit the speed *)
  |> S.update_speed 1.0

(** [draw state] draws the scene and return [state]. *)
let draw (s : S.t) =
  R.begin_drawing ();
  R.clear_background s.window.background;

  (* draw players *)
  R.draw_rectangle s.pleft.pos_x s.pleft.pos_y s.pleft.width s.pleft.height
    s.pleft.color;
  R.draw_rectangle s.pright.pos_x s.pright.pos_y s.pright.width s.pright.height
    s.pright.color;

  (* draw the ball *)
  R.draw_circle_v s.ball.pos s.ball.radius s.ball.color;

  (* draw the sepration line that in the middle *)
  R.draw_line (s.window.width / 2) 0 (s.window.width / 2) s.window.height
    R.Color.black;
  R.end_drawing ();
  s (* return it so it can be chained beautifully with loop *)

(** [loop state] is the main loop that updates and draws scene. *)
let rec loop (s : S.t) =
  match R.window_should_close () with
  | true -> R.close_window ()
  | false -> update s |> draw |> loop

let () = setup () |> loop
