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
      player1 =
        {
          pos_x = margin;
          pos_y = (w_height - p_height) / 2;
          width = p_width;
          height = p_height;
          color = R.Color.create 0xf5 0x68 0x83 0xff;
        };
      player2 =
        {
          pos_x = w_width - margin - p_width;
          pos_y = (w_height - p_height) / 2;
          width = p_width;
          height = p_height;
          color = R.Color.create 0x83 0xf5 0x68 0xff;
        };
      ball =
        {
          (* Let's put the ball in random place for now *)
          pos_x = 40;
          pos_y = 40;
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
      acceleration = 100.0;
    }
  in
  R.init_window w_width w_height "OCaml Pong";
  R.set_target_fps 60;
  s

(** [update state] updates and returns a new state.
 *   player1:
 *       a  --> move left   --> will be Q for french kb
 *       s  --> move down
 *       d  --> move up
 *       f  --> move right
 *   player2:
 *       j  --> move left
 *       k  --> move down
 *       l  --> move up
 *       semicolon  --> move right  --> will be m for french kb
 *)
let update (s : S.t) =
  let frate : float = R.get_frame_time () in
  let velocity = int_of_float @@ (frate *. s.acceleration) in
  let delta key_up key_down =
    if R.is_key_down key_down then velocity
    else if R.is_key_down key_up then -1 * velocity
    else 0
  in
  (* Update player 1 according to key press *)
  let s = S.update_player1 s (delta R.Key.A R.Key.F) (delta R.Key.D R.Key.S) in
  (* Update player 2 according to key press *)
  let s =
    S.update_player2 s (delta R.Key.J R.Key.Semicolon) (delta R.Key.K R.Key.L)
  in
  (* and return the state after incrementing a little bit the speed *)
  S.increment_acceleration s 0.1

(** [draw state] draws the scene and return [state]. *)
let draw (s : S.t) =
  R.begin_drawing ();
  R.clear_background s.window.background;

  (* draw players *)
  R.draw_rectangle s.player1.pos_x s.player1.pos_y s.player1.width
    s.player1.height s.player1.color;
  R.draw_rectangle s.player2.pos_x s.player2.pos_y s.player2.width
    s.player2.height s.player2.color;

  (* draw the ball *)
  R.draw_circle s.ball.pos_x s.ball.pos_y s.ball.radius s.ball.color;

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
