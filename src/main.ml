module R = Raylib
module S = State

(** [setup] sets state [s], init the window and return [s]. *)
let setup () =
  let w_width, w_height = (800.0, 450.0) in
  let p_width, p_height = (8.0, 64.0) in
  let margin = 10.0 in
  let s : S.t =
    {
      (* it is the upper left corner that is center... *)
      pleft =
        {
          pos = R.Vector2.create margin ((w_height -. p_height) /. 2.0);
          size = R.Vector2.create p_width p_height;
          color = R.Color.create 0xf5 0x68 0x83 0xff;
        };
      pright =
        {
          pos =
            R.Vector2.create
              (w_width -. margin -. p_width)
              ((w_height -. p_height) /. 2.0);
          size = R.Vector2.create p_width p_height;
          color = R.Color.create 0x83 0xf5 0x68 0xff;
        };
      ball =
        {
          (* At the beginning of the game it is the left player
             that will serve. So put the ball on him. *)
          pos = R.Vector2.create (margin +. p_width +. 5.0) (w_height /. 2.0);
          speed = R.Vector2.zero ();
          radius = 5.0;
          color = R.Color.black;
        };
      window =
        {
          size = R.Vector2.create w_width w_height;
          margin;
          background = R.Color.create 0x68 0x83 0xf5 0xff;
        };
      speed = 100.0;
      debug = false;
    }
  in
  R.init_window (int_of_float w_width) (int_of_float w_height) "OCaml Pong";
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
  let velocity = frate *. s.speed in
  let delta key_up key_down =
    if R.is_key_down key_down then velocity
    else if R.is_key_down key_up then -1.0 *. velocity
    else 0.0
  in
  let serve = R.is_key_pressed R.Key.Space in
  {
    s with
    debug = (if R.is_key_pressed R.Key.Enter then not s.debug else s.debug);
  }
  |> (* Update left player according to key pressed *)
  S.update_pleft
    (R.Vector2.create (delta R.Key.A R.Key.F) (delta R.Key.D R.Key.S))
  (* Update right player according to key pressed *)
  |> S.update_pright
       (R.Vector2.create
          (delta R.Key.J R.Key.Semicolon)
          (delta R.Key.K R.Key.L))
  |> S.update_ball serve
  (* and return the state after incrementing a little bit the speed *)
  |> S.update_speed 1.0

(** [draw state] draws the scene and return [state]. *)
let draw (s : S.t) =
  let win_width, win_height = State.Window.get_size s.window in
  let win_width = int_of_float win_width in
  let win_height = int_of_float win_height in

  R.begin_drawing ();
  R.clear_background s.window.background;

  (* draw players *)
  R.draw_rectangle_v s.pleft.pos s.pleft.size s.pleft.color;
  R.draw_rectangle_v s.pright.pos s.pright.size s.pright.color;

  (* draw the ball *)
  R.draw_circle_v s.ball.pos s.ball.radius s.ball.color;

  if s.debug then (
    let pleft_pos = State.Player.string_of_pos s.pleft in
    let pright_pos = State.Player.string_of_pos s.pright in
    let ball_pos = State.Ball.string_of_pos s.ball in

    R.draw_text ("Player left: " ^ pleft_pos) 30 5 10 R.Color.black;
    R.draw_text ("Player right: " ^ pright_pos) 30 15 10 R.Color.black;
    R.draw_text ("Ball: " ^ ball_pos) 30 25 10 R.Color.black)
  else ();

  (* draw the separation line that in the middle *)
  R.draw_line (win_width / 2) 0 (win_width / 2) win_height R.Color.black;

  R.end_drawing ();
  s (* return it so it can be chained beautifully with loop *)

(** [loop state] is the main loop that updates and draws scene. *)
let rec loop (s : S.t) =
  match R.window_should_close () with
  | true -> R.close_window ()
  | false -> update s |> draw |> loop

let () = setup () |> loop
