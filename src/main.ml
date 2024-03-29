(** [setup] sets state [s], init the window and return [s]. *)
let setup () =
  let w_width, w_height = (800, 450) in
  let p_width, p_height = (8, 64) in
  let s : State.t =
    {
      (* it is the upper left corner that is center... *)
      player1 =
        {
          pos_x = 10;
          pos_y = (w_height - p_height) / 2;
          width = p_width;
          height = p_height;
          color = Raylib.Color.create 0xf5 0x68 0x83 0xff;
        };
      player2 =
        {
          pos_x = w_width - 10 - p_width;
          pos_y = (w_height - p_height) / 2;
          width = p_width;
          height = p_height;
          color = Raylib.Color.create 0x83 0xf5 0x68 0xff;
        };
      window_width = w_width;
      window_height = w_height;
      background = Raylib.Color.create 0x68 0x83 0xf5 0xff;
      acceleration = 100.0;
    }
  in
  Raylib.init_window w_width w_height "OCaml Pong";
  Raylib.set_target_fps 60;
  s

(** [update state] updates and returns a new state.
 *   player1:
 *       a  --> move left
 *       s  --> move down
 *       d  --> move up
 *       f  --> move right
 *   player2:
 *       h  --> move left
 *       j  --> move down
 *       k  --> move up
 *       l  --> move right
 *)
let update (s : State.t) =
  let open Raylib in
  let frate : float = get_frame_time () in
  let velocity = frate *. s.acceleration in

  (* Update player 1 according to key press *)
  let delta_y =
    if is_key_down Key.S then velocity
    else if is_key_down Key.D then -1.0 *. velocity
    else 0.0
  in
  let delta_x =
    if is_key_down Key.F then velocity
    else if is_key_down Key.A then -1.0 *. velocity
    else 0.0
  in
  let s =
    State.update_player1 s (int_of_float delta_x) (int_of_float delta_y)
  in
  (* Update player 2 according to key press *)
  let delta_y =
    if is_key_down Key.J then velocity
    else if is_key_down Key.K then -1.0 *. velocity
    else 0.0
  in
  let delta_x =
    if is_key_down Key.L then velocity
    else if is_key_down Key.H then -1.0 *. velocity
    else 0.0
  in
  let s =
    State.update_player2 s (int_of_float delta_x) (int_of_float delta_y)
  in
  State.increment_acceleration s 1.0

(** [draw state] draws the scene and return [state]. *)
let draw (s : State.t) =
  let open Raylib in
  begin_drawing ();
  clear_background s.background;
  (* draw players *)
  draw_rectangle s.player1.pos_x s.player1.pos_y s.player1.width
    s.player1.height s.player1.color;
  draw_rectangle s.player2.pos_x s.player2.pos_y s.player2.width
    s.player2.height s.player2.color;
  (* draw the sepration line that in the middle *)
  draw_line (s.window_width / 2) 0 (s.window_width / 2) s.window_height
    Color.black;
  end_drawing ();
  s (* return it so it can be chained beautifully with loop *)

(** [loop state] is the main loop that updates and draws scene. *)
let rec loop (s : State.t) =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false -> update s |> draw |> loop

let () = setup () |> loop
