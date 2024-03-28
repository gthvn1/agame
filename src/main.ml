(** [setup] sets state [s], init the window and return [s]. *)
let setup () =
  let width, height = (800, 450) in
  let s : State.t =
    {
      (* it is the upper left corner that is center... *)
      player =
        { pos_x = width / 2; pos_y = height / 2; width = 64; height = 64 };
      window_width = width;
      window_height = height;
      background = Raylib.Color.create 0x68 0x83 0xf5 0xff;
      foreground = Raylib.Color.create 0xf5 0x68 0x83 0xff;
      acceleration = 100.0;
    }
  in
  Raylib.init_window width height "raylib example";
  Raylib.set_target_fps 60;
  s

(** [update state] updates and returns a new state. *)
let update (s : State.t) =
  let open Raylib in
  let frate : float = get_frame_time () in
  let velocity = frate *. s.acceleration in
  let delta_y =
    if is_key_down Key.Down then velocity
    else if is_key_down Key.Up then -1.0 *. velocity
    else 0.0
  in
  let delta_x =
    if is_key_down Key.Right then velocity
    else if is_key_down Key.Left then -1.0 *. velocity
    else 0.0
  in
  let s' =
    State.update_player s (int_of_float delta_x) (int_of_float delta_y)
  in
  State.increment_acceleration s' 1.0

(** [draw state] draws the scene and return [state]. *)
let draw (s : State.t) =
  let open Raylib in
  begin_drawing ();
  clear_background s.background;
  draw_rectangle s.player.pos_x s.player.pos_y s.player.width s.player.height
    s.foreground;
  end_drawing ();
  s (* return it so it can be chained beautifully with loop *)

(** [loop state] is the main loop that updates and draws scene. *)
let rec loop (s : State.t) =
  let open Raylib in
  match window_should_close () with
  | true -> close_window ()
  | false -> update s |> draw |> loop

let () = setup () |> loop
