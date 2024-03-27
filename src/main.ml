(* Setup and return the current state *)
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

let rec loop (s : State.t) =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let open Raylib in
      let rate : float = get_frame_time () in
      let rate = rate *. s.acceleration in

      (* Update the state *)
      let delta_y =
        if is_key_down Key.Down then rate
        else if is_key_down Key.Up then -1.0 *. rate
        else 0.0
      in
      let delta_x =
        if is_key_down Key.Right then rate
        else if is_key_down Key.Left then -1.0 *. rate
        else 0.0
      in
      let s' =
        State.update_player s (int_of_float delta_x) (int_of_float delta_y)
      in
      let s' = State.increment_acceleration s' 1.0 in

      (* Drawing *)
      begin_drawing ();
      clear_background s'.background;
      draw_rectangle s'.player.pos_x s'.player.pos_y s'.player.width
        s'.player.height s'.foreground;
      end_drawing ();
      loop s'

let () = setup () |> loop
