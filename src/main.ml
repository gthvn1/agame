(* Setup and return the current state *)
let setup () =
  let width, height = (800, 450) in
  let s : State.t =
    {
      player = { pos_x = 700; pos_y = 10; width = 10; height = 10 };
      window_width = width;
      window_height = height;
      background = Raylib.Color.create 0x68 0x83 0xf5 0xff;
      foreground = Raylib.Color.create 0xf5 0x68 0x83 0xff;
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
      (* Update the state *)
      let delta_y =
        if is_key_down Key.Down then 1 else if is_key_down Key.Up then -1 else 0
      in
      let delta_x =
        if is_key_down Key.Right then 1
        else if is_key_down Key.Left then -1
        else 0
      in
      let s' = State.update_player s delta_x delta_y in

      (* Drawing *)
      begin_drawing ();
      clear_background s'.background;
      draw_rectangle s'.player.pos_x s'.player.pos_y s'.player.width
        s'.player.height s'.foreground;
      end_drawing ();
      loop s'

let () = setup () |> loop
