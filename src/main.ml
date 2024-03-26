let setup () =
  Raylib.init_window 800 450 "raylib example";
  Raylib.set_target_fps 60

let rec loop () =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let open Raylib in
      begin_drawing ();
      clear_background Color.raywhite;
      draw_text "Congrats! You created your first window!" 190 200 20
        Color.lightgray;
      draw_rectangle 10 10 64 64 Color.red;
      end_drawing ();
      loop ()

let () = setup () |> loop
