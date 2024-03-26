type player = { pos_x : int; pos_y : int; width : int; height : int }

let update_player (p : player) (x : int) (y : int) =
  { p with pos_x = p.pos_x + x; pos_y = p.pos_y + y }

let setup () =
  Raylib.init_window 800 450 "raylib example";
  Raylib.set_target_fps 60

let rec loop p () =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let open Raylib in
      begin_drawing ();
      clear_background Color.raywhite;
      draw_text "Congrats! You created your first window!" 190 200 20
        Color.lightgray;
      draw_rectangle p.pos_x p.pos_y p.width p.height Color.red;
      end_drawing ();
      (* just add 1 to x and y for testing... *)
      loop (update_player p 1 1) ()

let () =
  let p : player = { pos_x = 10; pos_y = 10; width = 10; height = 10 } in
  setup () |> loop p
