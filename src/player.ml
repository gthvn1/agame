type t = {
  pos_x : int;
  pos_y : int;
  width : int;
  height : int;
  color : Raylib.Color.t;
}

let update_player (p : t) (dx : int) (dy : int) (maxx : int) (maxy : int) =
  (* Check x boundary *)
  let new_x = p.pos_x + dx in
  let upper_x = maxx - p.width in
  let new_x =
    if new_x > upper_x then upper_x else if new_x < 0 then 0 else new_x
  in
  (* check y boundary *)
  let new_y = p.pos_y + dy in
  let upper_y = maxy - p.height in
  let new_y =
    if new_y > upper_y then upper_y else if new_y < 0 then 0 else new_y
  in
  { p with pos_x = new_x; pos_y = new_y }
