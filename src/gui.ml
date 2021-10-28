open Graphics

let window_game_size = 800

let coordinate_size = 20

let () = open_graph " 1000x800"

let () = Graphics.set_window_title "Blackjack"

let hot_pink = 16722885

let pink = 16630538

let p1_stats_x = 200

let p2_stats_x = 500

let bet_stats_y = 225

let card_header_y = bet_stats_y - 50

let card_stats_y_pos = card_header_y - 35

let cards_stats_y = ref card_stats_y_pos

let ellipse_center_x = 400

let ellipse_center_y = 450

let ellipse_hor_rad = 350

let ellipse_vert_rad = 150

let dealer_card_x = 360

let dealer_card_y_header = ellipse_center_y + 50

let dealer_card_y_pos = dealer_card_y_header - 25

let dealer_card_y = ref dealer_card_y_pos

let directions_x = 100

let directions_y = 750

let draw_background (state : state) : unit =
  Graphics.clear_graph ();
  Graphics.set_color black;
  Graphics.fill_rect 0 0 window_game_size window_game_size;
  Graphics.set_color green;
  Graphics.fill_ellipse ellipse_center_x ellipse_center_y
    ellipse_hor_rad ellipse_vert_rad;
  let x_pos = window_game_size - 200 in
  Graphics.moveto x_pos 20;
  Graphics.draw_string "Press 'q' to quit";
  let y_pos = bet_stats_y + 50 in
  let p1_earnings_string = string_of_int state.p1_earnings in
  let p1_string = "Player 1 Earnings: " ^ p1_earnings_string in
  Graphics.moveto p1_stats_x y_pos;
  Graphics.draw_string p1_string;
  let p2_earnings_string = string_of_int state.p2_earnings in
  let p2_string = "Player 2 Earnings: " ^ p2_earnings_string in
  Graphics.moveto p2_stats_x y_pos;
  Graphics.draw_string p2_string;
  ()
