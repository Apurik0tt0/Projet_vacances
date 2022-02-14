open Graphics
(* L'objectif ici est d'essayer de programmer une boucle de gestion
   des événements sans latence *)

(* renvoie le coin en bas a gauche de la case*)
let coo_case x y =
  let new_x = x - (x mod 50) in
  let new_y = y - (y mod 50) in
  (new_x,new_y)

(* renvoie les coordonées de la case dans le tableau*)
let coo_case_tab x y =
  match coo_case x y with
  |a,b ->
      ((a/50),(b/50))

(* affiche la ligne de vue du sort a partir des coo données*)
let range x y po =
  let case = coo_case x y in
  for i=0 to po do
    match case with
    |a,b -> fill_rect (a-po*50+i*50) (b+i*50) (po*100-100*i+50) 50;
            fill_rect (a-po*50+i*50) (b-i*50) (po*100-100*i+50) 50;
  done

(* positionne le joueur aux coordonées données*)
let draw_player x y =
  let player1 = Bmp.load "iop.bmp" [] in
  match coo_case x y with
  |a,b -> Graphic_image.draw_image player1 a b


type case = Vide | Plein | Libre | Spell

let _ =

    open_graph " 1250x1000";

    let map = [|[|Spell;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide|];
    [|Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide|];
    [|Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide|];
    [|Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide|];
    [|Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide|];
    [|Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Vide|];
    [|Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Vide|];
    [|Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Plein;Libre;Libre;Libre;Libre;Vide|];
    [|Libre;Libre;Libre;Libre;Libre;Libre;Libre;Plein;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Plein;Libre;Libre;Libre;Vide|];
    [|Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Plein;Libre;Libre;Libre;Plein;Libre;Libre;Libre;Libre;Libre;Libre;Vide|];
    [|Libre;Libre;Libre;Libre;Libre;Libre;Plein;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Vide|];
    [|Libre;Libre;Libre;Libre;Libre;Libre;Plein;Vide;Vide;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Plein;Libre;Libre;Vide|];
    [|Libre;Libre;Libre;Libre;Libre;Libre;Plein;Vide;Vide;Libre;Libre;Libre;Libre;Libre;Vide;Vide;Libre;Libre;Libre;Vide|];
    [|Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Plein;Libre;Libre;Libre;Vide;Vide;Plein;Libre;Libre;Vide|];
    [|Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Vide|];
    [|Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Plein;Libre;Libre;Libre;Libre;Vide|];
    [|Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Plein;Libre;Libre;Libre;Libre;Libre;Plein;Libre;Libre;Libre;Vide|];
    [|Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Plein;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Vide|];
    [|Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Vide|];
    [|Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Vide|];
    [|Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide|];
    [|Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide|];
    [|Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide|];
    [|Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide|];
    [|Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide|]|] in 

    let bouchon = ref 0 in
    let bmp = Bmp.load "map.bmp" [] in
    let frame = ref 0 in
    let running = ref true in
    let x_p1 = ref 200 in
    let y_p1 = ref 350 in
    let is_sort = ref false in
    (* on va viser les 60 images secondes *)
    let minimal_frame_time = 1.0 /. 60. in
    (* on supprime la synchronisation automatique de l'écran avec le tampon *)
    auto_synchronize false;

    while !running do
        let start_time = Sys.time () in
        incr frame;
        clear_graph ();
        set_color black;

        Graphic_image.draw_image bmp 0 0;
        draw_player !x_p1 !y_p1;
      if button_down()
        then begin
          set_color (rgb 0 0 255);
          match mouse_pos() with
          |a,b -> 
          match coo_case a b with
          |x,y ->
              match coo_case_tab x y with
              |x',y' -> 
                match map.(x').(y') with
                |Vide -> bouchon := 1;
                |Plein -> bouchon := 2;
                |Libre -> x_p1 := x; 
                          y_p1 := y;
                |Spell -> is_sort := not !is_sort;
      
        end;
        
        if !is_sort
          then begin 
            set_color (rgb 0 0 255);
            range !x_p1 !y_p1 3;

          end;



        (* on rafraichit l'écran *)
        synchronize ();

        (* Une attente active si on va trop vite *)
        let t = Sys.time () in
        let dt = start_time +. minimal_frame_time -. t in
        if dt > 0.
        then Unix.sleepf dt
    done

