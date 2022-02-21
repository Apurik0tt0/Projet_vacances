open Graphics
(* L'objectif ici est d'essayer de programmer une boucle de gestion
   des événements sans latence *)

type entite =
   {
     nom : string;
     mutable vie : int;
     mutable pa : int;
     mutable pm: int;
     mutable coo_x : int;
     mutable coo_y : int;
   
   }
 
type spell =
 {
   nom : string;
   forme : string;
   cout : int;
   po : int;
 }
 
type case = Vide | Plein | Libre | Spell of string


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

let color_case_tab x y =
    fill_rect (50*x) (50*y) 50 50

let ligne_de_vue x y x' y' map =
  set_color (rgb 0 0 255);
  let debug = ref 0 in
  let vide = ref true in
  let i = ref 0. in
  let y_case = ref 0. in
  let pente = (float_of_int y' -. float_of_int y) /. (float_of_int x' -. float_of_int x) in

  if x < x' 
    then begin

      while !vide do
        i := !i +. 1.;
        if (!i = (float_of_int x' *. 50. +. 25.) -. (float_of_int x *. 50. +. 25.) )   then vide :=false;
        y_case := ( ((float_of_int y) *. 50. +. 25.) +. pente *. !i );
        if !y_case = 200. then vide :=false;
        match coo_case_tab (x * 50 + 25 + int_of_float !i) (int_of_float !y_case) with
        |(a,b) -> 
          match map.(a).(b) with 
          |Plein -> set_color (rgb 0 0 155);
          |Libre -> color_case_tab a b;
          |_ -> debug := 1;
      done;
    end
  else if x > x' 
    then begin 

      while !vide do
        i := !i -. 1.;
        if (!i = (float_of_int x' *. 50. +. 25.) -. (float_of_int x *. 50. +. 25.) )   then vide :=false;
        y_case := ( ((float_of_int y) *. 50. +. 25.) +. pente *. !i );
        if !y_case = 200. then vide :=false;
        match coo_case_tab (x * 50 + 25 + int_of_float !i) (int_of_float !y_case) with
        |(a,b) -> 
          match map.(a).(b) with 
          |Plein -> set_color (rgb 0 0 155);
          |Libre -> color_case_tab a b;
          |_ -> debug := 1;
      done;
    end
  else begin
    if y < y'
      then begin
        i := (float_of_int y) *. 50.;
        while !vide do
          i := !i +. 50. ;
          if int_of_float !i = y' * 50 then vide := false ;
          match coo_case_tab (x*50) (int_of_float !i) with
          |a,b -> 
            match map.(a).(b) with
            |Plein -> set_color (rgb 0 0 155);
            |Libre -> color_case_tab a b;
            |_ -> debug := 1;
        done;
      end
    else begin
      i := (float_of_int y) *. 50. ;
      while !vide do
        i := !i -. 50. ;
        if int_of_float !i = y' * 50 then vide := false ;
        match coo_case_tab (x * 50) (int_of_float !i) with
        |a,b -> 
          match map.(a).(b) with
          |Plein -> set_color (rgb 0 0 155);
          |Libre -> color_case_tab a b;
          |_ -> debug := 1;
      done;
    end;
  end
    
    

(* affiche la ligne de vue du sort a partir des coo données*)
(*let range x y po map =
  let triangle = ref 0 in
    match coo_case_tab x y with
    |x',y' ->
      for i = x'-po to x'+po do
        for j = y'- !triangle to y'+ !triangle do
          ligne_de_vue x y i j map;
        done;
      if i<x'
        then incr triangle
      else
        decr triangle; 
      done*)
let range x y map =
  ligne_de_vue x y (x-3) y map;
  ligne_de_vue x y (x-2) (y-1) map;
  ligne_de_vue x y (x-1) (y-2) map;
  ligne_de_vue x y (x+1) (y-2) map;
  ligne_de_vue x y (x+2) (y-1) map;
  ligne_de_vue x y (x+3) y map;
  ligne_de_vue x y (x+2) (y+1) map;
  ligne_de_vue x y (x+1) (y+2) map;
  ligne_de_vue x y (x-1) (y+2) map;
  ligne_de_vue x y (x-2) (y+1) map;
  ligne_de_vue x y x (y + 3) map;
  ligne_de_vue x y x (y - 3) map

  
(* positionne le joueur aux coordonées données*)
let draw_player x y =
  let player1 = Bmp.load "Images/iop.bmp" [] in
  match coo_case x y with
  |a,b -> Graphic_image.draw_image player1 a b


let _ =

    open_graph " 1250x1000";

    let map = [|[|Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide|];
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
    [|Spell "Pression";Spell "Fracture";Spell "Duel";Spell "Emprise";Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Vide|];
    [|Spell "Bond";Spell "Determination";Spell "Tempete de puissance";Spell "Tumulte";Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Plein;Libre;Libre;Libre;Libre;Vide|];
    [|Spell "Deferlement";Spell "Anneau destructeur";Spell "Epee celeste";Spell "Zenith";Libre;Libre;Libre;Libre;Libre;Plein;Libre;Libre;Libre;Libre;Libre;Plein;Libre;Libre;Libre;Vide|];
    [|Spell "Epee divine";Spell "Couperet";Spell "Puissance";Spell "Vertu";Libre;Libre;Libre;Libre;Libre;Libre;Plein;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Vide|];
    [|Spell "Epee destructrice";Spell "Accumulation";Spell "Precipitation";Spell "Agitation";Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Vide|];
    [|Spell "Intimidation";Spell "Conquete";Spell "Fustigation";Spell "Endurance";Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Libre;Vide|];
    [|Spell "Souffle";Spell "Violence";Spell "Epee de iop";Spell "Pugilat";Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide|];
    [|Spell "Massacre";Spell "Rassemblement";Spell "Vitalite";Spell "Vindicte";Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide|];
    [|Spell "Concentration";Spell "Epee de jugement";Spell "Epee du destin";Spell "Tannee";Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide|];
    [|Spell "Ferveur";Spell "Menace";Spell "Fendoir";Spell "Sentence";Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide|];
    [|Spell "Friction";Spell "Coup pour coup";Spell "Colere de iop";Spell "Fureur";Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide;Vide|]|] in 

    let bouchon = ref 0 in
    let bmp = Bmp.load "Images/map.bmp" [] in
    let spell = Bmp.load "Images/spell.bmp" [] in
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
        Graphic_image.draw_image spell 700 0;
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
                |Spell nom -> is_sort := not !is_sort;
                              moveto x y;
                              draw_string nom;
      
        end;
        
       (* match mouse_pos() with
        |x,y -> 
          match coo_case_tab x y with
          |x',y' -> 
            match map.(x').(y') with
            |Vide -> bouchon := 0;
            |Plein -> bouchon := 1;
            |Libre -> bouchon := 0;
            |Spell nom -> moveto x y;
                          draw_string nom; *)

        if !is_sort
          then begin 
            set_color (rgb 0 0 255);
            match coo_case_tab !x_p1 !y_p1 with
            |a,b ->
              range a b map;

          end;


        (* on rafraichit l'écran *)
        synchronize ();

        (* Une attente active si on va trop vite *)
        let t = Sys.time () in
        let dt = start_time +. minimal_frame_time -. t in
        if dt > 0.
        then Unix.sleepf dt
    done

