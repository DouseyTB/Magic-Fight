open Type
open Char
open Map

let create_joueur id = 
  if id mod 2 = 0 then 
      let joueur = 
        { id = id ; personnages = 
    [
      {
      id = 0 ;
      coord = ref (5, 5);
      force = 70 ;
      vie = ref 30 ;
      pm = ref 5 ;
      pa = ref 10 ;
      attaque = {
        dmg = 20 ;
        range = 1 ;
        pa = 3 ;
        }
      };
      {
      id = 2 ;
      coord = ref (10, 5);
      force = 10 ;
      vie = ref 90 ;
      pm = ref 10 ;
      pa = ref 5 ;
      attaque = {
        dmg = 5 ;
        range = 7 ;
        pa = 1 ;
        }
      };
      {
      id = 4;
      coord = ref (15, 5);
      force = 50 ;
      vie = ref 50 ;
      pm =  ref 7;
      pa = ref 8 ;
      attaque = {
        dmg = 10 ;
        range = 2 ;
        pa = 2 ;
        }
      }
    ] 
  } in 
    joueur
  else 
      let joueur = 
     { id = id ; personnages = 
    [
      {
      id = 1 ;
      coord = ref (5, 15);
      force = 70 ;
      vie = ref 30 ;
      pm = ref 5 ;
      pa = ref 10 ;
      attaque = {
        dmg = 20 ;
        range = 1 ;
        pa = 3 ;
        }
      };
      {
      id = 3 ;
      coord = ref (10, 15);
      force = 50 ;
      vie = ref 50 ;
      pm =  ref 7;
      pa = ref 8 ;
      attaque = {
        dmg = 10 ;
        range = 2 ;
        pa = 1 ;
        }
      };
      {
      id = 5;
      coord = ref (15, 15);
      force = 10 ;
      vie = ref 90 ;
      pm = ref 10 ;
      pa = ref 5 ;
      attaque = {
        dmg = 5 ;
        range = 7 ;
        pa = 1 ;
        }
      }
    ] 
  } in
  joueur

let create_personnage i force vie pa pm x y dmg range paa =
  let perso = 
    {
      id = i ;
      coord = ref (x, y);
      force = force ;
      vie = ref vie ;
      pm = ref pm ;
      pa = ref pa ;
      attaque = {
        dmg = dmg ;
        range = range ;
        pa = paa ;
        }
      } in
  perso   

let test_pm p coord = !(p.pm) - (abs(fst coord - fst !(p.coord)) + abs(snd coord - snd !(p.coord))) >= 0 

let calcule_pm p coord = 
    p.pm := !(p.pm) - (abs(fst coord - fst !(p.coord)) + abs(snd coord - snd !(p.coord)))
      
let isValideMove map coord =  
    map.(fst coord).(snd coord) = Vide 

(* let actualise_coord j cpt x y = 
    let perso = getP j cpt in
    perso.coord := (x,y) *)

let rec move map j i coords =
    let personnage = List.nth j.personnages i in 
    Printf.printf "%d\n" (fst !(personnage.coord));
    if isValideMove map coords && test_pm personnage coords then begin
      calcule_pm personnage coords ;
      map.(fst !(personnage.coord)).(snd !(personnage.coord)) <- Vide ;
      personnage.coord := coords;  
      Printf.printf "%d\n" (fst !(personnage.coord));
      end
    else  
      begin
        Printf.printf "%s\n" ("Le coup n'as pas pu être jouer ");
        Printf.printf "Où voulez-vous vous déplacer ?\n";
        Printf.printf "%s\n" ("x ? ") ; 
        let x = read_int() in 
        Printf.printf "%s\n" ("y ? ") ; 
        let y = read_int() in 
        move map j i (x,y) ;
      end   
 
let kill j = 
    List.iter (fun x -> x.vie := 0 ) j.personnages 

let calcule_attaque p1 p2 = 
    if !(p1.pa) >= p1.attaque.pa then begin
      if p1.force/10 + p1.attaque.dmg <= !(p2.vie) then  
        p2.vie := !(p2.vie) - p1.force/10 + p1.attaque.dmg 
      else 
        p2.vie := 0 ;
      p1.pa := !(p1.pa) - p1.attaque.pa ;
    end
    else 
      Printf.printf ("Vous n'avez pas assez de points d'actions pour effectuer cette action.")
    

let attaque m j i coord = 
  let personnage = List.nth j.personnages i in
  match m.(fst coord).(snd coord) with 
    |Mur -> Printf.printf ("Tu fonces dans le mur...") 
    |Vide -> Printf.printf ("Bravo t'as reussi à faire mal à Casper")
    |Personnage p -> calcule_attaque personnage p  

    
let int_of_L liste id =  int_of_string (List.nth liste id)
let string_of_p p = 

string_of_int p.force ^ "|" ^
string_of_int !(p.vie) ^ "|" ^
string_of_int !(p.pa) ^ "|" ^
string_of_int !(p.pm) ^ "|" ^
string_of_int (fst !(p.coord)) ^ "*" ^
string_of_int (snd !(p.coord)) ^ "|" ^
string_of_int p.attaque.dmg ^ "|" ^
string_of_int p.attaque.range ^ "|" ^
string_of_int p.attaque.pa ^ "\n"

let p_of_string s id =
  let indexS = String.index s '*' in 
  s.[indexS] <- '|' ; 
  let listp = String.split_on_char '|' s in
  let p = create_personnage id (int_of_L listp 0) (int_of_L listp 1) (int_of_L listp 2) (int_of_L listp 3) 
  (int_of_L listp 4)(int_of_L listp 5) (int_of_L listp 6) (int_of_L listp 7) (int_of_L listp 8) in 
  p 

let send_perso out_chan p = 
  let y = string_of_p p in
  (* Printf.printf "%s\n" y ; *)
  output_string out_chan y;
  flush out_chan  

let convert_persos j1 j2 s id = 
  let list1 = [] in
  let list2 = [] in 
  let stringL = String.split_on_char ';' s in 
    if id = 0 then begin 
      for i = 0 to 5 do 
        if i mod 2 = 0 then 
          List.cons (p_of_string (getL stringL i) i) list1 
        else 
          List.cons (p_of_string (getL stringL i) i) list2 ;
      done
      end    
    else
    begin 
        for i = 0 to 5 do 
        if i mod 2 = 1 then 
          List.cons (p_of_string (getL stringL i) (i-1)) list1 
        else 
          List.cons (p_of_string (getL stringL i) (i-1)) list2 ;
        done
    end;  

  j1.personnages = list1;
  j2.personnages = list2

let print_perso j1 j2 = 
    let allp1 = List.fold_left (fun acc x -> acc ^ "\n" ^ (string_of_p x)) "" (j1.personnages) in 
    let allp2 = List.fold_left (fun acc x -> acc ^ "\n" ^ (string_of_p x)) "" (j2.personnages) in 
    Printf.printf "Voici vos personnages : %s\n%!" allp1 ;
    Printf.printf "Voici les personnage de votre adversaire %s\n%!" allp2
