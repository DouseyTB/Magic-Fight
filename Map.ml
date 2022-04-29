open Type
open Printf
open Char
open String
open Unix


let create_map h l =
let m = Array.make_matrix h l Vide in
    for i = 0 to h-1 do
        (m.(0)).(i) <- Mur ;
        (m.(h-1)).(i) <- Mur 
    done ;
    for j = 1 to l-2 do
        m.(j).(0) <- Mur ;
        m.(j).(l-1) <- Mur
    done ;
    m

let create_personnageF i =
  let perso = 
    {
      id = i ;
      coord = ref (5, 20);
      force = 1 ;
      vie = ref 1 ;
      pm = ref 1 ;
      pa = ref 1 ;
      attaque = {
        dmg = 1 ;
        range = 1 ;
        pa = 1 ;
        }
      } in
  perso    

let getL liste id =
    List.nth liste id 
    
let getP joueur i =
  List.nth joueur.personnages i  
       
let string_of_cell c = match c with 
    |Mur -> "#" 
    |Vide  -> " "
    |Personnage p -> string_of_int p.id 

let cell_of_string c j1 j2 = match c with 
    |'#' -> Mur
    |' ' -> Vide
    |i -> match i with 
            |'0' -> Personnage (getP j1 0)
            |'1' -> Personnage (getP j2 0)
            |'2' -> Personnage (getP j1 1)
            |'3' -> Personnage (getP j2 1)
            |'4' -> Personnage (getP j1 2)
            |'5' -> Personnage (getP j2 2)
            (* if i = '5' then Personnage (getP j2 3) else 
            if (int_of_char i) mod 2 = 0 then Personnage (getP j1 ((int_of_char i)/2))
            else Personnage (getP j2 (((int_of_char i)-1)/2)) *)


let string_of_line m =
        Array.fold_left(fun acc x -> acc ^ " "^ string_of_cell x )  ""  m

let line_of_string s j1 j2= 
    let arrayl = Array.make (String.length s) Vide in 
    for i=0 to (Array.length arrayl)-1 do
        arrayl.(i) <- cell_of_string s.[i] j1 j2
    done;    
    arrayl
        
let string_of_map m =
        Array.fold_left (fun acc x -> acc ^ "\n" ^ string_of_line x ) "" m   

let print_map m = Printf.printf "%s\n%!" (string_of_map m) 

let map_of_string s j1 j2 =
    let l = String.split_on_char '|' s in
    let arrayl = Array.make_matrix (String.length (getL l 0)) (List.length l) Vide in 
    for i=0 to (Array.length arrayl)-1 do
        arrayl.(i) <- line_of_string (getL l i) j1 j2
    done;    
    arrayl

(* let Action = match read_line ().[0] with 
        "1" -> 
        "2" -> deplace_joueur map  read_line ().[0] *)

let lireString x = 
    let listS = String.split_on_char '|' x in
    let list = List.map (fun x -> int_of_string x) listS in
    list

let clear_map map = 
    for i = 0 to (Array.length map)-1 do
        for j = 1 to (Array.length map.(0))-1 do
            match map.(i).(j) with 
                |Personnage x -> map.(i).(j) <- Vide
                | _ -> ()
        done
    done            

let actualise map joueur1 joueur2 =
    clear_map map ; 
    for i = 0 to 2 do
        let list1 = getP joueur1 i in 
        let list2 = getP joueur2 i in 
        map.(fst !(list1.coord)).(snd !(list1.coord)) <- Personnage list1 ;
        map.(fst !(list2.coord)).(snd !(list2.coord)) <- Personnage list2 ; 
    done ;
    print_map map
    
let print_inputmap s = 
    let l = String.split_on_char '|' s in
    List.iter (fun x -> Printf.printf "%s\n%!" x) l  




