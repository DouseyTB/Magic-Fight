open Map
open Type
open Joueur
open String 
open Printf
open Unix


let () = 
    Printf.printf "Voulez-vous jouer en réseau ou en local ? (1|2) \n%!" ;
    if read_line () = "1" then begin 
    let joueurAMI = ref (create_joueur 0) in
    let perso1 = getP !(joueurAMI) 0 in
    let perso2 = getP !(joueurAMI) 1 in
    let perso3 = getP !(joueurAMI) 2 in 
    let joueurENEMI = ref (create_joueur 1) in 

    let s = socket PF_INET SOCK_STREAM 0 in
    connect s (ADDR_INET(inet_addr_of_string "92.89.116.186",6566));
    let in_chan = in_channel_of_descr s in
    let out_chan = out_channel_of_descr s in


    output_string out_chan "Seydou\n" ;
    send_perso out_chan perso1 ;
    send_perso out_chan perso2 ;
    send_perso out_chan perso3 ;

    while true do
        let input_ids = input_line in_chan in        
        Printf.printf "C'est au tour du joueur %s\n%!" input_ids ;
        let input_id = int_of_string input_ids in
        if (input_id = 6) then begin 
            Printf.printf "Bravo, Vous avez gagné\n%!" ; 
            exit 0 
            end;
        if (input_id = 7) then begin 
            Printf.printf "Dommage, vous avez perdu ...\n%!" ; 
            exit 0 
            end;
        let input_maps = input_line in_chan in 
        let map = map_of_string input_maps !joueurAMI !joueurENEMI in 
        let input_personnages = input_line in_chan in 
        print_map map ;
        convert_persos !joueurAMI !joueurENEMI input_personnages input_id; 
        print_perso !joueurAMI !joueurENEMI;
        let actions = ref "" in
        Printf.printf "%s\n%!" "Voulez-vous déplacer votre personnage (y/n) ?" ;
            if read_line() = "y" then begin 
                Printf.printf "Où voulez-vous le déplacer ?\n";
                Printf.printf "%s\n%!" ("En x ?") ; 
                let xd = read_line() in 
                Printf.printf "%s\n%!" ("en y ?") ; 
                let yd = read_line() in 
                Printf.printf "Voulez-vous attaquer (y/n)?\n" ;
                if read_line() = "y" then begin
                    Printf.printf "En x ?\n" ; 
                    let xa = read_line() in 
                    Printf.printf "En y ?\n" ; 
                    let ya = read_line() in 
                    actions := "2|" ^ xd ^ "|" ^ yd ^ ";1|" ^ xa ^ "|" ^ ya ^ "\n" ;
                    end      
                else
                    actions := "2|" ^ xd ^ "|" ^ yd ^ "\n" ;
            end        
            else
                begin     
                Printf.printf "Voulez-vous attaquer (y/n)?\n" ;
                if read_line() = "y" || read_line() = "Y"  then begin 
                    Printf.printf  "En x ?\n" ; 
                    let xa = read_line() in 
                    Printf.printf "En y ?\n" ; 
                    let ya = read_line() in 
                    actions := "1|"^ xa ^ "|" ^ ya ^ "\n" ; 
                end         
                else 
                    actions := "\n" ;     
                end ;           

        Printf.printf "Envoi de vos actions au serveur : %s\n%!" !actions ;                
        output_string out_chan !actions ;
        flush out_chan;
        done ;
    close s ;
    end 
    else begin
    let map = create_map 20 20 in
    let joueur1 = create_joueur 2 in
    let joueur2 = create_joueur 3 in
    actualise map joueur1 joueur2 ; 
    let stop = ref false in 
    while true && not !stop do 
        for i = 0 to 2 do  
            Printf.printf "%s\n" ("C'est au tour du personnage " ^ string_of_int (i+1) ^" du joueur 1 de jouer") ;
            Printf.printf "%s\n" ("Voulez-vous passer votre tour ? (y|n)") ;
            if read_line() = "n" then  
                Printf.printf "%s\n" ("Vous voulez attaquer ou vous deplacer ? (1|2)") ;
                if read_int() = 1 then begin
                    Printf.printf "%s\n" (" Attaquer quelle position ?\n") ;
                    Printf.printf "%s\n" ("x ? ") ; 
                    let x = read_int() in 
                    Printf.printf "%s\n" ("y ? ") ; 
                    let y = read_int() in 
                    attaque map joueur1 i (x,y) ;
                    end     
                else
                    Printf.printf "Où voulez-vous vous déplacer ?\n";
                    Printf.printf "%s\n" ("x ? ") ; 
                    let x = read_int() in 
                    Printf.printf "%s\n" ("y ? ") ; 
                    let y = read_int() in 
                    move map joueur1 i (x,y) ;
        
            actualise map joueur1 joueur2 ; 
  
            if (List.for_all (fun x -> !(x.vie) = 0) joueur2.personnages) then begin
                Printf.printf "%s\n" ("Le joueur 1 a gagné. ") ;
                stop := true ;
                exit 0;
            end ;
               
            Printf.printf "%s\n" ("C'est au tour du personnage " ^ string_of_int (i+1) ^" du joueur 2 de jouer") ;
            Printf.printf "%s\n" ("Voulez-vous passer votre tour ? (y|n)") ;
            if read_line() = "n" then begin
                Printf.printf "%s\n" ("Vous voulez attaquer ou vous deplacer ? (1|2)") ;
                if read_int() = 1 then begin
                    Printf.printf "%s\n" (" Attaquer quelle position ?\n") ;
                    Printf.printf "%s\n" ("x ? ") ; 
                    let x = read_int() in 
                    Printf.printf "%s\n" ("y ? ") ; 
                    let y = read_int() in 
                    attaque map joueur2 i (x,y) ;
                    end     
                else
                    Printf.printf "Où voulez-vous vous déplacer ?\n";
                    Printf.printf "%s\n" ("x ? ") ; 
                    let x = read_int() in 
                    Printf.printf "%s\n" ("y ? ") ; 
                    let y = read_int() in 
                    move map joueur2 i (x,y) ;        
                end
            else 
                actualise map joueur1 joueur2 ;

        if (List.for_all (fun x -> !(x.vie) = 0) joueur1.personnages) then begin
            Printf.printf "%s\n" ("Le joueur 2 a gagné. ") ;
            stop := true ;
            exit 0;
            end ;
    
        done   
    done 
    end 