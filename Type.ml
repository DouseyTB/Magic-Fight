type attaque = 
  {
    dmg : int ;
    range : int ;
    pa : int ;
  }
  
  type personnage =
  {
    id : int ; 
    coord : (int * int) ref; 
    force : int ;
    vie : int ref;
    pm : int ref ;
    pa : int ref;
    attaque : attaque ;
  }

type cell  =
| Mur   
| Vide
| Personnage of personnage 

type map = cell array array  


type joueur = {id : int ; mutable personnages : personnage list}

