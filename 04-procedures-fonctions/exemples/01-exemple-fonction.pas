{ ============================================================================
  Section 4.1 : Difference entre procedure et fonction
  Description : Fonction qui calcule le carre d'un nombre
  Fichier source : 01-difference-procedure-fonction.md
  ============================================================================ }
program ExempleFonction;

function CalculerCarre(nombre: Integer): Integer;  
begin  
  CalculerCarre := nombre * nombre;  // En Pascal, on retourne une valeur en assignant au nom de la fonction
end;

var
  resultat: Integer;
begin
  resultat := CalculerCarre(5);  // La fonction retourne 25
  WriteLn('Le carré de 5 est : ', resultat);
end.
