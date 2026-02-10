{ ============================================================================
  Section 4.3 : Parametres par valeur
  Description : Demonstration que le parametre par valeur est une copie
  Fichier source : 03-parametres-par-valeur.md
  ============================================================================ }
program DemonstrationValeur;

procedure ModifierNombre(n: Integer);  
begin  
  WriteLn('Dans la procédure, n vaut : ', n);
  n := n + 10;  // Modification de la copie
  WriteLn('Dans la procédure, n vaut maintenant : ', n);
end;

var
  nombre: Integer;
begin
  nombre := 5;
  WriteLn('Avant l''appel, nombre vaut : ', nombre);

  ModifierNombre(nombre);

  WriteLn('Après l''appel, nombre vaut : ', nombre);
end.
