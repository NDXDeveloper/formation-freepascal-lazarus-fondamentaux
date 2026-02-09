{ ============================================================================
  Section 3.9 : Debogage pas a pas
  Description : Pas a pas detaille (Step Into - F7), entre dans les fonctions
  Fichier source : 09-debogage-pas-a-pas.md
  ============================================================================ }
program StepInto;

procedure Afficher(x: Integer);
begin
  WriteLn('Valeur : ', x);  // F7 vous amène ici
end;

begin
  WriteLn('Début');        // Point d'arrêt ici
  Afficher(10);           // F7 : entre dans la procédure
  WriteLn('Fin');
end.
