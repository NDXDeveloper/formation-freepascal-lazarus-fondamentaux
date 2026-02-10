{ ============================================================================
  Section 3.9 : Debogage pas a pas
  Description : Pas a pas approfondi (Step Over - F8), execute sans entrer
                dans les fonctions
  Fichier source : 09-debogage-pas-a-pas.md
  ============================================================================ }
program StepOver;

procedure Afficher(x: Integer);  
begin  
  WriteLn('Valeur : ', x);
end;

begin
  WriteLn('Début');        // Point d'arrêt ici
  Afficher(10);           // F8 : exécute toute la procédure
  WriteLn('Fin');          // On arrive ici
end.
