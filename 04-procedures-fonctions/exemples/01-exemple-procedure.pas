{ ============================================================================
  Section 4.1 : Difference entre procedure et fonction
  Description : Procedure simple qui affiche un message
  Fichier source : 01-difference-procedure-fonction.md
  ============================================================================ }
program ExempleProcedure;

procedure AfficherMessage;
begin
  WriteLn('Bonjour ! Bienvenue dans le programme.');
end;

begin
  AfficherMessage;  // Appel de la procédure
  WriteLn('Fin du programme.');
end.
