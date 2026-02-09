{ ============================================================================
  Section 4.1 : Difference entre procedure et fonction
  Description : Exemple combinant procedure et fonction pour tester la parite
  Fichier source : 01-difference-procedure-fonction.md
  ============================================================================ }
program ProcedureVsFonction;

function EstPair(nombre: Integer): Boolean;
begin
  EstPair := (nombre mod 2 = 0);
end;

procedure AfficherParite(nombre: Integer);
begin
  if EstPair(nombre) then
    WriteLn(nombre, ' est pair.')
  else
    WriteLn(nombre, ' est impair.');
end;

var
  valeur: Integer;
begin
  Write('Entrez un nombre : ');
  ReadLn(valeur);

  // La fonction retourne une valeur (Boolean)
  if EstPair(valeur) then
    WriteLn('C''est un nombre pair !');

  // La procédure effectue une action (affichage)
  AfficherParite(valeur);
end.
