{ ============================================================================
  Section 3.4 : Boucles conditionnelles (while-do, repeat-until)
  Description : Recherche d'un nombre dans un tableau avec boucle while
  Fichier source : 04-boucles-conditionnelles-while-repeat.md
  ============================================================================ }

program RechercheNombre;
const
  TAILLE = 10;
var
  nombres: array[1..TAILLE] of Integer;
  i, recherche: Integer;
  trouve: Boolean;
begin
  WriteLn('Entrez ', TAILLE, ' nombres :');
  for i := 1 to TAILLE do
  begin
    Write('Nombre ', i, ' : ');
    ReadLn(nombres[i]);
  end;
  WriteLn;
  Write('Nombre à rechercher : ');
  ReadLn(recherche);
  i := 1;
  trouve := False;
  while (i <= TAILLE) and (not trouve) do
  begin
    if nombres[i] = recherche then
      trouve := True
    else
      i := i + 1;
  end;
  WriteLn;
  if trouve then
    WriteLn('Nombre ', recherche, ' trouvé à la position ', i)
  else
    WriteLn('Nombre ', recherche, ' non trouvé');
end.
