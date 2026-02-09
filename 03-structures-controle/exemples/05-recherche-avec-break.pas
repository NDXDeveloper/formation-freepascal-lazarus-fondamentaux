{ ============================================================================
  Section 3.5 : Instructions break et continue
  Description : Recherche dans un tableau avec arret par break
  Fichier source : 05-instructions-break-continue.md
  ============================================================================ }
program RechercheAvecBreak;
const
  TAILLE = 10;
var
  nombres: array[1..TAILLE] of Integer;
  i, recherche: Integer;
  position: Integer;
begin
  // Remplir le tableau
  WriteLn('Entrez ', TAILLE, ' nombres :');
  for i := 1 to TAILLE do
  begin
    Write('Nombre ', i, ' : ');
    ReadLn(nombres[i]);
  end;

  Write('Nombre à rechercher : ');
  ReadLn(recherche);

  // Recherche
  position := -1;  // -1 signifie "non trouvé"

  for i := 1 to TAILLE do
  begin
    if nombres[i] = recherche then
    begin
      position := i;
      break;  // Trouvé ! Inutile de continuer
    end;
  end;

  WriteLn;
  if position <> -1 then
    WriteLn('Nombre trouvé à la position ', position)
  else
    WriteLn('Nombre non trouvé');
end.
