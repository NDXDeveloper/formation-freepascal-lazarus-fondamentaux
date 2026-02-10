{ ============================================================================
  Section 3.5 : Instructions break et continue
  Description : Recherche du premier nombre pair et du premier multiple de 5
  Fichier source : 05-instructions-break-continue.md
  ============================================================================ }
program RechercheMulticriteres;  
const  
  TAILLE = 10;
var
  nombres: array[1..TAILLE] of Integer;
  i, premierPair, premierMultiple5: Integer;
begin
  // Génération de nombres
  Randomize;
  WriteLn('Nombres générés :');
  for i := 1 to TAILLE do
  begin
    nombres[i] := Random(50) + 1;
    Write(nombres[i], ' ');
  end;
  WriteLn;
  WriteLn;

  // Recherche du premier nombre pair
  premierPair := -1;
  for i := 1 to TAILLE do
  begin
    if (nombres[i] mod 2) = 0 then
    begin
      premierPair := nombres[i];
      break;  // Trouvé !
    end;
  end;

  // Recherche du premier multiple de 5
  premierMultiple5 := -1;
  for i := 1 to TAILLE do
  begin
    if (nombres[i] mod 5) = 0 then
    begin
      premierMultiple5 := nombres[i];
      break;  // Trouvé !
    end;
  end;

  WriteLn('Premier nombre pair : ', premierPair);
  WriteLn('Premier multiple de 5 : ', premierMultiple5);
end.
