{ ============================================================================
  Section 5.1 : Tableaux statiques unidimensionnels
  Description : Recherche de la temperature maximale dans un tableau
  Fichier source : 01-tableaux-statiques-unidimensionnels.md
  ============================================================================ }
program RechercheMax;  
var  
  temperatures: array[1..7] of Real;
  i: Integer;
  max: Real;
begin
  // Saisie des températures de la semaine
  WriteLn('Entrez les températures de la semaine :');
  for i := 1 to 7 do
  begin
    Write('Jour ', i, ' : ');
    ReadLn(temperatures[i]);
  end;

  // Recherche du maximum
  max := temperatures[1];  // On commence par la première valeur
  for i := 2 to 7 do
  begin
    if temperatures[i] > max then
      max := temperatures[i];
  end;

  WriteLn('La température maximale est : ', max:0:1, ' degrés');
end.
