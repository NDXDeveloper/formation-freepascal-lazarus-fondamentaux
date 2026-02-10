{ ============================================================================
  Section 3.6 : Imbrication de structures
  Description : Tri a bulles avec affichage des passes et echanges
  Fichier source : 06-imbrication-structures.md
  ============================================================================ }
program TriBulles;  
const  
  TAILLE = 8;
var
  tableau: array[1..TAILLE] of Integer;
  i, j, temp: Integer;
  echange: Boolean;
  passe: Integer;
begin
  // Génération aléatoire
  Randomize;
  WriteLn('Tableau initial :');
  for i := 1 to TAILLE do
  begin
    tableau[i] := Random(100);
    Write(tableau[i]:4);
  end;
  WriteLn;
  WriteLn;

  WriteLn('=== TRI EN COURS ===');
  passe := 0;

  // Tri à bulles
  for i := 1 to TAILLE - 1 do
  begin
    echange := False;
    passe := passe + 1;
    WriteLn('Passe ', passe, ' :');

    for j := 1 to TAILLE - i do
    begin
      if tableau[j] > tableau[j + 1] then
      begin
        // Échange
        temp := tableau[j];
        tableau[j] := tableau[j + 1];
        tableau[j + 1] := temp;
        echange := True;

        Write('  Échange : ', temp, ' <-> ', tableau[j]);
        WriteLn;
      end;
    end;

    // Affichage après chaque passe
    Write('  Résultat : ');
    for j := 1 to TAILLE do
      Write(tableau[j]:4);
    WriteLn;
    WriteLn;

    // Optimisation : si pas d'échange, le tableau est trié
    if not echange then
    begin
      WriteLn('Tableau trié ! Arrêt anticipé.');
      break;
    end;
  end;

  WriteLn('=== TABLEAU FINAL ===');
  for i := 1 to TAILLE do
    Write(tableau[i]:4);
  WriteLn;
end.
