{ ============================================================================
  Section 3.6 : Imbrication de structures
  Description : Calculatrice de statistiques - moyennes par ligne, colonne et generale
  Fichier source : 06-imbrication-structures.md
  ============================================================================ }
program StatistiquesMatrice;  
const  
  LIGNES = 3;
  COLS = 4;
var
  donnees: array[1..LIGNES, 1..COLS] of Real;
  i, j: Integer;
  sommeLigne, sommeColonne, sommeTotal: Real;
  moyenneLigne, moyenneColonne, moyenneGenerale: Real;
begin
  WriteLn('=== SAISIE DES DONNÉES ===');
  WriteLn;

  // Saisie
  for i := 1 to LIGNES do
  begin
    WriteLn('Ligne ', i, ' :');
    for j := 1 to COLS do
    begin
      Write('  Colonne ', j, ' : ');
      ReadLn(donnees[i, j]);
    end;
  end;

  WriteLn;
  WriteLn('=== TABLEAU ===');

  // Affichage avec statistiques par ligne
  sommeTotal := 0;

  for i := 1 to LIGNES do
  begin
    sommeLigne := 0;

    for j := 1 to COLS do
    begin
      Write(donnees[i, j]:8:2);
      sommeLigne := sommeLigne + donnees[i, j];
    end;

    moyenneLigne := sommeLigne / COLS;
    WriteLn('  | Moy: ', moyenneLigne:6:2);
    sommeTotal := sommeTotal + sommeLigne;
  end;

  WriteLn;

  // Statistiques par colonne
  Write('Moyennes colonnes : ');
  for j := 1 to COLS do
  begin
    sommeColonne := 0;

    for i := 1 to LIGNES do
      sommeColonne := sommeColonne + donnees[i, j];

    moyenneColonne := sommeColonne / LIGNES;
    Write(moyenneColonne:8:2);
  end;

  WriteLn;
  WriteLn;

  // Moyenne générale
  moyenneGenerale := sommeTotal / (LIGNES * COLS);
  WriteLn('Moyenne générale : ', moyenneGenerale:0:2);
end.
