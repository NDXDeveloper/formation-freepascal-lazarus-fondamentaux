{ ============================================================================
  Section 6.4 : Pointeurs et Tableaux
  Description : Tableau dynamique array of avec redimensionnement SetLength
  Fichier source : 04-pointeurs-tableaux.md
  ============================================================================ }
program ArrayOfRedim;  
var  
  donnees: array of Integer;
begin
  // Démarrer petit
  SetLength(donnees, 5);
  donnees[0] := 100;

  // Agrandir dynamiquement
  SetLength(donnees, 10);  // Garde les 5 premiers éléments
  donnees[9] := 200;

  WriteLn('Premier : ', donnees[0]);   // 100
  WriteLn('Dernier : ', donnees[9]);   // 200
  WriteLn('Taille : ', Length(donnees));  // 10
end.
