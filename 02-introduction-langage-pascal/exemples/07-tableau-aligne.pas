{ ============================================================================
  Section 2.7 : Formatage de sortie
  Description : Tableau avec colonnes de nombres alignees
  Fichier source : 07-formatage-sortie.md
  ============================================================================ }
program TableauAligneNum;  
begin  
  WriteLn('Numéro':8, 'Quantité':12, 'Prix':10);
  WriteLn('------':8, '--------':12, '----':10);
  WriteLn(1:8, 150:12, 2500:10);
  WriteLn(2:8, 75:12, 1200:10);
  WriteLn(3:8, 230:12, 3800:10);
end.
