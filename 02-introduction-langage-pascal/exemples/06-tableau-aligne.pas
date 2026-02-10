{ ============================================================================
  Section 2.6 : Entrees/sorties console
  Description : Tableau avec colonnes alignees grace au formatage
  Fichier source : 06-entrees-sorties-console.md
  ============================================================================ }
program TableauAligne;  
begin  
  // :N fonctionne aussi sur les litteraux (strings et entiers), pas seulement les variables
  WriteLn('Nom':15, 'Age':5, 'Ville':15);
  WriteLn('---------------':15, '-----':5, '---------------':15);
  WriteLn('Alice':15, 25:5, 'Paris':15);
  WriteLn('Bob':15, 30:5, 'Lyon':15);
  WriteLn('Charlie':15, 28:5, 'Marseille':15);
end.
