{ ============================================================================
  Section 2.7 : Formatage de sortie
  Description : Tableau de noms et villes avec colonnes alignees
  Fichier source : 07-formatage-sortie.md
  ============================================================================ }
program TableauNoms;  
begin  
  WriteLn('Prénom':15, 'Nom':15, 'Ville':20);
  WriteLn('--------------':15, '--------------':15, '-------------------':20);
  WriteLn('Alice':15, 'Dupont':15, 'Paris':20);
  WriteLn('Bob':15, 'Martin':15, 'Lyon':20);
  WriteLn('Charlie':15, 'Durand':15, 'Marseille':20);
end.
