{ ============================================================================
  Section 2.6 : Entrees/sorties console
  Description : Affichage du contenu de variables de differents types
  Fichier source : 06-entrees-sorties-console.md
  ============================================================================ }
program AfficherVariables;  
var  
  nom: string;
  age: integer;
  taille: real;
  estEtudiant: boolean;
begin
  nom := 'Alice';
  age := 20;
  taille := 1.65;
  estEtudiant := true;

  WriteLn('Nom : ', nom);
  WriteLn('Âge : ', age);
  WriteLn('Taille : ', taille);       // real sans formatage => notation scientifique
  WriteLn('Étudiant : ', estEtudiant); // boolean s'affiche TRUE ou FALSE
end.
