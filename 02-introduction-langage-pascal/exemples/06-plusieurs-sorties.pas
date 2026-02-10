{ ============================================================================
  Section 2.6 : Entrees/sorties console
  Description : Affichage de plusieurs valeurs sur une seule ligne
  Fichier source : 06-entrees-sorties-console.md
  ============================================================================ }
program PlusieursSorties;  
var  
  prenom: string;
  nom: string;
  age: integer;
begin
  prenom := 'Marie';
  nom := 'Dupont';
  age := 25;

  // Les virgules separent les arguments : chacun est affiche a la suite
  WriteLn('Bonjour ', prenom, ' ', nom, ', vous avez ', age, ' ans.');
end.
