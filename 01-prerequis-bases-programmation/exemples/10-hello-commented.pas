{ ============================================================================
  Section 1.10 : Premier programme - Hello World en Pascal
  Description : Les deux styles de commentaires en Pascal :
                // commentaire sur une ligne
                (* commentaire sur plusieurs lignes *)
  Fichier source : 10-premier-programme-hello-world-pascal.md
  ============================================================================ }
program HelloCommented;  
begin  
  // Ceci est un commentaire sur une ligne
  WriteLn('Hello, World!');

  { Ceci est un commentaire
    sur plusieurs lignes
    très utile ! }
  WriteLn('Programme terminé.');
end.
