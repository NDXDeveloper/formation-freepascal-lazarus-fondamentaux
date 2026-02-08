{ ============================================================================
  Section 1.5 : Algorithmes et pseudo-code
  Description : Calcule l'aire d'un rectangle a partir de la longueur et
                largeur saisies par l'utilisateur
  Fichier source : 05-algorithmes-pseudo-code.md
  ============================================================================ }
program AireRectangle;
var
  longueur, largeur, aire: Real;
begin
  WriteLn('Entrez la longueur : ');
  ReadLn(longueur);

  WriteLn('Entrez la largeur : ');
  ReadLn(largeur);

  aire := longueur * largeur;

  WriteLn('L''aire du rectangle est : ', aire:0:2);
end.
