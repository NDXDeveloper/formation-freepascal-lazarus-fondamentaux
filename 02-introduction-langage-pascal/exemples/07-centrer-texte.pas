{ ============================================================================
  Section 2.7 : Formatage de sortie
  Description : Centrage de texte en calculant les espaces necessaires
  Fichier source : 07-formatage-sortie.md
  ============================================================================ }
program CentrerTexte;
const
  LargeurEcran = 50;
var
  texte: string;
  espaces: integer;
begin
  texte := 'TITRE CENTRÉ';
  espaces := (LargeurEcran - Length(texte)) div 2;

  // '':espaces = chaine vide formatee sur N caracteres, ce qui produit N espaces
  WriteLn('':espaces, texte);
end.
