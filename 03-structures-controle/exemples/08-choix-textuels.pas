{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Choix parmi des options textuelles avec conversion en minuscules
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program ChoixTextuels;  
uses  
  SysUtils;
var
  couleur: String;
  valide: Boolean;
begin
  WriteLn('Couleurs disponibles : rouge, vert, bleu');

  repeat
    Write('Votre couleur : ');
    ReadLn(couleur);
    couleur := LowerCase(couleur);  // LowerCase (SysUtils) agit sur String, UpCase sur Char seul

    valide := (couleur = 'rouge') or (couleur = 'vert') or (couleur = 'bleu');

    if not valide then
      WriteLn('❌ Couleur non disponible');
  until valide;

  WriteLn('✓ Couleur choisie : ', couleur);
end.
