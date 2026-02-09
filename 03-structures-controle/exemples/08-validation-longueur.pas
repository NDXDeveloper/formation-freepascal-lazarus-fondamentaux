{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Validation de longueur - mot de passe entre 8 et 20 caracteres
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program ValidationLongueur;
var
  motDePasse: String;
const
  LONGUEUR_MIN = 8;
  LONGUEUR_MAX = 20;
begin
  repeat
    Write('Mot de passe (', LONGUEUR_MIN, '-', LONGUEUR_MAX, ' caractères) : ');
    ReadLn(motDePasse);

    if Length(motDePasse) < LONGUEUR_MIN then
      WriteLn('❌ Trop court (min ', LONGUEUR_MIN, ' caractères)')
    else if Length(motDePasse) > LONGUEUR_MAX then
      WriteLn('❌ Trop long (max ', LONGUEUR_MAX, ' caractères)');
  until (Length(motDePasse) >= LONGUEUR_MIN) and
        (Length(motDePasse) <= LONGUEUR_MAX);

  WriteLn('✓ Mot de passe accepté');
end.
