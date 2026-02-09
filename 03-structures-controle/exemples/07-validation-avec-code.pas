{ ============================================================================
  Section 3.7 : Gestion des erreurs simples
  Description : Validation de mot de passe avec codes d'erreur
  Fichier source : 07-gestion-erreurs-simples.md
  ============================================================================ }
program ValidationAvecCode;

function ValiderMotDePasse(mdp: String): Integer;
var
  i: Integer;
  contientChiffre: Boolean;
begin
  // Retourne 0 si OK, sinon code d'erreur
  if Length(mdp) < 8 then
    ValiderMotDePasse := 1  // Trop court
  else
  begin
    contientChiffre := False;
    for i := 1 to Length(mdp) do
      if mdp[i] in ['0'..'9'] then  // in : test d'appartenance à un ensemble de caractères
        contientChiffre := True;

    if not contientChiffre then
      ValiderMotDePasse := 2  // Pas de chiffre
    else
      ValiderMotDePasse := 0;  // OK
  end;
end;

var
  motDePasse: String;
  codeErreur: Integer;
begin
  Write('Créez un mot de passe : ');
  ReadLn(motDePasse);

  codeErreur := ValiderMotDePasse(motDePasse);

  case codeErreur of
    0: WriteLn('✓ Mot de passe accepté');
    1: WriteLn('✗ Le mot de passe doit contenir au moins 8 caractères');
    2: WriteLn('✗ Le mot de passe doit contenir au moins un chiffre');
  else
    WriteLn('✗ Erreur inconnue');
  end;
end.
