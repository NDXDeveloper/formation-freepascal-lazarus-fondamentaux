{ ============================================================================
  Section 3.7 : Gestion des erreurs simples
  Description : Validation d'email avec retour booleen
  Fichier source : 07-gestion-erreurs-simples.md
  ============================================================================ }
program ValidationAvecBooleen;

function ValiderEmail(email: String): Boolean;
begin
  // Validation simplifiée
  // Pos retourne la position (1-based) ou 0 si non trouvé
  ValiderEmail := (Pos('@', email) > 0) and (Pos('.', email) > 0);
end;

var
  email: String;
begin
  Write('Entrez votre email : ');
  ReadLn(email);

  if ValiderEmail(email) then
    WriteLn('✓ Email valide')
  else
  begin
    WriteLn('✗ Email invalide');
    WriteLn('Un email doit contenir @ et un point');
  end;
end.
