{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Validation avec while et drapeau - compteur de tentatives
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program ValidationWhile;
var
  valeur: Integer;
  valide: Boolean;
  tentatives: Integer;
const
  MAX_TENTATIVES = 3;
begin
  valide := False;
  tentatives := 0;

  while (not valide) and (tentatives < MAX_TENTATIVES) do
  begin
    tentatives := tentatives + 1;
    Write('Tentative ', tentatives, '/', MAX_TENTATIVES, ' - Valeur (1-10) : ');
    ReadLn(valeur);

    if (valeur >= 1) and (valeur <= 10) then
    begin
      valide := True;
      WriteLn('✓ Valeur acceptée');
    end
    else
      WriteLn('❌ Invalide');
  end;

  if not valide then
    WriteLn('Nombre maximum de tentatives atteint');
end.
