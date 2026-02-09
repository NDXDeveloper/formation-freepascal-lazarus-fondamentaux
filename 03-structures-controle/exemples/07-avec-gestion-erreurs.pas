{ ============================================================================
  Section 3.7 : Gestion des erreurs simples
  Description : Programme avec gestion d'erreurs - verification avant division
  Fichier source : 07-gestion-erreurs-simples.md
  ============================================================================ }
program AvecGestionErreurs;
var
  a, b: Integer;
  resultat: Real;
begin
  Write('Entrez le premier nombre : ');
  ReadLn(a);
  Write('Entrez le deuxième nombre : ');
  ReadLn(b);

  // ✓ Vérification avant le calcul
  if b = 0 then
    WriteLn('ERREUR : Division par zéro impossible !')
  else
  begin
    resultat := a / b;
    WriteLn('Résultat : ', resultat:0:2);
  end;
end.
