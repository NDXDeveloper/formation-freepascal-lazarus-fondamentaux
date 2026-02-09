{ ============================================================================
  Section 3.7 : Gestion des erreurs simples
  Description : Programme sans gestion d'erreurs - risque de division par zero
  Fichier source : 07-gestion-erreurs-simples.md
  ============================================================================ }
program SansGestionErreurs;
var
  a, b: Integer;
  resultat: Real;
begin
  Write('Entrez le premier nombre : ');
  ReadLn(a);
  Write('Entrez le deuxième nombre : ');
  ReadLn(b);

  resultat := a / b;  // ❌ Et si b = 0 ?
  WriteLn('Résultat : ', resultat:0:2);
end.
