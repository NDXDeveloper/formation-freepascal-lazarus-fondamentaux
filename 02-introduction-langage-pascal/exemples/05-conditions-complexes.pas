{ ============================================================================
  Section 2.5 : Operateurs arithmetiques et logiques
  Description : Evaluation de conditions logiques complexes avec and/or
  Fichier source : 05-operateurs-arithmetiques-logiques.md
  ============================================================================ }
program ConditionsComplexes;
var
  age: integer;
  aPermis: boolean;
  aVoiture: boolean;
  peutPartirEnVacances: boolean;
begin
  age := 25;
  aPermis := true;
  aVoiture := false;

  // Condition complexe
  peutPartirEnVacances := (age >= 18) and (aPermis or aVoiture);
  // Étapes :
  // 1. (age >= 18) = true
  // 2. (aPermis or aVoiture) = (true or false) = true
  // 3. true and true = true

  writeln('Peut partir en vacances : ', peutPartirEnVacances);
end.
