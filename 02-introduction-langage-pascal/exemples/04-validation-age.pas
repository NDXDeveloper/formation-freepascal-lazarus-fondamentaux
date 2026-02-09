{ ============================================================================
  Section 2.4 : Types de donnees primitifs
  Description : Validation d'age avec des expressions booleennes
                (comparaisons affectees a des variables boolean)
  Fichier source : 04-types-donnees-primitifs.md
  ============================================================================ }
program ValidationAge;
var
  age: integer;
  peutConduire: boolean;
  peutVoter: boolean;
  estRetraite: boolean;
begin
  age := 25;

  peutConduire := age >= 18;
  peutVoter := age >= 18;
  estRetraite := age >= 65;

  writeln('Âge : ', age, ' ans');
  writeln('Peut conduire : ', peutConduire);
  writeln('Peut voter : ', peutVoter);
  writeln('Est retraité : ', estRetraite);
end.
