{ ============================================================================
  Section 2.5 : Operateurs arithmetiques et logiques
  Description : Classification d'age avec des comparaisons et des
                operateurs logiques (and)
  Fichier source : 05-operateurs-arithmetiques-logiques.md
  ============================================================================ }
program ComparaisonsAge;
var
  age: integer;
  estEnfant: boolean;
  estAdolescent: boolean;
  estAdulte: boolean;
  estSenior: boolean;
begin
  age := 25;

  // Le resultat d'une comparaison est un boolean (true/false)
  estEnfant := age < 13;
  // En Pascal, and a priorite sur >= et <, donc les parentheses
  // autour de chaque comparaison sont obligatoires
  estAdolescent := (age >= 13) and (age < 18);
  estAdulte := (age >= 18) and (age < 65);
  estSenior := age >= 65;

  writeln('Âge : ', age);
  writeln('Enfant : ', estEnfant);          // false
  writeln('Adolescent : ', estAdolescent);  // false
  writeln('Adulte : ', estAdulte);          // true
  writeln('Senior : ', estSenior);          // false
end.
