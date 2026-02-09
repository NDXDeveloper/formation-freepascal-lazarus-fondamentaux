{ ============================================================================
  Section 3.1 : Instructions conditionnelles (if-then-else)
  Description : Exemple d'utilisation des opérateurs logiques (and, or) dans les conditions
  Fichier source : 01-instructions-conditionnelles-if-then-else.md
  ============================================================================ }
program ExempleOperateurs;
var
  age: Integer;
  etudiant: Boolean;
  revenus: Real;
  reponse: String;
begin
  Write('Âge : ');
  ReadLn(age);
  Write('Êtes-vous étudiant ? (true/false) : ');
  ReadLn(reponse);
  etudiant := (reponse = 'true');
  Write('Revenus annuels : ');
  ReadLn(revenus);
  { En Pascal, and/or ont une priorité PLUS HAUTE que < > = : les
    parenthèses autour de chaque comparaison sont donc obligatoires,
    sinon le compilateur interprète « age < (26 and etudiant) » }
  if ((age < 26) and etudiant) or (revenus < 10000) then
    WriteLn('Vous avez droit à une aide financière.')
  else
    WriteLn('Vous n''avez pas droit à une aide financière.');
    { '' (deux apostrophes) est la façon d'écrire une apostrophe dans une chaîne Pascal }
end.
