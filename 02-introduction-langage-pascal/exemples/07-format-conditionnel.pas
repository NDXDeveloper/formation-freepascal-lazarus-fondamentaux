{ ============================================================================
  Section 2.7 : Formatage de sortie
  Description : Affichage conditionnel du signe + ou - selon la valeur
                d'un solde bancaire
  Fichier source : 07-formatage-sortie.md
  ============================================================================ }
program FormatConditionnel;
var
  solde: real;
  symbole: string;
begin
  solde := -150.50;

  if solde >= 0 then
    symbole := '+'
  else
    symbole := '-';

  // abs() retourne la valeur absolue (sans le signe negatif)
  WriteLn('Solde : ', symbole, ' ', abs(solde):0:2, ' €');

  if solde < 0 then
    WriteLn('*** ATTENTION : Solde négatif ! ***');
end.
