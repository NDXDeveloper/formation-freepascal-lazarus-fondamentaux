{ ============================================================================
  Section 5.7 : Types enumeres
  Description : Affectation et comparaison de base avec un type enumere
  Fichier source : 07-types-enumeres.md
  ============================================================================ }
program ExempleEnum;
type
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  aujourdhui: TJourSemaine;
begin
  aujourdhui := Mercredi;

  if aujourdhui = Mercredi then
    WriteLn('Nous sommes mercredi');

  aujourdhui := Vendredi;
  WriteLn('Changement : maintenant c''est vendredi');
end.
