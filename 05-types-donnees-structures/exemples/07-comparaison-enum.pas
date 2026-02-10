{ ============================================================================
  Section 5.7 : Types enumeres
  Description : Comparaison de valeurs enumerees (egalite, difference, ordre)
  Fichier source : 07-types-enumeres.md
  ============================================================================ }
program ComparaisonEnum;  
type  
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  jour1, jour2: TJourSemaine;
begin
  jour1 := Mardi;
  jour2 := Jeudi;

  // Égalité
  if jour1 = Mardi then
    WriteLn('C''est mardi');

  // Différence
  if jour1 <> jour2 then
    WriteLn('Les jours sont différents');

  // Ordre (basé sur la position dans la déclaration)
  if jour1 < jour2 then
    WriteLn('Mardi vient avant jeudi');  // Vrai !

  if Samedi > Vendredi then
    WriteLn('Samedi vient après vendredi');  // Vrai !
end.
