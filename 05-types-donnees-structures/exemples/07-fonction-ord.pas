{ ============================================================================
  Section 5.7 : Types enumeres
  Description : Fonction Ord() pour obtenir la position ordinale
  Fichier source : 07-types-enumeres.md
  ============================================================================ }
program FonctionOrd;
type
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  jour: TJourSemaine;
begin
  jour := Lundi;
  WriteLn('Position de Lundi : ', Ord(jour));  // 0

  jour := Mercredi;
  WriteLn('Position de Mercredi : ', Ord(jour));  // 2

  jour := Dimanche;
  WriteLn('Position de Dimanche : ', Ord(jour));  // 6
end.
