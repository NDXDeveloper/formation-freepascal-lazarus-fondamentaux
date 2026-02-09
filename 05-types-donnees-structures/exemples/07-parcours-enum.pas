{ ============================================================================
  Section 5.7 : Types enumeres
  Description : Parcourir toutes les valeurs d'un type enumere avec une boucle
  Fichier source : 07-types-enumeres.md
  ============================================================================ }
program ParcoursEnum;
type
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  jour: TJourSemaine;
begin
  WriteLn('Tous les jours de la semaine :');
  for jour := Lundi to Dimanche do
  begin
    Write('Jour numéro ', Ord(jour) + 1);  // +1 pour afficher 1-7 au lieu de 0-6
    case jour of
      Lundi: WriteLn(' : Lundi');
      Mardi: WriteLn(' : Mardi');
      Mercredi: WriteLn(' : Mercredi');
      Jeudi: WriteLn(' : Jeudi');
      Vendredi: WriteLn(' : Vendredi');
      Samedi: WriteLn(' : Samedi');
      Dimanche: WriteLn(' : Dimanche');
    end;
  end;
end.
