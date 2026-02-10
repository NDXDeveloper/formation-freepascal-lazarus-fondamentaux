{ ============================================================================
  Section 5.7 : Types enumeres
  Description : Utilisation de CASE-OF avec un type enumere
  Fichier source : 07-types-enumeres.md
  ============================================================================ }
program CaseEnum;  
type  
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  jour: TJourSemaine;
begin
  jour := Mercredi;

  case jour of
    Lundi:
      WriteLn('Début de la semaine de travail');
    Mardi, Mercredi, Jeudi:
      WriteLn('En pleine semaine');
    Vendredi:
      WriteLn('Bientôt le week-end !');
    Samedi, Dimanche:
      WriteLn('C''est le week-end !');
  end;
end.
