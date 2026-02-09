{ ============================================================================
  Section 5.8 : Types ensemble (Set)
  Description : Jours de presence avec operations ensemblistes
  Fichier source : 08-types-ensemble-set.md
  ============================================================================ }
program JoursPresence;
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

var
  joursTravail: TJours;
  joursConges: TJours;
  joursPresents: TJours;
  jour: TJour;

function NomJour(j: TJour): String;
begin
  case j of
    Lundi: NomJour := 'Lundi';
    Mardi: NomJour := 'Mardi';
    Mercredi: NomJour := 'Mercredi';
    Jeudi: NomJour := 'Jeudi';
    Vendredi: NomJour := 'Vendredi';
    Samedi: NomJour := 'Samedi';
    Dimanche: NomJour := 'Dimanche';
  end;
end;

begin
  // Définir les jours de travail normaux
  joursTravail := [Lundi, Mardi, Mercredi, Jeudi, Vendredi];

  // Jours en congé cette semaine
  joursConges := [Mercredi];

  // Jours effectivement présents = travail SAUF congés
  joursPresents := joursTravail - joursConges;

  WriteLn('Jours de présence cette semaine :');
  for jour := Lundi to Vendredi do
  begin
    if jour in joursPresents then
      WriteLn('  ', NomJour(jour));
  end;
end.
