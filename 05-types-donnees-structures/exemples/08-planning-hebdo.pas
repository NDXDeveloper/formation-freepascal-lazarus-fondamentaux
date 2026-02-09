{ ============================================================================
  Section 5.8 : Types ensemble (Set)
  Description : Planification hebdomadaire avec ensembles dans des records
  Fichier source : 08-types-ensemble-set.md
  ============================================================================ }
program PlanningHebdo;
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

  TActivite = record
    nom: String;
    jours: TJours;
  end;

var
  sport, cours, travail: TActivite;
  joursLibres: TJours;
  tousLesJours: TJours;
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
  // Définir les activités
  sport.nom := 'Sport';
  sport.jours := [Mardi, Jeudi];

  cours.nom := 'Cours';
  cours.jours := [Lundi, Mardi, Mercredi, Jeudi, Vendredi];

  travail.nom := 'Travail';
  travail.jours := [Mercredi, Samedi];

  // Tous les jours de la semaine
  tousLesJours := [Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche];

  // Calculer les jours libres
  joursLibres := tousLesJours - (sport.jours + cours.jours + travail.jours);

  WriteLn('Planning de la semaine :');
  for jour := Lundi to Dimanche do
  begin
    Write(NomJour(jour), ' : ');

    if jour in sport.jours then Write('Sport ');
    if jour in cours.jours then Write('Cours ');
    if jour in travail.jours then Write('Travail ');
    if jour in joursLibres then Write('LIBRE');

    WriteLn;
  end;

  WriteLn;
  WriteLn('Jours complètement libres :');
  for jour := Lundi to Dimanche do
    if jour in joursLibres then
      WriteLn('  ', NomJour(jour));
end.
