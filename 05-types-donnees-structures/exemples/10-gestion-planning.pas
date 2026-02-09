{ ============================================================================
  Section 5.10 : Definition de types personnalises (Type)
  Description : Gestion de planning hebdomadaire avec types combines
  Fichier source : 10-definition-types-personnalises.md
  ============================================================================ }
program GestionPlanning;
type
  // Énumérés
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TTypeTache = (Reunion, Cours, Projet, Personnel);
  TPriorite = (Basse, Moyenne, Haute, Urgente);

  // Intervalles
  THeure = 0..23;
  TMinute = 0..59;

  // Structures de base
  THoraire = record
    heure: THeure;
    minute: TMinute;
  end;

  TTache = record
    titre: String;
    description: String;
    typeTache: TTypeTache;
    priorite: TPriorite;
    heureDebut: THoraire;
    heureFin: THoraire;
  end;

  // Planning journalier
  TJournee = record
    jour: TJour;
    taches: array[1..10] of TTache;
    nbTaches: Integer;
  end;

  // Planning hebdomadaire
  TPlanningHebdo = array[TJour] of TJournee;

var
  planning: TPlanningHebdo;
  tache: TTache;
  jour: TJour;
  i: Integer;

procedure InitialiserPlanning(var p: TPlanningHebdo);
var
  j: TJour;
begin
  for j := Lundi to Dimanche do
  begin
    p[j].jour := j;
    p[j].nbTaches := 0;
  end;
end;

procedure AjouterTache(var journee: TJournee; t: TTache);
begin
  if journee.nbTaches < 10 then
  begin
    journee.nbTaches := journee.nbTaches + 1;
    journee.taches[journee.nbTaches] := t;
  end;
end;

procedure AfficherTache(t: TTache);
begin
  WriteLn('  ', t.heureDebut.heure:2, ':', t.heureDebut.minute:2,
          ' - ', t.heureFin.heure:2, ':', t.heureFin.minute:2,
          ' : ', t.titre);
end;

begin
  InitialiserPlanning(planning);

  // Ajouter une tâche
  tache.titre := 'Réunion d''équipe';
  tache.typeTache := Reunion;
  tache.priorite := Haute;
  tache.heureDebut.heure := 9;
  tache.heureDebut.minute := 0;
  tache.heureFin.heure := 10;
  tache.heureFin.minute := 30;

  AjouterTache(planning[Lundi], tache);

  // Afficher le planning
  WriteLn('Planning de la semaine :');
  for jour := Lundi to Vendredi do
  begin
    WriteLn;
    Write('=== ');
    case jour of
      Lundi: Write('LUNDI');
      Mardi: Write('MARDI');
      Mercredi: Write('MERCREDI');
      Jeudi: Write('JEUDI');
      Vendredi: Write('VENDREDI');
      Samedi: Write('SAMEDI');
      Dimanche: Write('DIMANCHE');
    end;
    WriteLn(' ===');

    if planning[jour].nbTaches = 0 then
      WriteLn('  Aucune tâche')
    else
    begin
      for i := 1 to planning[jour].nbTaches do
        AfficherTache(planning[jour].taches[i]);
    end;
  end;
end.
