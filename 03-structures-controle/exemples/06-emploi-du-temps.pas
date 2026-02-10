{ ============================================================================
  Section 3.6 : Imbrication de structures
  Description : Generateur d'emploi du temps - affichage par jour et heure
  Fichier source : 06-imbrication-structures.md
  ============================================================================ }
program EmploiDuTemps;  
const  
  JOURS = 5;  // Lundi à Vendredi
  HEURES = 8;  // 8h à 15h
type
  TJour = 1..JOURS;
  THeure = 1..HEURES;
  TEmploi = array[TJour, THeure] of String;
var
  emploi: TEmploi;
  jour: TJour;
  heure: THeure;
  nomJour: String;
begin
  // Initialisation
  for jour := 1 to JOURS do
    for heure := 1 to HEURES do
      emploi[jour, heure] := 'Libre';

  // Ajout de cours
  emploi[1, 1] := 'Maths';
  emploi[1, 2] := 'Maths';
  emploi[1, 4] := 'Français';
  emploi[2, 1] := 'Anglais';
  emploi[2, 3] := 'Sport';
  emploi[3, 2] := 'Sciences';

  WriteLn('=== EMPLOI DU TEMPS ===');
  WriteLn;

  // Affichage
  for jour := 1 to JOURS do
  begin
    case jour of
      1: nomJour := 'LUNDI';
      2: nomJour := 'MARDI';
      3: nomJour := 'MERCREDI';
      4: nomJour := 'JEUDI';
      5: nomJour := 'VENDREDI';
    end;

    WriteLn('--- ', nomJour, ' ---');

    for heure := 1 to HEURES do
    begin
      Write((heure + 7), 'h-', (heure + 8), 'h : ');

      if emploi[jour, heure] <> 'Libre' then
        WriteLn(emploi[jour, heure], ' ★')
      else
        WriteLn(emploi[jour, heure]);
    end;

    WriteLn;
  end;
end.
