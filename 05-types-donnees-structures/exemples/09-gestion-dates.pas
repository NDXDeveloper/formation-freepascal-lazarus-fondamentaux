{ ============================================================================
  Section 5.9 : Types intervalle
  Description : Gestion de dates avec types intervalle pour jour, mois, annee
  Fichier source : 09-types-intervalle.md
  ============================================================================ }
program GestionDates;
type
  TJour = 1..31;
  TMois = 1..12;
  TAnnee = 1900..2100;

  TDate = record
    jour: TJour;
    mois: TMois;
    annee: TAnnee;
  end;

function DateValide(d: TDate): Boolean;
const
  JoursParMois: array[1..12] of Integer =
    (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
var
  joursMax: Integer;
begin
  joursMax := JoursParMois[d.mois];

  // Année bissextile : février a 29 jours
  if (d.mois = 2) and ((d.annee mod 4 = 0) and
     ((d.annee mod 100 <> 0) or (d.annee mod 400 = 0))) then
    joursMax := 29;

  DateValide := d.jour <= joursMax;
end;

procedure AfficherDate(d: TDate);
const
  NomsMois: array[1..12] of String =
    ('Janvier', 'Février', 'Mars', 'Avril', 'Mai', 'Juin',
     'Juillet', 'Août', 'Septembre', 'Octobre', 'Novembre', 'Décembre');
begin
  WriteLn(d.jour, ' ', NomsMois[d.mois], ' ', d.annee);
end;

var
  date: TDate;
begin
  WriteLn('Entrez une date :');
  Write('  Jour (1-31) : ');
  ReadLn(date.jour);
  Write('  Mois (1-12) : ');
  ReadLn(date.mois);
  Write('  Année (1900-2100) : ');
  ReadLn(date.annee);

  if DateValide(date) then
  begin
    Write('Date valide : ');
    AfficherDate(date);
  end
  else
    WriteLn('Date invalide !');
end.
