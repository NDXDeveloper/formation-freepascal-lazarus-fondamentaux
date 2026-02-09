{ ============================================================================
  Section 5.7 : Types enumeres
  Description : Conversion entre type enumere et String
  Fichier source : 07-types-enumeres.md
  ============================================================================ }
program ConversionEnum;
type
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

function JourEnString(jour: TJourSemaine): String;
begin
  case jour of
    Lundi:    JourEnString := 'Lundi';
    Mardi:    JourEnString := 'Mardi';
    Mercredi: JourEnString := 'Mercredi';
    Jeudi:    JourEnString := 'Jeudi';
    Vendredi: JourEnString := 'Vendredi';
    Samedi:   JourEnString := 'Samedi';
    Dimanche: JourEnString := 'Dimanche';
  end;
end;

var
  jour: TJourSemaine;
begin
  jour := Mercredi;
  WriteLn('Aujourd''hui c''est ', JourEnString(jour));

  for jour := Lundi to Dimanche do
    WriteLn(JourEnString(jour));
end.
