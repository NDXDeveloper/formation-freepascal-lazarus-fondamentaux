{ ============================================================================
  Section 5.7 : Types enumeres
  Description : Tableau indexe par un type enumere (temperatures par jour)
  Fichier source : 07-types-enumeres.md
  ============================================================================ }
program TableauEnum;  
type  
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  temperatures: array[TJourSemaine] of Real;  // Tableau indexé par le type énuméré (pas par Integer)
  jour: TJourSemaine;
begin
  // Saisie des températures
  for jour := Lundi to Dimanche do
  begin
    Write('Température pour ');
    case jour of
      Lundi: Write('lundi');
      Mardi: Write('mardi');
      Mercredi: Write('mercredi');
      Jeudi: Write('jeudi');
      Vendredi: Write('vendredi');
      Samedi: Write('samedi');
      Dimanche: Write('dimanche');
    end;
    Write(' : ');
    ReadLn(temperatures[jour]);
  end;

  // Affichage
  WriteLn;
  WriteLn('Températures de la semaine :');
  for jour := Lundi to Dimanche do
    WriteLn('  ', Ord(jour) + 1, '. ', temperatures[jour]:0:1, ' °C');
end.
