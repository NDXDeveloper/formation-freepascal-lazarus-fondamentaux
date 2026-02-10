{ ============================================================================
  Section 3.6 : Imbrication de structures
  Description : CASE dans boucle FOR - affichage des jours de la semaine
  Fichier source : 06-imbrication-structures.md
  ============================================================================ }
program JoursSemaine;  
var  
  jour: Integer;
begin
  WriteLn('Les jours de la semaine :');
  WriteLn;

  for jour := 1 to 7 do
  begin
    Write('Jour ', jour, ' : ');

    case jour of
      1: WriteLn('Lundi (travail)');
      2: WriteLn('Mardi (travail)');
      3: WriteLn('Mercredi (travail)');
      4: WriteLn('Jeudi (travail)');
      5: WriteLn('Vendredi (travail)');
      6: WriteLn('Samedi (week-end)');
      7: WriteLn('Dimanche (week-end)');
    end;
  end;
end.
