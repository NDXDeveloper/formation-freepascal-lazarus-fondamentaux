{ ============================================================================
  Section 2.7 : Formatage de sortie
  Description : Formatage de dates (JJ/MM/AAAA) et heures (HH:MM:SS)
                avec zero-padding manuel
  Fichier source : 07-formatage-sortie.md
  ============================================================================ }
program FormatDateTime;  
var  
  jour, mois, annee: integer;
  heure, minute, seconde: integer;
begin
  jour := 15;
  mois := 3;
  annee := 2024;
  heure := 14;
  minute := 5;
  seconde := 7;

  // Format français : JJ/MM/AAAA
  WriteLn('Date : ', jour:2, '/', mois:2, '/', annee:4);

  // Format avec zéros : utiliser Write multiple
  Write('Date : ');
  if jour < 10 then Write('0');
  Write(jour, '/');
  if mois < 10 then Write('0');
  Write(mois, '/', annee);
  WriteLn;

  // Heure : HH:MM:SS
  Write('Heure : ');
  if heure < 10 then Write('0');
  Write(heure, ':');
  if minute < 10 then Write('0');
  Write(minute, ':');
  if seconde < 10 then Write('0');
  WriteLn(seconde);
end.
