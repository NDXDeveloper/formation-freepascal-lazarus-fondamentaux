{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Affichage d'un calendrier mensuel avec premier jour et nombre de jours
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program CalendrierMois;  
var  
  jour, premierJour, joursDansMois: Integer;
  espace: Integer;
begin
  Write('Premier jour du mois (1=Lundi, 7=Dimanche) : ');
  ReadLn(premierJour);
  Write('Nombre de jours dans le mois : ');
  ReadLn(joursDansMois);
  WriteLn;
  WriteLn('Lun Mar Mer Jeu Ven Sam Dim');
  WriteLn('---------------------------');
  for espace := 1 to (premierJour - 1) do
    Write('    ');
  for jour := 1 to joursDansMois do
  begin
    Write(jour:3, ' ');
    { mod = reste de la division entiere ; ici detecte le passage a la ligne }
    if (premierJour + jour - 1) mod 7 = 0 then
      WriteLn;
  end;
  WriteLn;
end.
