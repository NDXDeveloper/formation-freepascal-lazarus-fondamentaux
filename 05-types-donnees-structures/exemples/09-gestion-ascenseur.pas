{ ============================================================================
  Section 5.9 : Types intervalle
  Description : Gestion d'un ascenseur avec type intervalle pour les etages
  Fichier source : 09-types-intervalle.md
  ============================================================================ }
program GestionAscenseur;
type
  TEtage = -2..50;  // Du sous-sol -2 au 50e étage

var
  etageActuel: TEtage;
  etageDestination: TEtage;
  i: TEtage;

procedure DeplacerVers(destination: TEtage);
begin
  WriteLn('Déplacement de l''étage ', etageActuel, ' vers l''étage ', destination);

  if destination > etageActuel then
  begin
    WriteLn('Monte...');
    for i := etageActuel + 1 to destination do
    begin
      WriteLn('  Étage ', i);
      // Simulation d'attente
    end;
  end
  else if destination < etageActuel then
  begin
    WriteLn('Descend...');
    for i := etageActuel - 1 downto destination do
    begin
      WriteLn('  Étage ', i);
      // Simulation d'attente
    end;
  end
  else
    WriteLn('Déjà à l''étage demandé');

  etageActuel := destination;
  WriteLn('Arrivé à l''étage ', etageActuel);
end;

begin
  etageActuel := 0;  // Départ au rez-de-chaussée

  WriteLn('=== ASCENSEUR ===');
  WriteLn('Étage actuel : ', etageActuel);

  Write('Quel étage souhaitez-vous (-2 à 50) ? ');
  ReadLn(etageDestination);

  DeplacerVers(etageDestination);
end.
