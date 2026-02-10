{ ============================================================================
  Section 3.7 : Gestion des erreurs simples
  Description : Structure d'un bon message d'erreur avec contexte et conseil
  Fichier source : 07-gestion-erreurs-simples.md
  ============================================================================ }
program BonMessageErreur;  
var  
  temperature: Real;
begin
  Write('Température en Celsius : ');
  ReadLn(temperature);

  if temperature < -273.15 then
  begin
    WriteLn('╔════════════════════════════════════╗');
    WriteLn('║         ERREUR DÉTECTÉE            ║');
    WriteLn('╚════════════════════════════════════╝');
    WriteLn;
    WriteLn('Problème : Température physiquement impossible');
    WriteLn('Valeur entrée : ', temperature:0:2, '°C');
    WriteLn('Limite minimale : -273.15°C (zéro absolu)');
    WriteLn;
    WriteLn('Action : Veuillez entrer une température valide.');
  end
  else
    WriteLn('Température acceptée : ', temperature:0:2, '°C');
end.
