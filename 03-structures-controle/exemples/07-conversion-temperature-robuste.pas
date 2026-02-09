{ ============================================================================
  Section 3.7 : Gestion des erreurs simples
  Description : Conversion de temperature avec verification du zero absolu
  Fichier source : 07-gestion-erreurs-simples.md
  ============================================================================ }
program ConversionTemperatureRobuste;
const
  ZERO_ABSOLU = -273.15;
var
  temperature, resultat: Real;
  choix: Integer;
  erreur: Boolean;
begin
  WriteLn('═══ CONVERSION DE TEMPÉRATURE ═══');
  WriteLn;
  WriteLn('1. Celsius → Fahrenheit');
  WriteLn('2. Fahrenheit → Celsius');
  WriteLn('3. Celsius → Kelvin');
  WriteLn;

  // Validation du choix
  repeat
    Write('Votre choix (1-3) : ');
    ReadLn(choix);
    if (choix < 1) or (choix > 3) then
      WriteLn('❌ Choix invalide. Entrez 1, 2 ou 3.');
  until (choix >= 1) and (choix <= 3);

  Write('Température : ');
  ReadLn(temperature);

  erreur := False;

  case choix of
    1:  // Celsius → Fahrenheit
      begin
        if temperature < ZERO_ABSOLU then
        begin
          WriteLn('❌ ERREUR : Température impossible');
          WriteLn('   Le zéro absolu est ', ZERO_ABSOLU:0:2, '°C');
          erreur := True;
        end
        else
        begin
          resultat := (temperature * 9/5) + 32;
          WriteLn('✓ ', temperature:0:2, '°C = ', resultat:0:2, '°F');
        end;
      end;

    2:  // Fahrenheit → Celsius
      begin
        resultat := (temperature - 32) * 5/9;
        if resultat < ZERO_ABSOLU then
        begin
          WriteLn('❌ ERREUR : Température impossible');
          WriteLn('   Le zéro absolu est -459.67°F');
          erreur := True;
        end
        else
          WriteLn('✓ ', temperature:0:2, '°F = ', resultat:0:2, '°C');
      end;

    3:  // Celsius → Kelvin
      begin
        if temperature < ZERO_ABSOLU then
        begin
          WriteLn('❌ ERREUR : Température impossible');
          WriteLn('   Le zéro absolu est ', ZERO_ABSOLU:0:2, '°C');
          erreur := True;
        end
        else
        begin
          resultat := temperature + 273.15;
          WriteLn('✓ ', temperature:0:2, '°C = ', resultat:0:2, 'K');
        end;
      end;
  end;

  if erreur then
    WriteLn('⚠️  Veuillez réessayer avec une température valide.');
end.
