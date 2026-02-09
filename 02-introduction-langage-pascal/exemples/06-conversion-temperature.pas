{ ============================================================================
  Section 2.6 : Entrees/sorties console
  Description : Conversion bidirectionnelle Celsius/Fahrenheit avec menu
  Fichier source : 06-entrees-sorties-console.md
  ============================================================================ }
program ConversionTemperatureIO;
var
  celsius, fahrenheit: real;
  choix: integer;
begin
  WriteLn('=== CONVERSION DE TEMPÉRATURE ===');
  WriteLn;
  WriteLn('1. Celsius vers Fahrenheit');
  WriteLn('2. Fahrenheit vers Celsius');
  WriteLn;

  Write('Votre choix (1 ou 2) : ');
  ReadLn(choix);
  WriteLn;

  if choix = 1 then
  begin
    Write('Température en Celsius : ');
    ReadLn(celsius);

    fahrenheit := (celsius * 9 / 5) + 32;

    WriteLn;
    WriteLn(celsius:0:1, ' °C = ', fahrenheit:0:1, ' °F');
  end
  else if choix = 2 then
  begin
    Write('Température en Fahrenheit : ');
    ReadLn(fahrenheit);

    celsius := (fahrenheit - 32) * 5 / 9;

    WriteLn;
    WriteLn(fahrenheit:0:1, ' °F = ', celsius:0:1, ' °C');
  end
  else
    WriteLn('Choix invalide !');

  WriteLn;
  Write('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
