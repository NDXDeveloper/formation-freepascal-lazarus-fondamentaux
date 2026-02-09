{ ============================================================================
  Section 2.3 : Variables et constantes
  Description : Conversion d'une temperature de Celsius vers Fahrenheit
  Fichier source : 03-variables-constantes.md
  ============================================================================ }
program ConversionTemperature;

const
  MessageBienvenue = 'Conversion Celsius vers Fahrenheit';

var
  celsius: real;
  fahrenheit: real;

begin
  writeln(MessageBienvenue);
  writeln('========================');
  writeln;

  // Température en Celsius
  celsius := 25.0;

  // Formule de conversion : F = (C × 9/5) + 32
  fahrenheit := (celsius * 9 / 5) + 32;

  // Affichage
  writeln(celsius:0:1, ' °C = ', fahrenheit:0:1, ' °F');
end.
