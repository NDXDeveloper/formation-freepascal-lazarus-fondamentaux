{ ============================================================================
  Section 10.4 : Attributs et methodes
  Description : Classe TTemperature avec conversions Celsius/Fahrenheit/Kelvin
  Fichier source : 04-attributs-methodes.md
  ============================================================================ }
program ExempleTemperature;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TTemperature = class
  private
    // Attribut : température en Celsius
    FCelsius: Real;

    // Méthode privée de validation
    function TemperatureValide(Temp: Real): Boolean;

  public
    // Modificateurs
    procedure DefinirCelsius(Valeur: Real);
    procedure DefinirFahrenheit(Valeur: Real);
    procedure DefinirKelvin(Valeur: Real);

    // Accesseurs
    function ObtenirCelsius: Real;
    function ObtenirFahrenheit: Real;
    function ObtenirKelvin: Real;

    // Méthodes utilitaires
    procedure Augmenter(Delta: Real);
    procedure Diminuer(Delta: Real);
    function EstGelant: Boolean;
    function EstBouillant: Boolean;
    procedure Afficher;
  end;

// === IMPLÉMENTATION ===

function TTemperature.TemperatureValide(Temp: Real): Boolean;  
begin  
  // Le zéro absolu est -273.15°C
  Result := Temp >= -273.15;
end;

procedure TTemperature.DefinirCelsius(Valeur: Real);  
begin  
  if TemperatureValide(Valeur) then
    FCelsius := Valeur
  else
    WriteLn('Erreur : température invalide (< -273.15°C)');
end;

procedure TTemperature.DefinirFahrenheit(Valeur: Real);  
var  
  TempCelsius: Real;
begin
  TempCelsius := (Valeur - 32) * 5 / 9;
  DefinirCelsius(TempCelsius);
end;

procedure TTemperature.DefinirKelvin(Valeur: Real);  
var  
  TempCelsius: Real;
begin
  TempCelsius := Valeur - 273.15;
  DefinirCelsius(TempCelsius);
end;

function TTemperature.ObtenirCelsius: Real;  
begin  
  Result := FCelsius;
end;

function TTemperature.ObtenirFahrenheit: Real;  
begin  
  Result := (FCelsius * 9 / 5) + 32;
end;

function TTemperature.ObtenirKelvin: Real;  
begin  
  Result := FCelsius + 273.15;
end;

procedure TTemperature.Augmenter(Delta: Real);  
begin  
  DefinirCelsius(FCelsius + Delta);
end;

procedure TTemperature.Diminuer(Delta: Real);  
begin  
  DefinirCelsius(FCelsius - Delta);
end;

function TTemperature.EstGelant: Boolean;  
begin  
  Result := FCelsius <= 0;
end;

function TTemperature.EstBouillant: Boolean;  
begin  
  Result := FCelsius >= 100;
end;

procedure TTemperature.Afficher;  
begin  
  WriteLn('=== Température ===');
  WriteLn('Celsius    : ', ObtenirCelsius:0:2, ' °C');
  WriteLn('Fahrenheit : ', ObtenirFahrenheit:0:2, ' °F');
  WriteLn('Kelvin     : ', ObtenirKelvin:0:2, ' K');

  if EstGelant then
    WriteLn('État : L''eau gèle')
  else if EstBouillant then
    WriteLn('État : L''eau bout')
  else
    WriteLn('État : L''eau est liquide');

  WriteLn('==================');
end;

// === PROGRAMME PRINCIPAL ===

var
  Temp: TTemperature;
begin
  Temp := TTemperature.Create;
  try
    // Test avec Celsius
    WriteLn('--- Définition en Celsius ---');
    Temp.DefinirCelsius(25);
    Temp.Afficher;

    WriteLn;
    WriteLn('--- Définition en Fahrenheit ---');
    Temp.DefinirFahrenheit(68);  // ~20°C
    Temp.Afficher;

    WriteLn;
    WriteLn('--- Augmentation de 10°C ---');
    Temp.Augmenter(10);
    Temp.Afficher;

    WriteLn;
    WriteLn('--- Test température négative ---');
    Temp.DefinirCelsius(-10);
    Temp.Afficher;

  finally
    Temp.Free;
  end;
end.
