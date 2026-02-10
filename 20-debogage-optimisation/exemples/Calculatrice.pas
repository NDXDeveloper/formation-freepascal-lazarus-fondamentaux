{ ============================================================================
  Section 20.9 : Tests unitaires avec FPCUnit (introduction)
  Description : Unite Calculatrice a tester avec FPCUnit
  Fichier source : 09-tests-unitaires-fpcunit-introduction.md
  ============================================================================ }
unit Calculatrice;

{$mode objfpc}{$H+}

interface

function Additionner(a, b: Integer): Integer;  
function Soustraire(a, b: Integer): Integer;  
function Multiplier(a, b: Integer): Integer;  
function Diviser(a, b: Double): Double;  

implementation

uses SysUtils;

function Additionner(a, b: Integer): Integer;  
begin  
  Result := a + b;
end;

function Soustraire(a, b: Integer): Integer;  
begin  
  Result := a - b;
end;

function Multiplier(a, b: Integer): Integer;  
begin  
  Result := a * b;
end;

function Diviser(a, b: Double): Double;  
begin  
  if b = 0 then
    raise Exception.Create('Division par zero');
  Result := a / b;
end;

end.
