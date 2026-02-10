{ ============================================================================
  Section 9.7 : Structure d'un projet Lazarus
  Description : Unité de calculs réutilisable (exemple d'organisation en unités)
  Fichier source : 07-structure-projet-lazarus.md
  ============================================================================ }
unit UnitCalculs;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

// Fonctions publiques (utilisables depuis d'autres unités)
function Additionner(a, b: Integer): Integer;  
function Multiplier(a, b: Integer): Integer;  
function Diviser(a, b: Real): Real;  

implementation

// Code des fonctions
function Additionner(a, b: Integer): Integer;  
begin  
  Result := a + b;
end;

function Multiplier(a, b: Integer): Integer;  
begin  
  Result := a * b;
end;

function Diviser(a, b: Real): Real;  
begin  
  if b = 0 then
    raise Exception.Create('Division par zéro !')
  else
    Result := a / b;
end;

end.
