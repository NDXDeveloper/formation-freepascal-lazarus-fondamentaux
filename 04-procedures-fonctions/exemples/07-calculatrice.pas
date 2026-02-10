{ ============================================================================
  Section 4.7 : Surcharge de procedures/fonctions
  Description : Calculatrice avec fonctions surchargees (overload)
  Fichier source : 07-surcharge-procedures-fonctions.md
  ============================================================================ }
{$mode objfpc}{$H+}
program Calculatrice;

// overload : permet plusieurs fonctions avec le même nom mais des paramètres différents
function Additionner(a, b: Integer): Integer; overload;  
begin  
  Result := a + b;
end;

// Addition avec 3 nombres
function Additionner(a, b, c: Integer): Integer; overload;  
begin  
  Result := a + b + c;
end;

// Addition de réels
function Additionner(a, b: Real): Real; overload;  
begin  
  Result := a + b;
end;

// Multiplication
function Multiplier(a, b: Integer): Integer; overload;  
begin  
  Result := a * b;
end;

function Multiplier(a, b: Real): Real; overload;  
begin  
  Result := a * b;
end;

// Affichage du résultat
procedure AfficherResultat(operation: String; resultat: Integer); overload;  
begin  
  WriteLn(operation, ' = ', resultat);
end;

procedure AfficherResultat(operation: String; resultat: Real); overload;  
begin  
  WriteLn(operation, ' = ', resultat:0:2);
end;

var
  resInt: Integer;
  resReal: Real;
begin
  resInt := Additionner(5, 3);
  AfficherResultat('5 + 3', resInt);

  resInt := Additionner(5, 3, 2);
  AfficherResultat('5 + 3 + 2', resInt);

  resReal := Additionner(2.5, 3.7);
  AfficherResultat('2.5 + 3.7', resReal);

  resInt := Multiplier(4, 7);
  AfficherResultat('4 × 7', resInt);

  resReal := Multiplier(2.5, 4.0);
  AfficherResultat('2.5 × 4.0', resReal);
end.
