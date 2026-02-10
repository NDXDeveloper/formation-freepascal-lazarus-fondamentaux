{ ============================================================================
  Section 6.2 : Declaration et Utilisation de Pointeurs
  Description : Fonction retournant un pointeur vers le maximum de deux entiers
  Fichier source : 02-declaration-utilisation-pointeurs.md
  ============================================================================ }
{$mode objfpc}{$H+}
program RetournerPointeur;  
type  
  PInteger = ^Integer;

function TrouverMaximum(var a, b: Integer): PInteger;  // var obligatoire : sans var, @a pointerait vers une copie locale détruite au retour  
begin  
  if a > b then
    Result := @a
  else
    Result := @b;
end;

var
  x, y: Integer;
  pMax: PInteger;
begin
  x := 10;
  y := 20;

  pMax := TrouverMaximum(x, y);
  WriteLn('Maximum : ', pMax^);  // Affiche 20
end.
