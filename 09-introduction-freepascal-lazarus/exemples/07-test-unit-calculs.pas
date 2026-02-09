{ ============================================================================
  Section 9.7 : Structure d'un projet Lazarus
  Description : Programme de test pour UnitCalculs (utilisation d'une unité)
  Fichier source : 07-structure-projet-lazarus.md
  ============================================================================ }
program TestUnitCalculs;

{$mode objfpc}{$H+}

uses
  SysUtils, UnitCalculs;

begin
  WriteLn('Test de UnitCalculs');
  WriteLn('===================');
  WriteLn;
  WriteLn('5 + 3 = ', Additionner(5, 3));
  WriteLn('4 * 7 = ', Multiplier(4, 7));
  WriteLn('10 / 3 = ', Diviser(10, 3):0:4);
  WriteLn;
  WriteLn('Test division par zéro :');
  try
    WriteLn(Diviser(10, 0):0:4);
  except
    on E: Exception do
      WriteLn('Exception : ', E.Message);
  end;
end.
