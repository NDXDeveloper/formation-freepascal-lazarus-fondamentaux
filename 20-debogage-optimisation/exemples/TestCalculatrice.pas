{ ============================================================================
  Section 20.9 : Tests unitaires avec FPCUnit (introduction)
  Description : Tests unitaires pour l'unite Calculatrice
  Fichier source : 09-tests-unitaires-fpcunit-introduction.md
  ============================================================================ }
unit TestCalculatrice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Calculatrice;

type
  TTestCalculatrice = class(TTestCase)
  published  // Les methodes published sont auto-detectees comme tests
    procedure TestAdditionner;
    procedure TestSoustraire;
    procedure TestMultiplier;
    procedure TestDiviser;
    procedure TestDivisionParZero;
  end;

implementation

procedure TTestCalculatrice.TestAdditionner;  
begin  
  AssertEquals('2 + 3 devrait etre 5', 5, Additionner(2, 3));
  AssertEquals('0 + 0 devrait etre 0', 0, Additionner(0, 0));
  AssertEquals('-5 + 5 devrait etre 0', 0, Additionner(-5, 5));
  AssertEquals('-10 + -5 devrait etre -15', -15, Additionner(-10, -5));
end;

procedure TTestCalculatrice.TestSoustraire;  
begin  
  AssertEquals('5 - 3 devrait etre 2', 2, Soustraire(5, 3));
  AssertEquals('0 - 0 devrait etre 0', 0, Soustraire(0, 0));
  AssertEquals('10 - (-5) devrait etre 15', 15, Soustraire(10, -5));
end;

procedure TTestCalculatrice.TestMultiplier;  
begin  
  AssertEquals('3 * 4 devrait etre 12', 12, Multiplier(3, 4));
  AssertEquals('0 * 100 devrait etre 0', 0, Multiplier(0, 100));
  AssertEquals('-5 * 3 devrait etre -15', -15, Multiplier(-5, 3));
end;

procedure TTestCalculatrice.TestDiviser;  
begin  
  AssertEquals('10 / 2 devrait etre 5', 5.0, Diviser(10, 2), 0.001);  // 0.001 = tolerance pour comparaison de reels
  AssertEquals('7 / 2 devrait etre 3.5', 3.5, Diviser(7, 2), 0.001);
end;

procedure TTestCalculatrice.TestDivisionParZero;  
begin  
  try
    Diviser(10, 0);
    Fail('Une exception devrait etre levee');
  except
    on E: Exception do
      AssertEquals('Division par zero', E.Message);
  end;
end;

initialization
  RegisterTest(TTestCalculatrice);  // Enregistre la classe de tests aupres du runner

end.
