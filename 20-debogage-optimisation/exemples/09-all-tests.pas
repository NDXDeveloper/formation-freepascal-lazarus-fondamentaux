{ ============================================================================
  Section 20.9 : Tests unitaires avec FPCUnit (introduction)
  Description : Programme principal executant tous les tests unitaires
  Fichier source : 09-tests-unitaires-fpcunit-introduction.md
  ============================================================================ }
program AllTests;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  fpcunit, testregistry, consoletestrunner,  // consoletestrunner fournit TTestRunner
  TestCalculatrice;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  try
    Application.Initialize;
    Application.Title := 'Tests Unitaires - Calculatrice';
    Application.Run;
  finally
    Application.Free;
  end;
end.
