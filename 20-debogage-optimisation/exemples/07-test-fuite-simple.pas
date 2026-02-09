{ ============================================================================
  Section 20.7 : Outils de detection des fuites memoire
  Description : Test HeapTrc avec fuite et sans fuite
  Fichier source : 07-outils-detection-fuites-memoire.md
  ============================================================================ }
program TestFuiteSimple;

{$mode objfpc}{$H+}

uses
  heaptrc, Classes;

procedure CreerFuite;
var
  liste: TStringList;
begin
  liste := TStringList.Create;
  liste.Add('Element 1');
  liste.Add('Element 2');
  // Oubli volontaire du Free !
end;

procedure SansFuite;
var
  liste: TStringList;
begin
  liste := TStringList.Create;
  try
    liste.Add('Element');
  finally
    liste.Free;  // Correct
  end;
end;

begin
  SetHeapTraceOutput('rapport.log');

  WriteLn('Test avec fuite...');
  CreerFuite;

  WriteLn('Test sans fuite...');
  SansFuite;

  WriteLn('Programme termine. Verifiez rapport.log');
end.
