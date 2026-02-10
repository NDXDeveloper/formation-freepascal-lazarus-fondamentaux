{ ============================================================================
  Section 19.8 : Tests sur differentes plateformes
  Description : Benchmark de performance multi-plateforme
  Fichier source : 08-tests-differentes-plateformes.md
  ============================================================================ }
program BenchmarkMultiPlateforme;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, Classes;

procedure BenchmarkCalculs;  
var  
  Start: TDateTime;
  i, Sum: Integer;
begin
  WriteLn('Benchmark : Calculs intensifs...');
  Start := Now;

  Sum := 0;
  for i := 1 to 10000000 do
    Sum := Sum + i mod 13;

  WriteLn('  Temps : ', MilliSecondsBetween(Now, Start), ' ms');
  WriteLn('  Resultat : ', Sum);
end;

procedure BenchmarkFichiers;  
var  
  Start: TDateTime;
  F: TextFile;
  i: Integer;
  TestFile: string;
begin
  WriteLn('Benchmark : Ecriture fichier...');
  TestFile := GetTempDir + 'bench.txt';
  Start := Now;

  AssignFile(F, TestFile);
  Rewrite(F);
  for i := 1 to 100000 do
    WriteLn(F, 'Ligne de test ', i);
  CloseFile(F);

  WriteLn('  Temps : ', MilliSecondsBetween(Now, Start), ' ms');

  DeleteFile(TestFile);
end;

procedure BenchmarkMemoire;  
var  
  Start: TDateTime;
  List: TStringList;
  i: Integer;
begin
  WriteLn('Benchmark : Allocation memoire...');
  Start := Now;

  List := TStringList.Create;
  try
    for i := 1 to 100000 do
      List.Add('Item ' + IntToStr(i));
  finally
    List.Free;
  end;

  WriteLn('  Temps : ', MilliSecondsBetween(Now, Start), ' ms');
end;

begin
  WriteLn('========================================');
  WriteLn('Benchmark Multi-Plateformes');
  WriteLn('Plateforme : ', {$I %FPCTARGETOS%});
  WriteLn('CPU : ', {$I %FPCTARGETCPU%});
  WriteLn('========================================');
  WriteLn;

  BenchmarkCalculs;
  WriteLn;
  BenchmarkFichiers;
  WriteLn;
  BenchmarkMemoire;

  WriteLn;
  WriteLn('========================================');
  WriteLn('Benchmark termine');
  WriteLn('========================================');
end.
