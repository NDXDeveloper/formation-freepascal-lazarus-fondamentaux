{ ============================================================================
  Section 20.7 : Outils de detection des fuites memoire
  Description : Detection de fuites multiples avec HeapTrc
  Fichier source : 07-outils-detection-fuites-memoire.md
  ============================================================================ }
program TestFuitesMultiples;

{$mode objfpc}{$H+}

uses
  heaptrc, Classes, SysUtils;

procedure Fuite1;
var
  s: TStringList;
begin
  s := TStringList.Create;
  s.Add('Fuite 1');
end;

procedure Fuite2;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create('test_fuite2.txt', fmCreate);
  fs.WriteBuffer('Test', 4);
  // Oubli du Free
end;

procedure Fuite3;
var
  i: Integer;
  liste: TList;
begin
  liste := TList.Create;
  for i := 1 to 10 do
    liste.Add(TObject.Create);  // 10 objets non liberes
  liste.Free;  // Libere la liste mais pas les objets !
end;

begin
  SetHeapTraceOutput('fuites_multiples.log');

  Fuite1;
  Fuite2;
  Fuite3;

  WriteLn('Programme termine. Verifiez fuites_multiples.log');
end.
