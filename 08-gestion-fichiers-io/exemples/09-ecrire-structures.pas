{ ============================================================================
  Section 8.9 : Introduction aux streams
  Description : Ecriture et lecture de structures (records) dans un stream
  Fichier source : 09-introduction-streams.md
  ============================================================================ }
{$mode objfpc}{$H+}
program EcrireStructures;

uses
  Classes, SysUtils;

type
  TPersonne = record
    ID: Integer;
    Nom: string[50];
    Age: Integer;
    Salaire: Double;
  end;

procedure EcrirePersonne(Stream: TStream; const P: TPersonne);
begin
  Stream.Write(P, SizeOf(P));
end;

function LirePersonne(Stream: TStream): TPersonne;
begin
  Stream.Read(Result, SizeOf(Result));
end;

var
  Stream: TFileStream;
  P1, P2, P3: TPersonne;
  PLue: TPersonne;

begin
  // Creer des personnes
  P1.ID := 1; P1.Nom := 'Dupont'; P1.Age := 30; P1.Salaire := 35000;
  P2.ID := 2; P2.Nom := 'Martin'; P2.Age := 25; P2.Salaire := 28000;
  P3.ID := 3; P3.Nom := 'Durand'; P3.Age := 45; P3.Salaire := 52000;

  // Ecrire
  Stream := TFileStream.Create('personnes.dat', fmCreate);
  try
    EcrirePersonne(Stream, P1);
    EcrirePersonne(Stream, P2);
    EcrirePersonne(Stream, P3);
    WriteLn('3 personnes écrites');
  finally
    Stream.Free;
  end;

  // Lire
  Stream := TFileStream.Create('personnes.dat', fmOpenRead);
  try
    WriteLn;
    WriteLn('=== LECTURE ===');

    while Stream.Position < Stream.Size do
    begin
      PLue := LirePersonne(Stream);
      WriteLn('ID: ', PLue.ID, ' - ', PLue.Nom, ' - ', PLue.Age, ' ans - ',
              PLue.Salaire:0:2, ' €');
    end;
  finally
    Stream.Free;
  end;

  // Nettoyage
  if FileExists('personnes.dat') then
    DeleteFile('personnes.dat');

  WriteLn;
  WriteLn('Fichier temporaire nettoyé.');
end.
