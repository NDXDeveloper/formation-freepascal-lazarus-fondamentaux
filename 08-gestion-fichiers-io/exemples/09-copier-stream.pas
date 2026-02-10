{ ============================================================================
  Section 8.9 : Introduction aux streams
  Description : Copie de fichier avec TFileStream et CopyFrom
  Fichier source : 09-introduction-streams.md
  ============================================================================ }
{$mode objfpc}{$H+}
program CopierStream;

uses
  Classes, SysUtils;

procedure CopierFichierStream(Source, Destination: string);  
var  
  StreamSource, StreamDest: TFileStream;
begin
  StreamSource := TFileStream.Create(Source, fmOpenRead);
  try
    StreamDest := TFileStream.Create(Destination, fmCreate);
    try
      StreamDest.CopyFrom(StreamSource, 0);  // 0 = copier tout
      WriteLn('Fichier copié : ', StreamDest.Size, ' octets');
    finally
      StreamDest.Free;
    end;
  finally
    StreamSource.Free;
  end;
end;

var
  FSource: TextFile;

begin
  // Creer un fichier source de test
  Assign(FSource, 'source_test.txt');
  Rewrite(FSource);
  WriteLn(FSource, 'Ligne 1 : Bonjour');
  WriteLn(FSource, 'Ligne 2 : Ceci est un test de copie par stream');
  WriteLn(FSource, 'Ligne 3 : Au revoir');
  Close(FSource);
  WriteLn('Fichier source créé.');

  // Copier avec stream
  CopierFichierStream('source_test.txt', 'copie_test.txt');

  // Nettoyage
  if FileExists('source_test.txt') then
    DeleteFile('source_test.txt');
  if FileExists('copie_test.txt') then
    DeleteFile('copie_test.txt');

  WriteLn('Fichiers temporaires nettoyés.');
end.
