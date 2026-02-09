{ ============================================================================
  Section 8.9 : Introduction aux streams
  Description : Ecriture et lecture avec TFileStream
  Fichier source : 09-introduction-streams.md
  ============================================================================ }
{$mode objfpc}{$H+}
program EcrireLireStream;

uses
  Classes, SysUtils;

var
  Stream: TFileStream;
  Texte: string;
  Nombre: Integer;
  Buffer: array[0..255] of Char;
  NbLus: Integer;

begin
  // Ecriture
  WriteLn('--- Écriture dans le stream ---');
  Stream := TFileStream.Create('test_stream.dat', fmCreate);
  try
    Texte := 'Bonjour le monde !';
    Stream.Write(Texte[1], Length(Texte));  // Texte[1] donne l'adresse du 1er caractère (Write attend un buffer, pas un pointeur de string)

    Nombre := 42;
    Stream.Write(Nombre, SizeOf(Nombre));

    WriteLn('Données écrites : ', Stream.Size, ' octets');
  finally
    Stream.Free;
  end;

  // Lecture
  WriteLn;
  WriteLn('--- Lecture depuis le stream ---');
  Stream := TFileStream.Create('test_stream.dat', fmOpenRead);
  try
    NbLus := Stream.Read(Buffer, SizeOf(Buffer));

    WriteLn('Octets lus : ', NbLus);
    WriteLn('Taille totale du stream : ', Stream.Size);
  finally
    Stream.Free;
  end;

  // Nettoyage
  if FileExists('test_stream.dat') then
    DeleteFile('test_stream.dat');

  WriteLn;
  WriteLn('Fichier de test nettoyé.');
end.
