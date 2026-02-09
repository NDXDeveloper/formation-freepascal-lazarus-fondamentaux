{ ============================================================================
  Section 8.9 : Introduction aux streams
  Description : Utilisation de TMemoryStream pour stocker des donnees en RAM
  Fichier source : 09-introduction-streams.md
  ============================================================================ }
{$mode objfpc}{$H+}
program StreamMemoire;

uses
  Classes, SysUtils;

var
  Stream: TMemoryStream;
  Texte: string;
  i: Integer;
  P: PByte;

begin
  Stream := TMemoryStream.Create;
  try
    // Ecrire des donnees en memoire
    Texte := 'Données en mémoire';
    Stream.Write(Texte[1], Length(Texte));

    // Ecrire des nombres
    for i := 1 to 10 do
      Stream.Write(i, SizeOf(i));

    WriteLn('Taille du stream en mémoire : ', Stream.Size, ' octets');

    // Retour au debut
    Stream.Position := 0;

    // Sauvegarder dans un fichier
    Stream.SaveToFile('backup_stream.dat');
    WriteLn('Stream sauvegardé dans backup_stream.dat');

    // Vider et recharger
    Stream.Clear;
    WriteLn('Stream vidé, taille : ', Stream.Size);

    Stream.LoadFromFile('backup_stream.dat');
    WriteLn('Stream rechargé : ', Stream.Size, ' octets');

    // Acces direct au buffer memoire via WriteByte
    WriteLn;
    Stream.Clear;
    for i := 0 to 9 do
      Stream.WriteByte(i * 10);

    P := Stream.Memory;
    Write('Données WriteByte : ');
    for i := 0 to Stream.Size - 1 do
      Write(P[i], ' ');
    WriteLn;

  finally
    Stream.Free;
  end;

  // Nettoyage
  if FileExists('backup_stream.dat') then
    DeleteFile('backup_stream.dat');

  WriteLn;
  WriteLn('Fichier temporaire nettoyé.');
end.
