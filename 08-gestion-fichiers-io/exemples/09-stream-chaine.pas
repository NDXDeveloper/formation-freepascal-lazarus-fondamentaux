{ ============================================================================
  Section 8.9 : Introduction aux streams
  Description : Utilisation de TStringStream pour manipuler des chaines
  Fichier source : 09-introduction-streams.md
  ============================================================================ }
{$mode objfpc}{$H+}
program StreamChaine;

uses
  Classes, SysUtils;

var
  Stream: TStringStream;
  Texte: string;

begin
  // Creer avec un contenu initial
  Stream := TStringStream.Create('Contenu initial');
  try
    WriteLn('Contenu : ', Stream.DataString);

    // Se placer a la fin pour ajouter du contenu
    Stream.Seek(0, soFromEnd);
    Stream.WriteString(' - Ajout de texte');

    WriteLn('Nouveau contenu : ', Stream.DataString);

    // Retour au debut et lecture
    Stream.Position := 0;

    WriteLn('Taille : ', Stream.Size, ' octets');
  finally
    Stream.Free;
  end;

  WriteLn;

  // Conversion entre chaines et streams
  Texte := 'Bonjour tout le monde !';
  Stream := TStringStream.Create(Texte);
  try
    WriteLn('Taille du stream : ', Stream.Size);

    // De stream vers chaine
    Texte := Stream.DataString;
    WriteLn('Récupéré : ', Texte);
  finally
    Stream.Free;
  end;
end.
