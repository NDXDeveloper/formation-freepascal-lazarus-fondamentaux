{ ============================================================================
  Section 7.7 : Unites standard du RTL
  Description : Demonstration de TStringList (unite Classes)
  Fichier source : 07-unites-standard-rtl.md
  ============================================================================ }
{$mode objfpc}{$H+}
program ClassesStringList;

uses
  Classes, SysUtils;

var
  liste: TStringList;
  i: Integer;

begin
  liste := TStringList.Create;
  try
    // Ajouter des elements
    liste.Add('Pomme');
    liste.Add('Banane');
    liste.Add('Orange');

    // Acceder aux elements
    WriteLn('Premier : ', liste[0]);  // 'Pomme'
    WriteLn('Nombre : ', liste.Count);  // 3

    // Parcourir la liste
    for i := 0 to liste.Count - 1 do
      WriteLn(i, ': ', liste[i]);

    // Trier
    liste.Sort;
    WriteLn('Apres tri :');
    for i := 0 to liste.Count - 1 do
      WriteLn(i, ': ', liste[i]);

    // Rechercher
    if liste.IndexOf('Banane') >= 0 then
      WriteLn('Banane trouvée !');

  finally
    liste.Free;  // Toujours liberer !
  end;
end.
