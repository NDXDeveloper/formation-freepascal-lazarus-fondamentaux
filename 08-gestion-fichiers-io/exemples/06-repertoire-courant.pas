{ ============================================================================
  Section 8.6 : Manipulation de repertoires
  Description : Afficher le repertoire courant avec GetCurrentDir
  Fichier source : 06-manipulation-repertoires.md
  ============================================================================ }
{$mode objfpc}{$H+}
program AfficherRepertoireCourant;

uses
  SysUtils;

var
  RepCourant: string;

begin
  RepCourant := GetCurrentDir;
  WriteLn('Répertoire courant : ', RepCourant);
end.
