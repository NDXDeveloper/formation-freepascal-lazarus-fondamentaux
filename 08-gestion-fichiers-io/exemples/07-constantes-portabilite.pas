{ ============================================================================
  Section 8.7 : Chemins et noms de fichiers
  Description : Constantes de portabilite Windows/Linux
  Fichier source : 07-chemins-noms-fichiers.md
  ============================================================================ }
{$mode objfpc}{$H+}
program ConstantesPortabilite;

uses
  SysUtils;

begin
  WriteLn('Séparateur de chemin    : ', PathDelim);
  WriteLn('Séparateur de répertoire: ', DirectorySeparator);
  WriteLn('Séparateur de lecteur   : ', DriveDelim);
  WriteLn('Séparateur de PATH      : ', PathSep);
  WriteLn('Suffixe bibliothèque    : ', SharedSuffix);
end.
