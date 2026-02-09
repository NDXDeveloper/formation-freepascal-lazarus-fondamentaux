{
  Section 19.1 - Différences fondamentales Windows/Linux
  Description : Affiche les constantes portables de FreePascal
                (PathDelim, DirectorySeparator, PathSeparator, LineEnding)
  Fichier source : 01-differences-windows-linux.md
}
program ConstantesPortables;

{$mode objfpc}{$H+}

uses
  SysUtils;

begin
  WriteLn('=== Constantes portables FreePascal ===');
  WriteLn;

  { Section 1 - Séparateurs de chemins }
  WriteLn('--- Séparateurs de chemins ---');
  WriteLn('PathDelim            : "', PathDelim, '"');
  WriteLn('DirectorySeparator   : "', DirectorySeparator, '"');
  WriteLn('PathSeparator        : "', PathSeparator, '"');
  WriteLn;

  { Section 4 - Fins de lignes }
  WriteLn('--- Fins de lignes ---');
  WriteLn('LineEnding (longueur): ', Length(LineEnding));
  {$IFDEF WINDOWS}
  WriteLn('LineEnding           : CR+LF (Windows)');
  {$ELSE}
  WriteLn('LineEnding           : LF (Unix/Linux)');
  {$ENDIF}
  WriteLn;

  { Construction d''un chemin portable }
  WriteLn('--- Exemple de chemin portable ---');
  WriteLn('Chemin : ', 'Data' + PathDelim + 'fichier.txt');
  WriteLn('Avec IncludeTrailingPathDelimiter :');
  WriteLn('  ', IncludeTrailingPathDelimiter(GetCurrentDir) + 'fichier.txt');
end.
