{ ============================================================================
  Section 19.2 : Gestion portable des chemins
  Description : Analyse des differentes parties d'un chemin de fichier
  Fichier source : 02-gestion-portable-chemins.md
  ============================================================================ }
program AnalyseChemin;

{$mode objfpc}{$H+}

uses
  SysUtils;

var
  Chemin: string;
begin
  Chemin := IncludeTrailingPathDelimiter(GetCurrentDir) + 'rapport_final.pdf';

  WriteLn('Chemin complet : ', Chemin);
  WriteLn('Repertoire     : ', ExtractFilePath(Chemin));
  WriteLn('Nom fichier    : ', ExtractFileName(Chemin));
  WriteLn('Extension      : ', ExtractFileExt(Chemin));
  WriteLn('Sans extension : ', ChangeFileExt(ExtractFileName(Chemin), ''));
end.
