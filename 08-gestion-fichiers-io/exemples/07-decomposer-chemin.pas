{ ============================================================================
  Section 8.7 : Chemins et noms de fichiers
  Description : Decomposition d'un chemin de fichier en ses composants
  Fichier source : 07-chemins-noms-fichiers.md
  ============================================================================ }
{$mode objfpc}{$H+}
program DecomposerChemin;

uses
  SysUtils;

procedure AnalyserChemin(Chemin: string);  
begin  
  WriteLn('=== ANALYSE DU CHEMIN ===');
  WriteLn('Chemin complet  : ', Chemin);
  WriteLn('Lecteur         : ', ExtractFileDrive(Chemin));
  WriteLn('Répertoire (/)  : ', ExtractFilePath(Chemin));
  WriteLn('Répertoire      : ', ExtractFileDir(Chemin));
  WriteLn('Nom du fichier  : ', ExtractFileName(Chemin));
  WriteLn('Extension       : ', ExtractFileExt(Chemin));
  WriteLn('========================');
end;

begin
  AnalyserChemin('/home/jean/documents/rapport.pdf');
  WriteLn;
  AnalyserChemin('config/settings.ini');
end.
