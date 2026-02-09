{ ============================================================================
  Section 19.8 : Tests sur differentes plateformes
  Description : Tests fonctionnels multi-plateformes (fichiers et repertoires)
  Fichier source : 08-tests-differentes-plateformes.md
  ============================================================================ }
program TestFonctionnel;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  TestsPasses, TestsEchoues: Integer;

procedure Test(const Nom: string; Condition: Boolean);
begin
  Write(Nom, '... ');
  if Condition then
  begin
    WriteLn('OK');
    Inc(TestsPasses);
  end
  else
  begin
    WriteLn('ECHEC');
    Inc(TestsEchoues);
  end;
end;

procedure TestsCreationFichier;
var
  F: TextFile;
  TestFile: string;
begin
  TestFile := GetTempDir + 'test.txt';

  try
    AssignFile(F, TestFile);
    Rewrite(F);
    WriteLn(F, 'Test');
    CloseFile(F);

    Test('Creation de fichier', FileExists(TestFile));

    DeleteFile(TestFile);
    Test('Suppression de fichier', not FileExists(TestFile));
  except
    Test('Creation/Suppression de fichier', False);
  end;
end;

procedure TestsRepertoires;
var
  TestDir: string;
begin
  TestDir := GetTempDir + 'test_dir';

  Test('Creation de repertoire', CreateDir(TestDir));
  Test('Repertoire existe', DirectoryExists(TestDir));
  Test('Suppression de repertoire', RemoveDir(TestDir));
end;

begin
  WriteLn('========================================');
  WriteLn('Tests Fonctionnels Multi-Plateformes');
  WriteLn('Plateforme : ', {$I %FPCTARGETOS%});
  WriteLn('========================================');
  WriteLn;

  TestsPasses := 0;
  TestsEchoues := 0;

  WriteLn('--- Tests Fichiers ---');
  TestsCreationFichier;

  WriteLn;
  WriteLn('--- Tests Repertoires ---');
  TestsRepertoires;

  WriteLn;
  WriteLn('========================================');
  WriteLn('Resultats :');
  WriteLn('  Reussis : ', TestsPasses);
  WriteLn('  Echoues : ', TestsEchoues);
  WriteLn('========================================');

  if TestsEchoues > 0 then
    Halt(1);
end.
