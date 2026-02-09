{ ============================================================================
  Section 8.2 : Fichiers texte : ouverture, lecture, ecriture
  Description : Lecture d'un fichier avec gestion basique des erreurs
  Fichier source : 02-fichiers-texte-ouverture-lecture-ecriture.md
  ============================================================================ }
program LectureSecurisee;

var
  MonFichier: TextFile;
  Ligne: string;

begin
  Assign(MonFichier, 'donnees.txt');

  // Tentative d'ouverture
  {$I-}  // Desactive la gestion automatique des erreurs
  Reset(MonFichier);
  {$I+}  // Reactive la gestion automatique des erreurs

  // Verification
  if IOResult <> 0 then
  begin
    WriteLn('ERREUR : Le fichier n''existe pas ou est inaccessible !');
    Exit;
  end;

  // Lecture normale
  while not EOF(MonFichier) do
  begin
    ReadLn(MonFichier, Ligne);
    WriteLn(Ligne);
  end;

  Close(MonFichier);
end.
