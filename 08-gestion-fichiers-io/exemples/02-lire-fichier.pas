{ ============================================================================
  Section 8.2 : Fichiers texte : ouverture, lecture, ecriture
  Description : Lecture d'un fichier texte ligne par ligne
  Fichier source : 02-fichiers-texte-ouverture-lecture-ecriture.md
  Note : Necessite d'executer 02-ecrire-fichier d'abord (cree nombres.txt)
  ============================================================================ }
program LireFichier;

var
  MonFichier: TextFile;
  Ligne: string;
  NumLigne: Integer;

begin
  // 1. Association
  Assign(MonFichier, 'nombres.txt');

  // 2. Ouverture en lecture
  Reset(MonFichier);

  // 3. Lecture ligne par ligne
  NumLigne := 1;

  while not EOF(MonFichier) do
  begin
    ReadLn(MonFichier, Ligne);
    WriteLn('Ligne ', NumLigne, ' : ', Ligne);
    Inc(NumLigne);
  end;

  // 4. Fermeture
  Close(MonFichier);

  WriteLn('Lecture terminée !');
end.
