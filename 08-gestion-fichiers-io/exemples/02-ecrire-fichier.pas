{ ============================================================================
  Section 8.2 : Fichiers texte : ouverture, lecture, ecriture
  Description : Ecriture dans un fichier texte (creation de nombres.txt)
  Fichier source : 02-fichiers-texte-ouverture-lecture-ecriture.md
  ============================================================================ }
program EcrireFichier;

var
  MonFichier: TextFile;
  i: Integer;

begin
  // 1. Association
  Assign(MonFichier, 'nombres.txt');

  // 2. Ouverture en ecriture (creation)
  Rewrite(MonFichier);

  // 3. Ecriture
  WriteLn(MonFichier, 'Liste des premiers nombres :');
  WriteLn(MonFichier, '----------------------------');

  for i := 1 to 10 do
    WriteLn(MonFichier, 'Nombre ', i, ' : ', i * i);

  // 4. Fermeture
  Close(MonFichier);

  WriteLn('Fichier créé avec succès !');
end.
