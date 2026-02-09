{ ============================================================================
  Section 8.3 : Fichiers binaires et acces direct
  Description : Copie d'un fichier binaire bloc par bloc
  Fichier source : 03-fichiers-binaires-acces-direct.md
  Note : Cree un fichier test puis le copie pour la demonstration
  ============================================================================ }
program CopierFichier;

const
  TAILLE_BUFFER = 8192;  // 8 Ko par bloc

var
  FichierSource, FichierDest: File;
  Buffer: array[1..TAILLE_BUFFER] of Byte;
  NbLus, NbEcrits: Word;
  TotalCopie: LongInt;
  FichierTest: TextFile;
  i: Integer;

begin
  // Creer un petit fichier source pour la demonstration
  Assign(FichierTest, 'test_source.txt');
  Rewrite(FichierTest);
  for i := 1 to 100 do
    WriteLn(FichierTest, 'Ligne de test numero ', i);
  Close(FichierTest);
  WriteLn('Fichier source créé.');

  // Association des fichiers
  Assign(FichierSource, 'test_source.txt');
  Assign(FichierDest, 'test_copie.txt');

  // Ouverture
  Reset(FichierSource, 1);
  Rewrite(FichierDest, 1);

  TotalCopie := 0;

  // Copie bloc par bloc
  repeat
    BlockRead(FichierSource, Buffer, TAILLE_BUFFER, NbLus);

    if NbLus > 0 then
    begin
      BlockWrite(FichierDest, Buffer, NbLus, NbEcrits);
      TotalCopie := TotalCopie + NbLus;
    end;

  until NbLus < TAILLE_BUFFER;  // Fin quand on lit moins qu'un bloc complet

  // Fermeture
  Close(FichierSource);
  Close(FichierDest);

  WriteLn('Fichier copié : ', TotalCopie, ' octets');

  // Nettoyage des fichiers de test
  Assign(FichierSource, 'test_source.txt');
  Erase(FichierSource);
  Assign(FichierDest, 'test_copie.txt');
  Erase(FichierDest);
end.
