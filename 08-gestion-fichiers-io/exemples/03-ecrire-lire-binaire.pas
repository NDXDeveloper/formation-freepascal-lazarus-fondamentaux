{ ============================================================================
  Section 8.3 : Fichiers binaires et acces direct
  Description : Ecriture et lecture de donnees binaires avec BlockWrite/BlockRead
  Fichier source : 03-fichiers-binaires-acces-direct.md
  ============================================================================ }
program EcrireLireBinaire;

var
  MonFichier: File;  // File sans type = fichier binaire (contrairement à TextFile)
  Buffer: array[1..100] of Byte;
  NbEcrits, NbLus: Word;
  i: Integer;

begin
  // --- Ecriture ---
  Assign(MonFichier, 'donnees.dat');
  Rewrite(MonFichier, 1);  // 1 = taille d'un enregistrement en octets (ici, octet par octet)

  // Remplir le buffer avec des donnees
  Buffer[1] := 65;   // 'A' en ASCII
  Buffer[2] := 66;   // 'B' en ASCII
  Buffer[3] := 67;   // 'C' en ASCII

  // Ecrire 3 octets dans le fichier
  BlockWrite(MonFichier, Buffer, 3, NbEcrits);
  WriteLn('Octets écrits : ', NbEcrits);
  Close(MonFichier);

  // --- Lecture ---
  Assign(MonFichier, 'donnees.dat');
  Reset(MonFichier, 1);

  // Lire 3 octets du fichier
  BlockRead(MonFichier, Buffer, 3, NbLus);
  WriteLn('Octets lus : ', NbLus);

  // Afficher les donnees lues
  for i := 1 to NbLus do
    Write(Chr(Buffer[i]));  // Convertit en caractere
  WriteLn;

  Close(MonFichier);

  // Nettoyage
  Erase(MonFichier);
end.
