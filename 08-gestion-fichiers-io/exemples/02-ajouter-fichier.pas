{ ============================================================================
  Section 8.2 : Fichiers texte : ouverture, lecture, ecriture
  Description : Ajout de donnees a un fichier existant (mode Append)
  Fichier source : 02-fichiers-texte-ouverture-lecture-ecriture.md
  Note : Necessite que journal.txt existe deja
  ============================================================================ }
program AjouterFichier;

var
  MonFichier: TextFile;

begin
  // 1. Association
  Assign(MonFichier, 'journal.txt');

  // 2. Ouverture en mode ajout
  Append(MonFichier);

  // 3. Ajout de nouvelles lignes
  WriteLn(MonFichier, '--- Nouvelle entrée ---');
  WriteLn(MonFichier, 'Date : 13/10/2025');
  WriteLn(MonFichier, 'Message : Programme exécuté avec succès');
  WriteLn(MonFichier);  // Ligne vide

  // 4. Fermeture
  Close(MonFichier);

  WriteLn('Données ajoutées au journal !');
end.
