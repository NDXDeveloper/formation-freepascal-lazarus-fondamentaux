{ ============================================================================
  Section 8.5 : Gestion des erreurs I/O
  Description : Ouverture d'un fichier avec gestion d'erreurs IOResult
  Fichier source : 05-gestion-erreurs-io.md
  ============================================================================ }
program OuvertureSecurisee;

var
  F: TextFile;
  Ligne: string;
  CodeErreur: Integer;

begin
  Assign(F, 'donnees.txt');

  // Tentative d'ouverture avec gestion d'erreur
  {$I-}
  Reset(F);
  {$I+}

  CodeErreur := IOResult;

  if CodeErreur <> 0 then
  begin
    WriteLn('ERREUR : Impossible d''ouvrir le fichier !');
    WriteLn('Code d''erreur : ', CodeErreur);

    case CodeErreur of
      2: WriteLn('Le fichier n''existe pas.');
      3: WriteLn('Le chemin est invalide.');
      5: WriteLn('Accès refusé (permissions insuffisantes).');
    else
      WriteLn('Erreur inconnue.');
    end;

    Halt(1);  // Quitter le programme avec un code d'erreur
  end;

  // Si on arrive ici, tout s'est bien passe
  WriteLn('Fichier ouvert avec succès !');

  while not EOF(F) do
  begin
    ReadLn(F, Ligne);
    WriteLn(Ligne);
  end;

  Close(F);
  WriteLn('Traitement terminé.');
end.
