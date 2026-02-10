{ ============================================================================
  Section 8.5 : Gestion des erreurs I/O
  Description : Ecriture dans un fichier avec gestion complete des erreurs
  Fichier source : 05-gestion-erreurs-io.md
  ============================================================================ }
program EcritureSecurisee;

function EcrireDansFichier(NomFichier: string; Donnees: array of string): Boolean;  
var  
  F: TextFile;
  i: Integer;
  CodeErreur: Integer;
begin
  EcrireDansFichier := False;

  Assign(F, NomFichier);

  // Ouverture en ecriture
  {$I-}
  Rewrite(F);
  {$I+}

  CodeErreur := IOResult;

  if CodeErreur <> 0 then
  begin
    WriteLn('Impossible de créer le fichier !');
    case CodeErreur of
      5: WriteLn('→ Accès refusé');
      101: WriteLn('→ Disque plein ou protégé en écriture');
    else
      WriteLn('→ Erreur ', CodeErreur);
    end;
    Exit;
  end;

  // Ecriture
  for i := Low(Donnees) to High(Donnees) do
  begin
    {$I-}
    WriteLn(F, Donnees[i]);
    {$I+}

    CodeErreur := IOResult;

    if CodeErreur <> 0 then
    begin
      WriteLn('Erreur lors de l''écriture !');
      case CodeErreur of
        101: WriteLn('→ Disque plein !');
      else
        WriteLn('→ Erreur ', CodeErreur);
      end;
      Close(F);
      Exit;
    end;
  end;

  // Fermeture
  {$I-}
  Close(F);
  {$I+}

  if IOResult = 0 then
    EcrireDansFichier := True;
end;

var
  Lignes: array[1..3] of string = ('Ligne 1', 'Ligne 2', 'Ligne 3');
  FClean: File;

begin
  if EcrireDansFichier('sortie.txt', Lignes) then
    WriteLn('Écriture réussie !')
  else
    WriteLn('Échec de l''écriture.');

  // Nettoyage du fichier de test
  Assign(FClean, 'sortie.txt');
  {$I-}
  Erase(FClean);
  {$I+}
  IOResult;  // Vider le code d'erreur
end.
