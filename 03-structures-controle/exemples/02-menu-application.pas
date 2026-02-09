{ ============================================================================
  Section 3.2 : Instructions de choix multiple (case-of)
  Description : Menu principal d'application avec blocs begin-end dans case-of
  Fichier source : 02-instructions-choix-multiple-case-of.md
  ============================================================================ }

program MenuApplication;
var
  choix: Integer;
begin
  WriteLn('=== MENU PRINCIPAL ===');
  WriteLn('1. Nouveau document');
  WriteLn('2. Ouvrir un document');
  WriteLn('3. Quitter');
  WriteLn;
  Write('Votre choix : ');
  ReadLn(choix);
  WriteLn;
  case choix of
    1:
      begin
        WriteLn('Creation d''un nouveau document...');
        WriteLn('Document cree avec succes !');
        WriteLn('Vous pouvez maintenant travailler.');
      end;
    2:
      begin
        WriteLn('Ouverture d''un document existant...');
        WriteLn('Veuillez selectionner un fichier.');
      end;
    3:
      begin
        WriteLn('Fermeture de l''application...');
        WriteLn('Au revoir !');
      end;
  else
    WriteLn('Choix invalide. Veuillez reessayer.');
  end;
end.
