{ ============================================================================
  Section 3.5 : Instructions break et continue
  Description : Utilisation de break avec une boucle repeat-until
  Fichier source : 05-instructions-break-continue.md
  ============================================================================ }
program BreakAvecRepeat;  
var  
  choix: Integer;
begin
  WriteLn('=== MENU ===');

  repeat
    WriteLn;
    WriteLn('1. Option A');
    WriteLn('2. Option B');
    WriteLn('3. Quitter');
    Write('Votre choix : ');
    ReadLn(choix);

    case choix of
      1: WriteLn('Option A sélectionnée');
      2: WriteLn('Option B sélectionnée');
      3:
        begin
          WriteLn('Au revoir !');
          break;  // Sort du repeat
        end;
    else
      WriteLn('Choix invalide');
    end;
  until False;  // Normalement boucle infinie

  WriteLn('Programme terminé');
end.
