{ ============================================================================
  Section 3.4 : Boucles conditionnelles (while-do, repeat-until)
  Description : Menu interactif avec boucle while et options A/B/C/Quitter
  Fichier source : 04-boucles-conditionnelles-while-repeat.md
  ============================================================================ }

program MenuSimple;
var
  choix: Integer;
begin
  choix := 0;
  while choix <> 4 do
  begin
    WriteLn;
    WriteLn('=== MENU ===');
    WriteLn('1. Option A');
    WriteLn('2. Option B');
    WriteLn('3. Option C');
    WriteLn('4. Quitter');
    Write('Votre choix : ');
    ReadLn(choix);
    WriteLn;
    case choix of
      1: WriteLn('Vous avez choisi l''option A');
      2: WriteLn('Vous avez choisi l''option B');
      3: WriteLn('Vous avez choisi l''option C');
      4: WriteLn('Au revoir !');
    else
      WriteLn('Choix invalide !');
    end;
  end;
end.
