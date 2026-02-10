{ ============================================================================
  Section 2.6 : Entrees/sorties console
  Description : Menu interactif avec boucle while et case..of
  Fichier source : 06-entrees-sorties-console.md
  ============================================================================ }
program MenuInteractif;  
var  
  choix: integer;
  continuer: boolean;
begin
  continuer := true;

  while continuer do
  begin
    WriteLn;
    WriteLn('===================');
    WriteLn('   MENU PRINCIPAL  ');
    WriteLn('===================');
    WriteLn('1. Option A');
    WriteLn('2. Option B');
    WriteLn('3. Option C');
    WriteLn('0. Quitter');
    WriteLn('===================');
    WriteLn;

    Write('Votre choix : ');
    ReadLn(choix);
    WriteLn;

    case choix of
      1: WriteLn('Vous avez choisi l''option A');
      2: WriteLn('Vous avez choisi l''option B');
      3: WriteLn('Vous avez choisi l''option C');
      0:
        begin
          WriteLn('Au revoir !');
          continuer := false;
        end;
    else
      WriteLn('Choix invalide !');
    end;

    if continuer then
    begin
      WriteLn;
      Write('Appuyez sur Entrée pour continuer...');
      ReadLn;
    end;
  end;
end.
