{ ============================================================================
  Section 3.6 : Imbrication de structures
  Description : REPEAT dans WHILE - menu interactif avec validation de saisie
  Fichier source : 06-imbrication-structures.md
  ============================================================================ }
program MenuAvecValidation;  
var  
  continuer: Boolean;
  choix: Integer;
  valeur: Integer;
begin
  continuer := True;

  while continuer do
  begin
    WriteLn;
    WriteLn('=== MENU ===');
    WriteLn('1. Entrer un nombre');
    WriteLn('2. Quitter');
    Write('Choix : ');
    ReadLn(choix);
    WriteLn;

    case choix of
      1:
        begin
          // Validation avec REPEAT imbriqué
          repeat
            Write('Entrez un nombre entre 1 et 100 : ');
            ReadLn(valeur);

            if (valeur < 1) or (valeur > 100) then
              WriteLn('Invalide ! Réessayez.');
          until (valeur >= 1) and (valeur <= 100);

          WriteLn('Valeur acceptée : ', valeur);
        end;

      2:
        begin
          WriteLn('Au revoir !');
          continuer := False;
        end;

    else
      WriteLn('Choix invalide');
    end;
  end;
end.
