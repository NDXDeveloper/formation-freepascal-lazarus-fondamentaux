{ ============================================================================
  Section 3.5 : Instructions break et continue
  Description : Menu interactif avance avec sous-menus et navigation
  Fichier source : 05-instructions-break-continue.md
  ============================================================================ }
program MenuInteractifAvance;
var
  choixPrincipal, sousChoix: Integer;
  continuer: Boolean;
begin
  continuer := True;

  WriteLn('=============================');
  WriteLn('   APPLICATION DE GESTION   ');
  WriteLn('=============================');

  while continuer do
  begin
    WriteLn;
    WriteLn('--- MENU PRINCIPAL ---');
    WriteLn('1. Gestion des fichiers');
    WriteLn('2. Gestion des utilisateurs');
    WriteLn('3. Paramètres');
    WriteLn('4. Quitter');
    Write('Choix : ');
    ReadLn(choixPrincipal);
    WriteLn;

    case choixPrincipal of
      1:  // Gestion fichiers
        begin
          WriteLn('--- FICHIERS ---');
          WriteLn('1. Nouveau');
          WriteLn('2. Ouvrir');
          WriteLn('3. Retour');
          Write('Choix : ');
          ReadLn(sousChoix);

          if sousChoix = 3 then
            continue;  // Retour au menu principal

          case sousChoix of
            1: WriteLn('Création d''un nouveau fichier...');
            2: WriteLn('Ouverture d''un fichier...');
          else
            WriteLn('Option invalide');
          end;
        end;

      2:  // Gestion utilisateurs
        begin
          WriteLn('--- UTILISATEURS ---');
          WriteLn('1. Ajouter');
          WriteLn('2. Modifier');
          WriteLn('3. Retour');
          Write('Choix : ');
          ReadLn(sousChoix);

          if sousChoix = 3 then
            continue;  // Retour au menu principal

          case sousChoix of
            1: WriteLn('Ajout d''un utilisateur...');
            2: WriteLn('Modification d''un utilisateur...');
          else
            WriteLn('Option invalide');
          end;
        end;

      3:  // Paramètres
        WriteLn('Accès aux paramètres...');

      4:  // Quitter
        begin
          WriteLn('Au revoir !');
          continuer := False;
          break;  // Sort de la boucle while
        end;

    else
      WriteLn('Choix invalide');
    end;
  end;
end.
