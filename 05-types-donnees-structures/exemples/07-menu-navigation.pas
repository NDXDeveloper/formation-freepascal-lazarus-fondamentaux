{ ============================================================================
  Section 5.7 : Types enumeres
  Description : Menu de navigation avec type enumere pour les actions
  Fichier source : 07-types-enumeres.md
  ============================================================================ }
program MenuNavigation;  
type  
  TMenuPrincipal = (Nouveau, Ouvrir, Enregistrer, Quitter);

var
  choix: TMenuPrincipal;
  saisie: Integer;

procedure AfficherMenu;  
begin  
  WriteLn;
  WriteLn('=== MENU PRINCIPAL ===');
  WriteLn('1. Nouveau');
  WriteLn('2. Ouvrir');
  WriteLn('3. Enregistrer');
  WriteLn('4. Quitter');
  Write('Votre choix : ');
end;

procedure ExecuterAction(action: TMenuPrincipal);  
begin  
  case action of
    Nouveau:
      WriteLn('Création d''un nouveau fichier...');
    Ouvrir:
      WriteLn('Ouverture d''un fichier existant...');
    Enregistrer:
      WriteLn('Enregistrement du fichier...');
    Quitter:
      WriteLn('Au revoir !');
  end;
end;

begin
  repeat
    AfficherMenu;
    ReadLn(saisie);

    case saisie of
      1: choix := Nouveau;
      2: choix := Ouvrir;
      3: choix := Enregistrer;
      4: choix := Quitter;
    else
      begin
        WriteLn('Choix invalide');
        Continue;
      end;
    end;

    ExecuterAction(choix);
  until choix = Quitter;
end.
