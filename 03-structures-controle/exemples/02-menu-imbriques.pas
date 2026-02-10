{ ============================================================================
  Section 3.2 : Instructions de choix multiple (case-of)
  Description : Case-of imbriques pour un systeme de menu a deux niveaux
  Fichier source : 02-instructions-choix-multiple-case-of.md
  ============================================================================ }

program MenuImbriques;  
var  
  categorie, choix: Integer;
begin
  WriteLn('Categories :');
  WriteLn('1. Fichier');
  WriteLn('2. Edition');
  Write('Choisissez une categorie : ');
  ReadLn(categorie);
  case categorie of
    1:
      begin
        WriteLn('1. Nouveau');
        WriteLn('2. Ouvrir');
        WriteLn('3. Enregistrer');
        Write('Votre choix : ');
        ReadLn(choix);
        case choix of
          1: WriteLn('Nouveau fichier cree');
          2: WriteLn('Ouvrir un fichier');
          3: WriteLn('Enregistrer le fichier');
        else
          WriteLn('Choix invalide');
        end;
      end;
    2:
      begin
        WriteLn('1. Copier');
        WriteLn('2. Coller');
        WriteLn('3. Couper');
        Write('Votre choix : ');
        ReadLn(choix);
        case choix of
          1: WriteLn('Texte copie');
          2: WriteLn('Texte colle');
          3: WriteLn('Texte coupe');
        else
          WriteLn('Choix invalide');
        end;
      end;
  else
    WriteLn('Categorie invalide');
  end;
end.
