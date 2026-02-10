{ ============================================================================
  Section 3.2 : Instructions de choix multiple (case-of)
  Description : Systeme complet de commande de restaurant avec menus imbriques
  Fichier source : 02-instructions-choix-multiple-case-of.md
  ============================================================================ }

program GestionRestaurant;  
var  
  categorie, plat: Integer;
  prix: Real;
begin
  WriteLn('=============================');
  WriteLn('  RESTAURANT LE BON PASCAL  ');
  WriteLn('=============================');
  WriteLn;
  WriteLn('Categories :');
  WriteLn('1. Entrees');
  WriteLn('2. Plats principaux');
  WriteLn('3. Desserts');
  WriteLn('4. Boissons');
  WriteLn;
  Write('Choisissez une categorie : ');
  ReadLn(categorie);
  WriteLn;
  case categorie of
    1:
      begin
        WriteLn('--- ENTREES ---');
        WriteLn('1. Salade mixte (5 euros)');
        WriteLn('2. Soupe du jour (4 euros)');
        WriteLn('3. Terrine (6 euros)');
        Write('Votre choix : ');
        ReadLn(plat);
        case plat of
          1: prix := 5.0;
          2: prix := 4.0;
          3: prix := 6.0;
        else
          begin
            WriteLn('Plat inconnu !');
            prix := 0;
          end;
        end;
      end;
    2:
      begin
        WriteLn('--- PLATS PRINCIPAUX ---');
        WriteLn('1. Steak-frites (15 euros)');
        WriteLn('2. Poisson grille (18 euros)');
        WriteLn('3. Pates carbonara (12 euros)');
        WriteLn('4. Pizza margherita (10 euros)');
        Write('Votre choix : ');
        ReadLn(plat);
        case plat of
          1: prix := 15.0;
          2: prix := 18.0;
          3: prix := 12.0;
          4: prix := 10.0;
        else
          begin
            WriteLn('Plat inconnu !');
            prix := 0;
          end;
        end;
      end;
    3:
      begin
        WriteLn('--- DESSERTS ---');
        WriteLn('1. Tarte aux pommes (5 euros)');
        WriteLn('2. Creme brulee (6 euros)');
        WriteLn('3. Mousse au chocolat (5 euros)');
        Write('Votre choix : ');
        ReadLn(plat);
        case plat of
          1, 3: prix := 5.0;
          2: prix := 6.0;
        else
          begin
            WriteLn('Dessert inconnu !');
            prix := 0;
          end;
        end;
      end;
    4:
      begin
        WriteLn('--- BOISSONS ---');
        WriteLn('1. Eau (2 euros)');
        WriteLn('2. Jus (3 euros)');
        WriteLn('3. Soda (3 euros)');
        WriteLn('4. Cafe (2.5 euros)');
        Write('Votre choix : ');
        ReadLn(plat);
        case plat of
          1: prix := 2.0;
          2, 3: prix := 3.0;
          4: prix := 2.5;
        else
          begin
            WriteLn('Boisson inconnue !');
            prix := 0;
          end;
        end;
      end;
  else
    begin
      WriteLn('Categorie inconnue !');
      prix := 0;
    end;
  end;
  WriteLn;
  if prix > 0 then
  begin
    WriteLn('=============================');
    WriteLn('Prix : ', prix:0:2, ' euros');
    WriteLn('Merci pour votre commande !');
    WriteLn('=============================');
  end;
end.
