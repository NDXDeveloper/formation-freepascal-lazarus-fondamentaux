{ ============================================================================
  Section 4.10 : Fonctions predefinies utiles
  Description : Mini-calculatrice avec fonctions predefinies (Math, SysUtils)
  Fichier source : 10-fonctions-predefinies-utiles.md
  ============================================================================ }
program MiniCalculatrice;

uses
  SysUtils, Math;

procedure AfficherMenu;
begin
  WriteLn('=== CALCULATRICE ===');
  WriteLn('1. Addition');
  WriteLn('2. Multiplication');
  WriteLn('3. Puissance');
  WriteLn('4. Racine carrée');
  WriteLn('5. Arrondir');
  WriteLn('6. Nombre aléatoire');
  WriteLn('0. Quitter');
  WriteLn('====================');
end;

var
  choix: Integer;
  a, b, resultat: Real;
  texte: String;
begin
  Randomize;  // Initialise le générateur aléatoire (à appeler une seule fois)

  repeat
    AfficherMenu;
    Write('Votre choix : ');
    ReadLn(texte);
    choix := StrToIntDef(texte, -1);  // Convertit en entier, retourne -1 si invalide

    case choix of
      1: begin
           Write('Premier nombre : ');
           ReadLn(a);
           Write('Deuxième nombre : ');
           ReadLn(b);
           resultat := a + b;
           WriteLn('Résultat : ', Format('%.2f', [resultat]));
         end;

      2: begin
           Write('Premier nombre : ');
           ReadLn(a);
           Write('Deuxième nombre : ');
           ReadLn(b);
           resultat := a * b;
           WriteLn('Résultat : ', Format('%.2f', [resultat]));
         end;

      3: begin
           Write('Base : ');
           ReadLn(a);
           Write('Exposant : ');
           ReadLn(b);
           resultat := Power(a, b);
           WriteLn('Résultat : ', Format('%.2f', [resultat]));
         end;

      4: begin
           Write('Nombre : ');
           ReadLn(a);
           if a >= 0 then
           begin
             resultat := Sqrt(a);
             WriteLn('Résultat : ', Format('%.5f', [resultat]));
           end
           else
             WriteLn('Erreur : nombre négatif');
         end;

      5: begin
           Write('Nombre : ');
           ReadLn(a);
           WriteLn('Arrondi : ', Round(a));
           WriteLn('Tronqué : ', Trunc(a));
         end;

      6: begin
           WriteLn('Nombre aléatoire entre 1 et 100 : ', Random(100) + 1);
         end;

      0: WriteLn('Au revoir !');

      else
        WriteLn('Choix invalide');
    end;

    WriteLn;
  until choix = 0;
end.
