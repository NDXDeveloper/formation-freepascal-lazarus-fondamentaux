{ ============================================================================
  Section 4.8 : Variables locales vs globales
  Description : Demonstration de la portee des variables locales et globales
  Fichier source : 08-variables-locales-vs-globales.md
  ============================================================================ }
program Demonstration;

var
  globale: Integer;

procedure Proc1;
var
  locale1: Integer;
begin
  globale := 5;
  locale1 := 10;
end;

procedure Proc2;
var
  locale2: Integer;
begin
  globale := 15;
  // locale1 := 20;  // Erreur : locale1 n'existe pas ici
  locale2 := 25;
end;

begin
  globale := 100;
  // locale1 := 50;  // Erreur : locale1 n'existe pas ici
  // locale2 := 75;  // Erreur : locale2 n'existe pas ici
end.
