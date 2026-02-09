{ ============================================================================
  Section 2.4 : Types de donnees primitifs
  Description : Declaration et utilisation de variables de type real
                (nombres a virgule)
  Fichier source : 04-types-donnees-primitifs.md
  ============================================================================ }
program ExempleReal;
var
  prix: real;
  taille: real;
  temperature: real;
  pi: real;
begin
  prix := 29.99;
  taille := 1.75;
  temperature := 20.5;
  pi := 3.14159;

  writeln('Prix : ', prix:0:2, ' €');            // :0:2 = afficher avec 2 decimales
  writeln('Taille : ', taille:0:2, ' m');
  writeln('Température : ', temperature:0:1, ' °C');
  writeln('Pi : ', pi:0:5);
end.
