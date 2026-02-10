{ ============================================================================
  Section 2.4 : Types de donnees primitifs
  Description : Declaration et utilisation de variables de type integer
  Fichier source : 04-types-donnees-primitifs.md
  ============================================================================ }
program ExempleInteger;  
var  
  age: integer;
  nombreEleves: integer;
  temperature: integer;
  annee: integer;
begin
  age := 25;
  nombreEleves := 30;
  temperature := -5;
  annee := 2024;

  writeln('Âge : ', age);
  writeln('Nombre d''élèves : ', nombreEleves);  // '' double l'apostrophe pour en afficher une
  writeln('Température : ', temperature, ' °C');
  writeln('Année : ', annee);
end.
