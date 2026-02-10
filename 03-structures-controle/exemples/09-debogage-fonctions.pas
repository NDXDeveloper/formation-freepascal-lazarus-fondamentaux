{ ============================================================================
  Section 3.9 : Debogage pas a pas
  Description : Debogage de fonctions avec appels multiples et pile d'appels
  Fichier source : 09-debogage-pas-a-pas.md
  ============================================================================ }
program DebogageFonctions;

function Carre(n: Integer): Integer;  
begin  
  Carre := n * n;  // Point d'arrêt ici
end;

function SommeCarres(a, b: Integer): Integer;  
var  
  carreA, carreB: Integer;
begin
  carreA := Carre(a);    // F7 pour entrer dans Carre
  carreB := Carre(b);    // F7 pour entrer à nouveau
  SommeCarres := carreA + carreB;
end;

begin
  WriteLn('Résultat : ', SommeCarres(3, 4));  // Point d'arrêt ici
end.
