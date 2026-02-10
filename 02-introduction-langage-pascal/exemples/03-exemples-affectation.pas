{ ============================================================================
  Section 2.3 : Variables et constantes
  Description : Affectation de valeurs a differents types de variables
                (string, integer, real, boolean)
  Fichier source : 03-variables-constantes.md
  ============================================================================ }
program ExemplesAffectation;  
var  
  prenom: string;
  age: integer;
  taille: real;
  estAdulte: boolean;
begin
  // Affectation de valeurs
  prenom := 'Sophie';
  age := 30;
  taille := 1.68;
  estAdulte := true;

  // Affichage
  writeln('Prénom : ', prenom);
  writeln('Âge : ', age, ' ans');
  // :0:2 = largeur minimale 0, 2 decimales (formate le nombre reel)
  writeln('Taille : ', taille:0:2, ' m');
  writeln('Adulte : ', estAdulte);
end.
