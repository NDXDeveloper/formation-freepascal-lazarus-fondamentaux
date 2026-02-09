{ ============================================================================
  Section 2.5 : Operateurs arithmetiques et logiques
  Description : Test de divisibilite d'un nombre par 2, 3, 5 et 10
                avec l'operateur mod
  Fichier source : 05-operateurs-arithmetiques-logiques.md
  ============================================================================ }
program TestDivisibilite;
var
  nombre: integer;
  divisiblePar2: boolean;
  divisiblePar3: boolean;
  divisiblePar5: boolean;
  divisiblePar10: boolean;
begin
  nombre := 30;

  // mod retourne le reste de la division entiere
  // Si le reste vaut 0, le nombre est divisible
  divisiblePar2 := (nombre mod 2) = 0;
  divisiblePar3 := (nombre mod 3) = 0;
  divisiblePar5 := (nombre mod 5) = 0;
  divisiblePar10 := (nombre mod 10) = 0;

  writeln('=== TEST DE DIVISIBILITÉ ===');
  writeln('Nombre : ', nombre);
  writeln('Divisible par 2 : ', divisiblePar2);
  writeln('Divisible par 3 : ', divisiblePar3);
  writeln('Divisible par 5 : ', divisiblePar5);
  writeln('Divisible par 10 : ', divisiblePar10);

  // Un nombre divisible par 10 est forcément divisible par 2 et 5
  writeln;
  writeln('Vérification : ', divisiblePar10 = (divisiblePar2 and divisiblePar5));
end.
