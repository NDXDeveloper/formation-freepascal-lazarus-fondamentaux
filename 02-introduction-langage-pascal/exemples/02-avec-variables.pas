{ ============================================================================
  Section 2.2 : Structure d'un programme Pascal
  Description : Declaration et utilisation de variables (string et integer)
  Fichier source : 02-structure-programme-pascal.md
  ============================================================================ }
program AvecVariables;
var
  prenom: string;
  age: integer;
begin
  prenom := 'Alice';
  age := 25;
  writeln('Bonjour ', prenom);
  writeln('Vous avez ', age, ' ans');
end.
