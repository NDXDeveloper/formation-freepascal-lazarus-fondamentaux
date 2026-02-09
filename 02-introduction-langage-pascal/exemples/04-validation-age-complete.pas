{ ============================================================================
  Section 2.4 : Types de donnees primitifs
  Description : Validation complete d'une personne avec tous les types :
                initiale (char), nom (string), age (integer), booleens
  Fichier source : 04-types-donnees-primitifs.md
  ============================================================================ }
program ValidationAgeComplete;
var
  nom: string;
  age: integer;
  estMajeur: boolean;
  estSenior: boolean;
  peutConduire: boolean;
  initiale: char;
begin
  nom := 'Dupont';
  age := 68;
  initiale := 'D';

  estMajeur := age >= 18;
  estSenior := age >= 65;
  peutConduire := age >= 18;

  writeln('=== INFORMATIONS ===');
  writeln('Initiale : ', initiale, '.');
  writeln('Nom : ', nom);
  writeln('Âge : ', age, ' ans');
  writeln('Majeur : ', estMajeur);
  writeln('Senior : ', estSenior);
  writeln('Peut conduire : ', peutConduire);
end.
