{ ============================================================================
  Section 2.5 : Operateurs arithmetiques et logiques
  Description : Validation de conditions multiples pour une inscription
                (age, accord parental, residence)
  Fichier source : 05-operateurs-arithmetiques-logiques.md
  ============================================================================ }
program ValidationInscription;  
var  
  age: integer;
  aParent: boolean;
  estResident: boolean;
  peutSInscrire: boolean;
begin
  age := 16;
  aParent := true;
  estResident := true;

  // Conditions :
  // - Avoir 18 ans OU avoir l'accord d'un parent
  // - ET être résident
  peutSInscrire := ((age >= 18) or aParent) and estResident;

  writeln('=== VALIDATION INSCRIPTION ===');
  writeln('Âge : ', age, ' ans');
  writeln('Accord parental : ', aParent);
  writeln('Résident : ', estResident);
  writeln('Peut s''inscrire : ', peutSInscrire);
end.
