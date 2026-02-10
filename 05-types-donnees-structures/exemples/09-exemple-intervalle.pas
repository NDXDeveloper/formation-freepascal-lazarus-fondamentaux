{ ============================================================================
  Section 5.9 : Types intervalle
  Description : Declaration et utilisation de types intervalle (age, note)
  Fichier source : 09-types-intervalle.md
  ============================================================================ }
program ExempleIntervalle;  
type  
  TAge = 0..120;
  TNote = 0..20;

var
  age: TAge;
  note: TNote;
begin
  // Affectations valides
  age := 25;
  note := 15;

  WriteLn('Age : ', age);
  WriteLn('Note : ', note);

  // Ces affectations généreraient des erreurs à l'exécution
  // age := 200;   // Erreur : hors de la plage 0..120
  // note := 50;   // Erreur : hors de la plage 0..20
end.
