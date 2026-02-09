{ ============================================================================
  Section 7.3 : Clauses Uses et dependances
  Description : Programme demontrant la chaine de dependances entre unites
  Fichier source : 03-clauses-uses-dependances.md
  ============================================================================ }
program MonProgramme;

uses
  UniteCalculs;

begin
  WriteLn(SommesCarres(3, 4));  // 3² + 4² = 9 + 16 = 25
end.
