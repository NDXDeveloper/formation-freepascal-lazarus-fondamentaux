{ ============================================================================
  Section 7.3 : Clauses Uses et dependances
  Description : Unite A avec procedure Afficher (conflit de noms)
  Fichier source : 03-clauses-uses-dependances.md
  ============================================================================ }
unit UniteA;
interface
  procedure Afficher;
implementation
  procedure Afficher;
  begin
    WriteLn('Version A');
  end;
end.
