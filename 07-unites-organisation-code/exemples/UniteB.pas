{ ============================================================================
  Section 7.3 : Clauses Uses et dependances
  Description : Unite B avec procedure Afficher (conflit de noms)
  Fichier source : 03-clauses-uses-dependances.md
  ============================================================================ }
unit UniteB;
interface
  procedure Afficher;
implementation
  procedure Afficher;
  begin
    WriteLn('Version B');
  end;
end.
