{ ============================================================================
  Section 7.6 : Sections initialization et finalization
  Description : Programme demonstrant l'ordre d'execution init/final
  Fichier source : 06-sections-initialization-finalization.md
  Note : Utilise UniteInitA/B/C au lieu de UniteA/B/C (conflit section 03)
  ============================================================================ }
program TestOrdreInit;

uses
  UniteInitC;

begin
  WriteLn('─── Programme Principal ───');
end.
