{ ============================================================================
  Section 7.6 : Sections initialization et finalization
  Description : Unite A demonstrant l'ordre d'initialization/finalization
  Fichier source : 06-sections-initialization-finalization.md
  Note : Renomme UniteInitA pour eviter conflit avec UniteA (section 03)
  ============================================================================ }
unit UniteInitA;

interface

implementation

initialization
  WriteLn('1. Initialisation UniteA');

finalization
  WriteLn('6. Finalisation UniteA');
  Flush(Output);

end.
