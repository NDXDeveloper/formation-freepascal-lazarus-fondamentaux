{ ============================================================================
  Section 7.6 : Sections initialization et finalization
  Description : Unite B demonstrant l'ordre d'initialization/finalization
  Fichier source : 06-sections-initialization-finalization.md
  Note : Renomme UniteInitB pour eviter conflit avec UniteB (section 03)
  ============================================================================ }
unit UniteInitB;

interface

uses
  UniteInitA;  // UniteB dépend de UniteA

implementation

initialization
  WriteLn('2. Initialisation UniteB');

finalization
  WriteLn('5. Finalisation UniteB');
  Flush(Output);

end.
