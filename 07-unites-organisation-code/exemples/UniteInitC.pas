{ ============================================================================
  Section 7.6 : Sections initialization et finalization
  Description : Unite C demonstrant l'ordre d'initialization/finalization
  Fichier source : 06-sections-initialization-finalization.md
  Note : Renomme UniteInitC pour eviter conflit
  ============================================================================ }
unit UniteInitC;

interface

uses
  UniteInitB;  // UniteC dépend de UniteB

implementation

initialization
  WriteLn('3. Initialisation UniteC');

finalization
  WriteLn('4. Finalisation UniteC');
  Flush(Output);

end.
