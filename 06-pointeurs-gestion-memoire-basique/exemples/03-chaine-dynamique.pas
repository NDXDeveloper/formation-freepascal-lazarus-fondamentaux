{ ============================================================================
  Section 6.3 : Allocation Dynamique (New, Dispose)
  Description : Allocation dynamique d'une chaine de caracteres
  Fichier source : 03-allocation-dynamique-new-dispose.md
  ============================================================================ }
program ChaineDynamique;
var
  pNom: ^String;
begin
  New(pNom);
  pNom^ := 'Alice Dubois';

  WriteLn('Nom : ', pNom^);
  WriteLn('Longueur : ', Length(pNom^));

  Dispose(pNom);
  pNom := nil;
end.
