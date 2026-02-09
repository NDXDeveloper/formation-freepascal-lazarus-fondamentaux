{ ============================================================================
  Section 6.4 : Pointeurs et Tableaux
  Description : Allocation manuelle d'un tableau avec New et Dispose
  Fichier source : 04-pointeurs-tableaux.md
  ============================================================================ }
program TableauDynamiqueNew;
type
  TTableau5 = array[1..5] of Integer;
  PTableau5 = ^TTableau5;

var
  pTab: PTableau5;
  i: Integer;
begin
  // Allocation du tableau
  New(pTab);

  // Initialisation
  for i := 1 to 5 do
    pTab^[i] := i * 100;

  // Utilisation
  for i := 1 to 5 do
    WriteLn('Element ', i, ' : ', pTab^[i]);

  // Libération
  Dispose(pTab);
  pTab := nil;
end.
