{ ============================================================================
  Section 6.3 : Allocation Dynamique (New, Dispose)
  Description : Creation de plusieurs variables dynamiques
  Fichier source : 03-allocation-dynamique-new-dispose.md
  ============================================================================ }
program PlusieursDynamiques;
var
  p1, p2, p3: ^Integer;
begin
  // Création de 3 entiers dynamiques
  New(p1);
  New(p2);
  New(p3);

  // Initialisation
  p1^ := 10;
  p2^ := 20;
  p3^ := 30;

  WriteLn('Somme : ', p1^ + p2^ + p3^);  // Affiche 60

  // Libération de chacun
  Dispose(p1);
  Dispose(p2);
  Dispose(p3);

  p1 := nil;
  p2 := nil;
  p3 := nil;
end.
