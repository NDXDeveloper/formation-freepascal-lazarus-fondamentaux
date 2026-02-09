{ ============================================================================
  Section 3.5 : Instructions break et continue
  Description : Traitement par lots avec continue pour ignorer les items pairs
  Fichier source : 05-instructions-break-continue.md
  ============================================================================ }
program TraitementParLots;
const
  TAILLE_LOT = 5;
  MAX_ITEMS = 20;
var
  i, lot, itemsDansLot: Integer;
begin
  WriteLn('=== TRAITEMENT PAR LOTS ===');
  WriteLn('Taille du lot : ', TAILLE_LOT);
  WriteLn;

  lot := 1;
  itemsDansLot := 0;

  for i := 1 to MAX_ITEMS do
  begin
    // Début d'un nouveau lot
    if itemsDansLot = 0 then
      WriteLn('Lot #', lot, ' :');

    Write('  Item #', i);

    // Simulation : ignorer les items pairs
    if (i mod 2) = 0 then
    begin
      WriteLn(' - IGNORÉ');
      continue;  // Ne compte pas dans le lot
    end;

    WriteLn(' - TRAITÉ');
    itemsDansLot := itemsDansLot + 1;

    // Lot complet
    if itemsDansLot >= TAILLE_LOT then
    begin
      WriteLn('  → Lot complet !');
      WriteLn;
      lot := lot + 1;
      itemsDansLot := 0;
    end;
  end;
end.
