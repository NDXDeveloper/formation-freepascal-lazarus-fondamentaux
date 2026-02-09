{ ============================================================================
  Section 6.8 : Fuites Memoire et Bonnes Pratiques
  Description : Gestion des references circulaires entre structures
  Fichier source : 08-fuites-memoire-bonnes-pratiques.md
  ============================================================================ }
program ReferencesCirculaires;
type
  PA = ^TA;
  PB = ^TB;

  TA = record
    data: Integer;
    refB: PB;
  end;

  TB = record
    data: Integer;
    refA: PA;
  end;

var
  a: PA;
  b: PB;
begin
  New(a);
  New(b);

  a^.data := 1;
  b^.data := 2;

  // PROBLEME POTENTIEL : références circulaires
  a^.refB := b;  // A pointe vers B
  b^.refA := a;  // B pointe vers A

  WriteLn('A.data = ', a^.data, ', A.refB^.data = ', a^.refB^.data);
  WriteLn('B.data = ', b^.data, ', B.refA^.data = ', b^.refA^.data);

  // SOLUTION : casser le cycle avant de libérer
  a^.refB := nil;  // Casser la référence circulaire
  Dispose(a);
  Dispose(b);
  WriteLn('Mémoire libérée correctement (cycle cassé)');
end.
