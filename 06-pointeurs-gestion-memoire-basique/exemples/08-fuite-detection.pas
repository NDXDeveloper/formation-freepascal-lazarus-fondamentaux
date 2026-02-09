{ ============================================================================
  Section 6.8 : Fuites Memoire et Bonnes Pratiques
  Description : Detection de fuite memoire avec HeapTrc (compiler avec -gh)
  Fichier source : 08-fuites-memoire-bonnes-pratiques.md
  ============================================================================ }
program TestFuite;

var
  p: ^Integer;
begin
  New(p);
  p^ := 42;
  WriteLn('Valeur : ', p^);
  // Oubli volontaire de Dispose(p)
  // Compiler avec : fpc -gh 08-fuite-detection.pas
  // pour voir le rapport de fuites mémoire
end.
