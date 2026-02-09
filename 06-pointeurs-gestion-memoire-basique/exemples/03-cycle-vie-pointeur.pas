{ ============================================================================
  Section 6.3 : Allocation Dynamique (New, Dispose)
  Description : Les 4 etapes du cycle de vie d'un pointeur dynamique
  Fichier source : 03-allocation-dynamique-new-dispose.md
  ============================================================================ }
program CycleViePointeur;
var
  p: ^Integer;
begin
  // ÉTAPE 1 : Déclaration
  // Le pointeur existe mais ne pointe vers rien

  // ÉTAPE 2 : Allocation
  New(p);
  // Le pointeur pointe vers une zone mémoire réservée

  // ÉTAPE 3 : Utilisation
  p^ := 100;
  WriteLn('Valeur : ', p^);
  p^ := p^ + 50;
  WriteLn('Nouvelle valeur : ', p^);

  // ÉTAPE 4 : Libération
  Dispose(p);
  p := nil;
  // La mémoire est rendue au système
end.
