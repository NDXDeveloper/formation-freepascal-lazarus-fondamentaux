{ ============================================================================
  Section 6.4 : Pointeurs et Tableaux
  Description : Parcours d'un tableau avec Inc (arithmetique de pointeurs)
  Fichier source : 04-pointeurs-tableaux.md
  ============================================================================ }
program ParcoursInc;
var
  nombres: array[1..5] of Integer;
  p: ^Integer;
  i: Integer;
begin
  // Initialisation
  for i := 1 to 5 do
    nombres[i] := i * 10;

  // Pointer vers le premier élément
  p := @nombres[1];

  // Afficher via le pointeur
  WriteLn('Premier : ', p^);

  // Avancer au suivant (décalage de 4 octets pour Integer)
  Inc(p);  // p pointe maintenant vers nombres[2]
  WriteLn('Deuxième : ', p^);

  Inc(p);  // p pointe vers nombres[3]
  WriteLn('Troisième : ', p^);
end.
