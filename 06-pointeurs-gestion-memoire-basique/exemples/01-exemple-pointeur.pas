{ ============================================================================
  Section 6.1 : Concept de Pointeur et Adresse Memoire
  Description : Introduction aux pointeurs - adresse, dereferencement, modification
  Fichier source : 01-concept-pointeur-adresse-memoire.md
  ============================================================================ }
program ExemplePointeur;

var
  nombre: Integer;      // Variable normale
  pNombre: ^Integer;    // Pointeur vers un Integer

begin
  // Étape 1 : Initialiser la variable
  nombre := 42;
  WriteLn('Valeur de nombre : ', nombre);  // Affiche : 42

  // Étape 2 : Faire pointer pNombre vers nombre
  pNombre := @nombre;  // @ donne l'adresse mémoire de la variable
  WriteLn('Adresse de nombre : ', PtrUInt(pNombre));  // Affiche l'adresse

  // Étape 3 : Accéder à la valeur via le pointeur
  WriteLn('Valeur via pointeur : ', pNombre^);  // ^ déréférence le pointeur (accède à la valeur)

  // Étape 4 : Modifier via le pointeur
  pNombre^ := 100;
  WriteLn('Nouvelle valeur de nombre : ', nombre);  // Affiche : 100

  ReadLn;
end.
