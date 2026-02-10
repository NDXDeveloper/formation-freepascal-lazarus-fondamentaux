{ ============================================================================
  Section 6.5 : Pointeurs et Enregistrements
  Description : Creation manuelle d'une liste chainee a 3 noeuds
  Fichier source : 05-pointeurs-enregistrements.md
  ============================================================================ }
program ListeSimple;  
type  
  PNoeud = ^TNoeud;  // Déclaration anticipée : Pascal autorise ^Type avant que Type soit défini
  TNoeud = record
    valeur: Integer;
    suivant: PNoeud;
  end;

var
  premier, deuxieme, troisieme: PNoeud;
begin
  // Créer le premier noeud
  New(premier);
  premier^.valeur := 10;
  premier^.suivant := nil;

  // Créer le deuxième noeud
  New(deuxieme);
  deuxieme^.valeur := 20;
  deuxieme^.suivant := nil;

  // Lier le premier au deuxième
  premier^.suivant := deuxieme;

  // Créer le troisième noeud
  New(troisieme);
  troisieme^.valeur := 30;
  troisieme^.suivant := nil;

  // Lier le deuxième au troisième
  deuxieme^.suivant := troisieme;

  // Parcourir la liste
  WriteLn('Premier : ', premier^.valeur);
  WriteLn('Deuxième : ', premier^.suivant^.valeur);
  WriteLn('Troisième : ', premier^.suivant^.suivant^.valeur);

  // Libération (à faire pour chaque noeud)
  Dispose(troisieme);
  Dispose(deuxieme);
  Dispose(premier);
end.
