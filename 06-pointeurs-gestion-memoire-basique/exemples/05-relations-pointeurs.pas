{ ============================================================================
  Section 6.5 : Pointeurs et Enregistrements
  Description : Enregistrement avec plusieurs pointeurs (relations conjoint)
  Fichier source : 05-pointeurs-enregistrements.md
  ============================================================================ }
program RelationsPointeurs;  
type  
  PPersonne = ^TPersonne;
  TPersonne = record
    nom: String;
    age: Integer;
    conjoint: PPersonne;      // Pointeur vers le conjoint
    meilleurAmi: PPersonne;   // Pointeur vers l'ami
  end;

var
  alice, bob: PPersonne;
begin
  // Créer Alice
  New(alice);
  alice^.nom := 'Alice';
  alice^.age := 30;
  alice^.conjoint := nil;
  alice^.meilleurAmi := nil;

  // Créer Bob
  New(bob);
  bob^.nom := 'Bob';
  bob^.age := 32;
  bob^.conjoint := nil;
  bob^.meilleurAmi := nil;

  // Établir les relations
  alice^.conjoint := bob;
  bob^.conjoint := alice;

  // Afficher
  WriteLn(alice^.nom, ' est mariée à ', alice^.conjoint^.nom);
  WriteLn(bob^.nom, ' est marié à ', bob^.conjoint^.nom);

  // Libération
  Dispose(bob);
  Dispose(alice);
end.
