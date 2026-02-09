{ ============================================================================
  Section 6.6 : Listes Chainees Simples
  Description : Liberation correcte de tous les noeuds d'une liste
  Fichier source : 06-listes-chainees-simples.md
  ============================================================================ }
program LibererListeDemo;
type
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: Integer;
    suivant: PNoeud;
  end;

procedure InsererFin(var liste: PNoeud; valeur: Integer);
var
  nouveau, courant: PNoeud;
begin
  New(nouveau);
  nouveau^.donnee := valeur;
  nouveau^.suivant := nil;

  if liste = nil then
    liste := nouveau
  else
  begin
    courant := liste;
    while courant^.suivant <> nil do
      courant := courant^.suivant;
    courant^.suivant := nouveau;
  end;
end;

procedure AfficherListe(liste: PNoeud);
var
  courant: PNoeud;
begin
  if liste = nil then
  begin
    WriteLn('Liste vide');
    Exit;
  end;

  courant := liste;
  Write('Liste : ');
  while courant <> nil do
  begin
    Write(courant^.donnee);
    if courant^.suivant <> nil then
      Write(' -> ');
    courant := courant^.suivant;
  end;
  WriteLn;
end;

procedure LibererListe(var liste: PNoeud);
var
  courant, suivant: PNoeud;
begin
  courant := liste;

  while courant <> nil do
  begin
    suivant := courant^.suivant;  // Sauvegarder le suivant
    Dispose(courant);              // Libérer le courant
    courant := suivant;            // Passer au suivant
  end;

  liste := nil;  // Liste vide
end;

var
  maListe: PNoeud;
begin
  maListe := nil;
  InsererFin(maListe, 10);
  InsererFin(maListe, 20);
  InsererFin(maListe, 30);

  AfficherListe(maListe);

  // IMPORTANT : toujours libérer !
  LibererListe(maListe);
end.
