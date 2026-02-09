{ ============================================================================
  Section 6.6 : Listes Chainees Simples
  Description : Parcours avec traitement - doubler les valeurs d'une liste
  Fichier source : 06-listes-chainees-simples.md
  ============================================================================ }
program DoublerValeursDemo;
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

procedure DoublerValeurs(liste: PNoeud);
var
  courant: PNoeud;
begin
  courant := liste;
  while courant <> nil do
  begin
    courant^.donnee := courant^.donnee * 2;
    courant := courant^.suivant;
  end;
end;

procedure LibererListe(var liste: PNoeud);
var
  courant, suivant: PNoeud;
begin
  courant := liste;
  while courant <> nil do
  begin
    suivant := courant^.suivant;
    Dispose(courant);
    courant := suivant;
  end;
  liste := nil;
end;

var
  maListe: PNoeud;
begin
  maListe := nil;
  InsererFin(maListe, 5);
  InsererFin(maListe, 10);
  InsererFin(maListe, 15);

  AfficherListe(maListe);   // 5 -> 10 -> 15
  DoublerValeurs(maListe);
  AfficherListe(maListe);   // 10 -> 20 -> 30

  LibererListe(maListe);
end.
