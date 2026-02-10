{ ============================================================================
  Section 6.6 : Listes Chainees Simples
  Description : Inverser une liste chainee
  Fichier source : 06-listes-chainees-simples.md
  ============================================================================ }
program InverserListeDemo;  
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

procedure InverserListe(var liste: PNoeud);  
var  
  precedent, courant, suivant: PNoeud;
begin
  if liste = nil then
    Exit;

  precedent := nil;
  courant := liste;

  while courant <> nil do
  begin
    suivant := courant^.suivant;    // Sauvegarder le suivant
    courant^.suivant := precedent;   // Inverser le lien
    precedent := courant;            // Avancer
    courant := suivant;
  end;

  liste := precedent;  // Le dernier devient le premier
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
  InsererFin(maListe, 10);
  InsererFin(maListe, 20);
  InsererFin(maListe, 30);

  AfficherListe(maListe);   // 10 -> 20 -> 30
  InverserListe(maListe);
  AfficherListe(maListe);   // 30 -> 20 -> 10

  LibererListe(maListe);
end.
