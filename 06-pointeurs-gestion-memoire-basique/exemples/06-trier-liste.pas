{ ============================================================================
  Section 6.6 : Listes Chainees Simples
  Description : Tri a bulles d'une liste chainee
  Fichier source : 06-listes-chainees-simples.md
  ============================================================================ }
program TrierListeDemo;  
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

procedure TrierListe(liste: PNoeud);  
var  
  i, j: PNoeud;
  temp: Integer;
begin
  if liste = nil then
    Exit;

  i := liste;
  while i <> nil do
  begin
    j := i^.suivant;
    while j <> nil do
    begin
      if i^.donnee > j^.donnee then
      begin
        // Échanger les valeurs
        temp := i^.donnee;
        i^.donnee := j^.donnee;
        j^.donnee := temp;
      end;
      j := j^.suivant;
    end;
    i := i^.suivant;
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
  InsererFin(maListe, 30);
  InsererFin(maListe, 10);
  InsererFin(maListe, 20);

  AfficherListe(maListe);  // 30 -> 10 -> 20
  TrierListe(maListe);
  AfficherListe(maListe);  // 10 -> 20 -> 30

  LibererListe(maListe);
end.
