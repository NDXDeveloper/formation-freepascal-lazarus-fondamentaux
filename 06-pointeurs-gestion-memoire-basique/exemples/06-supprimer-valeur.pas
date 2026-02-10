{ ============================================================================
  Section 6.6 : Listes Chainees Simples
  Description : Suppression d'un element par valeur dans une liste chainee
  Fichier source : 06-listes-chainees-simples.md
  ============================================================================ }
program SupprimerValeurDemo;  
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

procedure SupprimerValeur(var liste: PNoeud; valeur: Integer);  
var  
  courant, precedent: PNoeud;
begin
  if liste = nil then
    Exit;

  // Cas 1 : suppression en tête
  if liste^.donnee = valeur then
  begin
    courant := liste;
    liste := liste^.suivant;
    Dispose(courant);
    Exit;
  end;

  // Cas 2 : suppression ailleurs
  precedent := liste;
  courant := liste^.suivant;

  while courant <> nil do
  begin
    if courant^.donnee = valeur then
    begin
      precedent^.suivant := courant^.suivant;
      Dispose(courant);
      Exit;
    end;
    precedent := courant;
    courant := courant^.suivant;
  end;

  WriteLn('Valeur non trouvée');
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

  AfficherListe(maListe);        // 10 -> 20 -> 30
  SupprimerValeur(maListe, 20);
  AfficherListe(maListe);        // 10 -> 30

  LibererListe(maListe);
end.
