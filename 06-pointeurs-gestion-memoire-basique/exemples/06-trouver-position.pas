{ ============================================================================
  Section 6.6 : Listes Chainees Simples
  Description : Trouver la position d'une valeur dans une liste chainee
  Fichier source : 06-listes-chainees-simples.md
  ============================================================================ }
{$mode objfpc}{$H+}
program TrouverPositionDemo;  
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

function TrouverPosition(liste: PNoeud; valeur: Integer): Integer;  
var  
  courant: PNoeud;
  position: Integer;
begin
  courant := liste;
  position := 1;

  while courant <> nil do
  begin
    if courant^.donnee = valeur then
    begin
      Result := position;
      Exit;
    end;
    courant := courant^.suivant;
    Inc(position);
  end;

  Result := -1;  // Non trouvé
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
  pos: Integer;
begin
  maListe := nil;
  InsererFin(maListe, 10);
  InsererFin(maListe, 20);
  InsererFin(maListe, 30);

  pos := TrouverPosition(maListe, 20);
  if pos <> -1 then
    WriteLn('20 trouvé à la position ', pos)
  else
    WriteLn('20 non trouvé');

  LibererListe(maListe);
end.
