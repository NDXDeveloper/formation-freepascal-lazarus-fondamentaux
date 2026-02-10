{ ============================================================================
  Section 6.6 : Listes Chainees Simples
  Description : Compter les elements d'une liste chainee
  Fichier source : 06-listes-chainees-simples.md
  ============================================================================ }
{$mode objfpc}{$H+}
program CompterElementsDemo;  
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

function CompterElements(liste: PNoeud): Integer;  
var  
  courant: PNoeud;
  compte: Integer;
begin
  compte := 0;
  courant := liste;

  while courant <> nil do
  begin
    Inc(compte);
    courant := courant^.suivant;
  end;

  Result := compte;
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

  WriteLn('Nombre d''éléments : ', CompterElements(maListe));  // 3

  LibererListe(maListe);
end.
