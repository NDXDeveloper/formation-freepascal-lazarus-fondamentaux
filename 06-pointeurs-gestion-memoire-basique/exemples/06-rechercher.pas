{ ============================================================================
  Section 6.6 : Listes Chainees Simples
  Description : Recherche d'une valeur dans une liste chainee
  Fichier source : 06-listes-chainees-simples.md
  ============================================================================ }
{$mode objfpc}{$H+}
program RechercherDemo;  
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

function Rechercher(liste: PNoeud; valeur: Integer): Boolean;  
var  
  courant: PNoeud;
begin
  courant := liste;
  while courant <> nil do
  begin
    if courant^.donnee = valeur then
    begin
      Result := True;
      Exit;
    end;
    courant := courant^.suivant;
  end;
  Result := False;
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

  if Rechercher(maListe, 20) then
    WriteLn('20 trouvé')
  else
    WriteLn('20 non trouvé');

  LibererListe(maListe);
end.
