{ ============================================================================
  Section 6.6 : Listes Chainees Simples
  Description : Copier une liste chainee (copie independante)
  Fichier source : 06-listes-chainees-simples.md
  ============================================================================ }
{$mode objfpc}{$H+}
program CopierListeDemo;  
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

function CopierListe(liste: PNoeud): PNoeud;  
var  
  nouvelle, courant, dernier: PNoeud;
begin
  if liste = nil then
  begin
    Result := nil;
    Exit;
  end;

  // Copier le premier élément
  New(nouvelle);
  nouvelle^.donnee := liste^.donnee;
  nouvelle^.suivant := nil;
  dernier := nouvelle;

  // Copier le reste
  courant := liste^.suivant;
  while courant <> nil do
  begin
    New(dernier^.suivant);
    dernier := dernier^.suivant;
    dernier^.donnee := courant^.donnee;
    dernier^.suivant := nil;
    courant := courant^.suivant;
  end;

  Result := nouvelle;
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
  liste1, liste2: PNoeud;
begin
  liste1 := nil;
  InsererFin(liste1, 10);
  InsererFin(liste1, 20);

  liste2 := CopierListe(liste1);

  AfficherListe(liste1);  // 10 -> 20
  AfficherListe(liste2);  // 10 -> 20 (copie indépendante)

  LibererListe(liste1);
  LibererListe(liste2);
end.
