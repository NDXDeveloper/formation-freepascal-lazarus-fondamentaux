{ ============================================================================
  Section 6.5 : Pointeurs et Enregistrements
  Description : Liste chainee avec fonctions AjouterDebut, Afficher, Liberer
  Fichier source : 05-pointeurs-enregistrements.md
  ============================================================================ }
{$mode objfpc}{$H+}
program ListeChainee;
type
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: Integer;
    suivant: PNoeud;
  end;

// Ajouter un élément au début
function AjouterDebut(liste: PNoeud; valeur: Integer): PNoeud;
var
  nouveau: PNoeud;
begin
  New(nouveau);
  nouveau^.donnee := valeur;
  nouveau^.suivant := liste;
  Result := nouveau;
end;

// Afficher tous les éléments
procedure AfficherListe(liste: PNoeud);
var
  courant: PNoeud;
begin
  courant := liste;
  Write('Liste : ');
  while courant <> nil do
  begin
    Write(courant^.donnee, ' ');
    courant := courant^.suivant;
  end;
  WriteLn;
end;

// Libérer toute la liste
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
  maListe := nil;  // Liste vide

  // Ajouter des éléments
  maListe := AjouterDebut(maListe, 30);
  maListe := AjouterDebut(maListe, 20);
  maListe := AjouterDebut(maListe, 10);

  AfficherListe(maListe);  // Affiche : Liste : 10 20 30

  LibererListe(maListe);
end.
