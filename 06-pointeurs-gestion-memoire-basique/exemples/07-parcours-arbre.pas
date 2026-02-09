{ ============================================================================
  Section 6.7 : Arbres Binaires Basics
  Description : Les 4 types de parcours d'un arbre binaire (prefixe, infixe,
                postfixe, largeur)
  Fichier source : 07-arbres-binaires-basics.md
  ============================================================================ }
{$mode objfpc}{$H+}
program ParcoursArbre;
type
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: Integer;
    gauche: PNoeud;
    droite: PNoeud;
  end;

function CreerNoeud(valeur: Integer): PNoeud;
begin
  New(Result);
  Result^.donnee := valeur;
  Result^.gauche := nil;
  Result^.droite := nil;
end;

procedure Inserer(var racine: PNoeud; valeur: Integer);
begin
  if racine = nil then
  begin
    racine := CreerNoeud(valeur);
    Exit;
  end;

  if valeur < racine^.donnee then
    Inserer(racine^.gauche, valeur)
  else if valeur > racine^.donnee then
    Inserer(racine^.droite, valeur);
end;

// Parcours Préfixe : Racine -> Gauche -> Droite
procedure ParcoursPreordre(racine: PNoeud);
begin
  if racine = nil then
    Exit;

  Write(racine^.donnee, ' ');          // 1. Visiter la racine
  ParcoursPreordre(racine^.gauche);    // 2. Parcourir gauche
  ParcoursPreordre(racine^.droite);    // 3. Parcourir droite
end;

// Parcours Infixe : Gauche -> Racine -> Droite
procedure ParcoursInfixe(racine: PNoeud);
begin
  if racine = nil then
    Exit;

  ParcoursInfixe(racine^.gauche);      // 1. Parcourir gauche
  Write(racine^.donnee, ' ');          // 2. Visiter la racine
  ParcoursInfixe(racine^.droite);      // 3. Parcourir droite
end;

// Parcours Postfixe : Gauche -> Droite -> Racine
procedure ParcoursPostordre(racine: PNoeud);
begin
  if racine = nil then
    Exit;

  ParcoursPostordre(racine^.gauche);   // 1. Parcourir gauche
  ParcoursPostordre(racine^.droite);   // 2. Parcourir droite
  Write(racine^.donnee, ' ');          // 3. Visiter la racine
end;

// Parcours en Largeur : niveau par niveau
procedure ParcoursLargeur(racine: PNoeud);
var
  fileAttente: array[1..100] of PNoeud;
  debut, fin: Integer;
  courant: PNoeud;
begin
  if racine = nil then
    Exit;

  // Initialiser la file
  debut := 1;
  fin := 1;
  fileAttente[fin] := racine;

  while debut <= fin do
  begin
    courant := fileAttente[debut];
    Inc(debut);

    Write(courant^.donnee, ' ');

    if courant^.gauche <> nil then
    begin
      Inc(fin);
      fileAttente[fin] := courant^.gauche;
    end;

    if courant^.droite <> nil then
    begin
      Inc(fin);
      fileAttente[fin] := courant^.droite;
    end;
  end;
end;

procedure LibererArbre(var racine: PNoeud);
begin
  if racine = nil then
    Exit;
  LibererArbre(racine^.gauche);
  LibererArbre(racine^.droite);
  Dispose(racine);
  racine := nil;
end;

var
  arbre: PNoeud;
begin
  arbre := nil;
  Inserer(arbre, 50);
  Inserer(arbre, 30);
  Inserer(arbre, 70);
  Inserer(arbre, 20);
  Inserer(arbre, 40);

  Write('Préordre  : ');
  ParcoursPreordre(arbre);    // 50 30 20 40 70
  WriteLn;

  Write('Infixe    : ');
  ParcoursInfixe(arbre);      // 20 30 40 50 70
  WriteLn;

  Write('Postordre : ');
  ParcoursPostordre(arbre);   // 20 40 30 70 50
  WriteLn;

  Write('Largeur   : ');
  ParcoursLargeur(arbre);     // 50 30 70 20 40
  WriteLn;

  LibererArbre(arbre);
end.
