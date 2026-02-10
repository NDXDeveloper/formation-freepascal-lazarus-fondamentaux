{ ============================================================================
  Section 6.7 : Arbres Binaires Basics
  Description : Affichage visuel horizontal d'un arbre binaire
  Fichier source : 07-arbres-binaires-basics.md
  ============================================================================ }
{$mode objfpc}{$H+}
program AfficherArbreDemo;  
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

procedure AfficherArbre(racine: PNoeud; profondeur: Integer);  
var  
  i: Integer;
begin
  if racine = nil then
    Exit;

  // Afficher le sous-arbre droit
  AfficherArbre(racine^.droite, profondeur + 1);

  // Afficher le noeud avec indentation
  for i := 1 to profondeur do
    Write('    ');
  WriteLn(racine^.donnee);

  // Afficher le sous-arbre gauche
  AfficherArbre(racine^.gauche, profondeur + 1);
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

  AfficherArbre(arbre, 0);

  LibererArbre(arbre);
end.
