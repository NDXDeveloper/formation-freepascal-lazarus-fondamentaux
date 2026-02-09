{ ============================================================================
  Section 6.7 : Arbres Binaires Basics
  Description : Creation manuelle d'un arbre binaire avec noeuds
  Fichier source : 07-arbres-binaires-basics.md
  ============================================================================ }
{$mode objfpc}{$H+}
program CreerArbre;
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
  racine: PNoeud;
begin
  // Créer la racine
  racine := CreerNoeud(50);

  // Créer les enfants de la racine
  racine^.gauche := CreerNoeud(30);
  racine^.droite := CreerNoeud(70);

  // Créer les petits-enfants
  racine^.gauche^.gauche := CreerNoeud(20);
  racine^.gauche^.droite := CreerNoeud(40);
  racine^.droite^.gauche := CreerNoeud(60);
  racine^.droite^.droite := CreerNoeud(80);

  WriteLn('Arbre créé');
  WriteLn('Racine : ', racine^.donnee);
  WriteLn('Gauche : ', racine^.gauche^.donnee);
  WriteLn('Droite : ', racine^.droite^.donnee);

  LibererArbre(racine);
end.
