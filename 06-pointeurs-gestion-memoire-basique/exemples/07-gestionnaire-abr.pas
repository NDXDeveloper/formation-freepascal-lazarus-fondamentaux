{ ============================================================================
  Section 6.7 : Arbres Binaires Basics
  Description : Programme complet - Gestionnaire d'arbre binaire de recherche
  Fichier source : 07-arbres-binaires-basics.md
  ============================================================================ }
{$mode objfpc}{$H+}
program GestionnaireABR;

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

procedure ParcoursInfixe(racine: PNoeud);
begin
  if racine = nil then
    Exit;

  ParcoursInfixe(racine^.gauche);      // 1. Parcourir gauche
  Write(racine^.donnee, ' ');          // 2. Visiter la racine
  ParcoursInfixe(racine^.droite);      // 3. Parcourir droite
end;

function Rechercher(racine: PNoeud; valeur: Integer): Boolean;
begin
  // Cas de base : arbre vide
  if racine = nil then
  begin
    Result := False;
    Exit;
  end;

  // Cas de base : valeur trouvée
  if racine^.donnee = valeur then
    Result := True
  // Cas récursif : chercher dans le bon sous-arbre
  else if valeur < racine^.donnee then
    Result := Rechercher(racine^.gauche, valeur)
  else
    Result := Rechercher(racine^.droite, valeur);
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

  WriteLn('=== Gestionnaire d''Arbre Binaire de Recherche ===');

  Inserer(arbre, 50);
  Inserer(arbre, 30);
  Inserer(arbre, 70);
  Inserer(arbre, 20);
  Inserer(arbre, 40);
  Inserer(arbre, 60);
  Inserer(arbre, 80);

  Write('Parcours infixe : ');
  ParcoursInfixe(arbre);
  WriteLn;

  if Rechercher(arbre, 40) then
    WriteLn('40 trouvé dans l''arbre')
  else
    WriteLn('40 non trouvé');

  LibererArbre(arbre);
  WriteLn('Mémoire libérée');

  ReadLn;
end.
