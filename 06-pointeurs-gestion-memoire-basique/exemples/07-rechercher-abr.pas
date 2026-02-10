{ ============================================================================
  Section 6.7 : Arbres Binaires Basics
  Description : Recherche d'une valeur dans un arbre binaire de recherche
  Fichier source : 07-arbres-binaires-basics.md
  ============================================================================ }
{$mode objfpc}{$H+}
program RechercherABR;  
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
  begin
    Result := True;
    Exit;
  end;

  // Cas récursif : chercher dans le bon sous-arbre
  if valeur < racine^.donnee then
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
  Inserer(arbre, 50);
  Inserer(arbre, 30);
  Inserer(arbre, 70);

  if Rechercher(arbre, 30) then
    WriteLn('30 trouvé')
  else
    WriteLn('30 non trouvé');

  if Rechercher(arbre, 99) then
    WriteLn('99 trouvé')
  else
    WriteLn('99 non trouvé');

  LibererArbre(arbre);
end.
