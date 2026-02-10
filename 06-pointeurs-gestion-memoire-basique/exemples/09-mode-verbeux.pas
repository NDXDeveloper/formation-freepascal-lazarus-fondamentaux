{ ============================================================================
  Section 6.9 : Debogage des Problemes Memoire
  Description : Mode verbeux pour tracer les operations sur une liste chainee
  Fichier source : 09-debogage-problemes-memoire.md
  ============================================================================ }
{$mode objfpc}{$H+}
program ModeVerbeux;  
uses SysUtils;  
type  
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: Integer;
    suivant: PNoeud;
  end;

const
  DEBUG_MODE = True;  // Mettre à False en production

procedure DebugLog(const msg: String);  
begin  
  if DEBUG_MODE then
    WriteLn('[DEBUG] ', msg);
end;

procedure InsererFin(var liste: PNoeud; valeur: Integer);  
var  
  nouveau, courant: PNoeud;
begin
  DebugLog('InsererFin(' + IntToStr(valeur) + ')');
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
  DebugLog('  -> Insertion réussie');
end;

procedure TraiterListe(liste: PNoeud);  
var  
  courant: PNoeud;
  compteur: Integer;
begin
  DebugLog('Début traitement liste');

  courant := liste;
  compteur := 0;

  while courant <> nil do
  begin
    Inc(compteur);
    DebugLog('Noeud #' + IntToStr(compteur) + ' : ' + IntToStr(courant^.donnee));
    courant := courant^.suivant;
  end;

  DebugLog('Fin traitement, ' + IntToStr(compteur) + ' noeuds traités');
end;

procedure LibererListe(var liste: PNoeud);  
var  
  courant, suivant: PNoeud;
  compteur: Integer;
begin
  DebugLog('Début libération liste');
  courant := liste;
  compteur := 0;

  while courant <> nil do
  begin
    suivant := courant^.suivant;
    Inc(compteur);
    DebugLog('  Libération noeud #' + IntToStr(compteur));
    Dispose(courant);
    courant := suivant;
  end;
  liste := nil;
  DebugLog('Liste libérée (' + IntToStr(compteur) + ' noeuds)');
end;

var
  maListe: PNoeud;
begin
  maListe := nil;

  InsererFin(maListe, 10);
  InsererFin(maListe, 20);
  InsererFin(maListe, 30);

  TraiterListe(maListe);

  LibererListe(maListe);
end.
