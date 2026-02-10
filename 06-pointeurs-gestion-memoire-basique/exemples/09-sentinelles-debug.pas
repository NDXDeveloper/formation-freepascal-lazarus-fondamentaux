{ ============================================================================
  Section 6.9 : Debogage des Problemes Memoire
  Description : Pattern sentinelles pour detecter la corruption memoire
  Fichier source : 09-debogage-problemes-memoire.md
  ============================================================================ }
{$mode objfpc}{$H+}
program SentinellesDebug;  
type  
  PNoeudDebug = ^TNoeudDebug;
  TNoeudDebug = record
    magicDebut: Cardinal;     // = $DEADBEEF
    donnee: Integer;
    suivant: PNoeudDebug;
    magicFin: Cardinal;       // = $DEADBEEF
  end;

function CreerNoeudDebug(val: Integer): PNoeudDebug;  
begin  
  New(Result);
  Result^.magicDebut := $DEADBEEF;
  Result^.donnee := val;
  Result^.suivant := nil;
  Result^.magicFin := $DEADBEEF;
end;

procedure VerifierNoeud(p: PNoeudDebug; id: Integer);  
begin  
  if p^.magicDebut <> $DEADBEEF then
    WriteLn('CORRUPTION : Début du noeud #', id, ' écrasé !')
  else
    WriteLn('Noeud #', id, ' début OK');

  if p^.magicFin <> $DEADBEEF then
    WriteLn('CORRUPTION : Fin du noeud #', id, ' écrasée !')
  else
    WriteLn('Noeud #', id, ' fin OK');
end;

procedure LibererListe(var liste: PNoeudDebug);  
var  
  courant, suivant: PNoeudDebug;
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
  n1, n2, n3: PNoeudDebug;
begin
  WriteLn('=== Test des sentinelles de débogage ===');

  // Créer des noeuds avec sentinelles
  n1 := CreerNoeudDebug(10);
  n2 := CreerNoeudDebug(20);
  n3 := CreerNoeudDebug(30);
  n1^.suivant := n2;
  n2^.suivant := n3;

  // Vérifier l'intégrité
  WriteLn('--- Vérification après création ---');
  VerifierNoeud(n1, 1);
  VerifierNoeud(n2, 2);
  VerifierNoeud(n3, 3);

  // Simuler une corruption (en réalité, on ne le fait pas ici)
  WriteLn('--- Données des noeuds ---');
  WriteLn('Noeud 1 : ', n1^.donnee);
  WriteLn('Noeud 2 : ', n2^.donnee);
  WriteLn('Noeud 3 : ', n3^.donnee);

  LibererListe(n1);
  WriteLn('Mémoire libérée');
end.
