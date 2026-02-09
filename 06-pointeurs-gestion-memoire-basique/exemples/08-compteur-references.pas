{ ============================================================================
  Section 6.8 : Fuites Memoire et Bonnes Pratiques
  Description : Compteur de references pour partage de donnees
  Fichier source : 08-fuites-memoire-bonnes-pratiques.md
  ============================================================================ }
{$mode objfpc}{$H+}
program CompteurReferences;
type
  PDonneePartagee = ^TDonneePartagee;
  TDonneePartagee = record
    compteurRef: Integer;
    donnees: String;
  end;

function Creer: PDonneePartagee;
begin
  New(Result);
  Result^.compteurRef := 1;
  Result^.donnees := '';
end;

function Acquerir(p: PDonneePartagee): PDonneePartagee;
begin
  if p <> nil then
    Inc(p^.compteurRef);
  Result := p;
end;

procedure Liberer(var p: PDonneePartagee);
begin
  if p <> nil then
  begin
    Dec(p^.compteurRef);
    WriteLn('  Compteur après libération : ', p^.compteurRef);
    if p^.compteurRef <= 0 then
    begin
      WriteLn('  -> Mémoire effectivement libérée');
      Dispose(p);
    end;
    p := nil;
  end;
end;

// Utilisation
var
  original, copie: PDonneePartagee;
begin
  original := Creer;           // compteur = 1
  original^.donnees := 'Données partagées';
  WriteLn('Création : compteur = ', original^.compteurRef);

  copie := Acquerir(original); // compteur = 2
  WriteLn('Acquisition : compteur = ', original^.compteurRef);

  WriteLn('Libération copie :');
  Liberer(copie);    // compteur = 1

  WriteLn('Libération original :');
  Liberer(original); // compteur = 0, libération effective
end.
