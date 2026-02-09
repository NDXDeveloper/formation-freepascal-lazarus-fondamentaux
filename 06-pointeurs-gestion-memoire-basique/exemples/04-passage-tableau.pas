{ ============================================================================
  Section 6.4 : Pointeurs et Tableaux
  Description : Passage de tableaux dynamiques aux procedures (const, var)
  Fichier source : 04-pointeurs-tableaux.md
  ============================================================================ }
program PassageTableau;

procedure AfficherTableau(const tab: array of Integer);
var
  i: Integer;
begin
  for i := Low(tab) to High(tab) do
    WriteLn('Element ', i, ' : ', tab[i]);
end;

procedure InitialiserTableau(var tab: array of Integer; valeur: Integer);
var
  i: Integer;
begin
  for i := Low(tab) to High(tab) do
    tab[i] := valeur;
end;

var
  nombres: array of Integer;
begin
  SetLength(nombres, 5);

  InitialiserTableau(nombres, 100);
  AfficherTableau(nombres);
end.
