{ ============================================================================
  Section 20.5 : Optimisation des algorithmes courants
  Description : Comparaison tri a bulles O(n2) vs tri rapide O(n log n)
  Fichier source : 05-optimisation-algorithmes-courants.md
  ============================================================================ }
program ComparaisonTris;

{$mode objfpc}{$H+}

uses SysUtils;

procedure TriBulles(var tab: array of Integer);
var
  i, j, temp: Integer;
begin
  for i := Low(tab) to High(tab) - 1 do
    for j := Low(tab) to High(tab) - 1 - i do
      if tab[j] > tab[j + 1] then
      begin
        temp := tab[j];
        tab[j] := tab[j + 1];
        tab[j + 1] := temp;
      end;
end;

procedure TriRapide(var tab: array of Integer; gauche, droite: Integer);
var
  i, j, pivot, temp: Integer;
begin
  if gauche >= droite then Exit;

  pivot := tab[(gauche + droite) div 2];
  i := gauche;
  j := droite;

  repeat
    while tab[i] < pivot do Inc(i);
    while tab[j] > pivot do Dec(j);

    if i <= j then
    begin
      temp := tab[i];
      tab[i] := tab[j];
      tab[j] := temp;
      Inc(i);
      Dec(j);
    end;
  until i > j;

  if gauche < j then TriRapide(tab, gauche, j);
  if i < droite then TriRapide(tab, i, droite);
end;

procedure InitialiserTableau(var tab: array of Integer);
var
  i: Integer;
begin
  for i := Low(tab) to High(tab) do
    tab[i] := Random(100000);
end;

var
  tab1, tab2: array[1..10000] of Integer;
  i: Integer;
  debut: QWord;
begin
  Randomize;

  // Initialiser les deux tableaux identiques
  InitialiserTableau(tab1);
  for i := Low(tab1) to High(tab1) do
    tab2[i] := tab1[i];

  // Test Tri a Bulles
  WriteLn('Test Tri a Bulles...');
  debut := GetTickCount64;
  TriBulles(tab1);
  WriteLn('Tri a Bulles : ', GetTickCount64 - debut, ' ms');

  // Test Tri Rapide
  WriteLn('Test Tri Rapide...');
  debut := GetTickCount64;
  TriRapide(tab2, Low(tab2), High(tab2));
  WriteLn('Tri Rapide : ', GetTickCount64 - debut, ' ms');
end.
