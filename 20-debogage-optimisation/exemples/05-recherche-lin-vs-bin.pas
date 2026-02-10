{
  Section 20.5 - Optimisation des algorithmes courants
  Description : Comparaison recherche linéaire O(n) vs recherche binaire O(log n)
                Mesure de performance sur un tableau trié de 1 million d'éléments
  Fichier source : 05-optimisation-algorithmes-courants.md
}
program RechercheLinVsBin;

{$mode objfpc}{$H+}

uses
  SysUtils;

const
  TAILLE = 1000000;

var
  Tableau: array[1..TAILLE] of Integer;

function RechercheLineaire(valeur: Integer): Integer;  
var  
  i: Integer;
begin
  Result := -1;
  for i := 1 to TAILLE do
  begin
    if Tableau[i] = valeur then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function RechercheBinaire(valeur: Integer): Integer;  
var  
  gauche, droite, milieu: Integer;
begin
  Result := -1;
  gauche := 1;
  droite := TAILLE;

  while gauche <= droite do
  begin
    milieu := (gauche + droite) div 2;

    if Tableau[milieu] = valeur then
    begin
      Result := milieu;
      Exit;
    end
    else if Tableau[milieu] < valeur then
      gauche := milieu + 1
    else
      droite := milieu - 1;
  end;
end;

var
  i, pos: Integer;
  debut: QWord;
  valeurCherchee: Integer;
  repetitions: Integer;
begin
  WriteLn('=== Recherche Linéaire vs Binaire ===');
  WriteLn;

  { Initialiser le tableau trié }
  WriteLn('Initialisation du tableau (', TAILLE, ' éléments)...');
  for i := 1 to TAILLE do
    Tableau[i] := i * 2;  { Valeurs paires : 2, 4, 6, ..., 2000000 }

  valeurCherchee := TAILLE * 2;  { Dernière valeur (pire cas linéaire) }
  repetitions := 100;

  WriteLn('Valeur cherchée : ', valeurCherchee);
  WriteLn('Répétitions     : ', repetitions);
  WriteLn;

  { Test recherche linéaire }
  debut := GetTickCount64;
  for i := 1 to repetitions do
    pos := RechercheLineaire(valeurCherchee);
  WriteLn('Recherche Linéaire O(n) :');
  WriteLn('  Temps    : ', GetTickCount64 - debut, ' ms (', repetitions, ' recherches)');
  WriteLn('  Position : ', pos);
  WriteLn;

  { Test recherche binaire }
  debut := GetTickCount64;
  for i := 1 to repetitions do
    pos := RechercheBinaire(valeurCherchee);
  WriteLn('Recherche Binaire O(log n) :');
  WriteLn('  Temps    : ', GetTickCount64 - debut, ' ms (', repetitions, ' recherches)');
  WriteLn('  Position : ', pos);
  WriteLn;

  { Test avec valeur absente }
  valeurCherchee := 999999;  { Valeur impaire, absente du tableau }
  WriteLn('Recherche d''une valeur absente (', valeurCherchee, ') :');

  debut := GetTickCount64;
  for i := 1 to repetitions do
    pos := RechercheLineaire(valeurCherchee);
  WriteLn('  Linéaire : ', GetTickCount64 - debut, ' ms (pos=', pos, ')');

  debut := GetTickCount64;
  for i := 1 to repetitions do
    pos := RechercheBinaire(valeurCherchee);
  WriteLn('  Binaire  : ', GetTickCount64 - debut, ' ms (pos=', pos, ')');
end.
