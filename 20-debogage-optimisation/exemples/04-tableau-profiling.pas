{
  Section 20.4 - Profiling basique : identifier les goulots
  Description : Rapport de profiling avec tableau formaté
                Mesure de plusieurs fonctions et calcul des pourcentages
  Fichier source : 04-profiling-basique-identifier-goulots.md
}
program TableauProfiling;

{$mode objfpc}{$H+}

uses
  SysUtils;

procedure ChargerDonnees;  
var  
  i: Integer;
  total: Int64;
begin
  total := 0;
  for i := 1 to 5000000 do
    total := total + i;
end;

procedure TraiterDonnees;  
var  
  i, j: Integer;
  total: Double;
begin
  total := 0;
  for i := 1 to 2000 do
    for j := 1 to 2000 do
      total := total + Sqrt(i * j);
end;

procedure CalculerStatistiques;  
var  
  i: Integer;
  total: Double;
begin
  total := 0;
  for i := 1 to 10000000 do
    total := total + Sin(i / 1000.0);
end;

procedure GenererRapport;  
var  
  i: Integer;
  s: String;
begin
  s := '';
  for i := 1 to 100000 do
    s := IntToStr(i);
end;

procedure SauvegarderResultats;  
begin  
  Sleep(50);
end;

type
  TResultatProfiling = record
    NomFonction: String;
    TempsMs: QWord;
    Pourcentage: Double;
  end;

var
  Resultats: array[1..5] of TResultatProfiling;
  i: Integer;
  tempsTotal: QWord;
  debut: QWord;
begin
  WriteLn('=== Rapport de Profiling ===');
  WriteLn;
  WriteLn('Mesure en cours...');

  { Mesurer chaque fonction }
  debut := GetTickCount64;
  ChargerDonnees;
  Resultats[1].NomFonction := 'ChargerDonnees';
  Resultats[1].TempsMs := GetTickCount64 - debut;

  debut := GetTickCount64;
  TraiterDonnees;
  Resultats[2].NomFonction := 'TraiterDonnees';
  Resultats[2].TempsMs := GetTickCount64 - debut;

  debut := GetTickCount64;
  CalculerStatistiques;
  Resultats[3].NomFonction := 'CalculerStatistiques';
  Resultats[3].TempsMs := GetTickCount64 - debut;

  debut := GetTickCount64;
  GenererRapport;
  Resultats[4].NomFonction := 'GenererRapport';
  Resultats[4].TempsMs := GetTickCount64 - debut;

  debut := GetTickCount64;
  SauvegarderResultats;
  Resultats[5].NomFonction := 'SauvegarderResultats';
  Resultats[5].TempsMs := GetTickCount64 - debut;

  { Calculer le temps total }
  tempsTotal := 0;
  for i := 1 to 5 do
    tempsTotal := tempsTotal + Resultats[i].TempsMs;

  { Calculer les pourcentages }
  for i := 1 to 5 do
  begin
    if tempsTotal > 0 then
      Resultats[i].Pourcentage := (Resultats[i].TempsMs / tempsTotal) * 100
    else
      Resultats[i].Pourcentage := 0;
  end;

  { Afficher le rapport }
  WriteLn;
  WriteLn('RAPPORT DE PROFILING');
  WriteLn('====================');
  WriteLn;
  WriteLn('Fonction                    | Temps (ms) | % du total');
  WriteLn('----------------------------+------------+-----------');

  for i := 1 to 5 do
    WriteLn(Format('%-27s | %10d | %6.2f%%',
                   [Resultats[i].NomFonction,
                    Resultats[i].TempsMs,
                    Resultats[i].Pourcentage]));

  WriteLn('----------------------------+------------+-----------');
  WriteLn(Format('TOTAL                       | %10d | 100.00%%', [tempsTotal]));
  WriteLn;

  { Identifier le goulot }
  WriteLn('Analyse : Le goulot principal est la fonction qui a le % le plus élevé.');
end.
