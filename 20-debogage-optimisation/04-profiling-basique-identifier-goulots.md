🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 20.4 Profiling Basique : Identifier les Goulots

## Introduction

Le profiling (profilage en français) est une technique d'analyse qui permet de mesurer les performances de votre programme et d'identifier les parties du code qui consomment le plus de temps ou de ressources. Ces zones problématiques sont appelées **goulots d'étranglement** (bottlenecks en anglais).

**Analogie simple :** Imaginez une autoroute à 4 voies qui se réduit soudainement à 1 voie. Cette réduction crée un embouteillage : c'est un goulot d'étranglement. Dans un programme, c'est pareil : une fonction lente peut ralentir tout le reste.

**Dans cette section, vous apprendrez à :**
- Comprendre ce qu'est un goulot d'étranglement
- Mesurer le temps d'exécution de portions de code
- Utiliser des techniques de profiling simples
- Identifier les fonctions les plus coûteuses
- Prendre des décisions d'optimisation éclairées

---

## 1. Comprendre les Goulots d'Étranglement

### 1.1 Qu'est-ce qu'un Goulot d'Étranglement ?

Un goulot d'étranglement est une partie du code qui :
- Prend beaucoup de temps à s'exécuter
- Est exécutée très fréquemment
- Ralentit l'ensemble du programme

**Loi de Pareto appliquée à la programmation :**
> **80% du temps d'exécution** est généralement concentré dans **20% du code**

**Conséquence :** Optimiser les bons 20% peut avoir un impact énorme sur les performances globales.

### 1.2 Types de Goulots

**1. Goulot Algorithmique**
```pascal
// Algorithme inefficace - O(n²)
for i := 1 to 1000 do
  for j := 1 to 1000 do
    Traiter(i, j);  // 1 million d'appels !
```

**2. Goulot d'Accès aux Données**
```pascal
// Lecture de fichier répétitive
for i := 1 to 10000 do  
begin  
  AssignFile(F, 'data.txt');
  Reset(F);
  ReadLn(F, ligne);  // Ouvre/ferme le fichier 10 000 fois !
  CloseFile(F);
end;
```

**3. Goulot d'Allocations Mémoire**
```pascal
// Création/destruction répétitive d'objets
for i := 1 to 100000 do  
begin  
  obj := TMonObjet.Create;  // Allocation coûteuse
  obj.Traiter();
  obj.Free;  // Libération coûteuse
end;
```

**4. Goulot Réseau/Base de Données**
```pascal
// Requête SQL dans une boucle
for i := 1 to 1000 do  
begin  
  Query.SQL.Text := 'SELECT * FROM Table WHERE ID = ' + IntToStr(i);
  Query.Open;  // 1000 requêtes au lieu d'1 seule !
  Traiter(Query);
  Query.Close;
end;
```

### 1.3 Pourquoi le Profiling est Essentiel

**Éviter l'optimisation prématurée :**
> "Premature optimization is the root of all evil" - Donald Knuth

**Problème courant :** Les développeurs optimisent le mauvais code !

**Exemple :**
- Vous passez 2 heures à optimiser une fonction qui prend 0,01 secondes
- Vous ignorez une autre fonction qui prend 5 secondes
- **Résultat :** Perte de temps, impact négligeable

**Le profiling vous dit OÙ optimiser** pour un impact maximal.

---

## 2. Mesure Manuelle du Temps d'Exécution

### 2.1 Méthode 1 : GetTickCount64 (Simple et Efficace)

**La fonction `GetTickCount64` :**
- Retourne le nombre de millisecondes écoulées depuis le démarrage du système
- Disponible sur Windows et Linux (via l'unité `SysUtils`)
- Précision : milliseconde

**Syntaxe de Base :**

```pascal
uses SysUtils;

var
  debut, fin: QWord;
  tempsMs: QWord;
begin
  debut := GetTickCount64;

  // Code à mesurer
  Sleep(1000);  // Simule 1 seconde de travail

  fin := GetTickCount64;
  tempsMs := fin - debut;

  WriteLn('Temps écoulé : ', tempsMs, ' ms');
end.
```

**Sortie :**
```
Temps écoulé : 1000 ms
```

### 2.2 Exemple Complet : Mesurer une Boucle

```pascal
program MesurePerformance;

uses SysUtils;

procedure TraiterDonnees;  
var  
  i: Integer;
  total: Int64;
begin
  total := 0;
  for i := 1 to 10000000 do  // 10 millions d'itérations
    total := total + i;
end;

var
  debut, fin: QWord;
begin
  WriteLn('Démarrage du traitement...');

  debut := GetTickCount64;
  TraiterDonnees;
  fin := GetTickCount64;

  WriteLn('Traitement terminé en ', fin - debut, ' ms');
  ReadLn;
end.
```

**Exemple de sortie :**
```
Démarrage du traitement...  
Traitement terminé en 45 ms  
```

### 2.3 Méthode 2 : Now (Date et Heure)

**Pour des mesures plus longues ou nécessitant une précision horaire :**

```pascal
uses SysUtils, DateUtils;

var
  debut, fin: TDateTime;
  tempsMs: Int64;
begin
  debut := Now;

  // Code à mesurer
  Sleep(2500);  // 2,5 secondes

  fin := Now;
  tempsMs := MilliSecondsBetween(fin, debut);

  WriteLn('Temps écoulé : ', tempsMs, ' ms');
  WriteLn('Soit : ', tempsMs / 1000:0:2, ' secondes');
end.
```

**Sortie :**
```
Temps écoulé : 2500 ms  
Soit : 2.50 secondes  
```

### 2.4 Méthode 3 : QueryPerformanceCounter (Haute Précision Windows)

**Pour des mesures très précises (microsecondes) sur Windows :**

```pascal
{$IFDEF WINDOWS}
uses Windows;

var
  freq, debut, fin: Int64;
  tempsMs: Double;
begin
  QueryPerformanceFrequency(freq);  // Fréquence du compteur
  QueryPerformanceCounter(debut);

  // Code à mesurer
  Sleep(100);

  QueryPerformanceCounter(fin);
  tempsMs := ((fin - debut) / freq) * 1000;

  WriteLn('Temps écoulé : ', tempsMs:0:3, ' ms');
end.
{$ENDIF}
```

**Avantages :** Précision extrême (microsecondes)  
**Inconvénients :** Spécifique à Windows  

### 2.5 Encapsuler les Mesures dans une Classe

**Pour réutiliser facilement :**

```pascal
type
  TChronometer = class
  private
    FDebut: QWord;
    FEnCours: Boolean;
  public
    procedure Start;
    function Stop: QWord;
    function GetElapsedMs: QWord;
  end;

procedure TChronometer.Start;  
begin  
  FDebut := GetTickCount64;
  FEnCours := True;
end;

function TChronometer.Stop: QWord;  
begin  
  if FEnCours then
  begin
    Result := GetTickCount64 - FDebut;
    FEnCours := False;
  end
  else
    Result := 0;
end;

function TChronometer.GetElapsedMs: QWord;  
begin  
  if FEnCours then
    Result := GetTickCount64 - FDebut
  else
    Result := 0;
end;

// Utilisation
var
  chrono: TChronometer;
begin
  chrono := TChronometer.Create;
  try
    chrono.Start;
    TraiterDonnees;
    WriteLn('Temps : ', chrono.Stop, ' ms');
  finally
    chrono.Free;
  end;
end;
```

---

## 3. Profiling de Fonctions Multiples

### 3.1 Comparer Plusieurs Approches

**Scénario :** Vous avez deux algorithmes et voulez savoir lequel est le plus rapide.

```pascal
program ComparaisonAlgorithmes;

uses SysUtils;

function MethodeA: Int64;  
var  
  i: Integer;
  total: Int64;
begin
  total := 0;
  for i := 1 to 1000000 do
    total := total + i;
  Result := total;
end;

function MethodeB: Int64;  
var  
  n: Int64;
begin
  n := 1000000;
  Result := (n * (n + 1)) div 2;  // Formule mathématique
end;

var
  debut, fin: QWord;
  resultat: Int64;
begin
  // Test Méthode A
  debut := GetTickCount64;
  resultat := MethodeA;
  fin := GetTickCount64;
  WriteLn('Méthode A : ', fin - debut, ' ms (Résultat: ', resultat, ')');

  // Test Méthode B
  debut := GetTickCount64;
  resultat := MethodeB;
  fin := GetTickCount64;
  WriteLn('Méthode B : ', fin - debut, ' ms (Résultat: ', resultat, ')');

  ReadLn;
end.
```

**Sortie typique :**
```
Méthode A : 15 ms (Résultat: 500000500000)  
Méthode B : 0 ms (Résultat: 500000500000)  
```

**Conclusion :** La Méthode B (formule mathématique) est infiniment plus rapide !

### 3.2 Tableau de Profiling

**Pour mesurer plusieurs fonctions systématiquement :**

```pascal
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
begin
  // Remplir les résultats
  Resultats[1].NomFonction := 'ChargerDonnees';
  Resultats[1].TempsMs := 150;

  Resultats[2].NomFonction := 'TraiterDonnees';
  Resultats[2].TempsMs := 2500;

  Resultats[3].NomFonction := 'CalculerStatistiques';
  Resultats[3].TempsMs := 300;

  Resultats[4].NomFonction := 'GenererRapport';
  Resultats[4].TempsMs := 50;

  Resultats[5].NomFonction := 'SauvegarderResultats';
  Resultats[5].TempsMs := 100;

  // Calculer le temps total
  tempsTotal := 0;
  for i := 1 to 5 do
    tempsTotal := tempsTotal + Resultats[i].TempsMs;

  // Calculer les pourcentages
  for i := 1 to 5 do
    Resultats[i].Pourcentage := (Resultats[i].TempsMs / tempsTotal) * 100;

  // Afficher
  WriteLn('RAPPORT DE PROFILING');
  WriteLn('====================');
  WriteLn;
  WriteLn('Fonction                    | Temps (ms) | % du total');
  WriteLn('-----------------------------------------------------------');

  for i := 1 to 5 do
    WriteLn(Format('%-27s | %10d | %6.2f%%',
                   [Resultats[i].NomFonction,
                    Resultats[i].TempsMs,
                    Resultats[i].Pourcentage]));

  WriteLn('-----------------------------------------------------------');
  WriteLn(Format('TOTAL                       | %10d | 100.00%%', [tempsTotal]));
end.
```

**Sortie :**
```
RAPPORT DE PROFILING
====================

Fonction                    | Temps (ms) | % du total
-----------------------------------------------------------
ChargerDonnees              |        150 |   4.84%  
TraiterDonnees              |       2500 |  80.65%  ← GOULOT !  
CalculerStatistiques        |        300 |   9.68%  
GenererRapport              |         50 |   1.61%  
SauvegarderResultats        |        100 |   3.23%  
-----------------------------------------------------------
TOTAL                       |       3100 | 100.00%
```

**Analyse :** `TraiterDonnees` représente 80% du temps : c'est le goulot principal à optimiser !

---

## 4. Profiling dans les Boucles

### 4.1 Détecter les Opérations Coûteuses

**Problème :** Une boucle exécutée des millions de fois peut amplifier de petites inefficacités.

```pascal
procedure AnalyserBoucle;  
var  
  i: Integer;
  debut, fin, tempsTotal, tempsBoucle: QWord;
  s: String;
begin
  tempsTotal := GetTickCount64;

  for i := 1 to 100000 do
  begin
    tempsBoucle := GetTickCount64;

    // Opération coûteuse : concaténation de chaînes
    s := 'Ligne ' + IntToStr(i);

    if i mod 10000 = 0 then  // Affiche tous les 10 000
      WriteLn('Itération ', i, ' : ', GetTickCount64 - tempsBoucle, ' ms');
  end;

  WriteLn('Temps total : ', GetTickCount64 - tempsTotal, ' ms');
end;
```

**Sortie typique :**
```
Itération 10000 : 0 ms  
Itération 20000 : 0 ms  
Itération 30000 : 0 ms  
...
Temps total : 125 ms
```

### 4.2 Optimiser les Boucles

**Avant (inefficace) :**

```pascal
var
  i, j: Integer;
  total: Double;
  valeur: Double;
begin
  total := 0;
  for i := 1 to 1000 do
    for j := 1 to 1000 do
    begin
      valeur := Sqrt(i * j);  // Calcul coûteux répété
      total := total + valeur;
    end;
end;
```

**Après (optimisé) :**

```pascal
var
  i, j: Integer;
  total: Double;
  sqrtI: Double;
begin
  total := 0;
  for i := 1 to 1000 do
  begin
    sqrtI := Sqrt(i);  // Calcul une seule fois par i
    for j := 1 to 1000 do
      total := total + sqrtI * Sqrt(j);
  end;
end;
```

**Gain de performance :** Environ 30-40% plus rapide !

### 4.3 Mesurer l'Impact d'une Opération

```pascal
procedure MesurerImpactOperation;  
var  
  i, iterations: Integer;
  debut: QWord;
  s: String;

  procedure TestAvec;
  var j: Integer;
  begin
    for j := 1 to iterations do
      s := IntToStr(j);  // Avec conversion
  end;

  procedure TestSans;
  var j: Integer;
  begin
    for j := 1 to iterations do
      ; // Boucle vide
  end;
begin
  iterations := 1000000;

  // Mesure boucle vide (référence)
  debut := GetTickCount64;
  TestSans;
  WriteLn('Boucle vide : ', GetTickCount64 - debut, ' ms');

  // Mesure avec conversion
  debut := GetTickCount64;
  TestAvec;
  WriteLn('Avec IntToStr : ', GetTickCount64 - debut, ' ms');
end;
```

**Sortie :**
```
Boucle vide : 2 ms  
Avec IntToStr : 85 ms  
```

**Conclusion :** `IntToStr` ajoute ~83 ms pour 1 million d'appels (0,000083 ms par appel).

---

## 5. Profiling avec Lazarus

### 5.1 Utiliser le Débogueur pour le Profiling

**Technique 1 : Points d'Arrêt avec Timestamps**

1. Placez des points d'arrêt au début et à la fin d'une fonction
2. Notez les heures d'arrêt
3. Calculez la différence

**Limite :** Très manuel, peu pratique pour des analyses détaillées.

**Technique 2 : Logging avec Horodatage**

```pascal
procedure LogTemps(const msg: String);  
var  
  F: TextFile;
begin
  AssignFile(F, 'profiling.log');
  if FileExists('profiling.log') then
    Append(F)
  else
    Rewrite(F);

  WriteLn(F, FormatDateTime('hh:nn:ss.zzz', Now), ' - ', msg);
  CloseFile(F);
end;

procedure TraiterGrandesdonnees;  
begin  
  LogTemps('Début TraiterGrandesDonnees');

  // Traitement...
  Sleep(1500);

  LogTemps('Fin TraiterGrandesDonnees');
end;
```

**Fichier profiling.log :**
```
14:25:30.125 - Début TraiterGrandesDonnees
14:25:31.625 - Fin TraiterGrandesDonnees
```

**Analyse :** Différence = 1,5 seconde.

### 5.2 Unité LazLogger pour le Profiling

**Lazarus fournit l'unité `LazLogger` pour le logging structuré :**

```pascal
uses LazLogger;

procedure MaFonction;  
begin  
  DebugLn('Début de MaFonction');

  // Code

  DebugLn('Fin de MaFonction');
end;
```

**Avantages :**
- Logging intégré
- Niveaux de verbosité
- Désactivable en production

**Limite :** Pas spécialement conçu pour le profiling de performances.

---

## 6. Outils Externes de Profiling

### 6.1 Valgrind/Callgrind (Linux)

**Valgrind** est un outil puissant pour analyser les performances et la mémoire sur Linux.

**Installation Ubuntu/Debian :**

```bash
sudo apt update  
sudo apt install valgrind  
sudo apt install kcachegrind  # Visualiseur graphique  
```

**Utilisation basique :**

```bash
# Compiler votre programme avec infos de débogage
fpc -g monprogramme.pas

# Profiler avec Callgrind
valgrind --tool=callgrind ./monprogramme

# Analyser les résultats
kcachegrind callgrind.out.12345
```

**Ce que vous obtenez :**
- Nombre d'appels de chaque fonction
- Temps CPU par fonction
- Graphe d'appels (qui appelle quoi)
- Identification automatique des goulots

**Avantage :** Analyse très détaillée  
**Inconvénient :** Ralentit considérablement l'exécution (10-50x plus lent)  

### 6.2 gprof (Linux/Windows)

**gprof** est le profileur GNU, disponible sur les deux plateformes.

**Étapes :**

```bash
# 1. Compiler avec option de profiling
fpc -pg monprogramme.pas

# 2. Exécuter le programme (génère gmon.out)
./monprogramme

# 3. Analyser les résultats
gprof monprogramme gmon.out > analyse.txt
```

**Exemple de sortie `analyse.txt` :**

```
  %   cumulative   self              self     total
 time   seconds   seconds    calls  ms/call  ms/call  name
 45.67      1.52     1.52   100000     0.02     0.02  TraiterDonnees
 23.45      2.30     0.78    50000     0.02     0.04  CalculerStats
 15.32      2.81     0.51        1   510.00   510.00  ChargerFichier
  8.76      3.10     0.29    10000     0.03     0.03  Trier
  6.80      3.33     0.23        1   230.00   230.00  GenererRapport
```

**Lecture :**
- `TraiterDonnees` : 45,67% du temps total
- `CalculerStats` : 23,45% du temps
- Etc.

**Multi-plateforme :** Fonctionne sur Windows et Linux.

### 6.3 Sampling Profilers

**Alternative moderne :** Profileurs par échantillonnage (comme `perf` sur Linux).

**Avantages :**
- Impact minimal sur les performances
- Résultats en temps réel
- Analyse des hot spots (zones chaudes)

**Exemple avec `perf` (Linux) :**

```bash
# Compiler normalement (avec -g pour les symboles)
fpc -g monprogramme.pas

# Profiler
perf record ./monprogramme

# Analyser
perf report
```

---

## 7. Identifier les Goulots : Méthodologie

### 7.1 Approche Descendante (Top-Down)

**Principe :** Commencer par une vue d'ensemble, puis zoomer sur les détails.

**Étape 1 : Mesure globale**

```pascal
var
  debut: QWord;
begin
  debut := GetTickCount64;
  ExecuterProgramme;
  WriteLn('Temps total : ', GetTickCount64 - debut, ' ms');
end;
```

**Étape 2 : Diviser en grandes sections**

```pascal
var
  t1, t2, t3, t4: QWord;
begin
  t1 := GetTickCount64;
  Initialisation;
  t2 := GetTickCount64;
  TraitementPrincipal;
  t3 := GetTickCount64;
  Finalisation;
  t4 := GetTickCount64;

  WriteLn('Initialisation : ', t2 - t1, ' ms');
  WriteLn('Traitement : ', t3 - t2, ' ms');
  WriteLn('Finalisation : ', t4 - t3, ' ms');
end;
```

**Étape 3 : Zoomer sur la section la plus lente**

Si "Traitement" prend 90% du temps, détaillez-le :

```pascal
procedure TraitementPrincipal;  
var  
  t1, t2, t3, t4: QWord;
begin
  t1 := GetTickCount64;
  ChargerDonnees;
  t2 := GetTickCount64;
  CalculerResultats;
  t3 := GetTickCount64;
  SauvegarderResultats;
  t4 := GetTickCount64;

  WriteLn('  Chargement : ', t2 - t1, ' ms');
  WriteLn('  Calcul : ', t3 - t2, ' ms');
  WriteLn('  Sauvegarde : ', t4 - t3, ' ms');
end;
```

**Répétez** jusqu'à identifier la fonction précise qui pose problème.

### 7.2 Approche Ascendante (Bottom-Up)

**Principe :** Mesurer chaque fonction individuellement, puis agréger.

```pascal
type
  TProfilingData = record
    NomFonction: String;
    NombreAppels: Integer;
    TempsTotal: QWord;
  end;

var
  ProfilingDB: array[1..100] of TProfilingData;
  ProfilingIndex: Integer = 0;

procedure EnregistrerAppel(const nom: String; tempsMs: QWord);  
var  
  i: Integer;
  trouve: Boolean;
begin
  trouve := False;

  // Chercher si la fonction existe déjà
  for i := 1 to ProfilingIndex do
  begin
    if ProfilingDB[i].NomFonction = nom then
    begin
      Inc(ProfilingDB[i].NombreAppels);
      ProfilingDB[i].TempsTotal := ProfilingDB[i].TempsTotal + tempsMs;
      trouve := True;
      Break;
    end;
  end;

  // Sinon, ajouter
  if not trouve then
  begin
    Inc(ProfilingIndex);
    ProfilingDB[ProfilingIndex].NomFonction := nom;
    ProfilingDB[ProfilingIndex].NombreAppels := 1;
    ProfilingDB[ProfilingIndex].TempsTotal := tempsMs;
  end;
end;

// Dans chaque fonction à profiler
procedure MaFonction;  
var  
  debut: QWord;
begin
  debut := GetTickCount64;

  // Code de la fonction

  EnregistrerAppel('MaFonction', GetTickCount64 - debut);
end;
```

**À la fin du programme :**

```pascal
procedure AfficherRapportProfiling;  
var  
  i: Integer;
begin
  WriteLn('RAPPORT DE PROFILING');
  WriteLn('Fonction             | Appels | Temps Total | Temps Moyen');
  WriteLn('----------------------------------------------------------');

  for i := 1 to ProfilingIndex do
  begin
    with ProfilingDB[i] do
      WriteLn(Format('%-20s | %6d | %11d | %11.2f',
                     [NomFonction,
                      NombreAppels,
                      TempsTotal,
                      TempsTotal / NombreAppels]));
  end;
end;
```

### 7.3 La Règle des 80/20 en Pratique

**Stratégie efficace :**

1. **Profiler** tout le programme
2. **Identifier** les 20% de fonctions qui prennent 80% du temps
3. **Optimiser** uniquement ces fonctions
4. **Re-profiler** pour vérifier l'amélioration
5. **Répéter** si nécessaire

**Exemple de priorisation :**

| Fonction | Temps | % du total | Priorité |
|----------|-------|------------|----------|
| CalculComplexe | 4500 ms | 75% | ⭐⭐⭐ HAUTE |
| ChargerFichier | 800 ms | 13% | ⭐⭐ Moyenne |
| Affichage | 400 ms | 7% | ⭐ Basse |
| Initialisation | 300 ms | 5% | Ignorer |

**Action :** Concentrez-vous sur `CalculComplexe` en priorité.

---

## 8. Cas Pratiques et Scénarios

### 8.1 Scénario : Application Lente au Démarrage

**Symptôme :** Le programme met 10 secondes à démarrer.

**Analyse :**

```pascal
program AnalyseDemarrage;

procedure MesurerDemarrage;  
var  
  t0, t1, t2, t3, t4, t5: QWord;
begin
  t0 := GetTickCount64;

  t1 := GetTickCount64;
  ChargerConfiguration;
  WriteLn('Configuration : ', GetTickCount64 - t1, ' ms');

  t2 := GetTickCount64;
  ConnecterBaseDeDonnees;
  WriteLn('BDD : ', GetTickCount64 - t2, ' ms');

  t3 := GetTickCount64;
  ChargerRessources;
  WriteLn('Ressources : ', GetTickCount64 - t3, ' ms');

  t4 := GetTickCount64;
  InitialiserInterface;
  WriteLn('Interface : ', GetTickCount64 - t4, ' ms');

  t5 := GetTickCount64;
  ChargerPlugins;
  WriteLn('Plugins : ', GetTickCount64 - t5, ' ms');

  WriteLn('TOTAL : ', GetTickCount64 - t0, ' ms');
end;
```

**Résultats :**
```
Configuration : 50 ms  
BDD : 3000 ms              ← GOULOT !  
Ressources : 200 ms  
Interface : 1500 ms  
Plugins : 5200 ms          ← GOULOT MAJEUR !  
TOTAL : 9950 ms  
```

**Diagnostic :**
- Les plugins représentent 52% du temps
- La BDD représente 30% du temps

**Solutions possibles :**
- Charger les plugins en arrière-plan (thread)
- Optimiser la connexion BDD (pool de connexions)
- Charger uniquement les plugins essentiels au démarrage

### 8.2 Scénario : Traitement de Fichier Volumineux

```pascal
procedure AnalyserFichier(const nomFichier: String);  
var  
  F: TextFile;
  ligne: String;
  compteur: Integer;
  debut, tLecture, tTraitement: QWord;
  tempsLectureTotal, tempsTraitementTotal: QWord;
begin
  AssignFile(F, nomFichier);
  Reset(F);

  compteur := 0;
  tempsLectureTotal := 0;
  tempsTraitementTotal := 0;

  while not Eof(F) do
  begin
    // Mesurer la lecture
    debut := GetTickCount64;
    ReadLn(F, ligne);
    tLecture := GetTickCount64 - debut;
    tempsLectureTotal := tempsLectureTotal + tLecture;

    // Mesurer le traitement
    debut := GetTickCount64;
    TraiterLigne(ligne);
    tTraitement := GetTickCount64 - debut;
    tempsTraitementTotal := tempsTraitementTotal + tTraitement;

    Inc(compteur);

    if compteur mod 10000 = 0 then
      WriteLn('Lignes traitées : ', compteur);
  end;

  CloseFile(F);

  WriteLn('STATISTIQUES');
  WriteLn('Lignes : ', compteur);
  WriteLn('Temps lecture : ', tempsLectureTotal, ' ms (',
          (tempsLectureTotal * 100) div (tempsLectureTotal + tempsTraitementTotal), '%)');
  WriteLn('Temps traitement : ', tempsTraitementTotal, ' ms (',
          (tempsTraitementTotal * 100) div (tempsLectureTotal + tempsTraitementTotal), '%)');
end;
```

**Résultats typiques :**
```
Lignes traitées : 10000  
Lignes traitées : 20000  
...
STATISTIQUES  
Lignes : 100000  
Temps lecture : 450 ms (15%)  
Temps traitement : 2550 ms (85%)    ← Le traitement est le goulot  
```

**Conclusion :** Optimiser `TraiterLigne`, pas la lecture du fichier.

### 8.3 Scénario : Interface Graphique qui Gèle

**Symptôme :** L'interface se fige pendant un traitement.

**Cause probable :** Opération longue dans le thread principal.

**Profiling :**

```pascal
procedure TFormPrincipal.ButtonTraiterClick(Sender: TObject);  
var  
  debut: QWord;
begin
  debut := GetTickCount64;

  // Opération longue bloquante
  for i := 1 to 10000000 do
    Traiter(i);

  ShowMessage('Terminé en ' + IntToStr(GetTickCount64 - debut) + ' ms');
end;
```

**Résultat :** 5000 ms → L'interface gèle pendant 5 secondes.

**Solution :** Déplacer vers un thread (voir section 18 du plan de formation).

---

## 9. Optimisations Courantes Basées sur le Profiling

### 9.1 Mise en Cache (Caching)

**Avant (calcul répété) :**

```pascal
function ObtenirNomClient(id: Integer): String;  
begin  
  // Requête BDD coûteuse
  Result := RequeteBDD('SELECT Nom FROM Clients WHERE ID = ' + IntToStr(id));
end;

// Appelé 1000 fois avec les mêmes IDs
for i := 1 to 1000 do
  WriteLn(ObtenirNomClient(42));  // 1000 requêtes identiques !
```

**Après (avec cache) :**

```pascal
var
  Cache: TDictionary<Integer, String>;

function ObtenirNomClient(id: Integer): String;  
begin  
  if not Cache.ContainsKey(id) then
    Cache.Add(id, RequeteBDD('SELECT Nom FROM Clients WHERE ID = ' + IntToStr(id)));

  Result := Cache[id];
end;
```

**Gain :** De 1000 requêtes à 1 seule requête !

### 9.2 Précalcul et Tables de Lookup

**Avant (calcul à chaque fois) :**

```pascal
function EstPremier(n: Integer): Boolean;  
var  
  i: Integer;
begin
  if n < 2 then Exit(False);
  for i := 2 to Trunc(Sqrt(n)) do
    if n mod i = 0 then Exit(False);
  Result := True;
end;

// Utilisé des milliers de fois
for i := 1 to 100000 do
  if EstPremier(i) then ...
```

**Après (précalcul) :**

```pascal
var
  Premiers: array[1..100000] of Boolean;

procedure PrecalculerPremiers;  
var  
  i: Integer;
begin
  for i := 1 to 100000 do
    Premiers[i] := EstPremier(i);
end;

// Initialisation (une fois)
PrecalculerPremiers;

// Utilisation (très rapide)
for i := 1 to 100000 do
  if Premiers[i] then ...
```

**Gain :** Accès instantané vs calcul à chaque fois.

### 9.3 Réduire les Allocations

**Avant (allocations répétées) :**

```pascal
for i := 1 to 100000 do  
begin  
  liste := TStringList.Create;
  liste.Add('Element');
  Traiter(liste);
  liste.Free;
end;
```

**Après (réutilisation) :**

```pascal
liste := TStringList.Create;  
try  
  for i := 1 to 100000 do
  begin
    liste.Clear;
    liste.Add('Element');
    Traiter(liste);
  end;
finally
  liste.Free;
end;
```

**Gain :** 100 000 allocations → 1 seule allocation.

---

## 10. Multi-plateforme : Windows vs Linux

### 10.1 Différences de Performance

**Windows :**
- `GetTickCount64` : Précision ~15 ms (dépend de la configuration)
- `QueryPerformanceCounter` : Très haute précision (microsecondes)

**Linux :**
- `GetTickCount64` : Implémenté via `clock_gettime`, précision élevée
- `clock_gettime` : Directement accessible, précision nanoseconde

**Code portable haute précision :**

```pascal
{$IFDEF WINDOWS}
uses Windows;

function GetTimeMicroseconds: Int64;  
var  
  freq, count: Int64;
begin
  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(count);
  Result := (count * 1000000) div freq;
end;
{$ELSE}
uses Unix, BaseUnix;

function GetTimeMicroseconds: Int64;  
var  
  tp: TTimeSpec;
begin
  clock_gettime(CLOCK_MONOTONIC, @tp);
  Result := (Int64(tp.tv_sec) * 1000000) + (tp.tv_nsec div 1000);
end;
{$ENDIF}

// Utilisation identique sur les deux plateformes
var
  debut, fin: Int64;
begin
  debut := GetTimeMicroseconds;
  // Code à mesurer
  fin := GetTimeMicroseconds;
  WriteLn('Temps : ', (fin - debut) / 1000:0:3, ' ms');
end.
```

### 10.2 Outils Spécifiques

| Outil | Windows | Linux | Notes |
|-------|---------|-------|-------|
| **GetTickCount64** | ✅ | ✅ | Portable, précision ms |
| **QueryPerformanceCounter** | ✅ | ❌ | Windows uniquement |
| **clock_gettime** | ❌ | ✅ | Linux, précision ns |
| **gprof** | ✅ | ✅ | Multi-plateforme |
| **Valgrind** | ❌ | ✅ | Linux uniquement |
| **Visual Profiler** | ✅ | ❌ | Windows (commercial) |

---

## 11. Bonnes Pratiques de Profiling

### 11.1 Checklist du Profiling

**Avant de profiler :**
- [ ] Compilez en mode Release (optimisations activées)
- [ ] Désactivez le débogage (pas de -g)
- [ ] Fermez les autres applications
- [ ] Utilisez des données réelles (pas de données de test minuscules)
- [ ] Exécutez plusieurs fois pour la moyenne

**Pendant le profiling :**
- [ ] Mesurez plusieurs exécutions (variabilité)
- [ ] Notez les résultats de manière structurée
- [ ] Identifiez les goulots avant d'optimiser
- [ ] Mesurez les sections significatives, pas chaque ligne

**Après le profiling :**
- [ ] Documentez les résultats
- [ ] Priorisez les optimisations (80/20)
- [ ] Re-profiler après chaque optimisation
- [ ] Comparez avec les mesures initiales

### 11.2 Erreurs Courantes à Éviter

**❌ Erreur 1 : Profiler en mode Debug**
- Le mode Debug est beaucoup plus lent
- Les optimisations sont désactivées
- **Solution :** Toujours profiler en mode Release

**❌ Erreur 2 : Une seule exécution**
- Les résultats peuvent varier (cache, OS, etc.)
- **Solution :** Moyenne de 5-10 exécutions

**❌ Erreur 3 : Données de test non représentatives**
- Tester avec 10 lignes au lieu de 1 million
- **Solution :** Utilisez des données réelles

**❌ Erreur 4 : Optimiser sans mesurer**
- "Je pense que cette fonction est lente"
- **Solution :** Profilez d'abord, optimisez ensuite

**❌ Erreur 5 : Micro-optimisations**
- Optimiser une fonction qui prend 0,1% du temps
- **Solution :** Suivez la règle 80/20

### 11.3 Documenter les Résultats

**Créez un rapport de profiling :**

```
RAPPORT DE PROFILING - Version 1.0  
Date : 15/10/2025  
Environnement : Windows 11, i7-9700K, 16 GB RAM  

Configuration de test :
- Fichier : data_1M.csv (1 million de lignes)
- Exécutions : 5 fois, moyenne calculée

RÉSULTATS INITIAUX :  
Fonction                | Temps (ms) | % Total  
--------------------------------------------------
ChargerFichier          |        850 |     8.5%  
TraiterDonnees          |      7.250 |    72.5%  ← GOULOT  
CalculerStatistiques    |      1.200 |    12.0%  
GenererRapport          |        700 |     7.0%  
--------------------------------------------------
TOTAL                   |     10.000 |   100.0%

ANALYSE :
- TraiterDonnees représente 72.5% du temps
- Cause identifiée : Boucle O(n²) dans fonction Trier

OPTIMISATION PRÉVUE :
- Remplacer tri à bulles par QuickSort
- Gain estimé : 50-70%

PROCHAINES MESURES :
- Re-profiler après optimisation
- Comparer avec résultats actuels
```

---

## 12. Récapitulatif

Le profiling est une compétence essentielle pour créer des applications performantes. Les points clés à retenir :

**Concepts Fondamentaux :**
- Les goulots d'étranglement concentrent 80% du temps dans 20% du code
- Profiler AVANT d'optimiser (éviter l'optimisation prématurée)
- Mesurer, optimiser, re-mesurer (cycle itératif)

**Techniques de Mesure :**
- **GetTickCount64** : Simple, portable, précision milliseconde
- **QueryPerformanceCounter** (Windows) : Haute précision
- **clock_gettime** (Linux) : Précision nanoseconde

**Méthodologie :**
1. Mesure globale du programme
2. Identification des sections lentes
3. Profiling détaillé des goulots
4. Optimisation ciblée
5. Validation des gains

**Outils :**
- Mesures manuelles (GetTickCount64)
- Profileurs externes (gprof, Valgrind)
- Rapports structurés

**Règle d'Or :**
> "Measure, don't guess" - Mesurez, ne devinez pas

**Prochaine Étape :** La section 20.5 (Optimisation des algorithmes courants) vous montrera comment optimiser concrètement les goulots identifiés par le profiling.

---

**Multi-plateforme Windows/Ubuntu** ✓  
**Formation FreePascal/Lazarus - Niveau Débutant à Intermédiaire**  

⏭️ [Optimisation des algorithmes courants](/20-debogage-optimisation/05-optimisation-algorithmes-courants.md)
