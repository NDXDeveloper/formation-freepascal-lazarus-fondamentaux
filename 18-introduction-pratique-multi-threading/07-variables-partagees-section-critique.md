🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.7 Variables partagées et section critique

## Introduction

Jusqu'à présent, nous avons utilisé `Synchronize` et `Queue` pour communiquer avec l'interface graphique. Mais que se passe-t-il quand plusieurs threads doivent accéder aux **mêmes données** qui ne sont pas liées à l'interface ?

Par exemple : un compteur partagé, une liste de tâches, un cache de données, ou n'importe quelle variable accessible par plusieurs threads simultanément.

C'est là que les choses deviennent délicates. Dans cette section, nous allons comprendre les problèmes liés aux **variables partagées** et apprendre à les gérer avec les **sections critiques**.

## Qu'est-ce qu'une variable partagée ?

### Définition simple

Une **variable partagée** est une variable accessible par plusieurs threads en même temps.

### Exemples

```pascal
var
  CompteurGlobal: Integer = 0;  // Variable globale

type
  TFormMain = class(TForm)
  private
    FDonneesPartagees: TStringList;  // Variable de classe
  end;
```

Ces variables sont partagées car :
- Plusieurs threads peuvent y accéder
- Elles ne sont pas protégées par défaut
- Modifier leur valeur depuis plusieurs threads peut causer des problèmes

### Variables NON partagées

À l'inverse, ces variables ne sont PAS partagées :

```pascal
procedure TMyThread.Execute;
var
  CompteurLocal: Integer;  // Variable locale : unique à ce thread
begin
  CompteurLocal := 0;  // Pas de problème, elle est privée au thread
end;
```

Les variables locales à `Execute` appartiennent uniquement au thread qui les exécute.

## Le problème des variables partagées

### Exemple simple qui semble fonctionner

Imaginons un compteur global que deux threads incrémentent :

```pascal
var
  CompteurGlobal: Integer = 0;

type
  TThreadCompteur = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TThreadCompteur.Execute;
var
  i: Integer;
begin
  for i := 1 to 1000 do
  begin
    CompteurGlobal := CompteurGlobal + 1;  // Incrémenter
  end;
end;

// Dans le formulaire
procedure TFormMain.ButtonClick(Sender: TObject);
var
  Thread1, Thread2: TThreadCompteur;
begin
  CompteurGlobal := 0;

  Thread1 := TThreadCompteur.Create(False);
  Thread2 := TThreadCompteur.Create(False);

  Thread1.WaitFor;
  Thread2.WaitFor;

  // Résultat attendu : 2000 (1000 + 1000)
  ShowMessage('Compteur = ' + IntToStr(CompteurGlobal));

  Thread1.Free;
  Thread2.Free;
end;
```

**Question** : Quel sera le résultat ?

**Réponse attendue** : 2000 (Thread1 ajoute 1000, Thread2 ajoute 1000)

**Réponse réelle** : Peut-être 1847, ou 1923, ou 2000... **C'est aléatoire !**

### Pourquoi ce problème ?

L'opération `CompteurGlobal := CompteurGlobal + 1` semble simple, mais elle ne l'est pas pour le processeur.

Elle se décompose en **trois étapes** :

1. **Lire** la valeur actuelle de `CompteurGlobal` dans un registre
2. **Ajouter** 1 à cette valeur
3. **Écrire** le résultat dans `CompteurGlobal`

### Visualisation du problème (Race Condition)

Voici ce qui peut se passer quand deux threads exécutent ce code en même temps :

```
État initial : CompteurGlobal = 100

THREAD 1                          THREAD 2
   |                                 |
   | Lire CompteurGlobal             |
   | → Registre1 = 100               |
   |                                 | Lire CompteurGlobal
   |                                 | → Registre2 = 100
   | Ajouter 1                       |
   | → Registre1 = 101               |
   |                                 | Ajouter 1
   |                                 | → Registre2 = 101
   | Écrire 101                      |
   | → CompteurGlobal = 101          |
   |                                 | Écrire 101
   |                                 | → CompteurGlobal = 101
   |                                 |
État final : CompteurGlobal = 101
```

**Résultat attendu** : 102 (100 + 1 + 1)
**Résultat obtenu** : 101

**Une incrémentation a été perdue !** On appelle cela une **race condition** (condition de concurrence).

### Analogie : le compte bancaire

Imaginez un compte bancaire avec 100€. Deux guichets automatiques différents traitent deux dépôts de 50€ **en même temps** :

```
Guichet A                         Guichet B
   |                                 |
   | Lire solde = 100€               |
   |                                 | Lire solde = 100€
   | Calculer 100 + 50 = 150€        |
   |                                 | Calculer 100 + 50 = 150€
   | Écrire solde = 150€             |
   |                                 | Écrire solde = 150€
```

**Résultat** : Le compte affiche 150€ au lieu de 200€ !

Un des dépôts a été **perdu** à cause de l'accès simultané.

## Types de problèmes avec les variables partagées

### 1. Race Condition (Condition de concurrence)

Nous venons de le voir : deux threads lisent/modifient/écrivent en même temps, et le résultat est imprévisible.

### 2. Lecture de données incohérentes

```pascal
type
  TCoordonnees = record
    X: Double;
    Y: Double;
  end;

var
  Position: TCoordonnees;

// Thread 1 écrit
Position.X := 10.5;
Position.Y := 20.3;

// Thread 2 lit EN MÊME TEMPS
if Position.X > 10 then  // Lit X = 10.5 ✓
begin
  // Mais entre temps, Thread 1 a changé les valeurs !
  Distance := Sqrt(Position.X * Position.X + Position.Y * Position.Y);
  // Peut lire X = 10.5 et Y = ancienne valeur !
end;
```

Le Thread 2 peut lire des données **partiellement modifiées**.

### 3. Corruption de structures complexes

Avec des structures comme `TStringList`, `TList`, etc. :

```pascal
var
  Liste: TStringList;

// Thread 1 ajoute
Liste.Add('Item 1');

// Thread 2 supprime EN MÊME TEMPS
Liste.Delete(0);
```

**Conséquence** : Corruption de la structure interne, crash, violation d'accès.

## La solution : les sections critiques

### Qu'est-ce qu'une section critique ?

Une **section critique** est un mécanisme qui garantit qu'**un seul thread à la fois** peut exécuter un morceau de code.

C'est comme un verrou sur une porte :
- Le premier thread qui arrive ferme la porte (entre dans la section critique)
- Les autres threads doivent attendre dehors
- Quand le premier thread sort, il ouvre la porte
- Le thread suivant peut entrer

### La classe TCriticalSection

FreePascal fournit la classe **`TCriticalSection`** dans l'unité `SyncObjs`.

```pascal
uses
  SyncObjs;

var
  CS: TCriticalSection;
```

### Méthodes principales

| Méthode | Description |
|---------|-------------|
| `Create` | Crée une section critique |
| `Free` | Libère la section critique |
| `Enter` ou `Acquire` | Entre dans la section critique (bloque si occupée) |
| `Leave` ou `Release` | Sort de la section critique |
| `TryEnter` | Essaie d'entrer sans attendre (retourne True/False) |

## Utilisation de base

### Structure générale

```pascal
uses
  SyncObjs;

var
  CS: TCriticalSection;
  CompteurGlobal: Integer;

initialization
  CS := TCriticalSection.Create;

finalization
  CS.Free;

// Dans le thread
procedure TMyThread.Execute;
begin
  CS.Enter;
  try
    // SECTION CRITIQUE
    // Un seul thread à la fois peut être ici
    CompteurGlobal := CompteurGlobal + 1;
  finally
    CS.Leave;
  end;
end;
```

**Important** : Toujours utiliser `try-finally` pour garantir que `Leave` est appelé même en cas d'exception !

### Exemple corrigé du compteur

Reprenons notre exemple bugué et corrigeons-le :

```pascal
uses
  Classes, SysUtils, SyncObjs;

var
  CompteurGlobal: Integer = 0;
  CS: TCriticalSection;

type
  TThreadCompteur = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TThreadCompteur.Execute;
var
  i: Integer;
begin
  for i := 1 to 1000 do
  begin
    // PROTECTION : entrer dans la section critique
    CS.Enter;
    try
      CompteurGlobal := CompteurGlobal + 1;
    finally
      CS.Leave;
    end;
  end;
end;

procedure TFormMain.ButtonClick(Sender: TObject);
var
  Thread1, Thread2: TThreadCompteur;
begin
  CompteurGlobal := 0;

  Thread1 := TThreadCompteur.Create(False);
  Thread2 := TThreadCompteur.Create(False);

  Thread1.WaitFor;
  Thread2.WaitFor;

  ShowMessage('Compteur = ' + IntToStr(CompteurGlobal));
  // Résultat GARANTI : 2000 !

  Thread1.Free;
  Thread2.Free;
end;

initialization
  CS := TCriticalSection.Create;

finalization
  CS.Free;
```

**Résultat** : Toujours 2000, peu importe l'ordre d'exécution des threads !

### Comment ça fonctionne ?

```
THREAD 1                          THREAD 2
   |                                 |
   | CS.Enter                        |
   | → Succès, entre                 |
   | Lire Compteur = 100             |
   |                                 | CS.Enter
   |                                 | → BLOQUÉ, doit attendre
   | Ajouter 1 = 101                 |
   | Écrire Compteur = 101           | [Attend toujours...]
   | CS.Leave                        |
   |                                 | → Débloqué, entre !
   |                                 | Lire Compteur = 101
   |                                 | Ajouter 1 = 102
   |                                 | Écrire Compteur = 102
   |                                 | CS.Leave
```

Chaque thread attend son tour, garantissant l'intégrité des données.

## Exemple pratique : liste partagée

Créons une liste de tâches accessible par plusieurs threads.

```pascal
uses
  Classes, SysUtils, SyncObjs, Generics.Collections;

type
  TListeTachesPartagee = class
  private
    FListe: TStringList;
    FCS: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AjouterTache(const Tache: string);
    function ObtenirTache: string;
    function NombreTaches: Integer;
  end;

constructor TListeTachesPartagee.Create;
begin
  inherited Create;
  FListe := TStringList.Create;
  FCS := TCriticalSection.Create;
end;

destructor TListeTachesPartagee.Destroy;
begin
  FCS.Free;
  FListe.Free;
  inherited;
end;

procedure TListeTachesPartagee.AjouterTache(const Tache: string);
begin
  FCS.Enter;
  try
    FListe.Add(Tache);
  finally
    FCS.Leave;
  end;
end;

function TListeTachesPartagee.ObtenirTache: string;
begin
  Result := '';
  FCS.Enter;
  try
    if FListe.Count > 0 then
    begin
      Result := FListe[0];
      FListe.Delete(0);
    end;
  finally
    FCS.Leave;
  end;
end;

function TListeTachesPartagee.NombreTaches: Integer;
begin
  FCS.Enter;
  try
    Result := FListe.Count;
  finally
    FCS.Leave;
  end;
end;
```

### Utilisation avec plusieurs threads

```pascal
var
  ListeTaches: TListeTachesPartagee;

type
  TThreadProducteur = class(TThread)
  protected
    procedure Execute; override;
  end;

type
  TThreadConsommateur = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TThreadProducteur.Execute;
var
  i: Integer;
begin
  for i := 1 to 100 do
  begin
    ListeTaches.AjouterTache('Tâche ' + IntToStr(i));
    Sleep(10);
  end;
end;

procedure TThreadConsommateur.Execute;
var
  Tache: string;
begin
  while not Terminated do
  begin
    Tache := ListeTaches.ObtenirTache;
    if Tache <> '' then
    begin
      // Traiter la tâche
      ProcessTask(Tache);
    end
    else
      Sleep(50);  // Pas de tâche, attendre un peu
  end;
end;

// Lancement
procedure TFormMain.ButtonStartClick(Sender: TObject);
begin
  ListeTaches := TListeTachesPartagee.Create;

  TThreadProducteur.Create(False);
  TThreadConsommateur.Create(False);
  TThreadConsommateur.Create(False);  // Deux consommateurs
end;
```

**Sécurité** : Les trois threads (1 producteur, 2 consommateurs) accèdent à la liste en toute sécurité grâce aux sections critiques.

## Erreurs courantes avec les sections critiques

### ❌ Erreur 1 : Oublier Leave (Deadlock)

```pascal
CS.Enter;
try
  if condition then
    Exit;  // ❌ ERREUR : on sort sans appeler Leave !

  DoSomething();
finally
  CS.Leave;
end;
```

**Problème** : Si `condition` est vraie, on sort sans libérer la section critique. Les autres threads attendront **éternellement** (deadlock).

**Solution** : Le `try-finally` garantit que `Leave` est toujours appelé.

### ❌ Erreur 2 : Entrer deux fois sans sortir

```pascal
CS.Enter;
try
  CS.Enter;  // ❌ Re-entrer dans la même section !
  try
    DoSomething();
  finally
    CS.Leave;
  end;
finally
  CS.Leave;
end;
```

**Note** : `TCriticalSection` supporte en fait la ré-entrance (même thread peut entrer plusieurs fois), mais c'est une mauvaise pratique et source de confusion.

**Solution** : Un seul `Enter` / `Leave` par bloc de code.

### ❌ Erreur 3 : Section critique trop large

```pascal
CS.Enter;
try
  // Opération rapide
  Compteur := Compteur + 1;

  // Opération TRÈS lente (10 secondes !)
  Sleep(10000);

  // Autre opération rapide
  Total := Total + Compteur;
finally
  CS.Leave;
end;
```

**Problème** : La section critique dure 10 secondes ! Tous les autres threads attendent pendant tout ce temps.

**Solution** : Minimiser le code dans la section critique.

```pascal
CS.Enter;
try
  Compteur := Compteur + 1;
finally
  CS.Leave;
end;

// Travail long HORS de la section critique
Sleep(10000);

CS.Enter;
try
  Total := Total + Compteur;
finally
  CS.Leave;
end;
```

### ❌ Erreur 4 : Accès non protégé à côté de protégé

```pascal
// Thread 1 : accès protégé
CS.Enter;
try
  Compteur := Compteur + 1;
finally
  CS.Leave;
end;

// Thread 2 : accès NON protégé
Compteur := Compteur + 1;  // ❌ ERREUR : pas de protection !
```

**Problème** : Il suffit d'un seul accès non protégé pour que tout le système soit corrompu.

**Solution** : **TOUS** les accès à une variable partagée doivent être protégés.

### ❌ Erreur 5 : Plusieurs sections critiques mal ordonnées (Deadlock)

```pascal
// Thread 1
CS1.Enter;
try
  CS2.Enter;
  try
    DoSomething();
  finally
    CS2.Leave;
  end;
finally
  CS1.Leave;
end;

// Thread 2
CS2.Enter;  // Ordre inversé !
try
  CS1.Enter;
  try
    DoSomethingElse();
  finally
    CS1.Leave;
  end;
finally
  CS2.Leave;
end;
```

**Problème** : Deadlock possible :
- Thread1 entre dans CS1
- Thread2 entre dans CS2
- Thread1 attend CS2 (occupé par Thread2)
- Thread2 attend CS1 (occupé par Thread1)
- **Blocage éternel !**

**Solution** : Toujours acquérir les sections critiques **dans le même ordre**.

## Quand utiliser Synchronize vs Section Critique

### Utilisez Synchronize quand :

```pascal
// Accès à l'interface graphique
procedure TMyThread.Execute;
begin
  FResultat := Calculate();
  Synchronize(@AfficherResultat);  // ✓
end;

procedure TMyThread.AfficherResultat;
begin
  Label1.Caption := IntToStr(FResultat);
end;
```

**Raison** : Vous accédez à des composants visuels.

### Utilisez Section Critique quand :

```pascal
// Accès à des données non-UI
procedure TMyThread.Execute;
begin
  CS.Enter;
  try
    CompteurGlobal := CompteurGlobal + 1;  // ✓
  finally
    CS.Leave;
  end;
end;
```

**Raison** : Vous protégez des données partagées qui ne sont pas liées à l'UI.

### Tableau de décision

| Type de donnée | Méthode recommandée | Raison |
|----------------|---------------------|--------|
| TLabel, TEdit, etc. | Synchronize | Composants UI |
| Integer, String globaux | Section Critique | Données simples |
| TStringList partagée | Section Critique | Structure complexe |
| ShowMessage | Synchronize | Fonction UI |
| Cache de données | Section Critique | Données métier |
| Fichier log partagé | Section Critique | Ressource partagée |

### Peut-on combiner les deux ?

Oui ! Exemple :

```pascal
procedure TMyThread.Execute;
var
  LocalCount: Integer;
begin
  // Lire le compteur global de façon sécurisée
  CS.Enter;
  try
    LocalCount := CompteurGlobal;
    CompteurGlobal := CompteurGlobal + 1;
  finally
    CS.Leave;
  end;

  // Afficher dans l'UI
  FCount := LocalCount;
  Synchronize(@AfficherCompteur);
end;
```

## Alternatives aux sections critiques

### TMultiReadExclusiveWriteSynchronizer (TMultiReadWriteLock)

Pour des scénarios où :
- Plusieurs threads **lisent** en même temps (OK)
- Un seul thread **écrit** à la fois (exclusif)

```pascal
uses
  SyncObjs;

var
  Lock: TMultiReadExclusiveWriteSynchronizer;
  DonneesPartagees: TStringList;

// Lecture (plusieurs threads simultanés OK)
Lock.BeginRead;
try
  Valeur := DonneesPartagees[0];
finally
  Lock.EndRead;
end;

// Écriture (exclusif)
Lock.BeginWrite;
try
  DonneesPartagees.Add('Nouveau');
finally
  Lock.EndWrite;
end;
```

**Avantage** : Meilleure performance si vous avez beaucoup de lectures et peu d'écritures.

### Variables atomiques (pour types simples)

Pour des opérations simples sur des entiers, FreePascal offre des fonctions atomiques :

```pascal
uses
  System.SyncObjs;

var
  Compteur: Integer;

// Incrémenter de façon atomique
InterlockedIncrement(Compteur);

// Décrémenter de façon atomique
InterlockedDecrement(Compteur);

// Échanger une valeur
InterlockedExchange(Compteur, 42);
```

**Avantage** : Plus rapide qu'une section critique pour des opérations très simples.

**Inconvénient** : Limité aux opérations élémentaires.

## Bonnes pratiques

### ✓ Pratique 1 : Une section critique par ressource partagée

```pascal
var
  Liste1: TStringList;
  CS_Liste1: TCriticalSection;

  Liste2: TStringList;
  CS_Liste2: TCriticalSection;
```

Chaque ressource a sa propre protection.

### ✓ Pratique 2 : Minimiser le temps dans la section critique

```pascal
// Mauvais : trop long
CS.Enter;
try
  Data := GetData();
  ProcessData(Data);  // Peut prendre du temps !
  SaveData(Data);
finally
  CS.Leave;
end;

// Bon : rapide
Data := GetData();
ProcessedData := ProcessData(Data);  // Hors de la section critique

CS.Enter;
try
  SaveData(ProcessedData);
finally
  CS.Leave;
end;
```

### ✓ Pratique 3 : Toujours utiliser try-finally

```pascal
// TOUJOURS faire ça :
CS.Enter;
try
  // Code protégé
finally
  CS.Leave;
end;
```

### ✓ Pratique 4 : Créer des wrappers

Encapsulez vos ressources partagées dans des classes avec protection intégrée, comme notre exemple `TListeTachesPartagee`.

### ✓ Pratique 5 : Documenter les variables partagées

```pascal
var
  // Variable partagée entre threads - Protégée par CS_Compteur
  CompteurGlobal: Integer;
  CS_Compteur: TCriticalSection;
```

## Exemple complet : compteur de téléchargements

Voici un exemple complet montrant l'utilisation correcte des sections critiques :

```pascal
uses
  Classes, SysUtils, SyncObjs;

type
  TStatistiques = class
  private
    FFichiersTotal: Integer;
    FOctetsTotal: Int64;
    FCS: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AjouterFichier(Octets: Int64);
    procedure ObtenirStats(out Fichiers: Integer; out Octets: Int64);
  end;

constructor TStatistiques.Create;
begin
  inherited;
  FFichiersTotal := 0;
  FOctetsTotal := 0;
  FCS := TCriticalSection.Create;
end;

destructor TStatistiques.Destroy;
begin
  FCS.Free;
  inherited;
end;

procedure TStatistiques.AjouterFichier(Octets: Int64);
begin
  FCS.Enter;
  try
    Inc(FFichiersTotal);
    FOctetsTotal := FOctetsTotal + Octets;
  finally
    FCS.Leave;
  end;
end;

procedure TStatistiques.ObtenirStats(out Fichiers: Integer; out Octets: Int64);
begin
  FCS.Enter;
  try
    Fichiers := FFichiersTotal;
    Octets := FOctetsTotal;
  finally
    FCS.Leave;
  end;
end;

// Utilisation dans plusieurs threads
type
  TThreadDownload = class(TThread)
  private
    FStats: TStatistiques;
  protected
    procedure Execute; override;
  public
    constructor Create(Stats: TStatistiques);
  end;

constructor TThreadDownload.Create(Stats: TStatistiques);
begin
  inherited Create(True);
  FStats := Stats;
  FreeOnTerminate := True;
end;

procedure TThreadDownload.Execute;
var
  TailleFichier: Int64;
begin
  // Simuler téléchargement
  TailleFichier := Random(1000000);
  Sleep(Random(1000));

  // Mettre à jour les stats de façon sécurisée
  FStats.AjouterFichier(TailleFichier);
end;
```

## Récapitulatif

### Variables partagées
- Accessible par plusieurs threads
- Source de race conditions
- Doivent être protégées

### Section critique (TCriticalSection)
- Garantit qu'un seul thread à la fois accède à une ressource
- Méthodes : `Enter` / `Leave`
- Toujours utiliser avec `try-finally`

### Erreurs à éviter
1. Oublier `Leave` → Deadlock
2. Section critique trop large → Performance dégradée
3. Accès non protégé → Corruption de données
4. Ordre d'acquisition inversé → Deadlock

### Règles d'or
- Minimiser le code dans la section critique
- Toujours protéger **tous** les accès
- Une section critique par ressource
- Documenter les variables partagées

## Conclusion

Les sections critiques sont un outil puissant mais qui demande de la rigueur. Les règles sont simples :

1. **Identifiez** les variables partagées
2. **Créez** une section critique pour chacune
3. **Protégez** tous les accès avec `Enter` / `Leave`
4. **Minimisez** le temps passé dans la section critique

Avec ces précautions, vous pouvez créer des applications multi-threadées robustes qui partagent des données en toute sécurité.

Dans les sections suivantes, nous allons mettre en pratique tout ce que nous avons appris pour créer des fonctionnalités complètes : barres de progression fonctionnelles et possibilité d'annuler des opérations longues.

La théorie est maintenant derrière nous, place à la pratique !

⏭️ [Barres de progression et feedback utilisateur](/18-introduction-pratique-multi-threading/08-barres-progression-feedback-utilisateur.md)
