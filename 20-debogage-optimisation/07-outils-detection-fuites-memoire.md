🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 20.7 Outils de Détection de Fuites Mémoire

## Introduction

Détecter manuellement les fuites mémoire est comme chercher une aiguille dans une botte de foin. Heureusement, il existe des outils spécialisés qui automatisent cette tâche et vous indiquent précisément où se trouvent les problèmes.

**Analogie :** C'est comme avoir un détecteur de métaux pour retrouver une pièce perdue dans le sable au lieu de creuser partout au hasard.

**Dans cette section, vous apprendrez à :**
- Utiliser HeapTrc (outil intégré FreePascal)
- Maîtriser Valgrind sous Linux
- Exploiter les outils Windows spécifiques
- Créer vos propres systèmes de détection
- Interpréter les rapports d'analyse
- Automatiser la détection dans vos projets

---

## 1. Vue d'Ensemble des Outils

### 1.1 Comparaison Rapide

| Outil | Plateforme | Difficulté | Détails | Performance |
|-------|-----------|------------|---------|-------------|
| **HeapTrc** | Win/Linux | ⭐ Facile | Moyen | Impact léger |
| **Valgrind** | Linux | ⭐⭐ Moyenne | Excellent | Ralentit 10-50x |
| **Dr. Memory** | Win/Linux | ⭐⭐ Moyenne | Excellent | Ralentit 10-30x |
| **AQTime** | Windows | ⭐⭐⭐ Avancé | Excellent | Ralentit 5-10x |
| **Custom** | Win/Linux | ⭐⭐ Moyenne | Variable | Configurable |

### 1.2 Quand Utiliser Quel Outil ?

**HeapTrc :**
- ✅ Développement quotidien
- ✅ Tests rapides
- ✅ Première détection
- ✅ Intégration simple

**Valgrind (Linux) :**
- ✅ Analyse approfondie
- ✅ Détection d'accès invalides
- ✅ Profiling mémoire
- ✅ Avant release

**Dr. Memory (Windows) :**
- ✅ Équivalent de Valgrind sur Windows
- ✅ Analyse détaillée
- ✅ Détection d'erreurs complexes

**Outils personnalisés :**
- ✅ Besoins spécifiques
- ✅ Traçage ciblé
- ✅ Production (logging)

---

## 2. HeapTrc : L'Outil Intégré

### 2.1 Présentation

**HeapTrc** est l'outil de détection de fuites intégré à FreePascal. Simple, efficace et portable.

**Avantages :**
- 🟢 Intégré (pas d'installation)
- 🟢 Multi-plateforme
- 🟢 Configuration simple
- 🟢 Impact performance faible

**Limitations :**
- 🟡 Moins détaillé que Valgrind
- 🟡 Pas de détection d'accès invalides
- 🟡 Format de sortie basique

### 2.2 Activation par Code

**Méthode 1 : Clause Uses**

```pascal
program MonProgramme;

{$IFDEF DEBUG}
uses
  heaptrc;
{$ENDIF}

begin
  // Votre code
  WriteLn('Programme terminé');
end.
```

**Méthode 2 : Avec Configuration**

```pascal
program MonProgramme;

uses
  heaptrc,
  SysUtils;

begin
  // Configuration HeapTrc
  SetHeapTraceOutput('heaptrc_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.log');

  // Votre code avec fuites potentielles

  WriteLn('Vérifiez le fichier heaptrc_*.log');
end.
```

**Options de configuration :**

```pascal
// Fichier de sortie personnalisé
SetHeapTraceOutput('chemin/vers/rapport.log');

// Désactiver temporairement
DisableHeapTrace;

// Réactiver
EnableHeapTrace;
```

### 2.3 Activation par Options Compilateur

**Dans Lazarus :**

1. **Projet** → **Options du projet**
2. **Débogage**
3. Cochez **"Utiliser HeapTrc (détection de fuites)"**
4. OU dans **Options du compilateur**, onglet **Autre**, ajoutez : `-gh`

**Ligne de commande :**

```bash
fpc -gh -gl monprogramme.pas
```

**Options :**
- `-gh` : Active HeapTrc
- `-gl` : Ajoute les numéros de ligne (important !)
- `-gw` : Génère info DWARF (meilleure précision)

### 2.4 Interpréter les Rapports HeapTrc

**Programme de test avec fuite :**

```pascal
program TestFuiteSimple;

{$IFDEF DEBUG}
uses
  heaptrc, Classes;
{$ENDIF}

procedure CreerFuite;
var
  liste: TStringList;
begin
  liste := TStringList.Create;
  liste.Add('Element 1');
  liste.Add('Element 2');
  // Oubli volontaire du Free !
end;

procedure SansFuite;
var
  liste: TStringList;
begin
  liste := TStringList.Create;
  try
    liste.Add('Element');
  finally
    liste.Free;  // Correct
  end;
end;

begin
  {$IFDEF DEBUG}
  SetHeapTraceOutput('rapport.log');
  {$ENDIF}

  WriteLn('Test avec fuite...');
  CreerFuite;

  WriteLn('Test sans fuite...');
  SansFuite;

  WriteLn('Programme terminé. Vérifiez rapport.log');
end.
```

**Sortie dans rapport.log :**

```
Heap dump by heaptrc unit of C:\Projets\testfuite.exe
240 memory blocks allocated : 3456/3712
239 memory blocks freed     : 3408/3664
1 unfreed memory blocks : 48
True heap size : 131072
True free heap : 130976
Should be : 131024

Call trace for block $00007FF7A0012340 size 48
  $00402156  CREERFUITE,  line 12 of testfuite.pas
  $004025A8  main,  line 34 of testfuite.pas
```

**Analyse ligne par ligne :**

```
1 unfreed memory blocks : 48
```
→ **1 bloc non libéré de 48 octets** (la TStringList)

```
Call trace for block $00007FF7A0012340 size 48
```
→ **Adresse mémoire** du bloc et sa **taille**

```
$00402156  CREERFUITE,  line 12 of testfuite.pas
```
→ **Fonction** où l'allocation a eu lieu et **ligne précise**

```
$004025A8  main,  line 34 of testfuite.pas
```
→ **Appelant** (trace d'appel)

**Verdict :** Fuite dans `CreerFuite` à la ligne 12.

### 2.5 Cas Complexes : Fuites Multiples

**Programme avec plusieurs fuites :**

```pascal
program TestFuitesMultiples;

uses
  heaptrc, Classes, SysUtils;

procedure Fuite1;
var
  s: TStringList;
begin
  s := TStringList.Create;
  s.Add('Fuite 1');
end;

procedure Fuite2;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create('test.txt', fmCreate);
  fs.WriteBuffer('Test', 4);
  // Oubli du Free
end;

procedure Fuite3;
var
  i: Integer;
  liste: TList;
begin
  liste := TList.Create;
  for i := 1 to 10 do
    liste.Add(TObject.Create);  // 10 objets non libérés
  liste.Free;  // Libère la liste mais pas les objets !
end;

begin
  SetHeapTraceOutput('fuites_multiples.log');

  Fuite1;
  Fuite2;
  Fuite3;
end.
```

**Rapport généré :**

```
Heap dump by heaptrc unit
12 unfreed memory blocks : 512

Call trace for block $0000000000123400 size 48
  $00401234  FUITE1,  line 8 of testfuitesmultiples.pas
  $00401890  main,  line 36 of testfuitesmultiples.pas

Call trace for block $0000000000123500 size 256
  $00401456  FUITE2,  line 15 of testfuitesmultiples.pas
  $00401898  main,  line 37 of testfuitesmultiples.pas

Call trace for block $0000000000123600 size 16
  $00401678  FUITE3,  line 24 of testfuitesmultiples.pas
  $004018A0  main,  line 38 of testfuitesmultiples.pas

[... 9 autres blocs similaires pour les objets dans Fuite3 ...]
```

**Analyse :**
- **Fuite1** : TStringList (48 octets)
- **Fuite2** : TFileStream (256 octets)
- **Fuite3** : 10 TObject (16 octets chacun)

### 2.6 Options Avancées HeapTrc

**Contrôle fin de la trace :**

```pascal
uses heaptrc;

begin
  // Désactiver temporairement
  DisableHeapTrace;

  // Code qui alloue beaucoup (pas de trace)
  for i := 1 to 1000000 do
    // ...

  // Réactiver
  EnableHeapTrace;

  // Code à tracer
  CreerObjet;
end.
```

**Définir le niveau de détail (compilation) :**

```pascal
{$DEFINE HEAPTRC}
{$IFDEF HEAPTRC}
  // Trace complète
  {$DEFINE HEAPTRC_FULL}
{$ENDIF}
```

---

## 3. Valgrind : L'Outil de Référence (Linux)

### 3.1 Installation

**Ubuntu/Debian :**

```bash
sudo apt update
sudo apt install valgrind
```

**Fedora/CentOS :**

```bash
sudo dnf install valgrind
# ou
sudo yum install valgrind
```

**Vérification :**

```bash
valgrind --version
# Devrait afficher : valgrind-3.x.x
```

### 3.2 Utilisation Basique

**Compilation avec symboles de débogage :**

```bash
fpc -g -gl monprogramme.pas
```

**Exécution avec Valgrind :**

```bash
valgrind --leak-check=full ./monprogramme
```

**Options courantes :**

```bash
# Trace complète des fuites
valgrind --leak-check=full \
         --show-leak-kinds=all \
         --track-origins=yes \
         --verbose \
         ./monprogramme

# Sortie dans un fichier
valgrind --leak-check=full \
         --log-file=valgrind_rapport.txt \
         ./monprogramme
```

### 3.3 Interpréter les Rapports Valgrind

**Programme de test :**

```pascal
program TestValgrind;

uses Classes;

procedure CreerFuite;
var
  liste: TStringList;
begin
  liste := TStringList.Create;
  liste.Add('Test');
  // Pas de Free
end;

begin
  WriteLn('Début du programme');
  CreerFuite;
  WriteLn('Fin du programme');
end.
```

**Compilation et exécution :**

```bash
fpc -g -gl testvalgrind.pas
valgrind --leak-check=full --show-leak-kinds=all ./testvalgrind
```

**Sortie Valgrind :**

```
==12345== Memcheck, a memory error detector
==12345== Copyright (C) 2002-2022, and GNU GPL'd, by Julian Seward et al.
==12345== Using Valgrind-3.19.0 and LibVEX; rerun with -h for copyright info
==12345== Command: ./testvalgrind
==12345==
Début du programme
Fin du programme
==12345==
==12345== HEAP SUMMARY:
==12345==     in use at exit: 48 bytes in 1 blocks
==12345==   total heap usage: 25 allocs, 24 frees, 1,456 bytes allocated
==12345==
==12345== 48 bytes in 1 blocks are definitely lost in loss record 1 of 1
==12345==    at 0x4C2FB0F: malloc (in /usr/lib/valgrind/vgpreload_memcheck-amd64-linux.so)
==12345==    by 0x401234: SYSTEM_$$_GETMEM$POINTER$INT64$$POINTER (system.pas:1234)
==12345==    by 0x402456: CLASSES_$$_TSTRINGLIST_$$_CREATE$$TSTRINGLIST (classes.pas:2456)
==12345==    by 0x403123: CREERFUITE (testvalgrind.pas:8)
==12345==    by 0x403567: main (testvalgrind.pas:15)
==12345==
==12345== LEAK SUMMARY:
==12345==    definitely lost: 48 bytes in 1 blocks
==12345==    indirectly lost: 0 bytes in 0 blocks
==12345==      possibly lost: 0 bytes in 0 blocks
==12345==    still reachable: 0 bytes in 0 blocks
==12345==         suppressed: 0 bytes in 0 blocks
==12345==
==12345== For counts of detected and suppressed errors, rerun with: -v
==12345== ERROR SUMMARY: 1 errors from 1 contexts (suppressed: 0 from 0)
```

**Décryptage :**

```
48 bytes in 1 blocks are definitely lost
```
→ **Fuite certaine** de 48 octets

```
by 0x403123: CREERFUITE (testvalgrind.pas:8)
```
→ **Fonction et ligne** où l'allocation a eu lieu

```
definitely lost: 48 bytes in 1 blocks
```
→ **Bilan des fuites** (différentes catégories)

### 3.4 Catégories de Fuites Valgrind

**Valgrind classe les fuites en 4 catégories :**

| Catégorie | Signification | Gravité |
|-----------|---------------|---------|
| **Definitely lost** | Fuite certaine, aucune référence | 🔴 Critique |
| **Indirectly lost** | Perdue via pointeur parent | 🔴 Critique |
| **Possibly lost** | Référence ambiguë | 🟡 Attention |
| **Still reachable** | Référence existe mais pas libérée | 🟢 Info |

**Exemple avec plusieurs catégories :**

```pascal
type
  PNode = ^TNode;
  TNode = record
    Data: Integer;
    Next: PNode;
  end;

var
  head, current: PNode;
  i: Integer;
begin
  // Créer une liste chaînée
  New(head);
  head^.Data := 1;
  current := head;

  for i := 2 to 5 do
  begin
    New(current^.Next);
    current := current^.Next;
    current^.Data := i;
  end;
  current^.Next := nil;

  // Perdre la référence sans libérer
  head := nil;  // Fuite !
end.
```

**Rapport Valgrind :**

```
48 bytes in 1 blocks are definitely lost
  (le premier nœud, référence perdue)

192 bytes in 4 blocks are indirectly lost
  (les nœuds suivants, perdus indirectement)
```

### 3.5 Suppression des Faux Positifs

**Valgrind peut rapporter des "fuites" dans les bibliothèques système.**

**Créer un fichier de suppression : `suppressions.supp`**

```
{
   RTL_FreePascal
   Memcheck:Leak
   ...
   fun:SYSTEM_$$_*
}

{
   GTK_Internal
   Memcheck:Leak
   ...
   obj:/usr/lib/x86_64-linux-gnu/libgtk*.so*
}
```

**Utilisation :**

```bash
valgrind --leak-check=full \
         --suppressions=suppressions.supp \
         ./monprogramme
```

### 3.6 Callgrind : Profiling Mémoire

**Analyser les allocations en détail :**

```bash
valgrind --tool=callgrind ./monprogramme
```

**Visualisation graphique avec KCachegrind :**

```bash
# Installation
sudo apt install kcachegrind

# Analyse
kcachegrind callgrind.out.12345
```

**KCachegrind montre :**
- Fonctions qui allouent le plus
- Graphe d'appels
- Temps par fonction
- Nombre d'allocations

---

## 4. Dr. Memory : Valgrind pour Windows

### 4.1 Installation

**Téléchargement :**

Visitez : https://drmemory.org/page_download.html

**Installation Windows :**

```cmd
# Télécharger l'installeur
# Installer dans C:\Program Files\DrMemory

# Ajouter au PATH
set PATH=%PATH%;C:\Program Files\DrMemory\bin
```

### 4.2 Utilisation

**Compilation :**

```cmd
fpc -g -gl monprogramme.pas
```

**Exécution :**

```cmd
drmemory.exe -- monprogramme.exe
```

**Options recommandées :**

```cmd
drmemory.exe -light -show_reachable -- monprogramme.exe
```

### 4.3 Rapport Dr. Memory

**Sortie typique :**

```
Dr. Memory version 2.5.0 build 1
DUPLICATE ERROR: LEAK 48 bytes
# 0 system.pas:1234
# 1 classes.pas:2456
# 2 testfuite.pas:12
# 3 testfuite.pas:24

LEAK SUMMARY:
      0 unique,     0 total unaddressable access(es)
      0 unique,     0 total uninitialized access(es)
      0 unique,     0 total invalid heap argument(s)
      1 unique,     1 total memory leak(s)
```

**Similaire à Valgrind, avec support Windows !**

---

## 5. Outils Intégrés Lazarus

### 5.1 Débogueur et Espions

**Surveiller les allocations en temps réel :**

```pascal
type
  TMoniteur = class
  private
    class var FCompteur: Integer;
  public
    class procedure Incrementer;
    class procedure Decrementer;
    class function GetCompteur: Integer;
  end;

class procedure TMoniteur.Incrementer;
begin
  Inc(FCompteur);
  WriteLn('[DEBUG] Allocations actives : ', FCompteur);
end;

class procedure TMoniteur.Decrementer;
begin
  Dec(FCompteur);
  WriteLn('[DEBUG] Allocations actives : ', FCompteur);
end;

class function TMoniteur.GetCompteur: Integer;
begin
  Result := FCompteur;
end;

// Dans vos classes
constructor TMonObjet.Create;
begin
  inherited Create;
  TMoniteur.Incrementer;
end;

destructor TMonObjet.Destroy;
begin
  TMoniteur.Decrementer;
  inherited Destroy;
end;
```

**Espion dans le débogueur :**

Ajoutez un espion sur : `TMoniteur.FCompteur`

**À la fin du programme :** Si compteur > 0 → Fuites !

### 5.2 Points d'Arrêt Conditionnels

**Détecter quand le compteur dépasse un seuil :**

1. Point d'arrêt à la fin du programme
2. Condition : `TMoniteur.GetCompteur > 0`
3. Si déclenché → Inspecter la pile d'appels

---

## 6. Systèmes de Détection Personnalisés

### 6.1 Gestionnaire d'Allocation Customisé

**Intercepter toutes les allocations/libérations :**

```pascal
unit MemoryTracker;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TAllocationInfo = record
    Adresse: Pointer;
    Taille: PtrUInt;
    Classe: String;
    Stack: String;
    Horodatage: TDateTime;
  end;

  TMemoryTracker = class
  private
    class var FAllocations: TDictionary<Pointer, TAllocationInfo>;
    class var FActif: Boolean;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure Activer;
    class procedure Desactiver;
    class procedure EnregistrerAllocation(Adresse: Pointer; Taille: PtrUInt; const Classe: String);
    class procedure EnregistrerLiberation(Adresse: Pointer);
    class procedure GenererRapport(const NomFichier: String);
  end;

implementation

class constructor TMemoryTracker.Create;
begin
  FAllocations := TDictionary<Pointer, TAllocationInfo>.Create;
  FActif := False;
end;

class destructor TMemoryTracker.Destroy;
begin
  FAllocations.Free;
end;

class procedure TMemoryTracker.Activer;
begin
  FActif := True;
end;

class procedure TMemoryTracker.Desactiver;
begin
  FActif := False;
end;

class procedure TMemoryTracker.EnregistrerAllocation(Adresse: Pointer; Taille: PtrUInt; const Classe: String);
var
  info: TAllocationInfo;
begin
  if not FActif then Exit;

  info.Adresse := Adresse;
  info.Taille := Taille;
  info.Classe := Classe;
  info.Stack := '';  // Pourrait capturer la pile d'appels
  info.Horodatage := Now;

  FAllocations.AddOrSetValue(Adresse, info);
end;

class procedure TMemoryTracker.EnregistrerLiberation(Adresse: Pointer);
begin
  if not FActif then Exit;

  if FAllocations.ContainsKey(Adresse) then
    FAllocations.Remove(Adresse);
end;

class procedure TMemoryTracker.GenererRapport(const NomFichier: String);
var
  F: TextFile;
  paire: TPair<Pointer, TAllocationInfo>;
  totalFuites: PtrUInt;
begin
  AssignFile(F, NomFichier);
  Rewrite(F);
  try
    WriteLn(F, 'RAPPORT DE FUITES MÉMOIRE');
    WriteLn(F, '=========================');
    WriteLn(F, 'Date: ', FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));
    WriteLn(F, '');
    WriteLn(F, 'Allocations non libérées : ', FAllocations.Count);
    WriteLn(F, '');

    totalFuites := 0;
    for paire in FAllocations do
    begin
      WriteLn(F, Format('Adresse: %p | Taille: %d octets | Classe: %s | Date: %s',
        [paire.Value.Adresse,
         paire.Value.Taille,
         paire.Value.Classe,
         FormatDateTime('hh:nn:ss', paire.Value.Horodatage)]));

      totalFuites := totalFuites + paire.Value.Taille;
    end;

    WriteLn(F, '');
    WriteLn(F, 'Total mémoire non libérée : ', totalFuites, ' octets');
  finally
    CloseFile(F);
  end;
end;

end.
```

**Utilisation dans vos classes :**

```pascal
uses MemoryTracker;

type
  TObjetTrace = class
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TObjetTrace.Create;
begin
  inherited Create;
  TMemoryTracker.EnregistrerAllocation(Self, InstanceSize, ClassName);
end;

destructor TObjetTrace.Destroy;
begin
  TMemoryTracker.EnregistrerLiberation(Self);
  inherited Destroy;
end;

// Programme principal
begin
  TMemoryTracker.Activer;

  // Votre code

  TMemoryTracker.GenererRapport('fuites.txt');
end.
```

### 6.2 Macro de Traçage

**Simplifier l'instrumentation :**

```pascal
{$MACRO ON}        // Active le support des macros (requis pour {$DEFINE X := ...})
{$DEFINE TRACE_MEMORY}

{$IFDEF TRACE_MEMORY}
  {$DEFINE TRACE_CREATE := TMemoryTracker.EnregistrerAllocation(Self, InstanceSize, ClassName)}
  {$DEFINE TRACE_DESTROY := TMemoryTracker.EnregistrerLiberation(Self)}
{$ELSE}
  {$DEFINE TRACE_CREATE := }
  {$DEFINE TRACE_DESTROY := }
{$ENDIF}

// Utilisation
constructor TMonObjet.Create;
begin
  inherited Create;
  TRACE_CREATE;
end;

destructor TMonObjet.Destroy;
begin
  TRACE_DESTROY;
  inherited Destroy;
end;
```

### 6.3 Logging Centralisé

**Logger toutes les opérations mémoire :**

```pascal
unit MemoryLogger;

interface

procedure LogAlloc(const Classe: String; Taille: Integer);
procedure LogFree(const Classe: String);
procedure LogRapport;

implementation

uses SysUtils, Classes;

var
  LogFile: TextFile;
  LogActif: Boolean = False;

procedure InitLog;
begin
  if not LogActif then
  begin
    AssignFile(LogFile, 'memory_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.log');
    Rewrite(LogFile);
    LogActif := True;
  end;
end;

procedure LogAlloc(const Classe: String; Taille: Integer);
begin
  InitLog;
  WriteLn(LogFile, FormatDateTime('hh:nn:ss.zzz', Now), ' [+] ', Classe, ' (', Taille, ' octets)');
  Flush(LogFile);
end;

procedure LogFree(const Classe: String);
begin
  if LogActif then
  begin
    WriteLn(LogFile, FormatDateTime('hh:nn:ss.zzz', Now), ' [-] ', Classe);
    Flush(LogFile);
  end;
end;

procedure LogRapport;
begin
  if LogActif then
  begin
    WriteLn(LogFile, '=== FIN DU LOG ===');
    CloseFile(LogFile);
    LogActif := False;
  end;
end;

finalization
  LogRapport;

end.
```

---

## 7. Analyse et Diagnostic

### 7.1 Workflow d'Investigation

**Méthodologie en 5 étapes :**

```
1. DÉTECTION
   ↓ (Exécuter avec HeapTrc/Valgrind)

2. LOCALISATION
   ↓ (Identifier les fichiers/lignes)

3. ANALYSE
   ↓ (Comprendre le contexte)

4. CORRECTION
   ↓ (Ajouter Free/try-finally)

5. VÉRIFICATION
   ↓ (Re-tester avec l'outil)
```

### 7.2 Cas Pratique : Investigation Complète

**Programme avec fuite subtile :**

```pascal
program FuiteSubtile;

uses Classes, SysUtils;

type
  TGestionnaire = class
  private
    FListe: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Traiter(const donnees: String);
  end;

constructor TGestionnaire.Create;
begin
  inherited Create;
  FListe := TStringList.Create;
end;

destructor TGestionnaire.Destroy;
begin
  FListe.Free;
  inherited Destroy;
end;

procedure TGestionnaire.Traiter(const donnees: String);
var
  temp: TStringList;
begin
  temp := TStringList.Create;
  temp.CommaText := donnees;

  FListe.AddStrings(temp);

  // ERREUR : Oubli du temp.Free !
end;

var
  gest: TGestionnaire;
begin
  gest := TGestionnaire.Create;
  try
    gest.Traiter('a,b,c');
    gest.Traiter('d,e,f');
    gest.Traiter('g,h,i');
  finally
    gest.Free;
  end;
end.
```

**Étape 1 : Détection avec HeapTrc**

```bash
fpc -gh -gl fuitesubtile.pas
./fuitesubtile
cat heaptrc.log
```

**Résultat :**

```
3 unfreed memory blocks : 144

Call trace for block $... size 48
  $... TGESTIONNAIRE_$$_TRAITER$ANSISTRING line 26
  $... main line 42

Call trace for block $... size 48
  $... TGESTIONNAIRE_$$_TRAITER$ANSISTRING line 26
  $... main line 43

Call trace for block $... size 48
  $... TGESTIONNAIRE_$$_TRAITER$ANSISTRING line 26
  $... main line 44
```

**Étape 2 : Localisation**

→ Ligne 26 : `temp := TStringList.Create;`  
→ Appelé 3 fois (lignes 42-44)

**Étape 3 : Analyse**

```pascal
procedure TGestionnaire.Traiter(const donnees: String);
var
  temp: TStringList;
begin
  temp := TStringList.Create;  // ← Ligne 26 : Allocation
  temp.CommaText := donnees;

  FListe.AddStrings(temp);

  // Sortie de la procédure sans Free !
end;
```

**Étape 4 : Correction**

```pascal
procedure TGestionnaire.Traiter(const donnees: String);
var
  temp: TStringList;
begin
  temp := TStringList.Create;
  try
    temp.CommaText := donnees;
    FListe.AddStrings(temp);
  finally
    temp.Free;  // ← Correction
  end;
end;
```

**Étape 5 : Vérification**

```bash
fpc -gh -gl fuitesubtile.pas
./fuitesubtile
cat heaptrc.log
```

**Résultat :**

```
0 unfreed memory blocks : 0
```

✅ **Fuite corrigée !**

### 7.3 Fuites Intermittentes

**Problème : Fuite qui n'apparaît que parfois.**

**Stratégie de détection :**

```pascal
// Activer logging détaillé
{$DEFINE DEBUG_MEMORY}

type
  TObjetDebug = class
  private
    class var FInstances: TList<TObjetDebug>;
  public
    class constructor Create;
    class destructor Destroy;
    constructor Create;
    destructor Destroy; override;
    class procedure DumpInstances;
  end;

class constructor TObjetDebug.Create;
begin
  FInstances := TList<TObjetDebug>.Create;
end;

class destructor TObjetDebug.Destroy;
begin
  DumpInstances;
  FInstances.Free;
end;

constructor TObjetDebug.Create;
begin
  inherited Create;
  FInstances.Add(Self);
  WriteLn('[DEBUG] Instance créée: ', IntToHex(PtrUInt(Self), 16),
          ' (Total: ', FInstances.Count, ')');
end;

destructor TObjetDebug.Destroy;
begin
  FInstances.Remove(Self);
  WriteLn('[DEBUG] Instance détruite: ', IntToHex(PtrUInt(Self), 16),
          ' (Total: ', FInstances.Count, ')');
  inherited Destroy;
end;

class procedure TObjetDebug.DumpInstances;
var
  obj: TObjetDebug;
begin
  if FInstances.Count > 0 then
  begin
    WriteLn('[ALERTE] Instances non détruites : ', FInstances.Count);
    for obj in FInstances do
      WriteLn('  - ', IntToHex(PtrUInt(obj), 16));
  end;
end;
```

---

## 8. Automatisation et CI/CD

### 8.1 Script de Test Automatique

**Bash (Linux) :**

```bash
#!/bin/bash
# test_memory.sh

echo "=== Test de détection de fuites mémoire ==="

# Compiler avec HeapTrc
fpc -gh -gl -B monprogramme.pas

if [ $? -ne 0 ]; then
    echo "ERREUR: Compilation échouée"
    exit 1
fi

# Exécuter
./monprogramme > output.txt 2>&1

# Vérifier le rapport HeapTrc
if grep -q "unfreed memory blocks : 0" heaptrc.log; then
    echo "✓ SUCCÈS: Aucune fuite détectée"
    exit 0
else
    echo "✗ ÉCHEC: Fuites détectées"
    echo "--- Détails ---"
    cat heaptrc.log
    exit 1
fi
```

**Batch (Windows) :**

```batch
@echo off
REM test_memory.bat

echo === Test de detection de fuites memoire ===

REM Compiler
fpc -gh -gl -B monprogramme.pas

if errorlevel 1 (
    echo ERREUR: Compilation echouee
    exit /b 1
)

REM Executer
monprogramme.exe

REM Verifier
findstr /C:"unfreed memory blocks : 0" heaptrc.log > nul

if errorlevel 1 (
    echo ECHEC: Fuites detectees
    type heaptrc.log
    exit /b 1
) else (
    echo SUCCES: Aucune fuite detectee
    exit /b 0
)
```

### 8.2 Intégration GitLab CI

**.gitlab-ci.yml :**

```yaml
stages:
  - build
  - test
  - memory_check

build:
  stage: build
  script:
    - fpc -gh -gl -B monprogramme.pas
  artifacts:
    paths:
      - monprogramme
    expire_in: 1 hour

memory_test:
  stage: memory_check
  dependencies:
    - build
  script:
    - ./monprogramme
    - |
      if grep -q "unfreed memory blocks : 0" heaptrc.log; then
        echo "Aucune fuite détectée"
      else
        echo "Fuites détectées !"
        cat heaptrc.log
        exit 1
      fi
  artifacts:
    when: on_failure
    paths:
      - heaptrc.log
```

### 8.3 Intégration GitHub Actions

**.github/workflows/memory_check.yml :**

```yaml
name: Memory Leak Check

on: [push, pull_request]

jobs:
  memory-check:
    runs-on: ubuntu-latest

    steps:
    - uses: actions/checkout@v3

    - name: Install FreePascal
      run: |
        sudo apt-get update
        sudo apt-get install -y fpc

    - name: Compile with HeapTrc
      run: fpc -gh -gl -B monprogramme.pas

    - name: Run program
      run: ./monprogramme

    - name: Check for memory leaks
      run: |
        if grep -q "unfreed memory blocks : 0" heaptrc.log; then
          echo "✓ No memory leaks detected"
        else
          echo "✗ Memory leaks detected!"
          cat heaptrc.log
          exit 1
        fi

    - name: Upload HeapTrc report
      if: failure()
      uses: actions/upload-artifact@v3
      with:
        name: heaptrc-report
        path: heaptrc.log
```

---

## 9. Comparaison et Choix de l'Outil

### 9.1 Tableau Comparatif Détaillé

| Critère | HeapTrc | Valgrind | Dr. Memory | Custom |
|---------|---------|----------|------------|--------|
| **Installation** | Intégré | apt install | Téléchargement | À coder |
| **Plateforme** | Win/Linux | Linux | Win/Linux | Win/Linux |
| **Facilité** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐ |
| **Précision** | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ |
| **Performance** | -10% | -90% | -70% | Variable |
| **Faux positifs** | Faible | Moyen | Faible | Très faible |
| **Détails** | Basique | Excellent | Excellent | Personnalisable |
| **Accès invalides** | ❌ | ✅ | ✅ | ❌ |
| **Profiling** | ❌ | ✅ | ✅ | ⚠️ |

### 9.2 Guide de Sélection

**Utilisez HeapTrc si :**
- ✅ Développement quotidien
- ✅ Tests rapides
- ✅ Impact performance faible requis
- ✅ Multi-plateforme nécessaire

**Utilisez Valgrind si :**
- ✅ Linux exclusivement
- ✅ Analyse approfondie nécessaire
- ✅ Détection d'accès mémoire invalides
- ✅ Avant release majeure

**Utilisez Dr. Memory si :**
- ✅ Windows principalement
- ✅ Équivalent Valgrind souhaité
- ✅ Analyse détaillée Windows

**Créez un outil custom si :**
- ✅ Besoins très spécifiques
- ✅ Intégration avec systèmes existants
- ✅ Logging en production nécessaire
- ✅ Métriques personnalisées

---

## 10. Bonnes Pratiques

### 10.1 Intégration dans le Workflow

**Phase de développement :**

```
1. Coder la fonctionnalité
2. Tester fonctionnellement
3. Compiler avec -gh (HeapTrc)
4. Exécuter et vérifier heaptrc.log
5. Corriger les fuites détectées
6. Re-tester
```

**Avant commit :**

```bash
# Script pre-commit
#!/bin/bash
fpc -gh -gl -B *.pas
./programme_principal
if ! grep -q "unfreed memory blocks : 0" heaptrc.log; then
    echo "Commit bloqué : fuites mémoire détectées"
    exit 1
fi
```

**Avant release :**

```
1. Tests complets avec HeapTrc
2. Tests avec Valgrind/Dr. Memory (analyse approfondie)
3. Tests de charge avec monitoring mémoire
4. Validation aucune fuite sur tous les scénarios
```

### 10.2 Documentation des Résultats

**Template de rapport :**

```markdown
# Rapport de Détection de Fuites Mémoire

**Projet:** MonApplication v1.2.3  
**Date:** 2025-10-15  
**Outil:** HeapTrc + Valgrind  
**Testeur:** Jean Dupont  

## Résumé Exécutif
- Fuites détectées : 3
- Fuites corrigées : 3
- Fuites restantes : 0
- Statut : ✅ VALIDÉ

## Détails des Fuites

### Fuite #1
- **Fichier:** gestionnaire.pas
- **Ligne:** 145
- **Fonction:** `TGestionnaire.Traiter`
- **Taille:** 48 octets
- **Cause:** Oubli de `temp.Free`
- **Correction:** Ajout de try-finally
- **Statut:** ✅ Corrigé

### Fuite #2
[...]

## Tests de Validation
- [x] HeapTrc : 0 fuites
- [x] Valgrind : 0 fuites
- [x] Tests de charge : Stable
- [x] Tests sur Windows : OK
- [x] Tests sur Linux : OK

## Conclusion
Toutes les fuites ont été corrigées et validées.
Application prête pour release.
```

### 10.3 Formation de l'Équipe

**Checklist pour nouveaux développeurs :**

- [ ] Comprendre Stack vs Heap
- [ ] Connaître Create/Free et try-finally
- [ ] Savoir activer HeapTrc
- [ ] Savoir lire un rapport HeapTrc
- [ ] Connaître FreeAndNil
- [ ] Comprendre l'ownership
- [ ] Savoir utiliser Valgrind (Linux)
- [ ] Avoir corrigé au moins 5 fuites

---

## 11. Récapitulatif

### 11.1 Outils par Cas d'Usage

| Situation | Outil Recommandé | Commande |
|-----------|-----------------|----------|
| Développement quotidien | HeapTrc | `fpc -gh -gl` |
| Test avant commit | HeapTrc | Script automatique |
| Analyse approfondie Linux | Valgrind | `valgrind --leak-check=full` |
| Analyse approfondie Windows | Dr. Memory | `drmemory --` |
| CI/CD | HeapTrc | Pipeline automatisé |
| Production | Logging custom | Système maison |

### 11.2 Commandes Essentielles

**FreePascal + HeapTrc :**
```bash
fpc -gh -gl monprogramme.pas
./monprogramme
cat heaptrc.log
```

**Valgrind complet :**
```bash
valgrind --leak-check=full \
         --show-leak-kinds=all \
         --track-origins=yes \
         --verbose \
         --log-file=valgrind.log \
         ./monprogramme
```

**Dr. Memory :**
```cmd
drmemory.exe -light -show_reachable -- monprogramme.exe
```

### 11.3 Points Clés à Retenir

1. **HeapTrc = Premier réflexe** - Toujours activer en développement
2. **Valgrind = Analyse approfondie** - Avant chaque release sur Linux
3. **Automatiser = Gagner du temps** - Intégrer dans CI/CD
4. **Corriger rapidement** - Ne pas laisser s'accumuler
5. **Tester après correction** - Toujours vérifier avec l'outil

**Citation finale :**
> "The best memory leak is the one you detect and fix before it reaches production."

---

## Conclusion

Les outils de détection de fuites mémoire sont vos meilleurs alliés pour créer des applications stables et performantes. Adoptez-les dès aujourd'hui dans votre workflow de développement.

**Workflow recommandé :**
1. **Développement** : HeapTrc activé en permanence
2. **Tests** : Valgrind/Dr. Memory avant release
3. **Production** : Monitoring et logging
4. **Maintenance** : Analyse régulière

Avec ces outils et ces pratiques, vous maîtriserez totalement la gestion de la mémoire dans vos projets FreePascal/Lazarus.

---

**Multi-plateforme Windows/Ubuntu** ✓  
**Formation FreePascal/Lazarus - Niveau Débutant à Intermédiaire**  

⏭️ [Logging structuré et niveaux de log](/20-debogage-optimisation/08-logging-structure-niveaux-log.md)
