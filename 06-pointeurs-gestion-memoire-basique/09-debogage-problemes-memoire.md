🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 6.9 Débogage des Problèmes Mémoire

## Introduction

Le débogage des problèmes mémoire est l'une des compétences les plus importantes à maîtriser en programmation avec pointeurs. Les bugs mémoire peuvent être difficiles à détecter car ils ne se manifestent pas toujours immédiatement. Cette section vous apprendra à identifier, localiser et corriger ces problèmes efficacement.

## Types de Problèmes Mémoire

### 1. Access Violation (Violation d'Accès)

**Symptôme :** Le programme plante avec "Access Violation" ou "Segmentation Fault"

**Causes courantes :**
- Déréférencement d'un pointeur nil
- Utilisation d'un pointeur après Dispose
- Accès hors limites d'un tableau
- Corruption de la mémoire

```pascal
// Exemple 1 : Pointeur nil
var
  p: ^Integer;
begin
  p := nil;
  WriteLn(p^);  // ✗ CRASH : Access Violation
end;

// Exemple 2 : Après Dispose
var
  p: ^Integer;
begin
  New(p);
  p^ := 42;
  Dispose(p);
  WriteLn(p^);  // ✗ CRASH : Mémoire libérée
end;

// Exemple 3 : Hors limites
var
  tab: array[1..5] of Integer;
  p: ^Integer;
begin
  p := @tab[10];  // ✗ CRASH : Hors des limites
  WriteLn(p^);
end;
```

### 2. Fuites Mémoire

**Symptôme :** Consommation croissante de mémoire, ralentissement progressif

**Causes :**
- Oubli de Dispose
- Perte de référence sans libération
- Libération partielle de structures

```pascal
// Exemple : Fuite en boucle
var
  p: ^Integer;
  i: Integer;
begin
  for i := 1 to 1000 do
  begin
    New(p);
    p^ := i;
    // ✗ Oubli de Dispose(p)
  end;
  // Fuite de 1000 * SizeOf(Integer) octets !
end;
```

### 3. Double Libération

**Symptôme :** Crash lors du second Dispose, corruption mémoire

**Cause :** Appeler Dispose deux fois sur le même pointeur

```pascal
var
  p: ^Integer;
begin
  New(p);
  p^ := 42;

  Dispose(p);
  Dispose(p);  // ✗ CRASH : Double libération
end;
```

### 4. Corruption Mémoire

**Symptôme :** Données incohérentes, crashes aléatoires, comportement imprévisible

**Causes :**
- Écriture hors limites
- Utilisation de pointeurs invalides
- Écrasement de structures mémoire

```pascal
var
  tableau: array[1..5] of Integer;
  i: Integer;
begin
  for i := 1 to 10 do  // ✗ Dépasse les limites !
    tableau[i] := i;   // Écrase autre chose en mémoire
end;
```

## Messages d'Erreur Courants

### Interprétation des Messages

| Message | Signification | Cause Probable |
|---------|---------------|----------------|
| Access Violation | Accès à une zone interdite | Pointeur nil ou invalide |
| Segmentation Fault | Même chose sous Linux | Pointeur nil ou invalide |
| Heap Corruption | Tas mémoire corrompu | Double libération, écriture hors limites |
| Stack Overflow | Pile pleine | Récursion infinie, tableaux trop gros |
| Out of Memory | Mémoire épuisée | Fuite mémoire, allocation trop grande |
| Invalid Pointer | Pointeur invalide | Utilisation après Dispose |

### Exemples avec Contexte

```pascal
// Message : "Access violation reading address 0x00000000"
// → Pointeur nil
var
  p: ^Integer;
begin
  WriteLn(p^);  // p = nil par défaut
end;

// Message : "Access violation reading address 0xFEEEFEEE"
// → Mémoire déjà libérée (pattern de debug)
var
  p: ^Integer;
begin
  New(p);
  Dispose(p);
  WriteLn(p^);  // Accès après libération
end;

// Message : "Heap corruption detected"
// → Double Dispose ou corruption
var
  p: ^Integer;
begin
  New(p);
  Dispose(p);
  Dispose(p);  // Double libération
end;
```

## Utilisation du Débogueur Lazarus

### Configurer le Débogage

**Étape 1 : Compiler avec informations de debug**

```
Menu Projet → Options du projet → Compilation et édition de liens
☑ Générer des informations de débogage (-g)
☑ Utiliser HeapTrc (-gh)
```

**Étape 2 : Activer le débogueur**

```
Menu Exécuter → Options de débogage
Type de débogueur : GNU debugger (gdb)
```

### Points d'Arrêt (Breakpoints)

**Définir un point d'arrêt :**
1. Cliquer dans la marge gauche de l'éditeur
2. Un cercle rouge apparaît
3. Le programme s'arrêtera à cette ligne

```pascal
procedure TraiterDonnees;
var
  p: ^Integer;
begin
  New(p);  // ← Mettre un point d'arrêt ici
  p^ := 42;
  WriteLn(p^);
  Dispose(p);
end;
```

**Types de points d'arrêt :**

- **Simple** : S'arrête toujours
- **Conditionnel** : S'arrête si condition vraie
  ```
  Clic droit sur le point → Propriétés
  Condition : p = nil
  ```
- **Compteur** : S'arrête après N passages
  ```
  Propriétés → Nombre de passages : 100
  ```

### Inspection de Variables

**Pendant le débogage, vous pouvez :**

1. **Survoler** une variable avec la souris
   ```pascal
   p := @valeur;  // Survoler "p" montre son adresse
   ```

2. **Fenêtre Variables locales**
   ```
   Menu Vue → Fenêtres de débogage → Variables locales
   ```
   Affiche toutes les variables de la fonction courante

3. **Fenêtre Espions (Watches)**
   ```
   Menu Vue → Fenêtres de débogage → Espions
   Ajouter : p^
   ```
   Surveille l'évolution d'une expression

4. **Évaluer/Modifier**
   ```
   Menu Exécuter → Évaluer/Modifier
   Expression : p^
   ```

### Exécution Pas à Pas

**Commandes essentielles :**

| Touche | Action | Usage |
|--------|--------|-------|
| F8 | Pas à pas suivant | Passe à la ligne suivante (survole les fonctions) |
| F7 | Pas à pas approfondi | Entre dans les fonctions |
| Shift+F8 | Sortir | Sort de la fonction courante |
| F9 | Continuer | Continue jusqu'au prochain point d'arrêt |
| Ctrl+F2 | Arrêter | Arrête le débogage |

**Exemple de session :**

```pascal
procedure Exemple;
var
  p: ^Integer;
begin
  p := nil;           // ← F8 : p vaut maintenant nil
  New(p);             // ← F8 : p vaut une adresse (ex: $1234)
  p^ := 42;           // ← F8 : *p vaut 42
  WriteLn(p^);        // ← F7 : entre dans WriteLn
  Dispose(p);         // ← F8 : mémoire libérée
  p := nil;           // ← F8 : p vaut nil
end;
```

### Call Stack (Pile d'Appels)

**Voir la séquence d'appels :**

```
Menu Vue → Fenêtres de débogage → Pile d'appels
```

```pascal
procedure NiveauC;
var
  p: ^Integer;
begin
  WriteLn(p^);  // ← CRASH ici
end;

procedure NiveauB;
begin
  NiveauC;      // Appelée depuis ici
end;

procedure NiveauA;
begin
  NiveauB;      // Appelée depuis ici
end;

begin
  NiveauA;      // Tout commence ici
end.
```

**Call Stack affiché :**
```
NiveauC      ← Erreur ici
NiveauB
NiveauA
PROGRAMME
```

Permet de remonter la chaîne d'appels pour comprendre le contexte.

## Utilisation de HeapTrc

### Activation

**Dans le code :**

```pascal
program TestMemoire;

{$IFDEF DEBUG}
uses
  HeapTrc;  // Active le traçage du tas
{$ENDIF}

var
  p: ^Integer;
begin
  New(p);
  p^ := 42;
  // Oubli volontaire de Dispose
end.
```

**À la compilation :**

```bash
fpc -gh TestMemoire.pas
```

L'option `-gh` active HeapTrc automatiquement.

### Interpréter les Résultats

**Sortie normale (sans fuite) :**

```
Heap dump by heaptrc unit
127 memory blocks allocated : 2044/2136
127 memory blocks freed     : 2044/2136
0 unfreed memory blocks : 0
True heap size : 98304
True free heap : 98304
```

**Avec fuite mémoire :**

```
Heap dump by heaptrc unit
128 memory blocks allocated : 2048/2140
127 memory blocks freed     : 2044/2136
1 unfreed memory blocks : 4
True heap size : 98304
True free heap : 98300
Should be : 98304

Call trace for block $00410020 size 4
  $0040100C  PROGRAMME,  line 12 of test.pas
  $00401050  main,  line 18 of test.pas
```

**Analyse :**
- `1 unfreed memory blocks : 4` → Une fuite de 4 octets (un Integer)
- `line 12 of test.pas` → L'allocation qui a fuité est ligne 12

### Configuration Avancée

**Créer un fichier heap.trc :**

```pascal
program TestMemoire;

uses
  HeapTrc;

begin
  SetHeapTraceOutput('heap.trc');  // Sortie dans un fichier

  // ... votre code ...
end.
```

**Options de HeapTrc :**

```pascal
// Au début du programme
SetHeapExtraInfo(SizeOf(Pointer), True, True);
// Arguments :
// 1. Taille des infos supplémentaires
// 2. Utiliser gestion FPC
// 3. Arrêter sur erreur
```

## Techniques de Débogage Manuelles

### 1. Insertion de WriteLn

**Tracer le flux d'exécution :**

```pascal
procedure TraiterDonnees(var p: PNoeud);
begin
  WriteLn('DEBUG: Début TraiterDonnees, p=', PtrUInt(p));

  if p = nil then
  begin
    WriteLn('DEBUG: p est nil, sortie');
    Exit;
  end;

  WriteLn('DEBUG: Valeur de p^.donnee=', p^.donnee);

  // ... traitement ...

  WriteLn('DEBUG: Fin TraiterDonnees');
end;
```

**Avantages :**
- Simple et universel
- Fonctionne partout
- Pas besoin d'outils spéciaux

**Inconvénients :**
- Pollue la sortie
- Peut modifier le comportement (timing)
- À retirer après débogage

### 2. Assertions

**Vérifier les conditions :**

```pascal
procedure Inserer(var liste: PNoeud; valeur: Integer);
begin
  Assert(valeur > 0, 'La valeur doit être positive');

  // ... insertion ...

  Assert(liste <> nil, 'La liste ne devrait pas être nil');
end;
```

**Configuration :**

```pascal
{$ASSERTIONS ON}  // Activer les assertions
// ou
{$C+}  // Forme courte
```

**En production, désactiver :**

```pascal
{$ASSERTIONS OFF}
{$C-}
```

### 3. Logging Structuré

**Créer un système de logs :**

```pascal
var
  LogFile: TextFile;

procedure InitLog;
begin
  Assign(LogFile, 'debug.log');
  Rewrite(LogFile);
end;

procedure Log(const msg: String);
begin
  WriteLn(LogFile, FormatDateTime('hh:nn:ss', Now), ' - ', msg);
  Flush(LogFile);  // Écriture immédiate
end;

procedure CloseLog;
begin
  Close(LogFile);
end;

// Utilisation
begin
  InitLog;

  Log('Allocation de mémoire');
  New(p);
  Log('Mémoire allouée à ' + IntToHex(PtrUInt(p), 8));

  CloseLog;
end;
```

### 4. Compteurs de Diagnostic

**Suivre les allocations/libérations :**

```pascal
var
  AllocCount: Integer = 0;
  FreeCount: Integer = 0;

procedure DebugNew(var p: Pointer; taille: Integer);
begin
  GetMem(p, taille);
  Inc(AllocCount);
  WriteLn('New #', AllocCount, ' à ', PtrUInt(p));
end;

procedure DebugDispose(var p: Pointer);
begin
  if p <> nil then
  begin
    Inc(FreeCount);
    WriteLn('Dispose #', FreeCount, ' de ', PtrUInt(p));
    FreeMem(p);
    p := nil;
  end;
end;

// À la fin
begin
  WriteLn('Allocations : ', AllocCount);
  WriteLn('Libérations : ', FreeCount);
  if AllocCount <> FreeCount then
    WriteLn('ATTENTION : Fuite détectée !');
end;
```

## Stratégies de Recherche

### Méthode de Dichotomie

**Pour localiser un bug :**

1. **Diviser le code en deux**
2. **Commenter la moitié**
3. **Le bug persiste ?**
   - Oui → Bug dans la partie active
   - Non → Bug dans la partie commentée
4. **Répéter** sur la moitié problématique

```pascal
procedure Complexe;
begin
  PartieA;
  PartieB;
  PartieC;
  PartieD;  // Crash quelque part
end;

// Test 1 : Commenter C et D
// Crash ? → Bug dans A ou B
// Test 2 : Commenter A
// Crash ? → Bug dans B
// Sinon → Bug dans A
```

### Simplification Progressive

**Réduire au minimum reproductible :**

```pascal
// Code original complexe (100 lignes)
procedure Original;
begin
  // ... beaucoup de code ...
  // Crash quelque part
end;

// Version simplifiée 1 (50 lignes)
procedure Simplifie1;
begin
  // Garder seulement ce qui cause le crash
end;

// Version simplifiée 2 (10 lignes)
procedure Simplifie2;
begin
  // Cas minimal qui reproduit le bug
  New(p);
  WriteLn(p^);  // Oups, pas initialisé !
end;
```

### Vérifications Systématiques

**Checklist de débogage :**

```pascal
procedure Verifier(p: Pointer);
begin
  // 1. Le pointeur est-il nil ?
  if p = nil then
  begin
    WriteLn('ERREUR : Pointeur nil');
    Exit;
  end;

  // 2. L'adresse est-elle valide ?
  if (PtrUInt(p) < $1000) or (PtrUInt(p) > $7FFFFFFF) then
  begin
    WriteLn('ERREUR : Adresse suspecte : ', IntToHex(PtrUInt(p), 8));
    Exit;
  end;

  // 3. La valeur pointée est-elle valide ?
  try
    WriteLn('Valeur : ', PInteger(p)^);
  except
    WriteLn('ERREUR : Accès invalide');
  end;
end;
```

## Patterns de Débogage

### Pattern 1 : Encadrement

```pascal
procedure OperationCritique;
begin
  WriteLn('>>> ENTRÉE OperationCritique');

  // Afficher l'état AVANT
  WriteLn('Avant: p=', PtrUInt(p), ' valeur=', p^);

  // Opération dangereuse
  p^ := NouvelleValeur;

  // Afficher l'état APRÈS
  WriteLn('Après: p=', PtrUInt(p), ' valeur=', p^);

  WriteLn('<<< SORTIE OperationCritique');
end;
```

### Pattern 2 : Sentinelles

```pascal
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
```

### Pattern 3 : Mode Verbeux

```pascal
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
```

## Outils Complémentaires

### 1. Valgrind (Linux/Mac)

**Installation :**
```bash
sudo apt install valgrind  # Ubuntu/Debian
```

**Utilisation :**
```bash
# Compiler avec debug
fpc -g programme.pas

# Exécuter avec Valgrind
valgrind --leak-check=full --show-leak-kinds=all ./programme

# Options utiles
valgrind --track-origins=yes  # Tracer l'origine des valeurs
valgrind --log-file=valgrind.log  # Sauvegarder dans un fichier
```

**Sortie typique :**
```
==12345== HEAP SUMMARY:
==12345==     in use at exit: 4 bytes in 1 blocks
==12345==   total heap usage: 2 allocs, 1 frees, 8 bytes allocated
==12345==
==12345== 4 bytes in 1 blocks are definitely lost
==12345==    at 0x4C2FB0F: malloc (in /usr/lib/valgrind/vgpreload_memcheck)
==12345==    by 0x400537: PROGRAMME (programme.pas:12)
==12345==    by 0x4005D0: main (programme.pas:20)
```

### 2. Dr. Memory (Windows)

Alternative à Valgrind pour Windows :

```bash
# Télécharger sur drmemory.org
drmemory.exe -- programme.exe
```

### 3. AddressSanitizer (Moderne)

Si disponible avec votre compilateur :

```bash
fpc -g -O0 programme.pas
# Ajouter les options AddressSanitizer si supportées
```

## Cas Pratiques de Débogage

### Cas 1 : Crash Aléatoire

**Problème :**
```pascal
// Le programme plante parfois, pas toujours
procedure Mystere;
var
  p: ^Integer;
begin
  // ... code complexe ...
  WriteLn(p^);  // Crash aléatoire ici
end;
```

**Démarche :**

1. **Ajouter des vérifications**
   ```pascal
   if p = nil then
     raise Exception.Create('p est nil ligne X');
   ```

2. **Utiliser HeapTrc**
   ```pascal
   {$IFDEF DEBUG}
   uses HeapTrc;
   {$ENDIF}
   ```

3. **Points d'arrêt conditionnels**
   ```
   Breakpoint si p = nil
   ```

4. **Initialiser systématiquement**
   ```pascal
   var
     p: ^Integer;
   begin
     p := nil;  // Toujours !
   ```

### Cas 2 : Fuite Progressive

**Problème :**
```pascal
// Mémoire augmente sans cesse
procedure BouclePrincipale;
var
  i: Integer;
begin
  for i := 1 to 10000 do
  begin
    TraiterDonnees(i);
    // Fuite quelque part...
  end;
end;
```

**Démarche :**

1. **Activer HeapTrc**
   ```pascal
   uses HeapTrc;
   ```

2. **Ajouter des compteurs**
   ```pascal
   var AllocCount, FreeCount: Integer;

   procedure TraiterDonnees(n: Integer);
   begin
     Inc(AllocCount);
     // ...
     Inc(FreeCount);

     if AllocCount mod 100 = 0 then
       WriteLn('Alloc=', AllocCount, ' Free=', FreeCount);
   end;
   ```

3. **Tester avec peu d'itérations**
   ```pascal
   for i := 1 to 10 do  // Au lieu de 10000
   ```

4. **Utiliser Valgrind**
   ```bash
   valgrind --leak-check=full ./programme
   ```

### Cas 3 : Corruption Mystérieuse

**Problème :**
```pascal
// Les données changent inexplicablement
var
  important: Integer;
begin
  important := 42;
  // ... code ...
  WriteLn(important);  // Affiche 999 ?? Comment ?
end;
```

**Démarche :**

1. **Ajouter des sentinelles**
   ```pascal
   const
     MAGIC = $DEADBEEF;
   var
     sentinel1, sentinel2: Cardinal;
     important: Integer;
   begin
     sentinel1 := MAGIC;
     important := 42;
     sentinel2 := MAGIC;

     // ... code ...

     if sentinel1 <> MAGIC then
       WriteLn('CORRUPTION AVANT important !');
     if sentinel2 <> MAGIC then
       WriteLn('CORRUPTION APRÈS important !');
   end;
   ```

2. **Vérifier les écritures hors limites**
   ```pascal
   var
     tableau: array[1..10] of Integer;
   begin
     for i := 1 to 10 do  // Pas jusqu'à 11 !
       tableau[i] := i;
   end;
   ```

3. **Points d'arrêt sur modification**
   ```
   Debugger: Watch sur 'important'
   Break when value changes
   ```

## Bonnes Pratiques de Débogage

### 1. Reproduire le Bug

- **Toujours** essayer de reproduire de manière déterministe
- Noter les conditions exactes (données d'entrée, état)
- Créer un cas de test minimal

### 2. Hypothèses et Tests

- Formuler une hypothèse sur la cause
- Concevoir un test pour la vérifier
- Modifier UNE chose à la fois
- Noter les résultats

### 3. Documentation

```pascal
// Bug trouvé le 2025-01-15
// Cause : Pointeur non initialisé dans TraiterDonnees
// Solution : Ajouter p := nil; au début
// Test : Exécuté 1000 fois sans erreur
```

### 4. Tests de Régression

Après correction, créer un test :

```pascal
procedure TestCorrectionBug123;
var
  p: ^Integer;
begin
  p := nil;  // La correction
  New(p);
  try
    p^ := 42;
    Assert(p^ = 42, 'Test valeur');
  finally
    Dispose(p);
  end;
  WriteLn('Test Bug#123 : OK');
end;
```

## Checklist de Débogage

### Quand un Crash se Produit

- [ ] Quel est le message d'erreur exact ?
- [ ] À quelle ligne le crash se produit-il ?
- [ ] Quelle est la valeur des variables locales ?
- [ ] Quelle est la pile d'appels (call stack) ?
- [ ] Le crash est-il reproductible ?
- [ ] Avec quelles données d'entrée ?

### Pour une Fuite Mémoire

- [ ] HeapTrc activé ?
- [ ] Combien de blocs non libérés ?
- [ ] À quelle ligne l'allocation ?
- [ ] Y a-t-il un Dispose correspondant ?
- [ ] Le Dispose est-il toujours atteint ?
- [ ] Y a-t-il une structure chaînée ?

### Pour une Corruption

- [ ] Quelles données sont corrompues ?
- [ ] Quand la corruption apparaît-elle ?
- [ ] Y a-t-il des écritures hors limites ?
- [ ] Y a-t-il une double libération ?
- [ ] Les sentinelles sont-elles intactes ?

## Résumé des Points Clés

1. **Messages d'erreur** : Apprenez à les interpréter
2. **Débogueur Lazarus** : Maîtrisez F7, F8, points d'arrêt
3. **HeapTrc** : Activez systématiquement en debug
4. **WriteLn** : Simple mais efficace pour tracer
5. **Assertions** : Vérifiez vos hypothèses
6. **Call Stack** : Remontez la chaîne d'appels
7. **Valgrind** : Outil puissant sous Linux
8. **Sentinelles** : Détectez la corruption mémoire
9. **Reproduction** : Isolez le cas minimal
10. **Documentation** : Notez vos découvertes

## Conseils Finaux

### Pour les Débutants

- Commencez par des programmes simples
- Utilisez le débogueur dès le début (F7/F8)
- Activez toujours HeapTrc en développement
- N'hésitez pas à ajouter des WriteLn
- Un bug à la fois !

### Pour Progresser

- Apprenez à lire les call stacks
- Maîtrisez les points d'arrêt conditionnels
- Utilisez Valgrind régulièrement
- Créez vos propres outils de diagnostic
- Documentez les patterns de bugs

### Philosophie

**"Un bon développeur passe 20% de son temps à coder et 80% à déboguer"**

Le débogage n'est pas un échec, c'est une compétence essentielle. Plus vous pratiquez, plus vous devenez rapide à identifier et corriger les problèmes.

**Rappelez-vous :** Chaque bug corrigé est une leçon apprise !

## Conclusion

Le débogage des problèmes mémoire peut sembler intimidant au début, mais avec les bons outils et les bonnes techniques, il devient gérable. La clé est la méthodologie : observer, hypothétiser, tester, corriger, vérifier.

Avec l'expérience, vous développerez une intuition pour repérer rapidement les problèmes courants et vous apprendrez à anticiper les pièges avant qu'ils ne deviennent des bugs.

**Bravo !** Vous avez terminé le chapitre 6 sur les pointeurs et la gestion mémoire. Vous êtes maintenant équipé pour créer, gérer et déboguer des structures de données dynamiques en toute confiance !

⏭️ [Unités et Organisation du Code](/07-unites-organisation-code/README.md)
