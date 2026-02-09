🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 6.8 Fuites Mémoire et Bonnes Pratiques

## Introduction

La gestion manuelle de la mémoire avec les pointeurs offre une grande puissance, mais elle s'accompagne d'une grande responsabilité. Les **fuites mémoire** sont l'un des problèmes les plus courants et insidieux en programmation avec pointeurs. Cette section vous apprendra à les comprendre, les détecter et les éviter.

## Qu'est-ce qu'une Fuite Mémoire ?

### Définition

Une **fuite mémoire** (memory leak) se produit lorsque vous allouez de la mémoire avec `New` ou `GetMem`, mais que vous oubliez de la libérer avec `Dispose` ou `FreeMem`. Cette mémoire devient inaccessible mais reste occupée.

### Analogie de la Bibliothèque

Imaginez une bibliothèque :
- Vous **empruntez** un livre (New)
- Vous devez le **rendre** après lecture (Dispose)
- Si vous **oubliez de le rendre**, le livre reste indisponible pour les autres
- Si beaucoup de gens oublient de rendre leurs livres, la bibliothèque se vide !

C'est exactement ce qui se passe avec la mémoire de votre ordinateur.

### Impact des Fuites Mémoire

```
Démarrage du programme :
Mémoire disponible : ████████████████████ 100%

Après quelques fuites :
Mémoire disponible : ████████████░░░░░░░░ 60%
                                 ↑ perdue !

Après beaucoup de fuites :
Mémoire disponible : ████░░░░░░░░░░░░░░░░ 20%
Programme ralenti, risque de crash !

Fin du programme :
Mémoire disponible : ████████████████████ 100%
(libérée automatiquement par l'OS)
```

**Important :** Les fuites ne font perdre de la mémoire que pendant l'exécution du programme. À la fermeture, le système d'exploitation récupère toute la mémoire.

## Causes Courantes de Fuites

### 1. Oubli Simple de Dispose

**Le cas le plus fréquent**

```pascal
// ✗ FUITE MÉMOIRE
procedure CreerDonnees;
var
  p: ^Integer;
begin
  New(p);
  p^ := 42;
  WriteLn(p^);
  // Oubli : pas de Dispose(p) !
end;  // p est détruit, mais la mémoire allouée reste occupée !

// ✓ CORRECT
procedure CreerDonneesCorrect;
var
  p: ^Integer;
begin
  New(p);
  p^ := 42;
  WriteLn(p^);
  Dispose(p);  // Libération correcte
  p := nil;
end;
```

**Visualisation :**

```
Après New(p) :
┌─────────┬──────────┐
│ Stack   │   Heap   │
├─────────┼──────────┤
│ p=$1000 │ $1000: 42│
└─────────┴──────────┘

Fin de procédure (sans Dispose) :
┌─────────┬──────────┐
│ Stack   │   Heap   │
├─────────┼──────────┤
│ (vide)  │ $1000: 42│ ← FUITE ! Aucun pointeur n'y accède
└─────────┴──────────┘
```

### 2. Perte de Référence

**Écraser un pointeur avant de libérer**

```pascal
// ✗ FUITE MÉMOIRE
var
  p: ^Integer;
begin
  New(p);
  p^ := 10;

  New(p);  // ✗ Écrase l'ancien pointeur sans libérer !
  p^ := 20;

  Dispose(p);  // Libère seulement le deuxième
  p := nil;
end;  // Le premier bloc est perdu !

// ✓ CORRECT
var
  p: ^Integer;
begin
  New(p);
  p^ := 10;

  Dispose(p);  // Libérer avant de réallouer

  New(p);
  p^ := 20;

  Dispose(p);
  p := nil;
end;
```

### 3. Exception Avant Dispose

**Une erreur empêche d'atteindre le Dispose**

```pascal
// ✗ FUITE MÉMOIRE
procedure TraiterFichier;
var
  buffer: ^array[1..1000] of Byte;
begin
  New(buffer);

  // Si cette opération génère une erreur...
  LireFichier(buffer^);

  Dispose(buffer);  // ... on n'atteint jamais cette ligne !
  buffer := nil;
end;

// ✓ CORRECT : Utiliser try-finally
procedure TraiterFichierCorrect;
var
  buffer: ^array[1..1000] of Byte;
begin
  New(buffer);
  try
    LireFichier(buffer^);
  finally
    Dispose(buffer);  // Exécuté même en cas d'erreur
    buffer := nil;
  end;
end;
```

### 4. Structures Chaînées Partiellement Libérées

**Oublier de libérer tous les noeuds**

```pascal
// ✗ FUITE MÉMOIRE
procedure LibererMal(liste: PNoeud);
begin
  if liste <> nil then
    Dispose(liste);  // Libère seulement le premier !
end;  // Le reste de la liste est perdu !

// ✓ CORRECT
procedure LibererBien(var liste: PNoeud);
var
  courant, suivant: PNoeud;
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
```

### 5. Références Circulaires

**Deux structures se pointent mutuellement**

```pascal
program ReferencesCirculaires;
type
  PA = ^TA;
  PB = ^TB;

  TA = record
    data: Integer;
    refB: PB;
  end;

  TB = record
    data: Integer;
    refA: PA;
  end;

var
  a: PA;
  b: PB;
begin
  New(a);
  New(b);

  a^.data := 1;
  b^.data := 2;

  // PROBLEME POTENTIEL : références circulaires
  a^.refB := b;  // A pointe vers B
  b^.refA := a;  // B pointe vers A

  WriteLn('A.data = ', a^.data, ', A.refB^.data = ', a^.refB^.data);
  WriteLn('B.data = ', b^.data, ', B.refA^.data = ', b^.refA^.data);

  // SOLUTION : casser le cycle avant de libérer
  a^.refB := nil;  // Casser la référence circulaire
  Dispose(a);
  Dispose(b);
  WriteLn('Mémoire libérée correctement (cycle cassé)');
end.
```

## Détection des Fuites Mémoire

### 1. Observation des Symptômes

**Signes d'une fuite mémoire :**
- Le programme consomme de plus en plus de mémoire au fil du temps
- Ralentissement progressif
- Crash après une longue exécution
- Message "Out of Memory" sur des opérations répétées

### 2. Comptage Manuel

Compter les allocations et libérations :

```pascal
var
  compteurAlloc: Integer = 0;
  compteurFree: Integer = 0;

procedure MonNew(var p: Pointer; taille: Integer);
begin
  GetMem(p, taille);
  Inc(compteurAlloc);
  WriteLn('Allocation #', compteurAlloc, ' - ', taille, ' octets');
end;

procedure MonDispose(var p: Pointer);
begin
  if p <> nil then
  begin
    FreeMem(p);
    Inc(compteurFree);
    WriteLn('Libération #', compteurFree);
    p := nil;
  end;
end;

// À la fin du programme
begin
  WriteLn('Allocations : ', compteurAlloc);
  WriteLn('Libérations : ', compteurFree);
  if compteurAlloc <> compteurFree then
    WriteLn('ATTENTION : Fuite mémoire détectée !');
end.
```

### 3. Utiliser HeapTrc (FreePascal)

FreePascal inclut un outil de détection de fuites intégré :

```pascal
program TestFuite;

var
  p: ^Integer;
begin
  New(p);
  p^ := 42;
  WriteLn('Valeur : ', p^);
  // Oubli volontaire de Dispose(p)
  // Compiler avec : fpc -gh 08-fuite-detection.pas
  // pour voir le rapport de fuites mémoire
end.
```

**À la compilation :**
```
fpc -gh TestFuite.pas
```

**Résultat à l'exécution :**
```
Heap dump by heaptrc unit
127 memory blocks allocated : 2044/2136
127 memory blocks freed     : 2040/2132
1 unfreed memory blocks : 4  ← FUITE DÉTECTÉE !
```

### 4. Gestionnaire de Mémoire Personnalisé

Créer un wrapper pour tracer toutes les allocations :

```pascal
type
  PDebugInfo = ^TDebugInfo;
  TDebugInfo = record
    adresse: Pointer;
    taille: Integer;
    nomFichier: String;
    ligne: Integer;
    suivant: PDebugInfo;
  end;

var
  listeAllocations: PDebugInfo = nil;

procedure EnregistrerAllocation(p: Pointer; t: Integer; f: String; l: Integer);
var
  nouveau: PDebugInfo;
begin
  New(nouveau);
  nouveau^.adresse := p;
  nouveau^.taille := t;
  nouveau^.nomFichier := f;
  nouveau^.ligne := l;
  nouveau^.suivant := listeAllocations;
  listeAllocations := nouveau;
end;

procedure VerifierFuites;
var
  courant: PDebugInfo;
begin
  courant := listeAllocations;
  while courant <> nil do
  begin
    WriteLn('FUITE : ', courant^.taille, ' octets alloués dans ',
            courant^.nomFichier, ':', courant^.ligne);
    courant := courant^.suivant;
  end;
end;
```

## Prévention des Fuites : Bonnes Pratiques

### 1. Principe de Responsabilité Unique

**Qui alloue doit libérer**

```pascal
// ✓ BON PATTERN : Création et destruction au même niveau
procedure TraiterDonnees;
var
  donnees: ^TGrosseStructure;
begin
  New(donnees);  // Allocation
  try
    // ... traitement ...
  finally
    Dispose(donnees);  // Libération au même niveau
    donnees := nil;
  end;
end;
```

### 2. Toujours Utiliser Try-Finally

**Garantir la libération même en cas d'erreur**

```pascal
// ✓ PATTERN FONDAMENTAL
var
  ressource: ^TRessource;
begin
  New(ressource);
  try
    // Code qui peut générer des erreurs
    ressource^.Initialiser;
    ressource^.Traiter;
  finally
    // TOUJOURS exécuté
    Dispose(ressource);
    ressource := nil;
  end;
end;
```

### 3. Initialiser à nil

**Permettre la détection de pointeurs invalides**

```pascal
// ✓ BON
var
  p: ^Integer;
begin
  p := nil;  // Initialisation explicite

  // Plus tard...
  if p <> nil then
    WriteLn(p^)  // Sécurisé
  else
    WriteLn('Pointeur non alloué');
end;
```

### 4. Mettre à nil Après Dispose

**Éviter les pointeurs pendants**

```pascal
// ✓ BON
var
  p: ^Integer;
begin
  New(p);
  p^ := 42;

  Dispose(p);
  p := nil;  // Important !

  // Protection contre double libération
  if p <> nil then
    Dispose(p);  // Ne s'exécute pas
end;
```

### 5. Encapsulation dans des Fonctions

**Créer des fonctions de création/destruction**

```pascal
{$mode objfpc}{$H+}
program EncapsulationMemoire;
type
  PPerson = ^TPerson;
  TPerson = record
    nom: String;
    age: Integer;
  end;

function CreerPersonne(n: String; a: Integer): PPerson;
begin
  New(Result);
  Result^.nom := n;
  Result^.age := a;
end;

procedure DetruirePersonne(var p: PPerson);
begin
  if p <> nil then
  begin
    Dispose(p);
    p := nil;
  end;
end;

// Utilisation claire
var
  pers: PPerson;
begin
  pers := CreerPersonne('Alice', 30);
  try
    WriteLn('Nom : ', pers^.nom);
    WriteLn('Age : ', pers^.age);
  finally
    DetruirePersonne(pers);
  end;

  if pers = nil then
    WriteLn('Pointeur remis à nil après destruction');
end.
```

### 6. Documentation de l'Ownership

**Clarifier qui est responsable de la libération**

```pascal
// Cette fonction CRÉE un objet : l'appelant doit le libérer
function CreerConfiguration: PConfig;
begin
  New(Result);
  // ...
end;

// Cette fonction UTILISE un objet : elle ne le libère PAS
procedure AfficherConfiguration(cfg: PConfig);
begin
  // Ne pas faire Dispose ici !
  WriteLn(cfg^.serveur);
end;

// Cette fonction PREND OWNERSHIP : elle libère l'objet
procedure AppliquerEtLiberer(var cfg: PConfig);
begin
  // ... utilisation ...
  Dispose(cfg);
  cfg := nil;
end;
```

### 7. Pattern RAII Simulé

**Resource Acquisition Is Initialization**

```pascal
type
  TGestionnaireRessource = record
    ressource: ^TRessource;
  end;

procedure Initialiser(var g: TGestionnaireRessource);
begin
  New(g.ressource);
end;

procedure Finaliser(var g: TGestionnaireRessource);
begin
  if g.ressource <> nil then
  begin
    Dispose(g.ressource);
    g.ressource := nil;
  end;
end;

// Utilisation
var
  gestionnaire: TGestionnaireRessource;
begin
  Initialiser(gestionnaire);
  try
    // ... utilisation de gestionnaire.ressource ...
  finally
    Finaliser(gestionnaire);
  end;
end;
```

## Structures de Données : Cas Particuliers

### Listes Chaînées

**Libération complète obligatoire**

```pascal
procedure LibererListe(var liste: PNoeud);
var
  courant, suivant: PNoeud;
begin
  courant := liste;
  while courant <> nil do
  begin
    suivant := courant^.suivant;  // Sauvegarder avant
    Dispose(courant);              // Libérer
    courant := suivant;            // Avancer
  end;
  liste := nil;
end;
```

**Checklist pour les listes :**
- ✅ Sauvegarder le suivant avant Dispose
- ✅ Libérer tous les noeuds, pas seulement le premier
- ✅ Mettre la tête de liste à nil à la fin
- ✅ Utiliser try-finally si la liste est dans une procédure

### Arbres Binaires

**Libération récursive en postfixe**

```pascal
procedure LibererArbre(var racine: PNoeud);
begin
  if racine = nil then
    Exit;

  // Ordre CRUCIAL : enfants avant parent
  LibererArbre(racine^.gauche);   // 1. Gauche
  LibererArbre(racine^.droite);   // 2. Droite
  Dispose(racine);                 // 3. Parent
  racine := nil;
end;
```

**Pourquoi cet ordre ?**
```
Si on libère le parent d'abord :
    racine -> [50|●|●]
               /    \
           [30]    [70]

Après Dispose(racine) :
    racine -> LIBÉRÉ
               /    \
           [30]    [70]  ← PERDUS ! Plus d'accès !
```

### Tableaux Dynamiques dans Enregistrements

**Libérer les tableaux dynamiques avant l'enregistrement**

```pascal
type
  PData = ^TData;
  TData = record
    tableau: array of Integer;
    nom: String;
  end;

procedure LibererData(var p: PData);
begin
  if p <> nil then
  begin
    SetLength(p^.tableau, 0);  // Libérer le tableau d'abord
    Dispose(p);
    p := nil;
  end;
end;
```

## Outils et Techniques Avancées

### 1. Valgrind (Linux)

Outil puissant pour détecter les fuites :

```bash
# Compiler avec informations de debug
fpc -g programme.pas

# Exécuter avec Valgrind
valgrind --leak-check=full ./programme
```

### 2. Compteur de Références

Pour partager des ressources :

```pascal
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
```

### 3. Pool de Mémoire

Allouer un gros bloc et le subdiviser :

```pascal
type
  TMemoryPool = record
    buffer: Pointer;
    taille: Integer;
    utilise: Integer;
  end;

procedure CreerPool(var pool: TMemoryPool; t: Integer);
begin
  GetMem(pool.buffer, t);
  pool.taille := t;
  pool.utilise := 0;
end;

function AllouerDansPool(var pool: TMemoryPool; t: Integer): Pointer;
begin
  if pool.utilise + t <= pool.taille then
  begin
    Result := pool.buffer + pool.utilise;
    Inc(pool.utilise, t);
  end
  else
    Result := nil;  // Pool plein
end;

procedure LibererPool(var pool: TMemoryPool);
begin
  FreeMem(pool.buffer);
  pool.buffer := nil;
  pool.taille := 0;
  pool.utilise := 0;
end;
```

## Checklist de Sécurité Mémoire

### Avant d'Écrire du Code

- [ ] Ai-je vraiment besoin d'allocation dynamique ?
- [ ] Puis-je utiliser des variables locales à la place ?
- [ ] Puis-je utiliser `array of` au lieu de pointeurs ?

### Pendant l'Écriture

- [ ] Chaque `New` a-t-il son `Dispose` correspondant ?
- [ ] Les pointeurs sont-ils initialisés à `nil` ?
- [ ] Ai-je utilisé `try-finally` pour les ressources ?
- [ ] Les structures chaînées sont-elles entièrement libérées ?
- [ ] Ai-je mis les pointeurs à `nil` après `Dispose` ?

### Lors des Tests

- [ ] Le programme fonctionne-t-il correctement en boucle ?
- [ ] La mémoire augmente-t-elle indéfiniment ?
- [ ] HeapTrc détecte-t-il des fuites ?
- [ ] Tous les chemins de code libèrent-ils correctement ?

### Révision de Code

- [ ] Rechercher tous les `New` sans `Dispose`
- [ ] Vérifier tous les `Exit` prématurés
- [ ] Contrôler les cas d'erreur
- [ ] Examiner les boucles (fuites à chaque itération ?)

## Erreurs Classiques Récapitulées

### Top 10 des Erreurs

1. **Oublier Dispose** après New
2. **Écraser un pointeur** sans libérer l'ancienne valeur
3. **Exception avant Dispose** (pas de try-finally)
4. **Libération partielle** des structures chaînées
5. **Double libération** du même pointeur
6. **Utilisation après libération** (pointeur pendant)
7. **Fuite dans les boucles** (allocation répétée)
8. **Références circulaires** non gérées
9. **Oubli de libérer** les sous-structures
10. **Pas de vérification nil** avant Dispose

### Tableau de Diagnostic

| Symptôme | Cause Probable | Solution |
|----------|----------------|----------|
| Mémoire augmente sans cesse | Fuite dans une boucle | Ajouter Dispose dans la boucle |
| Crash aléatoire | Double libération | Mettre à nil après Dispose |
| "Access Violation" | Utilisation après libération | Vérifier ordre de libération |
| "Out of Memory" rapide | Grosse fuite | Activer HeapTrc, chercher New sans Dispose |
| Ralentissement progressif | Petites fuites accumulées | Profiler, compter allocations |

## Conseils Finaux

### Règle d'Or

**"Pour chaque New, un Dispose, toujours, sans exception !"**

### Philosophie Défensive

1. **Supposer que tout peut échouer** : Utiliser try-finally partout
2. **Vérifier nil systématiquement** avant tout accès
3. **Documenter l'ownership** de chaque ressource
4. **Tester les cas limites** : liste vide, arbre vide, erreurs
5. **Utiliser les outils** : HeapTrc, Valgrind, compteurs

### Progression Recommandée

**Pour les débutants :**
1. Commencer avec des variables locales simples
2. Passer aux tableaux dynamiques (`array of`)
3. Utiliser des pointeurs simples avec try-finally
4. Implémenter des listes chaînées courtes

**Pour progresser :**
1. Maîtriser les arbres et structures complexes
2. Implémenter des compteurs de références
3. Créer des gestionnaires de ressources
4. Utiliser les outils de détection avancés

### Derniers Mots

La gestion manuelle de la mémoire peut sembler intimidante, mais avec de la rigueur et les bonnes pratiques, elle devient naturelle. **L'important est la discipline** : suivez systématiquement les patterns de sécurité, utilisez les outils de détection, et ne laissez jamais une fuite "pour plus tard".

Rappelez-vous : **un bug de mémoire aujourd'hui peut devenir un crash en production demain** !

## Résumé des Points Clés

1. Une **fuite mémoire** = mémoire allouée mais jamais libérée
2. **Try-finally** est votre meilleur ami
3. **Initialiser à nil** et **remettre à nil** après Dispose
4. **Structures chaînées** : libérer TOUS les noeuds
5. **Arbres** : libération en postfixe obligatoire
6. **HeapTrc** pour détecter les fuites en développement
7. **Discipline** : chaque New a son Dispose
8. **Documentation** : qui crée doit détruire
9. **Tests** : vérifier avec HeapTrc systématiquement
10. **Pattern RAII** : try-finally pour toutes les ressources

## Pour Aller Plus Loin

- Étudier les langages avec ramasse-miettes (Java, C#, Python)
- Explorer les smart pointers (C++ moderne)
- Comprendre le comptage de références (Objective-C, Swift)
- Approfondir Valgrind et les outils de profiling
- Étudier les patterns de gestion de ressources (RAII, RRID)

**Bravo !** Vous avez terminé le chapitre sur les pointeurs et la gestion mémoire. Avec ces connaissances, vous êtes armé pour créer des programmes robustes et sans fuites !

⏭️ [Débogage des problèmes mémoire](/06-pointeurs-gestion-memoire-basique/09-debogage-problemes-memoire.md)

