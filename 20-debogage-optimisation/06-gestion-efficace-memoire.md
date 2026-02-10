🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 20.6 Gestion Efficace de la Mémoire

## Introduction

La gestion de la mémoire est un aspect crucial de la programmation. Une mauvaise gestion peut entraîner des fuites mémoire (memory leaks), des plantages, ou une dégradation progressive des performances. FreePascal/Lazarus utilise une gestion manuelle de la mémoire, ce qui vous donne un contrôle total mais aussi la responsabilité de bien gérer cette ressource.

**Analogie simple :** La mémoire est comme un parking :
- **Réserver une place** = Allouer de la mémoire
- **Libérer une place** = Désallouer la mémoire
- **Fuite mémoire** = Oublier de libérer, le parking se remplit et finit par saturer

**Dans cette section, vous apprendrez à :**
- Comprendre les différents types de mémoire
- Allouer et libérer correctement la mémoire
- Détecter et corriger les fuites mémoire
- Optimiser l'utilisation de la mémoire
- Utiliser les outils de détection de problèmes mémoire

---

## 1. Comprendre la Mémoire

### 1.1 Stack vs Heap

Il existe deux zones principales de mémoire :

**Stack (Pile) - Automatique**

```pascal
procedure Exemple;  
var  
  x: Integer;        // Allouée sur la stack
  s: String;         // Référence sur la stack, données sur le heap
  tab: array[1..10] of Integer;  // Stack si petit
begin
  x := 42;
  // À la fin de la procédure, x est automatiquement libérée
end;
```

**Caractéristiques de la Stack :**
- ✅ Allocation/libération automatique
- ✅ Très rapide
- ✅ Pas de fragmentation
- ⚠️ Taille limitée (généralement 1-8 MB)
- ⚠️ Données perdues en sortant de la fonction

**Heap (Tas) - Manuelle**

```pascal
var
  pointeur: ^Integer;
  objet: TMonObjet;
begin
  New(pointeur);           // Allocation sur le heap
  pointeur^ := 42;
  Dispose(pointeur);       // OBLIGATOIRE : libération manuelle

  objet := TMonObjet.Create;  // Heap
  objet.Traiter;
  objet.Free;              // OBLIGATOIRE : libération manuelle
end;
```

**Caractéristiques du Heap :**
- ✅ Taille très grande (limitée par la RAM)
- ✅ Données persistent entre les appels de fonctions
- ✅ Taille dynamique (peut grandir)
- ⚠️ Gestion manuelle nécessaire
- ⚠️ Plus lent que la stack
- ⚠️ Peut se fragmenter

### 1.2 Schéma Mental

```
MÉMOIRE DU PROGRAMME
════════════════════════════════════════
│                                      │
│  STACK (Pile)                        │
│  ┌─────────────────┐                 │
│  │ Variables locales│  ← Rapide      │
│  │ Paramètres      │  ← Automatique  │
│  │ Adresses retour │                 │
│  └─────────────────┘                 │
│                                      │
│  ─────────────────────               │
│                                      │
│  HEAP (Tas)                          │
│  ┌─────────────────┐                 │
│  │ Objets (Create) │  ← Manuel       │
│  │ New/GetMem      │  ← Flexible     │
│  │ Chaînes longues │  ← Grande taille│
│  │ Collections     │                 │
│  └─────────────────┘                 │
│                                      │
════════════════════════════════════════
```

---

## 2. Allocation et Libération Basique

### 2.1 Variables Simples (Stack)

**Gestion automatique - Rien à faire !**

```pascal
procedure TraiterDonnees;  
var  
  compteur: Integer;
  total: Double;
  nom: String;
  tableau: array[1..100] of Integer;
begin
  compteur := 0;
  total := 0.0;
  nom := 'Test';
  // ... traitement ...

  // Pas besoin de libérer : tout est automatique !
end;
```

### 2.2 Pointeurs avec New/Dispose

**Allocation manuelle sur le heap :**

```pascal
type
  PEntier = ^Integer;

var
  p: PEntier;
begin
  // Allocation
  New(p);

  // Utilisation
  p^ := 42;
  WriteLn('Valeur : ', p^);

  // Libération OBLIGATOIRE
  Dispose(p);

  // Bonne pratique : mettre à nil après libération
  p := nil;
end;
```

**❌ Erreur courante : Oublier Dispose**

```pascal
procedure FuiteMemoireSimple;  
var  
  p: ^Integer;
begin
  New(p);
  p^ := 42;
  // ERREUR : Pas de Dispose !
  // → Fuite mémoire de 4 octets
end;

// Appelé 1 million de fois → Fuite de 4 MB !
for i := 1 to 1000000 do
  FuiteMemoireSimple;
```

### 2.3 GetMem et FreeMem (Allocation Brute)

**Pour les allocations de taille variable :**

```pascal
var
  buffer: Pointer;
  taille: Integer;
begin
  taille := 1024;  // 1 KB

  // Allocation
  GetMem(buffer, taille);

  try
    // Utilisation
    // ... traiter buffer ...
  finally
    // Libération OBLIGATOIRE
    FreeMem(buffer);
  end;
end;
```

**Différence New vs GetMem :**

| Aspect | New/Dispose | GetMem/FreeMem |
|--------|-------------|----------------|
| Type | Typé (connaît la structure) | Non typé (octets bruts) |
| Taille | Automatique | Manuelle |
| Usage | Enregistrements, types définis | Buffers, données brutes |

---

## 3. Objets et Classes

### 3.1 Create et Free

**La règle d'or :**
> **Qui crée, libère !**

```pascal
procedure Exemple;  
var  
  liste: TStringList;
begin
  // Création (allocation heap)
  liste := TStringList.Create;

  try
    // Utilisation
    liste.Add('Element 1');
    liste.Add('Element 2');
    Traiter(liste);
  finally
    // Libération OBLIGATOIRE
    liste.Free;
  end;
end;
```

### 3.2 Try-Finally : Le Pattern Essentiel

**❌ Sans try-finally (DANGEREUX) :**

```pascal
procedure MauvaiseGestion;  
var  
  liste: TStringList;
begin
  liste := TStringList.Create;

  Traiter(liste);  // Et si cette fonction lève une exception ?

  liste.Free;  // Cette ligne ne sera jamais exécutée !
  // → Fuite mémoire garantie en cas d'erreur
end;
```

**✅ Avec try-finally (CORRECT) :**

```pascal
procedure BonneGestion;  
var  
  liste: TStringList;
begin
  liste := TStringList.Create;
  try
    Traiter(liste);  // Même si exception, finally sera exécuté
  finally
    liste.Free;  // TOUJOURS exécuté
  end;
end;
```

**Règle absolue :** TOUJOURS utiliser try-finally avec les objets.

### 3.3 FreeAndNil : Sécurité Supplémentaire

```pascal
procedure GestionSecurisee;  
var  
  objet: TMonObjet;
begin
  objet := TMonObjet.Create;
  try
    Traiter(objet);
  finally
    FreeAndNil(objet);  // Libère ET met à nil
  end;

  // objet vaut maintenant nil
  // Si on tente objet.Free à nouveau → pas de plantage
end;
```

**FreeAndNil fait deux choses :**
1. Appelle `Free` sur l'objet
2. Met la variable à `nil`

**Avantage :** Évite les "double free" (libération double).

### 3.4 Free vs Destroy

```pascal
// Free (RECOMMANDÉ)
objet.Free;  // Vérifie si objet <> nil avant de libérer

// Destroy (À ÉVITER en usage normal)
objet.Destroy;  // Ne vérifie pas si nil → plantage si nil !
```

**Règle :** Utilisez toujours `Free`, sauf dans des destructeurs personnalisés.

---

## 4. Fuites Mémoire : Comprendre et Détecter

### 4.1 Qu'est-ce qu'une Fuite Mémoire ?

**Définition :** Mémoire allouée qui n'est jamais libérée.

**Analogie :** Vous louez des voitures mais ne les rendez jamais. Au bout d'un moment, plus de voitures disponibles !

**Symptômes :**
- 📈 Utilisation mémoire qui augmente continuellement
- 🐌 Programme qui ralentit progressivement
- 💥 Plantage avec "Out of Memory"
- ⚠️ Peut prendre des heures/jours avant d'être visible

### 4.2 Exemples de Fuites Courantes

**Exemple 1 : Oubli de Free**

```pascal
procedure CreerListe;  
var  
  liste: TStringList;
begin
  liste := TStringList.Create;
  liste.Add('Element');
  // ERREUR : Pas de Free !
end;

// Appelé 1000 fois
for i := 1 to 1000 do
  CreerListe;  // Fuite de 1000 TStringList !
```

**Exemple 2 : Sortie Précoce**

```pascal
procedure TraiterFichier(const nom: String);  
var  
  liste: TStringList;
begin
  liste := TStringList.Create;

  if not FileExists(nom) then
    Exit;  // ERREUR : Sort sans libérer !

  liste.LoadFromFile(nom);
  Traiter(liste);
  liste.Free;
end;
```

**Correction :**

```pascal
procedure TraiterFichierCorrect(const nom: String);  
var  
  liste: TStringList;
begin
  if not FileExists(nom) then
    Exit;  // Sort AVANT création

  liste := TStringList.Create;
  try
    liste.LoadFromFile(nom);
    Traiter(liste);
  finally
    liste.Free;
  end;
end;
```

**Exemple 3 : Exception Non Gérée**

```pascal
procedure TraiterDonnees;  
var  
  connexion: TConnexion;
begin
  connexion := TConnexion.Create;

  connexion.Connecter;  // Peut lever une exception !
  connexion.Traiter;

  connexion.Free;  // Jamais exécuté si exception
end;
```

**Correction :**

```pascal
procedure TraiterDonneesCorrect;  
var  
  connexion: TConnexion;
begin
  connexion := TConnexion.Create;
  try
    connexion.Connecter;
    connexion.Traiter;
  finally
    connexion.Free;  // Toujours exécuté
  end;
end;
```

### 4.3 Fuites dans les Collections

**❌ Erreur : Objets dans une liste**

```pascal
var
  liste: TObjectList;  // Non propriétaire par défaut dans certaines versions
  i: Integer;
begin
  liste := TObjectList.Create(False);  // False = non propriétaire

  for i := 1 to 100 do
    liste.Add(TMonObjet.Create);

  liste.Free;  // Libère la liste, mais pas les objets !
  // → Fuite de 100 TMonObjet
end;
```

**✅ Solution 1 : Liste propriétaire**

```pascal
var
  liste: TObjectList;
begin
  liste := TObjectList.Create(True);  // True = propriétaire

  for i := 1 to 100 do
    liste.Add(TMonObjet.Create);

  liste.Free;  // Libère la liste ET tous les objets ✓
end;
```

**✅ Solution 2 : Libération manuelle**

```pascal
var
  liste: TObjectList;
  i: Integer;
begin
  liste := TObjectList.Create(False);
  try
    for i := 1 to 100 do
      liste.Add(TMonObjet.Create);

    // Traitement
  finally
    // Libérer manuellement chaque objet
    for i := 0 to liste.Count - 1 do
      TObject(liste[i]).Free;
    liste.Free;
  end;
end;
```

---

## 5. Détection des Fuites Mémoire

### 5.1 HeapTrc (Outil Intégré FreePascal)

**HeapTrc** est un outil de détection de fuites intégré à FreePascal.

**Activation :**

**Méthode 1 : Dans le code**

```pascal
program MonProgramme;

{$IFDEF DEBUG}
  {$DEFINE HEAPTRC}
{$ENDIF}

uses
  {$IFDEF HEAPTRC}
  heaptrc,
  {$ENDIF}
  SysUtils;

begin
  {$IFDEF HEAPTRC}
  SetHeapTraceOutput('heaptrc.log');  // Fichier de sortie
  {$ENDIF}

  // Votre code
end.
```

**Méthode 2 : Options de compilation**

Dans Lazarus :
1. **Projet** → **Options du projet**
2. **Débogage** → Cochez **Utiliser HeapTrc**
3. Ou ajoutez `-gh` aux options du compilateur

**Exemple de Programme avec Fuite :**

```pascal
program TestFuite;

{$DEFINE HEAPTRC}

uses
  {$IFDEF HEAPTRC}
  heaptrc,
  {$ENDIF}
  Classes;

procedure CreerFuite;  
var  
  liste: TStringList;
begin
  liste := TStringList.Create;
  liste.Add('Element');
  // Pas de Free !
end;

begin
  {$IFDEF HEAPTRC}
  SetHeapTraceOutput('heaptrc.log');
  {$ENDIF}

  CreerFuite;

  WriteLn('Programme terminé');
end.
```

**Sortie dans heaptrc.log :**

```
Heap dump by heaptrc unit
48 memory blocks allocated : 1892/2032
48 memory blocks freed     : 1844/1984
0 unfreed memory blocks : 48
True heap size : 98304  
True free heap : 98176  

Call trace for block $00007F8A40001234 size 48
  $00402345  CREERFUITE,  line 12 of testfuite.pas
  $00402678  main,  line 21 of testfuite.pas
```

**Interprétation :**
- **48 octets non libérés** (la TStringList)
- **Ligne 12** : Où l'allocation a eu lieu
- **Call trace** : Chemin d'appel

### 5.2 Valgrind (Linux)

**Valgrind** est un outil puissant pour Linux.

**Installation Ubuntu/Debian :**

```bash
sudo apt update  
sudo apt install valgrind  
```

**Utilisation :**

```bash
# Compiler avec infos de débogage
fpc -g monprogramme.pas

# Exécuter avec Valgrind
valgrind --leak-check=full --show-leak-kinds=all ./monprogramme
```

**Exemple de sortie :**

```
==12345== HEAP SUMMARY:
==12345==     in use at exit: 48 bytes in 1 blocks
==12345==   total heap usage: 52 allocs, 51 frees, 2,032 bytes allocated
==12345==
==12345== 48 bytes in 1 blocks are definitely lost in loss record 1 of 1
==12345==    at 0x4C2FB0F: malloc (in /usr/lib/valgrind/...)
==12345==    by 0x401234: CREERFUITE (testfuite.pas:12)
==12345==    by 0x401567: main (testfuite.pas:21)
==12345==
==12345== LEAK SUMMARY:
==12345==    definitely lost: 48 bytes in 1 blocks
```

**Avantages de Valgrind :**
- Très détaillé
- Détecte aussi les accès mémoire invalides
- Analyse de performance disponible

**Inconvénient :** Ralentit beaucoup l'exécution (10-50x).

### 5.3 Débogueur Lazarus

**Utiliser les espions pour surveiller les allocations :**

```pascal
var
  compteurObjets: Integer = 0;

constructor TMonObjet.Create;  
begin  
  inherited Create;
  Inc(compteurObjets);
  WriteLn('Objets créés : ', compteurObjets);
end;

destructor TMonObjet.Destroy;  
begin  
  Dec(compteurObjets);
  WriteLn('Objets actifs : ', compteurObjets);
  inherited Destroy;
end;
```

**Point d'arrêt à la fin :**
- Si `compteurObjets > 0` → Fuite !

---

## 6. Bonnes Pratiques de Gestion Mémoire

### 6.1 Règle du Try-Finally

**Template standard :**

```pascal
procedure Template;  
var  
  objet: TMonObjet;
begin
  objet := TMonObjet.Create;
  try
    // Utilisation de objet
  finally
    objet.Free;
  end;
end;
```

**Pour plusieurs objets :**

```pascal
procedure TemplateMultiple;  
var  
  objet1: TObjet1;
  objet2: TObjet2;
begin
  objet1 := TObjet1.Create;
  try
    objet2 := TObjet2.Create;
    try
      // Utilisation
    finally
      objet2.Free;
    end;
  finally
    objet1.Free;
  end;
end;
```

**Ou avec FreeAndNil :**

```pascal
procedure TemplateMultipleSimplifiee;  
var  
  objet1: TObjet1;
  objet2: TObjet2;
begin
  objet1 := nil;
  objet2 := nil;
  try
    objet1 := TObjet1.Create;
    objet2 := TObjet2.Create;
    // Utilisation
  finally
    FreeAndNil(objet2);
    FreeAndNil(objet1);
  end;
end;
```

### 6.2 Initialiser à nil

**Bonne pratique pour les champs de classe :**

```pascal
type
  TGestionnaire = class
  private
    FConnexion: TConnexion;
    FListe: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TGestionnaire.Create;  
begin  
  inherited Create;
  FConnexion := nil;  // Initialisation explicite
  FListe := nil;
end;

destructor TGestionnaire.Destroy;  
begin  
  FreeAndNil(FConnexion);  // Safe même si nil
  FreeAndNil(FListe);
  inherited Destroy;
end;
```

### 6.3 Ownership (Propriété)

**Qui possède l'objet ?**

**Exemple : Formulaire et composants**

```pascal
procedure TFormPrincipal.FormCreate(Sender: TObject);  
var  
  bouton: TButton;
begin
  bouton := TButton.Create(Self);  // Self = propriétaire
  bouton.Parent := Self;
  bouton.Caption := 'Cliquez';

  // Pas besoin de Free : le formulaire libérera automatiquement
end;
```

**Règle :** Si vous passez un propriétaire au `Create`, il gère la libération.

### 6.4 Références et Propriété

**❌ Dangereux : Stocker une référence sans propriété**

```pascal
type
  TGestionnaire = class
  private
    FListe: TStringList;  // Référence, pas propriétaire
  public
    procedure SetListe(AListe: TStringList);
    destructor Destroy; override;
  end;

procedure TGestionnaire.SetListe(AListe: TStringList);  
begin  
  FListe := AListe;  // Simple référence
end;

destructor TGestionnaire.Destroy;  
begin  
  // Doit-on libérer FListe ?
  // Problème : On ne sait pas si on est propriétaire !
  inherited Destroy;
end;
```

**✅ Solution : Clarifier la propriété**

```pascal
type
  TGestionnaire = class
  private
    FListe: TStringList;
    FProprietaire: Boolean;  // Indicateur de propriété
  public
    procedure SetListe(AListe: TStringList; AProprietaire: Boolean);
    destructor Destroy; override;
  end;

procedure TGestionnaire.SetListe(AListe: TStringList; AProprietaire: Boolean);  
begin  
  if FProprietaire and Assigned(FListe) then
    FListe.Free;  // Libérer l'ancienne si propriétaire

  FListe := AListe;
  FProprietaire := AProprietaire;
end;

destructor TGestionnaire.Destroy;  
begin  
  if FProprietaire then
    FreeAndNil(FListe);
  inherited Destroy;
end;
```

---

## 7. Optimisation de l'Utilisation Mémoire

### 7.1 Réutilisation d'Objets

**❌ Inefficace : Créer/Détruire répétitivement**

```pascal
for i := 1 to 100000 do  
begin  
  liste := TStringList.Create;
  liste.Add('Element');
  Traiter(liste);
  liste.Free;
end;
```

**✅ Optimisé : Réutiliser**

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

**Gain :**
- Évite 100 000 allocations/désallocations
- Plus rapide
- Moins de fragmentation mémoire

### 7.2 Pré-allocation de Capacité

**TStringList et TList :**

```pascal
liste := TStringList.Create;  
try  
  liste.Capacity := 10000;  // Pré-alloue pour 10 000 éléments

  for i := 1 to 10000 do
    liste.Add('Element ' + IntToStr(i));
    // Pas de réallocation pendant la boucle
finally
  liste.Free;
end;
```

**Gain typique :** 2-3x plus rapide pour de grandes listes.

### 7.3 Libération de Mémoire Inutilisée

**Après traitement de grandes données :**

```pascal
procedure TraiterGrandesDonnees;  
var  
  liste: TStringList;
begin
  liste := TStringList.Create;
  try
    // Charger 1 million de lignes
    liste.LoadFromFile('gros_fichier.txt');

    // Traiter
    Traiter(liste);

    // Nettoyer
    liste.Clear;
    liste.Capacity := 0;  // Libère la mémoire allouée

    // Continuer avec peu de données
    liste.Add('Résumé');
  finally
    liste.Free;
  end;
end;
```

### 7.4 Chaînes de Caractères et Mémoire

**Les chaînes Pascal sont gérées automatiquement, mais...**

**Copy-on-Write (COW) :**

```pascal
var
  s1, s2: String;
begin
  s1 := 'Hello World';
  s2 := s1;  // Pas de copie ! s2 pointe vers les mêmes données

  WriteLn(s2);  // Pas de modification, pas de copie

  s2 := s2 + '!';  // Maintenant copie (COW)
end;
```

**Optimisation : UniqueString**

```pascal
procedure ModifierChaine(var s: String);  
begin  
  UniqueString(s);  // Force une copie si partagée
  // Maintenant on peut modifier sans affecter l'original
  s[1] := 'X';
end;
```

---

## 8. Pointeurs et Sécurité

### 8.1 Pointeurs Pendants (Dangling Pointers)

**❌ Dangereux :**

```pascal
var
  p1, p2: ^Integer;
begin
  New(p1);
  p1^ := 42;

  p2 := p1;  // p2 pointe vers la même mémoire

  Dispose(p1);  // Libère la mémoire
  p1 := nil;

  WriteLn(p2^);  // DANGER ! p2 pointe vers mémoire libérée
  // → Comportement indéfini, plantage possible
end;
```

**✅ Solution :**

```pascal
var
  p1, p2: ^Integer;
begin
  New(p1);
  p1^ := 42;

  p2 := p1;

  Dispose(p1);
  p1 := nil;
  p2 := nil;  // Mettre TOUS les pointeurs à nil
end;
```

### 8.2 Vérifier les Pointeurs

**Toujours vérifier avant utilisation :**

```pascal
procedure Traiter(p: PMonType);  
begin  
  if not Assigned(p) then  // ou : if p = nil then
  begin
    WriteLn('Erreur : pointeur nil');
    Exit;
  end;

  // Utilisation sécurisée
  p^.Champ := 10;
end;
```

### 8.3 Double Free

**❌ Erreur fatale :**

```pascal
var
  objet: TMonObjet;
begin
  objet := TMonObjet.Create;
  objet.Free;
  objet.Free;  // ERREUR ! Déjà libéré → Plantage
end;
```

**✅ Protection :**

```pascal
var
  objet: TMonObjet;
begin
  objet := TMonObjet.Create;
  FreeAndNil(objet);  // Libère et met à nil

  objet.Free;  // Maintenant safe : Free sur nil ne fait rien
end;
```

---

## 9. Patterns Avancés

### 9.1 Auto-Free avec Interface

**Utiliser les interfaces pour gestion automatique :**

```pascal
type
  IAutoFree = interface
    function GetObject: TObject;
    property Obj: TObject read GetObject;
  end;

  TAutoFree = class(TInterfacedObject, IAutoFree)
  private
    FObject: TObject;
    function GetObject: TObject;
  public
    constructor Create(AObject: TObject);
    destructor Destroy; override;
    property Obj: TObject read GetObject;
  end;

constructor TAutoFree.Create(AObject: TObject);  
begin  
  inherited Create;
  FObject := AObject;
end;

destructor TAutoFree.Destroy;  
begin  
  FreeAndNil(FObject);
  inherited Destroy;
end;

function TAutoFree.GetObject: TObject;  
begin  
  Result := FObject;
end;

// Utilisation
procedure Exemple;  
var  
  auto: IAutoFree;
  liste: TStringList;
begin
  liste := TStringList.Create;
  auto := TAutoFree.Create(liste);  // auto gérera la libération

  (auto.Obj as TStringList).Add('Element');

  // Pas besoin de Free : l'interface le fera automatiquement
end;
```

**Avantage :** Libération automatique par comptage de références.

### 9.2 Pool d'Objets

**Pour des créations/destructions fréquentes :**

```pascal
type
  TObjectPool<T: class, constructor> = class
  private
    FPool: TQueue<T>;
  public
    constructor Create;
    destructor Destroy; override;
    function Acquire: T;
    procedure Release(AObject: T);
  end;

constructor TObjectPool<T>.Create;  
begin  
  inherited Create;
  FPool := TQueue<T>.Create;
end;

destructor TObjectPool<T>.Destroy;  
begin  
  while FPool.Count > 0 do
    FPool.Dequeue.Free;
  FPool.Free;
  inherited Destroy;
end;

function TObjectPool<T>.Acquire: T;  
begin  
  if FPool.Count > 0 then
    Result := FPool.Dequeue
  else
    Result := T.Create;
end;

procedure TObjectPool<T>.Release(AObject: T);  
begin  
  // Réinitialiser l'objet si nécessaire
  FPool.Enqueue(AObject);
end;

// Utilisation
var
  pool: TObjectPool<TStringList>;
  liste: TStringList;
begin
  pool := TObjectPool<TStringList>.Create;
  try
    liste := pool.Acquire;  // Réutilise ou crée
    try
      liste.Add('Element');
      Traiter(liste);
    finally
      pool.Release(liste);  // Remet dans le pool
    end;
  finally
    pool.Free;
  end;
end;
```

---

## 10. Multi-plateforme : Particularités

### 10.1 Gestion Mémoire Windows vs Linux

**Globalement identique**, mais quelques différences :

| Aspect | Windows | Linux |
|--------|---------|-------|
| **Allocateur** | HeapAlloc (Windows API) | malloc/brk (système) |
| **Fragmentation** | Peut être plus importante | Généralement mieux géré |
| **Outils** | Task Manager, PerfMon | top, htop, valgrind |
| **Détection fuites** | HeapTrc, outils tiers | HeapTrc, Valgrind |

### 10.2 Surveillance de la Mémoire

**Windows :**

```pascal
uses Windows;

function GetMemoryUsage: Int64;  
var  
  pmc: TProcessMemoryCounters;
begin
  pmc.cb := SizeOf(pmc);
  if GetProcessMemoryInfo(GetCurrentProcess, @pmc, pmc.cb) then
    Result := pmc.WorkingSetSize
  else
    Result := 0;
end;
```

**Linux :**

```pascal
function GetMemoryUsageLinux: Int64;  
var  
  F: TextFile;
  ligne: String;
begin
  Result := 0;
  AssignFile(F, '/proc/self/status');
  try
    Reset(F);
    while not Eof(F) do
    begin
      ReadLn(F, ligne);
      if Pos('VmRSS:', ligne) = 1 then
      begin
        // Parser la valeur
        Delete(ligne, 1, 6);
        ligne := Trim(ligne);
        Result := StrToInt64Def(Copy(ligne, 1, Pos(' ', ligne) - 1), 0) * 1024;
        Break;
      end;
    end;
    CloseFile(F);
  except
    // Erreur
  end;
end;
```

**Portable :**

```pascal
function GetMemoryUsage: Int64;  
begin  
  {$IFDEF WINDOWS}
  Result := GetMemoryUsageWindows;
  {$ENDIF}
  {$IFDEF LINUX}
  Result := GetMemoryUsageLinux;
  {$ENDIF}
end;
```

### 10.3 Compilation avec Vérifications

**Options recommandées pour le développement :**

```pascal
{$MODE OBJFPC}
{$H+}           // Chaînes longues
{$R+}           // Range checking
{$I+}           // I/O checking
{$Q+}           // Overflow checking

{$IFDEF DEBUG}
  {$DEFINE HEAPTRC}
  {$ASSERTIONS ON}
{$ENDIF}
```

---

## 11. Checklist de Gestion Mémoire

### 11.1 Pour Chaque Allocation

- [ ] Qui est responsable de la libération ?
- [ ] La libération est-elle dans un bloc try-finally ?
- [ ] Y a-t-il des sorties précoces (Exit) avant le Free ?
- [ ] Les exceptions sont-elles gérées ?
- [ ] Le pointeur est-il mis à nil après libération ?

### 11.2 Pour les Classes

- [ ] Chaque Create a-t-il un Free correspondant ?
- [ ] Les destructeurs libèrent-ils tous les objets possédés ?
- [ ] Les références circulaires sont-elles évitées ?
- [ ] L'ownership est-il clairement défini ?

### 11.3 Pour les Collections

- [ ] TObjectList est-elle propriétaire (OwnsObjects = True) ?
- [ ] Les objets sont-ils libérés avant Clear ?
- [ ] La capacité est-elle appropriée ?
- [ ] Les doublons sont-ils gérés correctement ?

### 11.4 Tests

- [ ] Tests avec HeapTrc activé
- [ ] Tests sous Valgrind (Linux)
- [ ] Surveillance de l'utilisation mémoire pendant l'exécution
- [ ] Tests de charge (stress tests)
- [ ] Profiling mémoire

---

## 12. Erreurs Courantes et Solutions

### 12.1 Oubli de Libération

**Erreur :**
```pascal
procedure Traiter;  
var  
  liste: TStringList;
begin
  liste := TStringList.Create;
  // ... utilisation ...
  // Oubli du Free !
end;
```

**Solution :** Template try-finally systématique.

### 12.2 Libération Prématurée

**Erreur :**
```pascal
function CreerListe: TStringList;  
begin  
  Result := TStringList.Create;
  Result.Add('Element');
  Result.Free;  // ERREUR ! Libéré avant retour
end;
```

**Solution :** L'appelant doit libérer ce que la fonction retourne.

### 12.3 Références Multiples

**Erreur :**
```pascal
var
  liste1, liste2: TStringList;
begin
  liste1 := TStringList.Create;
  liste2 := liste1;  // Même objet !

  liste1.Free;
  liste2.Free;  // ERREUR ! Double free
end;
```

**Solution :** Clarifier la propriété ou copier l'objet.

### 12.4 Objets dans TList Non Propriétaire

**Erreur :**
```pascal
var
  liste: TList;
  i: Integer;
begin
  liste := TList.Create;  // Non propriétaire par défaut
  for i := 1 to 10 do
    liste.Add(TObject.Create);
  liste.Free;  // Les objets ne sont pas libérés !
end;
```

**Solution :** Utiliser TObjectList(True) ou libérer manuellement.

---

## 13. Outils et Techniques de Diagnostic

### 13.1 Compteurs Personnalisés

```pascal
var
  CompteurAllocations: Integer = 0;
  CompteurLiberations: Integer = 0;

type
  TObjetTrace = class
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TObjetTrace.Create;  
begin  
  inherited Create;
  Inc(CompteurAllocations);
  WriteLn('[+] Allocation #', CompteurAllocations, ' (Actifs: ',
          CompteurAllocations - CompteurLiberations, ')');
end;

destructor TObjetTrace.Destroy;  
begin  
  Inc(CompteurLiberations);
  WriteLn('[-] Libération #', CompteurLiberations, ' (Actifs: ',
          CompteurAllocations - CompteurLiberations, ')');
  inherited Destroy;
end;
```

### 13.2 Logging des Allocations

```pascal
procedure LogAllocation(const nom: String; taille: Integer);  
var  
  F: TextFile;
begin
  AssignFile(F, 'allocations.log');
  if FileExists('allocations.log') then
    Append(F)
  else
    Rewrite(F);
  try
    WriteLn(F, FormatDateTime('hh:nn:ss.zzz', Now),
            ' - Allocation: ', nom, ' (', taille, ' octets)');
  finally
    CloseFile(F);
  end;
end;
```

### 13.3 Assertions

```pascal
{$ASSERTIONS ON}

procedure Traiter(objet: TMonObjet);  
begin  
  Assert(Assigned(objet), 'Objet ne doit pas être nil');
  Assert(objet is TMonObjet, 'Type incorrect');

  // Traitement
end;
```

---

## 14. Récapitulatif

### 14.1 Règles d'Or

1. **Qui crée, libère** - Responsabilité claire
2. **Try-finally TOUJOURS** - Pour tous les objets
3. **FreeAndNil** - Sécurité supplémentaire
4. **Vérifier Assigned** - Avant utilisation de pointeurs
5. **Clarifier l'ownership** - Documentation et design

### 14.2 Patterns à Mémoriser

**Pattern de base :**
```pascal
objet := TObjet.Create;  
try  
  // Utilisation
finally
  objet.Free;
end;
```

**Pattern multi-objets :**
```pascal
objet1 := nil;  
objet2 := nil;  
try  
  objet1 := TObjet1.Create;
  objet2 := TObjet2.Create;
  // Utilisation
finally
  FreeAndNil(objet2);
  FreeAndNil(objet1);
end;
```

**Pattern de fonction :**
```pascal
function CreerObjet: TObjet;  
begin  
  Result := TObjet.Create;
  // L'appelant est responsable du Free
end;

// Utilisation
objet := CreerObjet;  
try  
  // ...
finally
  objet.Free;
end;
```

### 14.3 Outils Essentiels

| Outil | Plateforme | Usage |
|-------|-----------|-------|
| **HeapTrc** | Windows/Linux | Détection fuites basique |
| **Valgrind** | Linux | Analyse détaillée |
| **Débogueur Lazarus** | Windows/Linux | Inspection runtime |
| **Compteurs perso** | Windows/Linux | Traçage d'allocations |

---

## 15. Conclusion

La gestion de la mémoire est une compétence fondamentale. Les points clés :

**Comprendre :**
- Stack vs Heap
- Allocation manuelle vs automatique
- Ownership et responsabilité

**Pratiquer :**
- Try-finally systématique
- Vérifications de pointeurs
- Libération dans le bon ordre

**Détecter :**
- HeapTrc pour les fuites
- Valgrind pour analyse détaillée
- Surveillance de l'utilisation mémoire

**Optimiser :**
- Réutilisation d'objets
- Pré-allocation de capacité
- Pool d'objets si nécessaire

**Citation finale :**
> "Memory management is not about being perfect, it's about being systematic."

Une approche méthodique et des outils appropriés vous éviteront 99% des problèmes de mémoire.

**Prochaine Étape :** La section 20.7 (Outils de détection de fuites mémoire) approfondira les outils et techniques avancées pour traquer les problèmes mémoire les plus subtils.

---

**Multi-plateforme Windows/Ubuntu** ✓  
**Formation FreePascal/Lazarus - Niveau Débutant à Intermédiaire**  

⏭️ [Outils de détection de fuites mémoire](/20-debogage-optimisation/07-outils-detection-fuites-memoire.md)
