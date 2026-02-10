🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.4 IInterface et IUnknown

## Introduction : Les interfaces de base

Vous avez déjà utilisé `TInterfacedObject` sans vraiment savoir ce qu'il faisait en coulisses. Il est temps de lever le voile sur la magie qui se cache derrière !

Lorsque vous écrivez :

```pascal
type
  MaClasse = class(TInterfacedObject, IMonInterface)
```

`TInterfacedObject` implémente automatiquement une interface spéciale appelée **IInterface** (ou **IUnknown** sous Windows). C'est l'interface **mère de toutes les interfaces** en FreePascal.

**Analogie :** Pensez à IInterface comme le "contrat de base" que toute interface doit respecter. C'est comme le mode d'emploi minimum que tout appareil électronique doit avoir (bouton on/off, prise électrique).

---

## Qu'est-ce que IInterface ?

### Définition

`IInterface` est l'interface de base dont héritent **toutes** les interfaces en FreePascal. Elle définit trois méthodes fondamentales pour :
1. Interroger un objet sur ses capacités
2. Gérer automatiquement la durée de vie des objets (comptage de références)

### Déclaration de IInterface

Voici à quoi ressemble `IInterface` (simplifié) :

```pascal
type
  IInterface = interface
    ['{00000000-0000-0000-C000-000000000046}']
    function QueryInterface(const IID: TGUID; out Obj): HResult;
    function _AddRef: Integer;
    function _Release: Integer;
  end;
```

**Les trois méthodes :**
- `QueryInterface` : "Est-ce que tu sais faire ça ?"
- `_AddRef` : "Quelqu'un utilise cet objet" (incrémente le compteur)
- `_Release` : "Je n'utilise plus cet objet" (décrémente le compteur)

---

## IInterface vs IUnknown : Quelle différence ?

### IUnknown (Windows COM)

`IUnknown` est l'interface de base du modèle **COM** (Component Object Model) de Microsoft Windows.

### IInterface (FreePascal)

`IInterface` est l'équivalent FreePascal/Delphi de `IUnknown`. En fait :

```pascal
// Dans FreePascal, ce sont des alias (même chose)
IInterface = IUnknown
```

**En résumé :**
- **IUnknown** : Nom COM (Microsoft, Windows)
- **IInterface** : Nom FreePascal/Delphi (multi-plateforme)
- **En pratique** : Utilisez `IInterface` dans vos programmes FreePascal

---

## Les trois méthodes fondamentales

### 1. QueryInterface : "Sais-tu faire ça ?"

Cette méthode permet de demander à un objet s'il implémente une interface particulière.

**Analogie :** C'est comme demander à quelqu'un : "Parles-tu anglais ? Sais-tu conduire ?"

```pascal
function QueryInterface(const IID: TGUID; out Obj): HResult;
```

**Paramètres :**
- `IID` : L'identifiant (GUID) de l'interface recherchée
- `Obj` : Si trouvée, contiendra une référence à l'interface
- **Retour** : `S_OK` si succès, `E_NOINTERFACE` si échec

**Utilisation (rare en pratique) :**

```pascal
var
  MonObjet: TMonObjet;
  Volant: IVolant;
  ResultatQuery: HResult;
begin
  MonObjet := TMonObjet.Create;

  // Demander si l'objet implémente IVolant
  ResultatQuery := MonObjet.QueryInterface(IVolant, Volant);

  if ResultatQuery = S_OK then
    WriteLn('Cet objet peut voler !')
  else
    WriteLn('Cet objet ne peut pas voler');

  MonObjet.Free;
end;
```

**En pratique :** On utilise plutôt les opérateurs `is` et `as` qui sont plus simples (voir section suivante).

---

### 2. _AddRef : Compteur de références +1

Cette méthode **incrémente** le compteur de références. À chaque fois qu'une nouvelle variable pointe vers l'objet, le compteur augmente.

```pascal
function _AddRef: Integer;
```

**Analogie :** C'est comme un compteur de visiteurs dans un magasin. Chaque fois que quelqu'un entre, on incrémente : 1, 2, 3...

**Fonctionnement automatique :**

```pascal
var
  Objet1, Objet2: IMonInterface;
begin
  Objet1 := TImplementation.Create;  // Compteur = 1
  Objet2 := Objet1;                   // Compteur = 2 (_AddRef appelé automatiquement)

  // Objet1 et Objet2 pointent vers le même objet
end;  // Compteur décrémenté automatiquement
```

**Important :** Vous n'appelez **jamais** `_AddRef` manuellement ! FreePascal le fait pour vous.

---

### 3. _Release : Compteur de références -1

Cette méthode **décrémente** le compteur de références. Quand le compteur atteint zéro, l'objet se détruit automatiquement.

```pascal
function _Release: Integer;
```

**Analogie :** Quand un visiteur sort du magasin, on décrémente : 3, 2, 1, 0. Quand il n'y a plus personne (0), on peut fermer et faire le ménage.

**Fonctionnement automatique :**

```pascal
var
  Objet1, Objet2: IMonInterface;
begin
  Objet1 := TImplementation.Create;  // Compteur = 1
  Objet2 := Objet1;                   // Compteur = 2

  Objet1 := nil;                      // Compteur = 1 (_Release appelé)
  Objet2 := nil;                      // Compteur = 0 → Objet DÉTRUIT automatiquement !
end;
```

**Important :** Vous n'appelez **jamais** `_Release` manuellement ! FreePascal le fait pour vous.

---

## Le rôle de TInterfacedObject

### Implémentation automatique

`TInterfacedObject` est une classe qui **implémente déjà** les trois méthodes de `IInterface` pour vous !

```pascal
type
  TInterfacedObject = class(TObject, IInterface)
  protected
    FRefCount: Integer;  // Compteur de références
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual;
    function _AddRef: Integer; virtual;
    function _Release: Integer; virtual;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
  end;
```

### Pourquoi hériter de TInterfacedObject ?

Si vous héritez de `TInterfacedObject`, vous obtenez **gratuitement** :
- ✅ Le comptage automatique des références
- ✅ La destruction automatique quand plus personne n'utilise l'objet
- ✅ L'implémentation de `QueryInterface`, `_AddRef`, `_Release`

**Sans TInterfacedObject**, vous devriez tout implémenter vous-même (complexe et source d'erreurs).

---

## Le comptage de références en détail

### Principe de fonctionnement

Chaque objet qui implémente `IInterface` possède un **compteur interne** :

```
Compteur = 0  →  Objet n'existe pas encore  
Compteur = 1  →  1 variable utilise l'objet  
Compteur = 2  →  2 variables utilisent l'objet  
Compteur = 3  →  3 variables utilisent l'objet  
...
Compteur = 0  →  Plus personne n'utilise l'objet → DESTRUCTION AUTOMATIQUE
```

### Exemple détaillé avec compteur

```pascal
program ComptageReferences;

{$mode objfpc}{$H+}

type
  ITest = interface
    ['{12345678-1234-1234-1234-123456789012}']
    procedure Afficher;
  end;

  TTest = class(TInterfacedObject, ITest)
  private
    FNom: string;
  public
    constructor Create(const Nom: string);
    destructor Destroy; override;
    procedure Afficher;
  end;

constructor TTest.Create(const Nom: string);  
begin  
  inherited Create;
  FNom := Nom;
  WriteLn('✅ Objet "', FNom, '" créé - Compteur: ', RefCount);
end;

destructor TTest.Destroy;  
begin  
  WriteLn('❌ Objet "', FNom, '" détruit');
  inherited;
end;

procedure TTest.Afficher;  
begin  
  WriteLn('👋 Je suis "', FNom, '" - Compteur: ', RefCount);
end;

var
  Ref1, Ref2, Ref3: ITest;
begin
  WriteLn('=== Démonstration du comptage de références ===');
  WriteLn('');

  WriteLn('1. Création de l''objet et assignation à Ref1');
  Ref1 := TTest.Create('MonObjet');  // Compteur = 1
  Ref1.Afficher;
  WriteLn('');

  WriteLn('2. Assignation à Ref2 (même objet)');
  Ref2 := Ref1;                       // Compteur = 2
  Ref2.Afficher;
  WriteLn('');

  WriteLn('3. Assignation à Ref3 (toujours le même objet)');
  Ref3 := Ref1;                       // Compteur = 3
  Ref3.Afficher;
  WriteLn('');

  WriteLn('4. Libération de Ref1');
  Ref1 := nil;                        // Compteur = 2
  WriteLn('   Objet toujours vivant car Ref2 et Ref3 existent');
  WriteLn('');

  WriteLn('5. Libération de Ref2');
  Ref2 := nil;                        // Compteur = 1
  WriteLn('   Objet toujours vivant car Ref3 existe');
  WriteLn('');

  WriteLn('6. Libération de Ref3');
  Ref3 := nil;                        // Compteur = 0 → DESTRUCTION !
  WriteLn('');

  WriteLn('Fin du programme');
end.
```

**Résultat :**
```
=== Démonstration du comptage de références ===

1. Création de l'objet et assignation à Ref1
✅ Objet "MonObjet" créé - Compteur: 1
👋 Je suis "MonObjet" - Compteur: 1

2. Assignation à Ref2 (même objet)
👋 Je suis "MonObjet" - Compteur: 2

3. Assignation à Ref3 (toujours le même objet)
👋 Je suis "MonObjet" - Compteur: 3

4. Libération de Ref1
   Objet toujours vivant car Ref2 et Ref3 existent

5. Libération de Ref2
   Objet toujours vivant car Ref3 existe

6. Libération de Ref3
❌ Objet "MonObjet" détruit

Fin du programme
```

---

## Avantages du comptage de références

### 1. Pas de fuite mémoire

Avec les interfaces, vous n'avez plus besoin d'appeler `.Free` :

```pascal
// ❌ Avec classes : risque d'oubli
var
  Obj: TMonObjet;
begin
  Obj := TMonObjet.Create;
  // ... code ...
  Obj.Free;  // ← Si on oublie = FUITE MÉMOIRE !
end;

// ✅ Avec interfaces : destruction automatique
var
  Obj: IMonInterface;
begin
  Obj := TImplementation.Create;
  // ... code ...
  // Pas de Free nécessaire → libération automatique !
end;
```

### 2. Partage sécurisé d'objets

Plusieurs variables peuvent pointer vers le même objet sans risque :

```pascal
var
  Ref1, Ref2, Ref3: IMonInterface;
begin
  Ref1 := TImplementation.Create;
  Ref2 := Ref1;  // Partage l'objet
  Ref3 := Ref1;  // Idem

  // L'objet ne sera détruit que quand les 3 seront à nil
end;
```

### 3. Code plus simple et sûr

Moins de gestion manuelle = moins d'erreurs.

---

## Attention : Mélanger classes et interfaces

### ⚠️ Danger : Ne pas appeler Free sur une interface

**ERREUR GRAVE :**

```pascal
var
  Obj: IMonInterface;
begin
  Obj := TImplementation.Create;
  // ... code ...
  TImplementation(Obj).Free;  // ❌ DANGER ! Double libération !
end;
```

**Pourquoi c'est dangereux ?**
1. L'interface va automatiquement détruire l'objet quand `Obj` sera à nil
2. Si vous appelez `.Free` manuellement, vous détruisez l'objet
3. L'interface essaiera de détruire un objet déjà détruit → **CRASH !**

### ✅ Règle d'or

**Si vous utilisez une interface, ne jamais appeler `.Free` !**

```pascal
// ✅ Correct
var
  Obj: IMonInterface;
begin
  Obj := TImplementation.Create;
  // ... code ...
  // Rien à faire, destruction automatique
end;

// ✅ Correct aussi
var
  Obj: IMonInterface;
begin
  Obj := TImplementation.Create;
  // ... code ...
  Obj := nil;  // Libération explicite (optionnel)
end;
```

---

## Cas particulier : Référence circulaire

### Le problème

Le comptage de références a une limite : les **références circulaires**.

```pascal
type
  IPersonne = interface;

  IPersonne = interface
    ['{...}']
    procedure DefinirAmi(Ami: IPersonne);
  end;

  TPersonne = class(TInterfacedObject, IPersonne)
  private
    FAmi: IPersonne;  // Référence vers un ami
  public
    procedure DefinirAmi(Ami: IPersonne);
  end;

var
  Alice, Bob: IPersonne;
begin
  Alice := TPersonne.Create;
  Bob := TPersonne.Create;

  Alice.DefinirAmi(Bob);  // Alice → Bob
  Bob.DefinirAmi(Alice);  // Bob → Alice

  // ⚠️ PROBLÈME : Alice et Bob se référencent mutuellement
  // Compteur d'Alice = 2 (Bob + variable)
  // Compteur de Bob = 2 (Alice + variable)

  Alice := nil;  // Compteur Alice = 1 (Bob référence toujours Alice)
  Bob := nil;    // Compteur Bob = 1 (Alice référence toujours Bob)

  // Les deux objets restent en mémoire → FUITE MÉMOIRE !
end;
```

### Solution

Pour éviter les références circulaires :
1. Utiliser des **références faibles** (weak references) - avancé
2. Casser manuellement une des références avant de libérer
3. Utiliser un design différent qui évite les cycles

**Pour les débutants :** Évitez simplement de créer des références circulaires dans votre design.

---

## Quand s'en préoccuper ?

### En tant que débutant

**Bonne nouvelle :** Vous n'avez **presque jamais** à vous préoccuper de `IInterface`, `_AddRef` et `_Release` directement !

✅ **Ce que vous faites :**
- Hériter de `TInterfacedObject`
- Utiliser des variables de type interface
- Laisser le système gérer automatiquement

❌ **Ce que vous ne faites PAS :**
- Appeler manuellement `_AddRef` ou `_Release`
- Implémenter vous-même `QueryInterface`
- Mélanger `.Free` avec des interfaces

### En tant que développeur avancé

Vous pourriez avoir besoin de comprendre `IInterface` pour :
- Créer des interfaces COM sous Windows
- Déboguer des problèmes de durée de vie d'objets
- Optimiser la gestion mémoire
- Créer des frameworks ou bibliothèques

---

## Tester si un objet implémente une interface

### Opérateur `is` : "Est-ce que... ?"

```pascal
var
  MonObjet: TObject;
begin
  MonObjet := TCanard.Create;

  if MonObjet is TCanard then
    WriteLn('C''est un canard');

  // Avec interface (conversion en interface d'abord)
  if Supports(MonObjet, IVolant) then
    WriteLn('Cet objet peut voler');

  MonObjet.Free;
end;
```

### Opérateur `as` : "Considère comme..."

```pascal
var
  MonObjet: TObject;
  Volant: IVolant;
begin
  MonObjet := TCanard.Create;

  // Conversion sécurisée
  if Supports(MonObjet, IVolant, Volant) then
  begin
    Volant.Voler;
    // Volant pointe maintenant vers MonObjet en tant que IVolant
  end;

  MonObjet.Free;
end;
```

### Fonction `Supports` : La plus pratique

```pascal
uses
  SysUtils;  // Pour Supports

var
  MonObjet: TCanard;
  Volant: IVolant;
begin
  MonObjet := TCanard.Create;

  // Vérifier et obtenir l'interface en une seule fois
  if Supports(MonObjet, IVolant, Volant) then
  begin
    WriteLn('✅ L''objet supporte IVolant');
    Volant.Voler;
  end
  else
    WriteLn('❌ L''objet ne supporte pas IVolant');

  MonObjet.Free;
end;
```

---

## Résumé

### IInterface (ou IUnknown)
- **L'interface mère** de toutes les interfaces
- Définit 3 méthodes : `QueryInterface`, `_AddRef`, `_Release`
- Gère le **comptage de références** automatique

### Les trois méthodes
- **QueryInterface** : Interroger les capacités d'un objet
- **_AddRef** : Incrémenter le compteur de références
- **_Release** : Décrémenter le compteur (détruit si = 0)

### TInterfacedObject
- Implémente automatiquement `IInterface`
- Fournit le comptage de références
- **À utiliser** comme classe de base pour vos implémentations

### Comptage de références
- Gestion automatique de la durée de vie
- Pas besoin de `.Free` avec les interfaces
- ⚠️ Attention aux références circulaires

### Règles importantes
✅ Hériter de `TInterfacedObject`  
✅ Utiliser des variables de type interface  
✅ Laisser la destruction automatique fonctionner  
❌ Ne jamais appeler `.Free` sur une interface  
❌ Ne jamais appeler `_AddRef`/`_Release` manuellement  

---

## Pour aller plus loin

**Vous êtes débutant ?**
➜ Retenez juste : utilisez `TInterfacedObject` et laissez la magie opérer !  

**Vous voulez approfondir ?**
➜ Consultez la documentation FreePascal sur COM et les interfaces  
➜ Étudiez les design patterns utilisant les interfaces  
➜ Explorez les références faibles (weak references) pour éviter les cycles  

---

## Prochaine étape

Dans la section suivante (12.5), vous découvrirez le **comptage de références** en détail pratique avec des cas d'usage concrets et des pièges à éviter.

⏭️ [Comptage de références](/12-interfaces-poo-avancee/05-comptage-references.md)
