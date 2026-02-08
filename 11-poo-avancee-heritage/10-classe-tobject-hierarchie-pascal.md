🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.10 Classe TObject et hiérarchie Pascal

## Introduction

**TObject** est la "mère de toutes les classes" en FreePascal/Lazarus. C'est la classe racine à partir de laquelle **toutes** les autres classes héritent, directement ou indirectement. Comprendre TObject est essentiel pour maîtriser la programmation orientée objet en Pascal.

### Analogie du monde réel

Imaginez l'arbre généalogique de l'humanité. Si nous pouvions remonter suffisamment loin, nous trouverions un ancêtre commun à tous les êtres humains. TObject joue exactement ce rôle dans le monde des classes Pascal : c'est l'**ancêtre universel** dont toutes les classes descendent.

## Qu'est-ce que TObject ?

TObject est une classe définie dans l'unité `System` de FreePascal. Elle fournit les **fonctionnalités de base** dont toute classe a besoin :

```pascal
type
  TObject = class
  public
    constructor Create;
    destructor Destroy; virtual;
    procedure Free;
    class function ClassName: string;
    class function ClassType: TClass;
    function InstanceSize: Integer;
    // ... et d'autres méthodes
  end;
```

### Héritage implicite

Quand vous créez une classe sans spécifier de parent, elle hérite automatiquement de TObject :

```pascal
type
  MaClasse = class
    // Hérite implicitement de TObject
  end;

  // Équivalent à :
  MaClasse = class(TObject)
  end;
```

## La hiérarchie Pascal complète

```
                 TObject (racine universelle)
                    ↓
        ┌───────────┴───────────┐
        ↓                       ↓
   TPersistent           TInterfacedObject
        ↓
   ┌────┴────────┐
   ↓             ↓
TCollectionItem TComponent
                 ↓
              TControl
                 ↓
             TWinControl
                 ↓
           ┌─────┴─────┐
           ↓           ↓
         TForm      TButton
         TLabel     TEdit
         TPanel     etc.
```

**Toutes les classes** que vous utilisez dans Lazarus (TForm, TButton, TEdit, etc.) descendent de TObject.

## Les méthodes essentielles de TObject

### 1. Create : Le constructeur

Le constructeur par défaut de toutes les classes.

```pascal
constructor TObject.Create;
begin
  // Initialisation de base de l'objet
end;
```

**Utilisation :**

```pascal
var
  MonObjet: TObject;
begin
  MonObjet := TObject.Create;  // Crée une instance
  // ...
  MonObjet.Free;
end;
```

**Important** : Dans vos propres classes, vous devez toujours appeler `inherited Create` :

```pascal
type
  MaClasse = class
    constructor Create;
  end;

constructor MaClasse.Create;
begin
  inherited Create;  // Appelle TObject.Create
  // Votre initialisation
end;
```

### 2. Destroy : Le destructeur

Libère les ressources allouées par l'objet.

```pascal
destructor TObject.Destroy;
begin
  // Nettoyage de base
end;
```

**Important** : Toujours déclarer vos destructeurs avec `override` :

```pascal
type
  MaClasse = class
    destructor Destroy; override;
  end;

destructor MaClasse.Destroy;
begin
  // Votre nettoyage
  inherited Destroy;  // Appelle TObject.Destroy
end;
```

### 3. Free : Destruction sécurisée

La méthode la plus importante pour libérer un objet.

```pascal
procedure TObject.Free;
begin
  if Self <> nil then
    Destroy;
end;
```

**Pourquoi Free au lieu de Destroy ?**

```pascal
var
  MonObjet: TMonType;
begin
  MonObjet := nil;

  // ❌ DANGER : plante si MonObjet = nil
  MonObjet.Destroy;

  // ✅ SÉCURISÉ : vérifie si l'objet existe
  MonObjet.Free;
end;
```

**Règle d'or** : Utilisez **toujours** `Free`, jamais `Destroy` directement.

### 4. FreeAndNil : Libération et réinitialisation

Libère l'objet ET met la variable à `nil`.

```pascal
procedure FreeAndNil(var Obj);
begin
  if Obj <> nil then
  begin
    TObject(Obj).Free;
    Obj := nil;
  end;
end;
```

**Utilisation :**

```pascal
var
  MonObjet: TMonType;
begin
  MonObjet := TMonType.Create;

  // Utilisation...

  FreeAndNil(MonObjet);  // Libère ET met à nil

  // MonObjet vaut maintenant nil
  if MonObjet = nil then
    WriteLn('Objet libéré');
end;
```

**Avantage** : Évite les "dangling pointers" (pointeurs vers de la mémoire libérée).

## Méthodes d'information de classe

### 1. ClassName : Nom de la classe

Retourne le nom de la classe sous forme de chaîne.

```pascal
class function TObject.ClassName: string;
```

**Exemple :**

```pascal
program DemoClassName;

{$mode objfpc}{$H+}

type
  TAnimal = class
  end;

  TChien = class(TAnimal)
  end;

var
  Animal: TAnimal;
  Chien: TChien;
begin
  Animal := TAnimal.Create;
  Chien := TChien.Create;

  WriteLn('Nom classe Animal : ', Animal.ClassName);  // "TAnimal"
  WriteLn('Nom classe Chien : ', Chien.ClassName);    // "TChien"

  // Libération du premier objet avant réaffectation
  Animal.Free;

  // Utilisation polymorphe
  Animal := Chien;
  WriteLn('Animal pointe vers : ', Animal.ClassName); // "TChien"

  // Libération une seule fois (Animal et Chien pointent vers le même objet)
  Chien.Free;
end.
```

**Utilité** : Debug, logging, messages d'erreur.

### 2. ClassType : Type de classe

Retourne une référence au type de classe.

```pascal
class function TObject.ClassType: TClass;
```

**Exemple :**

```pascal
var
  TypeClasse: TClass;
  Objet: TObject;
begin
  Objet := TChien.Create;
  TypeClasse := Objet.ClassType;

  WriteLn('Type : ', TypeClasse.ClassName);  // "TChien"
end;
```

### 3. InheritsFrom : Vérification d'héritage

Vérifie si une classe hérite d'une autre.

```pascal
class function TObject.InheritsFrom(AClass: TClass): Boolean;
```

**Exemple :**

```pascal
program DemoInheritsFrom;

{$mode objfpc}{$H+}

type
  TAnimal = class
  end;

  TMammifere = class(TAnimal)
  end;

  TChien = class(TMammifere)
  end;

  TOiseau = class(TAnimal)
  end;

var
  Chien: TChien;
begin
  Chien := TChien.Create;

  WriteLn('TChien hérite de TChien : ', Chien.InheritsFrom(TChien));      // True
  WriteLn('TChien hérite de TMammifere : ', Chien.InheritsFrom(TMammifere)); // True
  WriteLn('TChien hérite de TAnimal : ', Chien.InheritsFrom(TAnimal));    // True
  WriteLn('TChien hérite de TObject : ', Chien.InheritsFrom(TObject));    // True
  WriteLn('TChien hérite de TOiseau : ', Chien.InheritsFrom(TOiseau));    // False

  Chien.Free;
end.
```

**Différence avec `is`** :

```pascal
// InheritsFrom : au niveau de la classe (statique)
if TChien.InheritsFrom(TAnimal) then
  WriteLn('TChien descend de TAnimal');

// is : au niveau de l'instance (dynamique)
if MonObjet is TAnimal then
  WriteLn('MonObjet est un TAnimal');
```

### 4. InstanceSize : Taille de l'instance

Retourne la taille en octets d'une instance de la classe.

```pascal
function TObject.InstanceSize: Integer;
```

**Exemple :**

```pascal
type
  TPersonne = class
    FNom: string;
    FAge: Integer;
  end;

var
  P: TPersonne;
begin
  P := TPersonne.Create;
  WriteLn('Taille instance : ', P.InstanceSize, ' octets');
  P.Free;
end;
```

## Exemple complet : Utilisation des méthodes de TObject

```pascal
program DemoTObject;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Classe de base }
  TAnimal = class
  private
    FNom: string;
  public
    constructor Create(ANom: string);
    destructor Destroy; override;
    procedure Afficher; virtual;
    property Nom: string read FNom;
  end;

  { Classe dérivée }
  TChien = class(TAnimal)
  private
    FRace: string;
  public
    constructor Create(ANom, ARace: string);
    destructor Destroy; override;
    procedure Afficher; override;
  end;

{ === TAnimal === }

constructor TAnimal.Create(ANom: string);
begin
  inherited Create;  // Appelle TObject.Create
  FNom := ANom;
  WriteLn('[', ClassName, '.Create] Création de ', FNom);
end;

destructor TAnimal.Destroy;
begin
  WriteLn('[', ClassName, '.Destroy] Destruction de ', FNom);
  inherited Destroy;  // Appelle TObject.Destroy
end;

procedure TAnimal.Afficher;
begin
  WriteLn('Animal : ', FNom);
  WriteLn('Classe : ', ClassName);
  WriteLn('Taille : ', InstanceSize, ' octets');
end;

{ === TChien === }

constructor TChien.Create(ANom, ARace: string);
begin
  inherited Create(ANom);
  FRace := ARace;
  WriteLn('[', ClassName, '.Create] Race : ', FRace);
end;

destructor TChien.Destroy;
begin
  WriteLn('[', ClassName, '.Destroy] Nettoyage spécifique');
  inherited Destroy;
end;

procedure TChien.Afficher;
begin
  inherited Afficher;
  WriteLn('Race : ', FRace);
end;

{ === Fonctions de démonstration === }

procedure AfficherInfosClasse(Obj: TObject);
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('Informations sur l''objet');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('Nom de classe : ', Obj.ClassName);
  WriteLn('Taille instance : ', Obj.InstanceSize, ' octets');
  WriteLn('Type de classe : ', Obj.ClassType.ClassName);
  WriteLn;
end;

procedure TesterHeritage;
var
  Animal: TAnimal;
  Chien: TChien;
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('TEST D''HERITAGE');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  Animal := TAnimal.Create('Générique');
  Chien := TChien.Create('Rex', 'Berger Allemand');

  WriteLn('--- Tests InheritsFrom ---');
  WriteLn('TAnimal hérite de TObject : ', TAnimal.InheritsFrom(TObject));
  WriteLn('TChien hérite de TAnimal : ', TChien.InheritsFrom(TAnimal));
  WriteLn('TChien hérite de TObject : ', TChien.InheritsFrom(TObject));
  WriteLn('TAnimal hérite de TChien : ', TAnimal.InheritsFrom(TChien));
  WriteLn;

  WriteLn('--- Tests avec is ---');
  WriteLn('Animal is TObject : ', Animal is TObject);
  WriteLn('Animal is TAnimal : ', Animal is TAnimal);
  WriteLn('Animal is TChien : ', Animal is TChien);
  WriteLn;
  WriteLn('Chien is TObject : ', Chien is TObject);
  WriteLn('Chien is TAnimal : ', Chien is TAnimal);
  WriteLn('Chien is TChien : ', Chien is TChien);
  WriteLn;

  Animal.Free;
  Chien.Free;
end;

procedure TesterPolymorphisme;
var
  Animal: TAnimal;
  Chien: TChien;
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('TEST DE POLYMORPHISME');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  Chien := TChien.Create('Max', 'Labrador');
  Animal := Chien;  // Polymorphisme

  WriteLn('Variable Animal pointe vers un Chien');
  WriteLn('Animal.ClassName = ', Animal.ClassName);  // "TChien"
  WriteLn;

  AfficherInfosClasse(Animal);

  Animal.Afficher;  // Appelle TChien.Afficher (polymorphisme)

  Chien.Free;
end;

procedure DemoFreeAndNil;
var
  Obj1, Obj2: TAnimal;
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('DEMO FREE vs FREEANDNIL');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  // Test avec Free
  WriteLn('--- Test avec Free ---');
  Obj1 := TAnimal.Create('Test1');
  WriteLn('Avant Free : Obj1 = ', IntToHex(PtrUInt(Obj1), 8));
  Obj1.Free;
  WriteLn('Après Free : Obj1 = ', IntToHex(PtrUInt(Obj1), 8));
  WriteLn('⚠️  Obj1 pointe toujours vers une adresse !');
  WriteLn;

  // Test avec FreeAndNil
  WriteLn('--- Test avec FreeAndNil ---');
  Obj2 := TAnimal.Create('Test2');
  WriteLn('Avant FreeAndNil : Obj2 = ', IntToHex(PtrUInt(Obj2), 8));
  FreeAndNil(Obj2);
  WriteLn('Après FreeAndNil : Obj2 = ', IntToHex(PtrUInt(Obj2), 8));
  WriteLn('✓ Obj2 vaut maintenant nil');
  WriteLn;
end;

{ === Programme principal === }
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('   DEMONSTRATION DE TOBJECT');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  TesterHeritage;
  TesterPolymorphisme;
  DemoFreeAndNil;

  WriteLn('═══════════════════════════════════════════════');
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## La hiérarchie standard FreePascal/Lazarus

### Classes fondamentales

#### TObject
```
TObject (racine)
  - Create, Destroy, Free
  - ClassName, ClassType, InheritsFrom
  - Base de toutes les classes
```

#### TPersistent
```
TPersistent (hérite de TObject)
  - Assign : copie d'objets
  - DefineProperties : persistance
  - Utilisé pour : composants, collections
```

**Exemple :**

```pascal
type
  TPersonne = class(TPersistent)
  private
    FNom: string;
  public
    procedure Assign(Source: TPersistent); override;
    property Nom: string read FNom write FNom;
  end;

procedure TPersonne.Assign(Source: TPersistent);
begin
  if Source is TPersonne then
    FNom := TPersonne(Source).FNom
  else
    inherited Assign(Source);
end;

var
  P1, P2: TPersonne;
begin
  P1 := TPersonne.Create;
  P2 := TPersonne.Create;

  P1.Nom := 'Dupont';
  P2.Assign(P1);  // Copie P1 dans P2

  WriteLn(P2.Nom);  // "Dupont"
end;
```

#### TComponent
```
TComponent (hérite de TPersistent)
  - Owner : gestion de propriété
  - Name : identification
  - Tag : donnée utilisateur
  - FindComponent : recherche
  - Base de tous les composants visuels
```

**Exemple :**

```pascal
var
  Button: TButton;
begin
  Button := TButton.Create(Form1);  // Form1 est le propriétaire
  Button.Parent := Form1;
  Button.Name := 'MonBouton';
  Button.Caption := 'Cliquez-moi';

  // Form1 libérera automatiquement Button
end;
```

#### TControl
```
TControl (hérite de TComponent)
  - Left, Top, Width, Height
  - Visible, Enabled
  - OnClick, OnMouseMove
  - Base des contrôles visuels
```

### Hiérarchie complète

```
TObject
  ↓
TPersistent
  ├─ TCollection
  ├─ TStrings
  │   ├─ TStringList
  │   └─ TMemoStrings
  ├─ TCollectionItem
  ├─ TGraphicsObject
  │   ├─ TFont
  │   ├─ TPen
  │   └─ TBrush
  └─ TComponent
      ├─ TDataSet
      ├─ TMenu
      │   ├─ TMainMenu
      │   └─ TPopupMenu
      └─ TControl
          ├─ TGraphicControl
          │   ├─ TLabel
          │   ├─ TShape
          │   └─ TImage
          └─ TWinControl
              ├─ TForm
              ├─ TButton
              ├─ TEdit
              ├─ TPanel
              ├─ TListBox
              └─ etc.
```

## Méthodes avancées de TObject

### 1. ClassParent : Classe parent

```pascal
class function TObject.ClassParent: TClass;
```

**Exemple :**

```pascal
var
  ParentClass: TClass;
begin
  ParentClass := TChien.ClassParent;
  WriteLn('Parent de TChien : ', ParentClass.ClassName);  // "TAnimal"

  ParentClass := ParentClass.ClassParent;
  WriteLn('Grand-parent : ', ParentClass.ClassName);  // "TObject"
end;
```

### 2. ClassInfo : Informations RTTI

Retourne des informations de type à l'exécution (RTTI).

```pascal
class function TObject.ClassInfo: Pointer;
```

**Utilité** : Introspection avancée, sérialisation, frameworks.

### 3. GetInterface : Support des interfaces

```pascal
function TObject.GetInterface(const IID: TGUID; out Obj): Boolean;
```

Utilisé pour l'implémentation des interfaces (vu au chapitre 12).

## Bonnes pratiques avec TObject

### ✅ À FAIRE

1. **Toujours appeler `inherited` dans les constructeurs**
   ```pascal
   constructor MaClasse.Create;
   begin
     inherited Create;  // OBLIGATOIRE
     // Initialisation
   end;
   ```

2. **Toujours appeler `inherited` dans les destructeurs**
   ```pascal
   destructor MaClasse.Destroy;
   begin
     // Nettoyage
     inherited Destroy;  // OBLIGATOIRE
   end;
   ```

3. **Utiliser `Free` au lieu de `Destroy`**
   ```pascal
   MonObjet.Free;  // ✅ Sécurisé
   ```

4. **Utiliser `FreeAndNil` pour éviter les dangling pointers**
   ```pascal
   FreeAndNil(MonObjet);  // ✅ Libère ET met à nil
   ```

5. **Déclarer les destructeurs avec `override`**
   ```pascal
   destructor Destroy; override;  // ✅ Correct
   ```

### ❌ À ÉVITER

1. **Appeler `Destroy` directement**
   ```pascal
   MonObjet.Destroy;  // ❌ Dangereux si nil
   ```

2. **Oublier `inherited` dans les constructeurs**
   ```pascal
   constructor MaClasse.Create;
   begin
     // ❌ OUBLI de inherited Create
     FAttribut := 10;
   end;
   ```

3. **Ne pas libérer les objets créés**
   ```pascal
   MonObjet := TMaClasse.Create;
   // ❌ FUITE MEMOIRE : pas de Free
   ```

4. **Utiliser un objet après l'avoir libéré**
   ```pascal
   MonObjet.Free;
   MonObjet.Afficher;  // ❌ ERREUR : objet libéré
   ```

## Comparaison avec d'autres langages

### Java
```java
// En Java : Object est la racine
public class MaClasse extends Object {
    // ...
}

// Équivalent Pascal
type
  TMaClasse = class(TObject)
  end;
```

### C#
```csharp
// En C# : object (System.Object) est la racine
public class MaClasse : object {
    // ...
}
```

### Python
```python
# En Python : object est la racine (Python 3)
class MaClasse(object):
    pass
```

**Point commun** : Toutes les classes héritent d'une classe racine universelle.

**Différence** : En Pascal, TObject nécessite une gestion manuelle de la mémoire (`Free`), contrairement à Java/C#/Python qui ont un garbage collector.

## Cas d'usage pratiques

### 1. Affichage d'informations de debug

```pascal
procedure DebugObjet(Obj: TObject);
begin
  if Obj = nil then
  begin
    WriteLn('Objet = nil');
    Exit;
  end;

  WriteLn('Classe : ', Obj.ClassName);
  WriteLn('Taille : ', Obj.InstanceSize, ' octets');
  WriteLn('Adresse : $', IntToHex(PtrUInt(Obj), 8));
end;
```

### 2. Copie polymorphe

```pascal
function CopierObjet(Source: TObject): TObject;
var
  TypeClasse: TClass;
begin
  if Source = nil then
  begin
    Result := nil;
    Exit;
  end;

  TypeClasse := Source.ClassType;
  Result := TypeClasse.Create;  // Crée une instance du même type

  if Source is TPersistent then
    TPersistent(Result).Assign(TPersistent(Source));
end;
```

### 3. Logger universel

```pascal
procedure Log(Obj: TObject; const Message: string);
var
  NomClasse: string;
begin
  if Obj <> nil then
    NomClasse := Obj.ClassName
  else
    NomClasse := 'nil';

  WriteLn('[', DateTimeToStr(Now), '] [', NomClasse, '] ', Message);
end;

// Utilisation
var
  MonChien: TChien;
begin
  MonChien := TChien.Create('Rex', 'Berger');
  Log(MonChien, 'Chien créé');
  MonChien.Free;
end;
```

### 4. Collection générique d'objets

```pascal
type
  TListeObjets = class
  private
    FListe: array of TObject;
  public
    procedure Ajouter(Obj: TObject);
    function GetObjet(Index: Integer): TObject;
    procedure Liberer;
  end;

procedure TListeObjets.Ajouter(Obj: TObject);
var
  L: Integer;
begin
  L := Length(FListe);
  SetLength(FListe, L + 1);
  FListe[L] := Obj;
end;

procedure TListeObjets.Liberer;
var
  i: Integer;
begin
  for i := 0 to High(FListe) do
    FreeAndNil(FListe[i]);
  SetLength(FListe, 0);
end;
```

## Points importants à retenir

### TObject est partout

```pascal
// Ces trois déclarations sont identiques :
type
  MaClasse = class
  end;

  MaClasse = class(TObject)
  end;

  // TObject est toujours la racine
```

### Tout objet EST un TObject

```pascal
var
  Button: TButton;
  Obj: TObject;
begin
  Button := TButton.Create(nil);

  Obj := Button;  // ✅ OK : TButton EST UN TObject

  WriteLn(Obj.ClassName);  // "TButton"

  Button.Free;
end;
```

### Méthodes de classe vs méthodes d'instance

```pascal
// Méthode de CLASSE (pas besoin d'instance)
WriteLn(TChien.ClassName);  // "TChien"

// Méthode d'INSTANCE (nécessite une instance)
var
  Chien: TChien;
begin
  Chien := TChien.Create;
  WriteLn(Chien.InstanceSize);  // Taille de l'instance
  Chien.Free;
end;
```

## Résumé

TObject est la classe racine de la hiérarchie Pascal qui fournit :
- ✅ Les **méthodes de base** : Create, Destroy, Free
- ✅ Les **informations de classe** : ClassName, ClassType, InheritsFrom
- ✅ Le **support du polymorphisme**
- ✅ La **gestion de la mémoire** : Free, FreeAndNil

**Hiérarchie standard :**
```
TObject → TPersistent → TComponent → TControl → TWinControl
```

**Règles essentielles :**
1. Toutes les classes héritent de TObject (explicitement ou implicitement)
2. Toujours appeler `inherited Create` et `inherited Destroy`
3. Utiliser `Free` ou `FreeAndNil`, jamais `Destroy` directement
4. Les destructeurs doivent être `override`

**Méthodes clés :**
- `Create` : constructeur de base
- `Destroy` : destructeur (utiliser `override`)
- `Free` : destruction sécurisée
- `ClassName` : nom de la classe
- `InheritsFrom` : vérification d'héritage
- `is` et `as` : transtypage sécurisé

**Analogie finale** : TObject est comme l'ADN de base que tous les êtres vivants partagent. Chaque classe ajoute ses propres caractéristiques, mais toutes partagent ce code génétique fondamental !

Comprendre TObject, c'est comprendre les fondations de la POO en Pascal. C'est l'ancêtre universel qui donne à toutes les classes leurs super-pouvoirs de base !

⏭️ [Interfaces et POO Avancée](/12-interfaces-poo-avancee/README.md)
