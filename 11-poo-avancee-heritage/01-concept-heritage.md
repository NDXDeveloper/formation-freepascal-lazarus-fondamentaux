🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.1 Concept d'héritage

## Introduction

L'héritage est l'un des piliers fondamentaux de la Programmation Orientée Objet (POO). C'est un mécanisme qui permet à une classe de **récupérer** (hériter) les propriétés et méthodes d'une autre classe, tout en ayant la possibilité d'ajouter ses propres caractéristiques ou de modifier celles héritées.

## Pourquoi l'héritage ?

### Le problème sans héritage

Imaginons que vous développez un programme de gestion pour une bibliothèque. Vous avez besoin de gérer différents types de documents :
- Des livres
- Des magazines
- Des DVD

Sans héritage, vous devriez créer trois classes complètement séparées :

```pascal
type
  TLivre = class
    FTitre: string;
    FAuteur: string;
    FDateAchat: TDateTime;
    procedure Afficher;
  end;

  TMagazine = class
    FTitre: string;
    FEditeur: string;
    FDateAchat: TDateTime;
    procedure Afficher;
  end;

  TDVD = class
    FTitre: string;
    FRealisateur: string;
    FDateAchat: TDateTime;
    procedure Afficher;
  end;
```

**Problème** : On répète beaucoup de code ! Les propriétés `FTitre` et `FDateAchat` sont identiques dans les trois classes. Si on doit ajouter une nouvelle propriété commune (comme un numéro d'inventaire), il faudra modifier les trois classes.

### La solution avec l'héritage

Avec l'héritage, on peut créer une classe **parent** (ou classe de base) contenant les éléments communs, puis des classes **enfants** (ou classes dérivées) qui héritent de ces éléments :

```pascal
type
  // Classe PARENT (ou classe de base)
  TDocument = class
    FTitre: string;
    FDateAchat: TDateTime;
    procedure Afficher;
  end;

  // Classe ENFANT qui hérite de TDocument
  TLivre = class(TDocument)
    FAuteur: string;
    // TLivre possède automatiquement FTitre et FDateAchat
  end;

  // Autre classe ENFANT
  TMagazine = class(TDocument)
    FEditeur: string;
  end;

  // Encore une classe ENFANT
  TDVD = class(TDocument)
    FRealisateur: string;
  end;
```

## La syntaxe de l'héritage

En Pascal/FreePascal, on déclare l'héritage ainsi :

```pascal
type
  TClasseEnfant = class(TClasseParent)
    // Nouveaux attributs et méthodes spécifiques
  end;
```

Le nom de la classe parent est indiqué entre parenthèses après le mot-clé `class`.

## Vocabulaire important

- **Classe parent** / **Classe de base** / **Superclasse** : la classe dont on hérite
- **Classe enfant** / **Classe dérivée** / **Sous-classe** : la classe qui hérite
- **Héritage** : le mécanisme de transmission des caractéristiques
- **Hiérarchie de classes** : l'arbre généalogique des classes

## Un exemple concret et complet

Voici un exemple simple pour bien comprendre :

```pascal
program ExempleHeritage;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  // CLASSE PARENT : Animal
  TAnimal = class
  private
    FNom: string;
    FAge: Integer;
  public
    constructor Create(ANom: string; AAge: Integer);
    procedure SePresenter;
    procedure Manger;
  end;

  // CLASSE ENFANT : Chien hérite de Animal
  TChien = class(TAnimal)
  private
    FRace: string;
  public
    constructor Create(ANom: string; AAge: Integer; ARace: string);
    procedure Aboyer;
  end;

  // CLASSE ENFANT : Chat hérite de Animal
  TChat = class(TAnimal)
  private
    FCouleur: string;
  public
    constructor Create(ANom: string; AAge: Integer; ACouleur: string);
    procedure Miauler;
  end;

{ Implémentation de TAnimal }

constructor TAnimal.Create(ANom: string; AAge: Integer);  
begin  
  FNom := ANom;
  FAge := AAge;
end;

procedure TAnimal.SePresenter;  
begin  
  WriteLn('Je m''appelle ', FNom, ' et j''ai ', FAge, ' ans.');
end;

procedure TAnimal.Manger;  
begin  
  WriteLn(FNom, ' est en train de manger.');
end;

{ Implémentation de TChien }

constructor TChien.Create(ANom: string; AAge: Integer; ARace: string);  
begin  
  inherited Create(ANom, AAge);  // Appel du constructeur parent
  FRace := ARace;
end;

procedure TChien.Aboyer;  
begin  
  WriteLn(FNom, ' aboie : Wouaf wouaf !');
end;

{ Implémentation de TChat }

constructor TChat.Create(ANom: string; AAge: Integer; ACouleur: string);  
begin  
  inherited Create(ANom, AAge);  // Appel du constructeur parent
  FCouleur := ACouleur;
end;

procedure TChat.Miauler;  
begin  
  WriteLn(FNom, ' miaule : Miaou miaou !');
end;

{ Programme principal }
var
  MonChien: TChien;
  MonChat: TChat;
begin
  // Création d'un chien
  MonChien := TChien.Create('Rex', 5, 'Berger Allemand');

  // Le chien peut utiliser les méthodes héritées de TAnimal
  MonChien.SePresenter;    // Méthode héritée
  MonChien.Manger;         // Méthode héritée
  MonChien.Aboyer;         // Méthode propre à TChien

  WriteLn;

  // Création d'un chat
  MonChat := TChat.Create('Félix', 3, 'Tigré');

  // Le chat peut aussi utiliser les méthodes héritées
  MonChat.SePresenter;     // Méthode héritée
  MonChat.Manger;          // Méthode héritée
  MonChat.Miauler;         // Méthode propre à TChat

  // Libération de la mémoire
  MonChien.Free;
  MonChat.Free;

  ReadLn;
end.
```

**Sortie du programme :**
```
Je m'appelle Rex et j'ai 5 ans.  
Rex est en train de manger.  
Rex aboie : Wouaf wouaf !  

Je m'appelle Félix et j'ai 3 ans.  
Félix est en train de manger.  
Félix miaule : Miaou miaou !  
```

## Que se passe-t-il dans cet exemple ?

1. **TAnimal** est la classe parent qui contient les caractéristiques communes à tous les animaux (nom, âge, capacité de se présenter et manger)

2. **TChien** hérite de TAnimal :
   - Il possède automatiquement `FNom`, `FAge`, `SePresenter()` et `Manger()`
   - Il ajoute sa propre propriété `FRace`
   - Il ajoute sa propre méthode `Aboyer()`

3. **TChat** hérite aussi de TAnimal :
   - Il possède aussi `FNom`, `FAge`, `SePresenter()` et `Manger()`
   - Il ajoute sa propre propriété `FCouleur`
   - Il ajoute sa propre méthode `Miauler()`

## Le mot-clé `inherited`

Vous avez remarqué le mot-clé `inherited` dans les constructeurs des classes enfants :

```pascal
constructor TChien.Create(ANom: string; AAge: Integer; ARace: string);  
begin  
  inherited Create(ANom, AAge);  // ← ICI
  FRace := ARace;
end;
```

`inherited` permet d'**appeler la méthode de la classe parent**. C'est essentiel dans les constructeurs pour s'assurer que la partie "parent" de l'objet est correctement initialisée avant d'initialiser la partie "enfant".

**Ordre d'exécution :**
1. Le constructeur enfant est appelé
2. Le constructeur parent est appelé via `inherited`
3. Le reste du code du constructeur enfant s'exécute

## Avantages de l'héritage

### 1. Réutilisation du code
Au lieu de réécrire le même code dans plusieurs classes, on le place une seule fois dans la classe parent.

### 2. Facilité de maintenance
Si on doit modifier une méthode commune, on ne la modifie qu'à un seul endroit (dans la classe parent).

### 3. Organisation logique
Le code reflète les relations naturelles entre les concepts (un chien **est un** animal).

### 4. Extensibilité
On peut facilement ajouter de nouveaux types d'animaux sans toucher au code existant :

```pascal
type
  TOiseau = class(TAnimal)
  private
    FEnvergure: Real;
  public
    procedure Voler;
  end;
```

## Règles importantes

### 1. L'héritage est transitif
Si C hérite de B, et B hérite de A, alors C hérite de A :

```pascal
type
  TVehicule = class
    // propriétés communes
  end;

  TVehiculeTerrestre = class(TVehicule)
    // ajoute des roues
  end;

  TVoiture = class(TVehiculeTerrestre)
    // TVoiture hérite de tout : TVehicule ET TVehiculeTerrestre
  end;
```

### 2. Héritage simple uniquement
En Pascal, une classe ne peut hériter que d'**une seule** classe parent (pas d'héritage multiple comme en C++). Si vous avez besoin de combiner plusieurs comportements, vous utiliserez les **interfaces** (vu plus tard dans le chapitre 12).

### 3. Tout objet hérite de TObject
En FreePascal, toutes les classes héritent implicitement de la classe `TObject` si aucun parent n'est spécifié :

```pascal
type
  MaClasse = class
    // équivalent à : class(TObject)
  end;
```

`TObject` est la "mère de toutes les classes" et fournit des méthodes de base comme `Create`, `Free`, `ClassName`, etc.

## Analogie du monde réel

Pensez à l'héritage comme à un arbre généalogique ou à une classification biologique :

```
                    Animal (classe parent)
                       |
        +--------------+--------------+
        |              |              |
     Mammifère      Oiseau         Poisson
        |              |              |
    +---+---+      +---+---+      +---+---+
    |       |      |       |      |       |
  Chien   Chat  Aigle  Moineau Requin  Thon
```

Chaque niveau hérite des caractéristiques du niveau supérieur tout en ajoutant ses propres spécificités.

## Quand utiliser l'héritage ?

Utilisez l'héritage quand :
- Vous avez une relation **"est un"** (un chien **est un** animal)
- Plusieurs classes partagent des caractéristiques communes
- Vous voulez créer une hiérarchie logique de concepts

N'utilisez PAS l'héritage quand :
- La relation est **"a un"** (une voiture **a un** moteur) → utilisez la composition
- Les classes n'ont pas vraiment de lien logique
- Vous voulez juste réutiliser quelques méthodes → créez une unité de fonctions utilitaires

## Résumé

L'héritage permet de :
- ✅ Créer des hiérarchies de classes logiques
- ✅ Réutiliser le code efficacement
- ✅ Faciliter la maintenance
- ✅ Modéliser les relations du monde réel

**Syntaxe clé :**
```pascal
type
  TEnfant = class(TParent)
    // Nouveaux membres
  end;
```

**Mot-clé important :**
- `inherited` : appelle la méthode de la classe parent

Dans les sections suivantes, nous verrons comment modifier le comportement des méthodes héritées (redéfinition), comment créer des méthodes virtuelles pour le polymorphisme, et bien plus encore !

⏭️ [Classes dérivées](/11-poo-avancee-heritage/02-classes-derivees.md)
