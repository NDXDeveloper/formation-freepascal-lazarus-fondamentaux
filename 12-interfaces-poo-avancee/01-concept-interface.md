🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.1 Concept d'interface

## Introduction : Pourquoi les interfaces ?

Imaginez que vous dirigez une société de transport. Vous avez besoin de véhicules pour livrer des colis. Peu importe que ce soit un camion, une voiture ou un vélo : ce qui compte, c'est que chaque véhicule puisse **démarrer**, **avancer** et **s'arrêter**.

Vous ne vous souciez pas de *comment* chaque véhicule fonctionne en interne (moteur diesel, essence, électrique ou pédalage), vous voulez juste être sûr que tous peuvent effectuer ces actions de base.

**C'est exactement ce qu'est une interface en programmation : un contrat qui garantit qu'un objet peut faire certaines choses, sans se préoccuper de comment il les fait.**

---

## Le problème que les interfaces résolvent

Dans le chapitre précédent, vous avez appris l'héritage. Avec l'héritage, on peut créer une hiérarchie de classes :

```
TAnimal
  ├─ TChien
  └─ TChat
```

Mais que se passe-t-il si vous voulez qu'un `TChien` et un `TRobot` puissent tous les deux effectuer l'action "Marcher" ?

- Un chien est un animal (biologique)
- Un robot est une machine (mécanique)

Ils n'ont **rien en commun** dans leur nature, donc on ne peut pas créer de classe parent commune qui aurait du sens.

**Solution : créer une interface `IMarcheur` que les deux peuvent implémenter !**

---

## Qu'est-ce qu'une interface ?

### Définition simple

Une **interface** est une liste de méthodes (actions) qu'une classe **promet** de fournir, sans dire comment elle va les réaliser.

C'est comme un cahier des charges ou un mode d'emploi :
- ✅ Elle dit **QUOI** faire
- ❌ Elle ne dit **PAS COMMENT** le faire

### Caractéristiques d'une interface

1. **Pas d'implémentation** : une interface ne contient que des déclarations de méthodes, pas de code
2. **Contrat obligatoire** : une classe qui implémente une interface DOIT fournir le code de toutes ses méthodes
3. **Héritage multiple** : une classe peut implémenter plusieurs interfaces (contrairement à l'héritage de classes)
4. **Polymorphisme** : on peut manipuler différents objets via la même interface

---

## Syntaxe de base en FreePascal

### Déclaration d'une interface

```pascal
type
  IMarcheur = interface
    ['{GUID-UNIQUE}']  // Identifiant unique (optionnel mais recommandé)
    procedure Marcher;
    procedure Arreter;
    function ObtenirVitesse: Integer;
  end;
```

**Points importants :**
- Le nom commence généralement par `I` (convention)
- On ne met que des **déclarations** de méthodes
- Pas de mot-clé `public`, `private` : tout est public
- Le GUID (identifiant unique) aide le compilateur

### Implémentation dans une classe

```pascal
type
  TChien = class(TInterfacedObject, IMarcheur)
  private
    FVitesse: Integer;
  public
    procedure Marcher;
    procedure Arreter;
    function ObtenirVitesse: Integer;
  end;

implementation

procedure TChien.Marcher;  
begin  
  FVitesse := 5;
  WriteLn('Le chien court à 4 pattes');
end;

procedure TChien.Arreter;  
begin  
  FVitesse := 0;
  WriteLn('Le chien s''arrête');
end;

function TChien.ObtenirVitesse: Integer;  
begin  
  Result := FVitesse;
end;
```

**Note importante :** La classe hérite de `TInterfacedObject` et implémente l'interface `IMarcheur` (séparés par une virgule).

---

## Exemple complet et simple

Voici un exemple concret pour bien comprendre :

```pascal
program ExempleInterface;

{$mode objfpc}{$H+}

type
  // Déclaration de l'interface
  IVolant = interface
    procedure Voler;
    procedure Atterrir;
  end;

  // Un oiseau qui vole
  // TInterfacedObject gère le comptage de références (pas besoin de Free)
  TOiseau = class(TInterfacedObject, IVolant)
    procedure Voler;
    procedure Atterrir;
  end;

  // Un avion qui vole aussi
  TAvion = class(TInterfacedObject, IVolant)
    procedure Voler;
    procedure Atterrir;
  end;

// Implémentation de TOiseau
procedure TOiseau.Voler;  
begin  
  WriteLn('L''oiseau bat des ailes et s''envole gracieusement');
end;

procedure TOiseau.Atterrir;  
begin  
  WriteLn('L''oiseau se pose sur une branche');
end;

// Implémentation de TAvion
procedure TAvion.Voler;  
begin  
  WriteLn('L''avion décolle avec ses réacteurs');
end;

procedure TAvion.Atterrir;  
begin  
  WriteLn('L''avion se pose sur la piste');
end;

// Procédure qui utilise n'importe quel objet volant
procedure FaireVoler(UnVolant: IVolant);  
begin  
  WriteLn('--- Démarrage du vol ---');
  UnVolant.Voler;
  UnVolant.Atterrir;
  WriteLn('');
end;

var
  MonOiseau: IVolant;  // Type interface : active le comptage de références
  MonAvion: IVolant;
begin
  MonOiseau := TOiseau.Create;
  MonAvion := TAvion.Create;

  // La magie : on utilise la même procédure pour les deux !
  FaireVoler(MonOiseau);
  FaireVoler(MonAvion);

  // Pas besoin de Free : gestion automatique via les interfaces !
end.
```

**Résultat :**
```
--- Démarrage du vol ---
L'oiseau bat des ailes et s'envole gracieusement  
L'oiseau se pose sur une branche  

--- Démarrage du vol ---
L'avion décolle avec ses réacteurs  
L'avion se pose sur la piste  
```

---

## Interface vs Classe : Les différences clés

| Aspect | Classe | Interface |
|--------|--------|-----------|
| **Implémentation** | Contient du code | Seulement des déclarations |
| **Héritage** | Un seul parent | Plusieurs interfaces possibles |
| **Instanciation** | On peut créer des objets | On ne peut PAS créer d'objet |
| **Variables** | Peut avoir des attributs | Pas d'attributs |
| **Constructeur** | Oui | Non |
| **Usage** | Définir un type d'objet | Définir un comportement |

---

## Les avantages des interfaces

### 1. Flexibilité
Des classes totalement différentes peuvent partager le même comportement sans lien de parenté.

### 2. Héritage multiple de comportements
En Pascal, une classe ne peut hériter que d'une seule classe, mais peut implémenter plusieurs interfaces :

```pascal
type
  TCanardRobot = class(TInterfacedObject, INageur, IVolant, IParleur)
    // Implémente les 3 interfaces !
  end;
```

### 3. Code plus modulaire
Vous programmez contre une "façade" (l'interface) et non contre une implémentation spécifique. Cela facilite les changements futurs.

### 4. Tests plus faciles
On peut créer de fausses implémentations (mocks) pour tester le code sans dépendances réelles.

---

## Analogie du monde réel : Les prises électriques

Pensez à une **prise électrique** :
- Elle définit une **interface** : deux trous d'une certaine forme
- N'importe quel appareil avec une **fiche compatible** peut s'y brancher
- Peu importe que ce soit un ordinateur, une lampe ou un grille-pain

La prise ne sait pas ce qui se passe *à l'intérieur* de l'appareil. Elle garantit juste que l'appareil peut recevoir de l'électricité.

**En programmation, c'est pareil :** l'interface garantit qu'un objet peut faire certaines choses, sans se soucier de comment il le fait en interne.

---

## Quand utiliser une interface ?

Utilisez une interface quand :

✅ Plusieurs classes différentes doivent partager un comportement commun  
✅ Vous voulez découpler votre code (le rendre moins dépendant)  
✅ Vous avez besoin de polymorphisme sans héritage  
✅ Vous voulez définir un "contrat" que d'autres développeurs doivent respecter  

Ne vous compliquez pas la vie avec des interfaces si :

❌ Une simple classe suffit  
❌ Vous n'avez qu'une seule implémentation et aucune raison d'en avoir plusieurs  

---

## Résumé

- Une **interface** est un contrat : elle définit **QUOI** faire, pas **COMMENT**
- Elle ne contient que des déclarations de méthodes, pas de code
- Une classe qui implémente une interface **doit** fournir le code de toutes ses méthodes
- Les interfaces permettent le **polymorphisme** : des objets différents peuvent être manipulés de la même façon
- Une classe peut implémenter **plusieurs interfaces** (contrairement à l'héritage)
- Convention : les noms d'interfaces commencent par `I` (ex: `IVolant`, `IMarcheur`)

---

## À retenir pour la suite

Dans les prochaines sections, vous verrez :
- Comment déclarer et implémenter des interfaces en détail
- L'héritage entre interfaces
- Le concept de comptage de références
- Des design patterns utilisant les interfaces

L'important maintenant est de comprendre le **concept** : une interface est un engagement qu'une classe prend pour fournir certaines fonctionnalités, permettant ainsi à des objets différents de collaborer harmonieusement.

⏭️ [Déclaration et implémentation](/12-interfaces-poo-avancee/02-declaration-implementation.md)
