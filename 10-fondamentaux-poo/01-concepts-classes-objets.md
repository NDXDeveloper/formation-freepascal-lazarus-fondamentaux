🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.1 Concepts : Classes et Objets

## Introduction à la Programmation Orientée Objet

Jusqu'à présent, nous avons utilisé la **programmation procédurale** : nous écrivions des procédures et des fonctions qui manipulaient des données. La **Programmation Orientée Objet (POO)** propose une approche différente : elle organise le code autour d'**objets** qui regroupent à la fois les données et les opérations qui les manipulent.

## Comprendre les concepts avec une analogie

Imaginons que nous voulons modéliser une voiture dans un programme.

### Approche procédurale (ce que nous connaissons)

```pascal
var
  MarqueVoiture: string;
  CouleurVoiture: string;
  VitesseVoiture: Integer;

procedure Accelerer(var Vitesse: Integer; Increment: Integer);
begin
  Vitesse := Vitesse + Increment;
end;

procedure Freiner(var Vitesse: Integer; Decrement: Integer);
begin
  Vitesse := Vitesse - Decrement;
  if Vitesse < 0 then
    Vitesse := 0;
end;
```

Dans cette approche, les données (marque, couleur, vitesse) et les actions (accélérer, freiner) sont séparées. Si nous avons plusieurs voitures, cela devient rapidement compliqué.

### Approche orientée objet (nouvelle approche)

En POO, nous créons une **classe** `TVoiture` qui définit ce qu'est une voiture : ses caractéristiques (données) et ses comportements (actions). Ensuite, nous créons des **objets** qui sont des voitures concrètes basées sur ce modèle.

```pascal
type
  TVoiture = class
  private
    FMarque: string;
    FCouleur: string;
    FVitesse: Integer;
  public
    procedure Accelerer(Increment: Integer);
    procedure Freiner(Decrement: Integer);
  end;
```

## Qu'est-ce qu'une Classe ?

Une **classe** est un **modèle** ou un **plan de construction** qui décrit :
- Les **attributs** (données, caractéristiques)
- Les **méthodes** (actions, comportements)

**Analogie** : Une classe est comme un plan d'architecte pour une maison. Le plan décrit ce que sera la maison, mais ce n'est pas la maison elle-même.

### Caractéristiques d'une classe

- Elle définit la **structure** d'un type de données
- Elle regroupe des données et les fonctions qui les manipulent
- Elle sert de modèle pour créer des objets
- Elle est déclarée dans la section `type`

## Qu'est-ce qu'un Objet ?

Un **objet** est une **instance** d'une classe. C'est une réalisation concrète du modèle défini par la classe.

**Analogie** : Si la classe est le plan d'architecte, l'objet est la maison construite d'après ce plan. On peut construire plusieurs maisons (objets) à partir du même plan (classe).

### Créer des objets

```pascal
var
  MaVoiture: TVoiture;
  VoitureDeJean: TVoiture;
begin
  // Création d'objets
  MaVoiture := TVoiture.Create;
  VoitureDeJean := TVoiture.Create;

  // Maintenant nous avons deux voitures distinctes
  // basées sur le même modèle (classe TVoiture)
end;
```

Chaque objet possède :
- Ses propres données (sa marque, sa couleur, sa vitesse)
- Les mêmes méthodes définies dans la classe

## Les Avantages de la POO

### 1. Encapsulation

Les données et les méthodes qui les manipulent sont regroupées dans une même structure. Cela rend le code plus organisé et plus facile à comprendre.

```pascal
// Au lieu de passer la vitesse en paramètre...
Accelerer(VitesseVoiture, 10);

// ...l'objet "sait" quelle est sa vitesse
MaVoiture.Accelerer(10);
```

### 2. Réutilisabilité

Une fois qu'une classe est définie, on peut créer autant d'objets que nécessaire.

```pascal
var
  Voiture1, Voiture2, Voiture3: TVoiture;
begin
  Voiture1 := TVoiture.Create;
  Voiture2 := TVoiture.Create;
  Voiture3 := TVoiture.Create;
  // Trois voitures indépendantes avec le même comportement
end;
```

### 3. Modélisation naturelle

La POO permet de modéliser le monde réel de manière plus intuitive. Les objets du programme correspondent souvent aux objets du monde réel.

### 4. Maintenance facilitée

Quand on modifie une classe, tous les objets bénéficient automatiquement des améliorations.

## Syntaxe de base d'une classe en Pascal

```pascal
type
  TNomClasse = class
  private
    // Attributs privés (données)
    FAttribut1: Type1;
    FAttribut2: Type2;
  public
    // Méthodes publiques (actions)
    procedure Methode1;
    function Methode2: TypeRetour;
  end;
```

### Convention de nommage

- Les noms de classes commencent par **T** (pour "Type") : `TVoiture`, `TPersonne`, `TCompte`
- Les attributs privés commencent par **F** (pour "Field") : `FNom`, `FVitesse`, `FCouleur`
- Cette convention aide à identifier rapidement les classes et les attributs dans le code

## Exemple complet simple

Voici un exemple complet d'une classe `TCompteur` :

```pascal
program ExempleClasse;

{$mode objfpc}{$H+}

type
  TCompteur = class
  private
    FValeur: Integer;
  public
    procedure Incrementer;
    procedure Decrementer;
    procedure Afficher;
  end;

procedure TCompteur.Incrementer;
begin
  FValeur := FValeur + 1;
end;

procedure TCompteur.Decrementer;
begin
  FValeur := FValeur - 1;
end;

procedure TCompteur.Afficher;
begin
  WriteLn('Valeur du compteur : ', FValeur);
end;

var
  MonCompteur: TCompteur;
begin
  // Création de l'objet
  MonCompteur := TCompteur.Create;

  // Utilisation de l'objet
  MonCompteur.Incrementer;
  MonCompteur.Incrementer;
  MonCompteur.Afficher;  // Affiche : Valeur du compteur : 2

  MonCompteur.Decrementer;
  MonCompteur.Afficher;  // Affiche : Valeur du compteur : 1

  // Libération de la mémoire
  MonCompteur.Free;
end.
```

### Analyse du code

1. **Déclaration de la classe** : définit la structure avec un attribut `FValeur` et trois méthodes
2. **Implémentation des méthodes** : chaque méthode est définie en préfixant son nom par `TCompteur.`
3. **Création de l'objet** : `MonCompteur := TCompteur.Create;` crée une instance de la classe
4. **Utilisation** : on appelle les méthodes avec la notation pointée `ObjetNom.Methode`
5. **Libération** : `MonCompteur.Free;` libère la mémoire utilisée par l'objet

## Points importants à retenir

- Une **classe** est un modèle, un **objet** est une instance de ce modèle
- La POO regroupe données et méthodes dans une structure cohérente
- La notation pointée (`Objet.Methode`) permet d'accéder aux méthodes d'un objet
- Chaque objet possède ses propres données, mais partage les méthodes définies dans la classe
- Il faut toujours créer un objet avec `.Create` avant de l'utiliser
- Il faut toujours libérer la mémoire avec `.Free` après utilisation

## Transition vers la suite

Dans les sections suivantes, nous approfondirons ces concepts en explorant :
- L'encapsulation et la visibilité des membres
- Les constructeurs et destructeurs personnalisés
- Les propriétés pour accéder aux attributs de manière contrôlée
- Et bien d'autres aspects de la POO en Pascal

La POO peut sembler complexe au début, mais elle deviendra naturelle avec la pratique. Elle est la base de la programmation moderne et vous permettra de créer des applications plus structurées et maintenables.

⏭️ [Encapsulation et visibilité](/10-fondamentaux-poo/02-encapsulation-visibilite.md)
