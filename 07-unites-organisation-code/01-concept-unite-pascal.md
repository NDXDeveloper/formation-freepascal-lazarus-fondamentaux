🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 7.1 Concept d'unité en Pascal

## Qu'est-ce qu'une unité ?

Une **unité** (ou *unit* en anglais) est un fichier Pascal qui contient du code réutilisable : des procédures, des fonctions, des types de données et des constantes que vous pouvez utiliser dans vos programmes.

Imaginez une unité comme une **boîte à outils** : au lieu de recréer les mêmes outils à chaque fois que vous en avez besoin, vous les rangez dans une boîte et vous les sortez quand nécessaire.

## Pourquoi utiliser des unités ?

### 1. **Réutilisation du code**
Si vous avez écrit une fonction utile pour calculer la moyenne de notes, vous pouvez la placer dans une unité et l'utiliser dans tous vos programmes sans la réécrire.

### 2. **Organisation du code**
Au lieu d'avoir un seul fichier de 2000 lignes, vous pouvez diviser votre code en plusieurs unités thématiques :
- Une unité pour les calculs mathématiques
- Une unité pour la gestion des fichiers
- Une unité pour l'affichage

### 3. **Travail en équipe**
Chaque membre de l'équipe peut travailler sur une unité différente sans interférer avec les autres.

### 4. **Maintenance facilitée**
Si vous devez corriger un bug ou améliorer une fonction, vous savez exactement dans quelle unité la chercher.

## Analogie avec la vie réelle

Pensez à une bibliothèque :
- **Le programme principal** = Le lecteur qui a besoin de livres
- **Les unités** = Les étagères thématiques (histoire, sciences, littérature)
- **La clause Uses** = La carte de bibliothèque qui indique quelles étagères vous souhaitez consulter

Vous n'avez pas besoin d'avoir tous les livres chez vous. Vous allez à la bibliothèque et vous empruntez seulement ceux dont vous avez besoin.

## Structure minimale d'un programme avec unité

```pascal
program MonProgramme;

uses
  MaUnite;  // On indique qu'on veut utiliser l'unité "MaUnite"

begin
  // Ici, on peut utiliser les fonctions définies dans MaUnite
end.
```

## Les unités que vous utilisez déjà

Même si vous ne le savez peut-être pas, vous utilisez déjà des unités ! Par exemple :

```pascal
program Exemple;

uses
  SysUtils,  // Unité pour les fonctions système
  Math;      // Unité pour les fonctions mathématiques

var
  resultat: Real;

begin
  resultat := Power(2, 10);  // Power vient de l'unité Math
  WriteLn(resultat:0:0);     // Affiche 1024
end.
```

Dans cet exemple, la fonction `Power` (puissance) n'est pas définie par vous : elle vient de l'unité `Math` que FreePascal fournit automatiquement.

## Unités standards de FreePascal

FreePascal fournit de nombreuses unités prêtes à l'emploi :

| Unité | Utilité |
|-------|---------|
| `SysUtils` | Fonctions système (gestion de chaînes, fichiers, dates) |
| `Math` | Fonctions mathématiques avancées |
| `Classes` | Classes de base pour la programmation orientée objet |
| `StrUtils` | Manipulation avancée de chaînes de caractères |
| `DateUtils` | Manipulation de dates et heures |

## Quand créer ses propres unités ?

Vous devriez créer une unité quand :

1. **Vous répétez le même code** dans plusieurs programmes
2. **Votre programme devient trop long** (plus de 300-400 lignes)
3. **Vous voulez organiser votre code** par thématiques
4. **Vous travaillez en équipe** et chacun a une responsabilité différente

## Exemple concret

Imaginons que vous créez plusieurs programmes qui ont besoin de calculer des moyennes :

**Sans unité** : Vous copiez-collez cette fonction dans chaque programme
```pascal
function CalculerMoyenne(a, b, c: Real): Real;  
begin  
  Result := (a + b + c) / 3;
end;
```

**Avec unité** : Vous la placez une fois dans une unité `UniteMaths` et vous l'utilisez partout où vous en avez besoin !

## Résumé

- Une **unité** est un fichier contenant du code réutilisable
- Les unités permettent d'**organiser** et de **réutiliser** votre code
- On utilise la clause **uses** pour indiquer quelles unités on veut utiliser
- FreePascal fournit de nombreuses unités standards
- Vous pouvez créer vos propres unités pour organiser votre code

Dans les sections suivantes, nous verrons comment créer concrètement une unité et comment l'utiliser dans vos programmes.

⏭️ [Structure d'une unité (interface/implementation)](/07-unites-organisation-code/02-structure-unite-interface-implementation.md)
