🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 7.9 Documentation des unités

## Pourquoi documenter son code ?

Imaginez que vous devez réparer une voiture sans manuel d'utilisation, ou construire un meuble IKEA sans les instructions. Frustrant, n'est-ce pas ?

C'est exactement ce que ressentent les personnes (y compris **vous-même** dans 6 mois !) qui doivent utiliser ou modifier un code non documenté.

### Les trois raisons principales

1. **Pour les autres développeurs** qui utiliseront votre unité
2. **Pour vous-même** quand vous reviendrez sur votre code plus tard
3. **Pour la maintenance** et l'évolution du projet

## Types de commentaires en Pascal

Pascal offre trois façons de créer des commentaires :

### 1. Commentaires en ligne : //

```pascal
// Ceci est un commentaire sur une seule ligne
x := 10;  // On peut aussi commenter à la fin d'une ligne
```

### 2. Commentaires multi-lignes : { }

```pascal
{
  Ceci est un commentaire
  sur plusieurs lignes
  très utile pour les explications longues
}
```

### 3. Commentaires multi-lignes : (* *)

```pascal
(*
  Alternative aux accolades
  Moins courante mais valide
*)
```

**Conseil :** Utilisez `//` pour les commentaires courts et `{ }` pour les explications longues.

## Documentation d'en-tête d'unité

Chaque unité devrait commencer par un **en-tête documentaire** qui explique son but.

### Exemple d'en-tête complet

```pascal
{*******************************************************************************
  Nom de l'unité    : UniteMaths
  Auteur            : Jean Dupont
  Date de création  : 13/10/2025
  Dernière modif.   : 13/10/2025
  Version           : 1.0

  Description :
    Cette unité fournit des fonctions mathématiques de base pour les calculs
    courants : additions, soustractions, moyennes, etc.

  Dépendances :
    - Math (pour les fonctions avancées)
    - SysUtils (pour la gestion des exceptions)

  Utilisation :
    uses UniteMaths;
    var resultat: Real;
    begin
      resultat := CalculerMoyenne([10, 15, 20]);
      WriteLn(resultat);  // Affiche 15.00
    end;

  Historique des modifications :
    v1.0 (13/10/2025) - Création initiale
*******************************************************************************}

unit UniteMaths;

interface

// Suite du code...
```

### Version minimale (pour débuter)

```pascal
{
  UniteMaths - Fonctions mathématiques de base
  Auteur : Jean Dupont
  Date : 13/10/2025
}

unit UniteMaths;
```

## Documentation des fonctions et procédures

Chaque fonction publique devrait être documentée dans la section `interface`.

### Structure recommandée

```pascal
interface

{
  Calcule la moyenne d'un tableau de nombres.

  Paramètres :
    - valeurs : Tableau de réels à moyenner

  Retourne :
    La moyenne arithmétique des valeurs

  Exceptions :
    - Lève une exception si le tableau est vide

  Exemple :
    moyenne := CalculerMoyenne([10, 15, 20]);  // Retourne 15.0
}
function CalculerMoyenne(valeurs: array of Real): Real;

{
  Affiche un message de bienvenue personnalisé.

  Paramètres :
    - nom : Nom de l'utilisateur
    - age : Âge de l'utilisateur
}
procedure AfficherBienvenue(nom: String; age: Integer);
```

### Version courte (acceptable)

```pascal
// Calcule la moyenne d'un tableau de réels
function CalculerMoyenne(valeurs: array of Real): Real;

// Affiche un message de bienvenue
procedure AfficherBienvenue(nom: String; age: Integer);
```

## Documentation des types et constantes

Les types personnalisés et les constantes importantes méritent aussi une documentation.

```pascal
interface

const
  // Taux de TVA français (20%)
  TAUX_TVA = 0.20;

  // Nombre maximum d'éléments dans une liste
  MAX_ELEMENTS = 1000;

  // Version de l'API
  VERSION_API = '2.1.0';

type
  {
    Représente les informations d'un élève.

    Champs :
      - Nom       : Nom de famille de l'élève
      - Prenom    : Prénom de l'élève
      - DateNaiss : Date de naissance
      - Classe    : Classe actuelle (ex: '5eB')
  }
  TEleve = record
    Nom: String;
    Prenom: String;
    DateNaiss: TDateTime;
    Classe: String;
  end;

  // États possibles d'une connexion réseau
  TEtatConnexion = (ecDeconnecte, ecConnexion, ecConnecte, ecErreur);
```

## Documenter le code dans implementation

### Commentaires d'explication

Expliquez **pourquoi** vous faites quelque chose, pas **quoi** (le code montre déjà le quoi).

```pascal
// ❌ Mauvais commentaire (répète le code)
// Incrémenter i de 1
Inc(i);

// ✅ Bon commentaire (explique le pourquoi)
// Passer à l'élément suivant car le premier est un en-tête
Inc(i);
```

### Commentaires de sections

Pour les fonctions longues, divisez-les en sections logiques.

```pascal
function TraiterFichier(nomFichier: String): Boolean;  
var  
  fichier: TextFile;
  ligne: String;
begin
  Result := False;

  // === Vérification de l'existence ===
  if not FileExists(nomFichier) then
  begin
    WriteLn('Erreur : fichier introuvable');
    Exit;
  end;

  // === Ouverture du fichier ===
  try
    AssignFile(fichier, nomFichier);
    Reset(fichier);
  except
    WriteLn('Erreur : impossible d''ouvrir le fichier');
    Exit;
  end;

  // === Traitement ligne par ligne ===
  try
    while not Eof(fichier) do
    begin
      ReadLn(fichier, ligne);
      // Traiter la ligne...
    end;
    Result := True;
  finally
    // === Nettoyage ===
    CloseFile(fichier);
  end;
end;
```

## Commentaires TODO et FIXME

Utilisez des marqueurs standards pour signaler ce qui reste à faire.

```pascal
// TODO: Ajouter la validation des entrées
function CalculerRemise(prix: Real): Real;  
begin  
  Result := prix * 0.10;
end;

// FIXME: Cette fonction plante avec des nombres négatifs
function CalculerRacine(x: Real): Real;  
begin  
  Result := Sqrt(x);
end;

// HACK: Solution temporaire, à remplacer par un algorithme optimal
function RechercheLineaire(valeur: Integer): Boolean;  
begin  
  // Code...
end;

// NOTE: Cette valeur doit correspondre à celle de la base de données
const
  TIMEOUT = 30;
```

Ces marqueurs sont reconnus par de nombreux éditeurs et IDEs qui peuvent les lister.

## Documentation pour PasDoc

**PasDoc** est un outil qui génère automatiquement de la documentation HTML à partir de vos commentaires spéciaux.

### Format PasDoc

```pascal
{**
  Calcule le prix TTC à partir du prix HT.

  @param(prixHT Prix hors taxes en euros)
  @param(tauxTVA Taux de TVA (ex: 0.20 pour 20%))
  @returns(Le prix TTC)
  @raises(EInvalidOp si le prix est négatif)

  @example(
    <code>
    prixTTC := CalculerPrixTTC(100, 0.20);  // Retourne 120.0
    </code>
  )
}
function CalculerPrixTTC(prixHT, tauxTVA: Real): Real;
```

### Balises PasDoc courantes

| Balise | Usage |
|--------|-------|
| `@param(nom description)` | Décrit un paramètre |
| `@returns(description)` | Décrit la valeur de retour |
| `@raises(Exception description)` | Exceptions possibles |
| `@example(code)` | Exemple d'utilisation |
| `@see(NomAutreElement)` | Référence à un autre élément |
| `@deprecated` | Marque comme obsolète |
| `@author(nom)` | Auteur |
| `@created(date)` | Date de création |

## Bonnes pratiques de documentation

### 1. Documentez l'interface, pas l'implémentation

```pascal
interface

// ✅ Documentation complète ici
function CalculerTotal(prix: Real; quantite: Integer): Real;

implementation

// Pas besoin de redocumenter ici, sauf détails d'implémentation
function CalculerTotal(prix: Real; quantite: Integer): Real;  
begin  
  Result := prix * quantite;
end;
```

### 2. Gardez la documentation à jour

```pascal
// ❌ Documentation obsolète
// Retourne True si le fichier existe
function VerifierFichier(nom: String): Integer;  // Retourne maintenant un code d'erreur !

// ✅ Documentation mise à jour
// Retourne un code d'erreur : 0=OK, 1=introuvable, 2=inaccessible
function VerifierFichier(nom: String): Integer;
```

### 3. Soyez concis mais clair

```pascal
// ❌ Trop verbeux
{
  Cette fonction a été créée pour permettre de réaliser l'addition de deux
  nombres entiers passés en paramètres et de retourner le résultat de cette
  opération mathématique fondamentale qui consiste à...
}
function Additionner(a, b: Integer): Integer;

// ✅ Concis et clair
// Additionne deux entiers et retourne le résultat
function Additionner(a, b: Integer): Integer;
```

### 4. Documentez les cas particuliers

```pascal
{
  Divise deux nombres.

  @param(a Dividende)
  @param(b Diviseur - NE DOIT PAS ÊTRE ZÉRO)
  @returns(Résultat de la division)

  ATTENTION : Cette fonction ne gère pas la division par zéro.
  Le programme plantera si b = 0. Vérifiez avant d'appeler.
}
function Diviser(a, b: Real): Real;
```

### 5. Incluez des exemples pour les fonctions complexes

```pascal
{
  Formate une date selon un patron personnalisé.

  Paramètres :
    - date   : La date à formater
    - patron : Le patron de formatage

  Patrons supportés :
    'dd'   : Jour sur 2 chiffres
    'mm'   : Mois sur 2 chiffres
    'yyyy' : Année sur 4 chiffres
    'hh'   : Heure sur 2 chiffres

  Exemples :
    FormaterDate(Now, 'dd/mm/yyyy')     -> '13/10/2025'
    FormaterDate(Now, 'yyyy-mm-dd')     -> '2025-10-13'
    FormaterDate(Now, 'dd/mm/yyyy hh:nn') -> '13/10/2025 14:30'
}
function FormaterDate(date: TDateTime; patron: String): String;
```

## Documentation des variables globales (à éviter)

Si vous devez absolument utiliser des variables globales, documentez-les bien.

```pascal
implementation

var
  {
    Compteur global des connexions actives.
    Thread-safe : NON - À protéger par section critique.
    Initialisé à 0 dans la section initialization.
  }
  NombreConnexions: Integer;

  {
    Dernier message d'erreur enregistré.
    Utiliser GetDernierErreur() pour y accéder.
  }
  DernierErreur: String;
```

## En-têtes de fichiers pour la licence

Pour les projets open-source ou commerciaux, incluez les informations légales.

```pascal
{*******************************************************************************
  Project   : MonProjet
  Unit      : UniteCalculs

  Copyright (C) 2025 Jean Dupont

  Ce logiciel est fourni sous licence MIT.
  Vous pouvez l'utiliser, le modifier et le distribuer librement.

  Pour plus d'informations, consultez le fichier LICENSE.txt
*******************************************************************************}

unit UniteCalculs;
```

## Outils pour générer la documentation

### 1. PasDoc (recommandé)

PasDoc génère de la documentation HTML, LaTeX, ou PDF depuis vos commentaires.

**Installation :**
- Téléchargez depuis : http://pasdoc.sourceforge.net/
- Utilisez en ligne de commande : `pasdoc --format html MonUnite.pas`

### 2. Lazarus Code Tools

Lazarus peut générer automatiquement des squelettes de documentation.

**Utilisation :**
- Placez le curseur sur une fonction
- Menu **Code** → **Compléter le code** (Ctrl+Shift+C)
- Lazarus génère un squelette de commentaire

### 3. Documentation manuelle

Créez simplement un fichier texte ou markdown expliquant votre unité.

````markdown
# UniteMaths

## Description
Fonctions mathématiques de base.

## Installation
Ajoutez simplement `uses UniteMaths;` dans votre programme.

## Fonctions disponibles

### CalculerMoyenne
Calcule la moyenne d'un tableau de réels.

**Syntaxe :** `function CalculerMoyenne(valeurs: array of Real): Real;`

**Exemple :**
```pascal
moyenne := CalculerMoyenne([10, 15, 20]);  // Retourne 15.0
```
````

## Vérifier la qualité de votre documentation

Posez-vous ces questions :

- [ ] Ai-je expliqué **ce que fait** chaque fonction publique ?
- [ ] Ai-je documenté tous les **paramètres** ?
- [ ] Ai-je indiqué ce que la fonction **retourne** ?
- [ ] Ai-je mentionné les **exceptions** possibles ?
- [ ] Ai-je donné un **exemple** d'utilisation si nécessaire ?
- [ ] Les commentaires sont-ils **à jour** avec le code ?
- [ ] Un développeur externe peut-il **utiliser mon unité** sans me demander d'aide ?

## Exemple d'unité bien documentée

```pascal
{*******************************************************************************
  UniteGestionEleves - Gestion d'une liste d'élèves

  Cette unité fournit des fonctions pour gérer une liste d'élèves :
  ajout, suppression, recherche, et calcul de statistiques.

  Auteur  : Jean Dupont
  Version : 1.2
  Date    : 13/10/2025
*******************************************************************************}

unit UniteGestionEleves;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  {
    Représente un élève avec ses informations de base.
  }
  TEleve = record
    Nom: String;
    Prenom: String;
    Age: Integer;
    Moyenne: Real;
  end;

{
  Ajoute un élève à la liste.

  @param(liste Liste des élèves)
  @param(eleve Élève à ajouter)
  @returns(True si l'ajout a réussi, False sinon)
}
function AjouterEleve(var liste: array of TEleve; const eleve: TEleve): Boolean;

{
  Calcule la moyenne générale de tous les élèves.

  @param(liste Liste des élèves)
  @returns(Moyenne générale ou 0.0 si la liste est vide)
}
function CalculerMoyenneGenerale(const liste: array of TEleve): Real;

{
  Recherche un élève par son nom.

  @param(liste Liste des élèves)
  @param(nom Nom recherché - insensible à la casse)
  @returns(Index de l'élève ou -1 si non trouvé)
}
function RechercherEleve(const liste: array of TEleve; const nom: String): Integer;

implementation

function AjouterEleve(var liste: array of TEleve; const eleve: TEleve): Boolean;  
begin  
  // Implémentation...
  Result := True;
end;

function CalculerMoyenneGenerale(const liste: array of TEleve): Real;  
var  
  i: Integer;
  somme: Real;
begin
  // Vérifier si la liste est vide
  if Length(liste) = 0 then
  begin
    Result := 0.0;
    Exit;
  end;

  // Calculer la somme des moyennes
  somme := 0;
  for i := Low(liste) to High(liste) do
    somme := somme + liste[i].Moyenne;

  // Retourner la moyenne
  Result := somme / Length(liste);
end;

function RechercherEleve(const liste: array of TEleve; const nom: String): Integer;  
var  
  i: Integer;
  nomRecherche: String;
begin
  Result := -1;  // Non trouvé par défaut
  nomRecherche := UpperCase(nom);  // Recherche insensible à la casse

  // Parcourir la liste
  for i := Low(liste) to High(liste) do
  begin
    if UpperCase(liste[i].Nom) = nomRecherche then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

initialization
  // Rien à initialiser

finalization
  // Rien à nettoyer

end.
```

## Résumé

- **Documentez toujours** vos unités et fonctions publiques
- Utilisez des **commentaires clairs** et **concis**
- Incluez un **en-tête** au début de chaque unité
- Documentez les **paramètres**, **retours**, et **exceptions**
- Donnez des **exemples** pour les fonctions complexes
- Expliquez le **pourquoi**, pas seulement le **quoi**
- Utilisez des **marqueurs** (TODO, FIXME) pour le suivi
- Maintenez la documentation **à jour** avec le code
- Considérez **PasDoc** pour générer une documentation automatique
- Une bonne documentation est un **investissement** qui facilite la maintenance

Une unité bien documentée est une unité que vous (et les autres) aimerez utiliser et maintenir !

Vous avez maintenant toutes les connaissances nécessaires pour créer, organiser et documenter vos propres unités Pascal de manière professionnelle.

⏭️ [Gestion des Fichiers et I/O](/08-gestion-fichiers-io/README.md)
