🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 20.3 Inspection de Variables et Expressions

## Introduction

L'inspection de variables est au cœur du débogage. Une fois que vous avez arrêté votre programme au bon endroit (grâce aux points d'arrêt), il est temps d'examiner l'état de vos données. Lazarus offre plusieurs méthodes pour inspecter les variables, des plus simples aux plus sophistiquées. Cette section vous apprendra à utiliser tous ces outils pour comprendre exactement ce qui se passe dans votre programme.

**Ce que vous allez apprendre :**
- Les différentes méthodes d'inspection des variables
- Comment utiliser les fenêtres de débogage spécialisées
- L'évaluation et la modification d'expressions en temps réel
- L'inspection de structures complexes (tableaux, records, objets)
- Les techniques avancées pour des diagnostics efficaces

---

## 1. Méthodes Rapides d'Inspection

### 1.1 Info-bulles de Débogage (Hover Tooltips)

**La méthode la plus simple et la plus rapide.**

**Comment l'utiliser :**
1. Mettez votre programme en pause (point d'arrêt ou F8)
2. Placez le curseur de votre souris sur une variable dans le code
3. Attendez 1 seconde
4. Une info-bulle apparaît avec la valeur

**Exemple :**

```pascal
procedure Calculer;  
var  
  x: Integer;
  prix: Double;
  nom: String;
begin
  x := 42;
  prix := 19.99;
  nom := 'Produit';
  // Point d'arrêt ici
  WriteLn(x, prix, nom);
end;
```

Si vous arrêtez le programme après les affectations et survolez :
- `x` → affiche `42`
- `prix` → affiche `19.99`
- `nom` → affiche `'Produit'`

**Avantages :**
- Très rapide
- Pas besoin d'ouvrir de fenêtre
- Idéal pour une inspection ponctuelle

**Limitations :**
- Affichage limité pour les structures complexes
- Ne fonctionne que pendant la pause
- Pas de modification possible

### 1.2 Sélection et Évaluation Rapide

**Technique pour évaluer une expression complexe rapidement.**

**Méthode :**
1. Sélectionnez une expression dans le code (ex: `Tableau[i] + 10`)
2. Survolez la sélection
3. L'info-bulle affiche le résultat de l'expression

**Exemple :**

```pascal
var
  tab: array[1..10] of Integer;
  i: Integer;
begin
  i := 5;
  tab[5] := 100;
  // Point d'arrêt ici
  WriteLn(tab[i] * 2);
end;
```

**Sélectionnez `tab[i] * 2`** → L'info-bulle affiche `200`

**Cas d'usage :**
- Vérifier un calcul sans le coder
- Tester une condition avant de l'écrire
- Comprendre une expression complexe

### 1.3 Évaluation Express avec Ctrl+F7

**Raccourci clavier pour évaluer n'importe quelle expression.**

**Utilisation :**
1. Programme en pause
2. Appuyez sur **Ctrl+F7** (Windows/Linux)
3. Une boîte de dialogue **"Évaluer/Modifier"** s'ouvre
4. Entrez votre expression
5. Cliquez **Évaluer**

**Exemple d'expressions :**

```pascal
// Variables simples
x  
prix * 1.2  
Length(nom)  

// Expressions complexes
(a + b) / 2
Tableau[i] + Tableau[i+1]  
UpperCase(prenom + ' ' + nom)  

// Appels de fonctions
Sqrt(valeur)  
Pos('test', chaine)  
FormatFloat('0.00', prix)  
```

**Avantage majeur :** Vous pouvez tester des expressions qui n'existent pas dans votre code !

---

## 2. Fenêtre Variables Locales

### 2.1 Présentation

La fenêtre Variables Locales affiche automatiquement toutes les variables accessibles dans le contexte actuel.

**Accès :** Menu **Voir** → **Fenêtres de débogage** → **Variables locales**

**Contenu affiché :**
- Toutes les variables de la procédure/fonction courante
- Les paramètres de la fonction
- Les variables globales (selon configuration)
- Mise à jour automatique à chaque pas de débogage

### 2.2 Comprendre l'Affichage

**Structure de la fenêtre :**

| Colonne | Contenu |
|---------|---------|
| **Nom** | Nom de la variable |
| **Valeur** | Valeur actuelle |
| **Type** | Type de données |

**Exemple de lecture :**

```pascal
procedure ExempleSimple;  
var  
  compteur: Integer;
  ratio: Double;
  actif: Boolean;
  message: String;
begin
  compteur := 10;
  ratio := 0.75;
  actif := True;
  message := 'Traitement en cours';
  // Point d'arrêt ici
end;
```

**Affichage dans Variables Locales :**
```
Nom         | Valeur                    | Type
------------|---------------------------|----------
compteur    | 10                        | Integer  
ratio       | 0.75                      | Double  
actif       | True                      | Boolean  
message     | 'Traitement en cours'     | String  
```

### 2.3 Navigation dans les Structures

**Variables structurées (Records, Objets) :**

Les structures complexes peuvent être **dépliées** :

```pascal
type
  TPersonne = record
    Nom: String;
    Prenom: String;
    Age: Integer;
  end;

var
  personne: TPersonne;
begin
  personne.Nom := 'Dupont';
  personne.Prenom := 'Jean';
  personne.Age := 30;
  // Point d'arrêt ici
end;
```

**Affichage hiérarchique :**
```
► personne : TPersonne
  ├─ Nom : 'Dupont'
  ├─ Prenom : 'Jean'
  └─ Age : 30
```

**Cliquez sur le ►** (ou le triangle) pour déplier/replier.

### 2.4 Tableaux dans Variables Locales

**Tableaux statiques :**

```pascal
var
  nombres: array[1..5] of Integer;
begin
  nombres[1] := 10;
  nombres[2] := 20;
  nombres[3] := 30;
  nombres[4] := 40;
  nombres[5] := 50;
  // Point d'arrêt ici
end;
```

**Affichage :**
```
► nombres : array[1..5] of Integer
  ├─ [1] : 10
  ├─ [2] : 20
  ├─ [3] : 30
  ├─ [4] : 40
  └─ [5] : 50
```

**Tableaux dynamiques :**

```pascal
var
  liste: array of String;
begin
  SetLength(liste, 3);
  liste[0] := 'Premier';
  liste[1] := 'Deuxième';
  liste[2] := 'Troisième';
  // Point d'arrêt ici
end;
```

**Affichage :**
```
► liste : array of String (Length: 3)
  ├─ [0] : 'Premier'
  ├─ [1] : 'Deuxième'
  └─ [2] : 'Troisième'
```

### 2.5 Variables Non Initialisées

**Important :** Les variables non initialisées affichent des valeurs aléatoires !

```pascal
var
  x: Integer;
  s: String;
begin
  // Point d'arrêt ici - AVANT initialisation
  x := 10;
  s := 'Test';
end;
```

**Affichage au point d'arrêt :**
```
x : 32764  (valeur aléatoire ! ⚠️)  
s : ''     (chaîne vide par défaut)  
```

**Leçon :** Si vous voyez une valeur bizarre, vérifiez que la variable a bien été initialisée avant ce point.

---

## 3. Fenêtre Inspecteur

### 3.1 Principe de l'Inspecteur

L'inspecteur permet d'examiner **en détail** une variable spécifique, en particulier les structures complexes.

**Accès :** Menu **Voir** → **Fenêtres de débogage** → **Inspecteur**

**Ou bien :**
- Clic droit sur une variable → **Inspecter**
- Sélectionnez la variable et appuyez sur **Alt+F5**

### 3.2 Inspecter un Enregistrement (Record)

**Exemple complet :**

```pascal
type
  TAdresse = record
    Rue: String;
    Ville: String;
    CodePostal: String;
  end;

  TPersonne = record
    Nom: String;
    Prenom: String;
    Age: Integer;
    Adresse: TAdresse;
  end;

var
  personne: TPersonne;
begin
  personne.Nom := 'Martin';
  personne.Prenom := 'Sophie';
  personne.Age := 28;
  personne.Adresse.Rue := '15 rue de la Paix';
  personne.Adresse.Ville := 'Paris';
  personne.Adresse.CodePostal := '75001';
  // Point d'arrêt ici
end;
```

**Dans l'inspecteur sur `personne` :**

```
personne : TPersonne
├─ Nom : 'Martin'
├─ Prenom : 'Sophie'
├─ Age : 28
└─ Adresse : TAdresse
   ├─ Rue : '15 rue de la Paix'
   ├─ Ville : 'Paris'
   └─ CodePostal : '75001'
```

**Navigation :** Double-cliquez sur `Adresse` pour voir ses sous-champs.

### 3.3 Inspecter un Tableau

**Tableaux de grande taille :**

```pascal
var
  notes: array[1..100] of Integer;
  i: Integer;
begin
  for i := 1 to 100 do
    notes[i] := Random(20) + 1;
  // Point d'arrêt ici
end;
```

**L'inspecteur affiche :**
- Toutes les 100 valeurs
- Possibilité de faire défiler
- Recherche d'une valeur spécifique (selon version)

**Astuce :** Pour voir uniquement une plage :
- Variables Locales → `notes[50]` (entrez directement l'indice)
- Ou ajoutez un espion : `notes[50]`

### 3.4 Inspecter un Objet

**Classes et objets :**

```pascal
type
  TCompte = class
  private
    FSolde: Double;
  public
    Numero: String;
    Proprietaire: String;
    property Solde: Double read FSolde write FSolde;
    constructor Create(ANumero: String);
  end;

var
  compte: TCompte;
begin
  compte := TCompte.Create('FR123456');
  compte.Proprietaire := 'Durand';
  compte.Solde := 1500.50;
  // Point d'arrêt ici
end;
```

**Inspecteur :**

```
compte : TCompte ($12A4F680)  ← Adresse mémoire de l'objet
├─ Numero : 'FR123456'
├─ Proprietaire : 'Durand'
├─ FSolde : 1500.50
└─ [Méthodes...] (selon configuration)
```

**Note :** `$12A4F680` est l'adresse mémoire de l'objet (hexadécimal).

### 3.5 Pointeurs et Références

**Inspecter un pointeur :**

```pascal
var
  p: ^Integer;
  x: Integer;
begin
  x := 42;
  p := @x;
  // Point d'arrêt ici
end;
```

**Inspecteur sur `p` :**

```
p : ^Integer ($0012FF7C)  ← Adresse pointée
└─ p^ : 42                 ← Valeur pointée
```

**Double affichage :**
1. L'adresse mémoire que contient le pointeur
2. La valeur à cette adresse (en dépliant `p^`)

**Pointeur nil :**

```pascal
var
  p: ^Integer;
begin
  p := nil;
  // Point d'arrêt ici
end;
```

**Affichage :**
```
p : ^Integer (nil)
```

---

## 4. Fenêtre Espions (Watches)

### 4.1 Concept des Espions

Les espions sont des **variables ou expressions** que vous choisissez de surveiller en permanence, indépendamment du contexte.

**Différence avec Variables Locales :**
- **Variables Locales** : Affiche automatiquement TOUT dans le contexte actuel
- **Espions** : Affiche UNIQUEMENT ce que vous avez choisi, même hors contexte

**Accès :** Menu **Voir** → **Fenêtres de débogage** → **Espions**

### 4.2 Ajouter un Espion

**Méthode 1 : Depuis le code**
1. Sélectionnez une variable dans le code
2. Clic droit → **Ajouter un espion**
3. Ou appuyez sur **Ctrl+F5**

**Méthode 2 : Depuis la fenêtre Espions**
1. Ouvrez la fenêtre Espions
2. Cliquez sur le bouton **"+"** ou **"Ajouter"**
3. Entrez le nom de la variable ou l'expression
4. Cliquez **OK**

**Exemple :**

```pascal
procedure TraiterDonnees;  
var  
  i: Integer;
  total: Double;
  compteur: Integer;
begin
  total := 0;
  compteur := 0;
  for i := 1 to 100 do
  begin
    total := total + Tableau[i];
    Inc(compteur);
    // Vous voulez surveiller i, total, compteur en permanence
  end;
end;
```

**Ajoutez les espions :** `i`, `total`, `compteur`

### 4.3 Expressions en Espion

**La puissance des espions :** Vous pouvez espionner des **expressions calculées**, pas seulement des variables !

**Exemples d'expressions :**

```pascal
// Calculs
total / compteur                    // Moyenne
(prix * quantite) * 1.2            // Prix TTC
Length(nom) + Length(prenom)       // Longueur totale

// Comparaisons (résultat Boolean)
solde > 1000                       // True ou False
(i mod 10) = 0                     // True tous les 10

// Accès tableau/record
Tableau[i]                         // Élément courant  
client.Adresse.Ville               // Ville du client  
ListeClients[0].Nom                // Premier client  

// Fonctions
UpperCase(nom)                     // Nom en majuscules  
Sqrt(valeur)                       // Racine carrée  
FormatDateTime('dd/mm/yyyy', date) // Date formatée  
```

### 4.4 Gestion des Espions

**Fenêtre Espions - Actions disponibles :**

| Action | Description |
|--------|-------------|
| **Ajouter** | Créer un nouvel espion |
| **Supprimer** | Retirer un espion |
| **Modifier** | Changer l'expression |
| **Activer/Désactiver** | Cocher/décocher |
| **Tout supprimer** | Nettoyer la liste |

**Organisation :**
- Gardez uniquement les espions utiles actifs
- Supprimez les espions temporaires après usage
- Groupez les espions par fonctionnalité (selon version)

### 4.5 Espions et Portée (Scope)

**Important :** Un espion peut être **hors de portée** (out of scope).

**Exemple :**

```pascal
procedure A;  
var  
  x: Integer;
begin
  x := 10;
  B();  // Appel de B
end;

procedure B;  
var  
  y: Integer;
begin
  y := 20;
  // Point d'arrêt ici
end;
```

**Si vous avez un espion sur `x` et que vous êtes dans `B` :**
```
Espion: x  
Valeur: <hors de portée> ou <unavailable>  
```

**Pourquoi ?** La variable `x` n'existe que dans la fonction `A`.

**Solution :** Les espions restent affichés, ils se mettront à jour quand vous reviendrez dans la portée appropriée.

### 4.6 Espions Conditionnels

**Certaines versions avancées** permettent des espions qui ne s'activent que sous condition.

**Exemple (si supporté) :**
- Espion : `Tableau[i]`
- Condition : `i > 50`
- **Résultat :** L'espion n'affiche une valeur que si `i > 50`

**Note :** Fonctionnalité variable selon la version de Lazarus/GDB.

---

## 5. Évaluation et Modification d'Expressions

### 5.1 Fenêtre Évaluer/Modifier

**Accès :**
- Menu **Exécuter** → **Évaluer/Modifier**
- Raccourci : **Ctrl+F7**

**Deux modes :**
1. **Évaluer** : Calculer la valeur d'une expression
2. **Modifier** : Changer la valeur d'une variable

### 5.2 Évaluer une Expression

**Processus :**
1. Programme en pause
2. Ouvrez Évaluer/Modifier (Ctrl+F7)
3. Entrez l'expression dans **"Expression"**
4. Cliquez **"Évaluer"**
5. Le résultat apparaît dans **"Résultat"**

**Exemples pratiques :**

```pascal
procedure Calculer;  
var  
  a, b, c: Integer;
  prix: Double;
  nom: String;
begin
  a := 10;
  b := 20;
  c := 5;
  prix := 99.99;
  nom := 'Produit';
  // Point d'arrêt ici
end;
```

**Expressions à tester :**

| Expression | Résultat | Utilité |
|------------|----------|---------|
| `a + b` | `30` | Vérifier un calcul |
| `a * b - c` | `195` | Tester une formule |
| `prix * 1.2` | `119.988` | Prix avec taxe |
| `Length(nom)` | `7` | Longueur de chaîne |
| `(a > 5) and (b < 30)` | `True` | Tester une condition |
| `UpperCase(nom)` | `'PRODUIT'` | Transformation |

**Avantage :** Vous testez des calculs sans modifier le code !

### 5.3 Modifier une Variable

**Processus :**
1. Programme en pause
2. Ouvrez Évaluer/Modifier (Ctrl+F7)
3. Entrez le nom de la variable dans **"Expression"**
4. Entrez la nouvelle valeur dans **"Nouvelle valeur"**
5. Cliquez **"Modifier"**

**Exemple :**

```pascal
procedure Traiter;  
var  
  compteur: Integer;
  valide: Boolean;
begin
  compteur := 10;
  valide := False;
  // Point d'arrêt ici

  if valide then
    ProcesserDonnees(compteur);
end;
```

**Scénario :** Vous voulez tester le code comme si `valide = True` et `compteur = 50`.

**Actions :**
1. Modifiez `valide` → `True`
2. Modifiez `compteur` → `50`
3. Continuez l'exécution (F9)
4. Le code s'exécute avec les nouvelles valeurs !

**Cas d'usage :**
- Tester un cas limite sans relancer le programme
- Contourner une condition pour tester une branche spécifique
- Corriger une valeur temporairement pour continuer le débogage

### 5.4 Limitations de la Modification

**Ce que vous POUVEZ modifier :**
- Variables simples (Integer, Double, String, Boolean...)
- Champs d'enregistrements
- Éléments de tableaux

**Ce que vous NE POUVEZ PAS modifier :**
- Constantes
- Expressions calculées (ex: `a + b`)
- Paramètres `const`
- Certaines propriétés en lecture seule

**Attention :** Les modifications sont temporaires ! Elles ne changent pas le code source.

---

## 6. Inspection de Structures Complexes

### 6.1 Tableaux Multidimensionnels

```pascal
var
  matrice: array[1..3, 1..3] of Integer;
  i, j: Integer;
begin
  for i := 1 to 3 do
    for j := 1 to 3 do
      matrice[i, j] := i * 10 + j;
  // Point d'arrêt ici
end;
```

**Inspecteur :**

```
► matrice : array[1..3, 1..3] of Integer
  ► [1]
    ├─ [1,1] : 11
    ├─ [1,2] : 12
    └─ [1,3] : 13
  ► [2]
    ├─ [2,1] : 21
    ├─ [2,2] : 22
    └─ [2,3] : 23
  ► [3]
    ├─ [3,1] : 31
    ├─ [3,2] : 32
    └─ [3,3] : 33
```

**Espions ciblés :**
- `matrice[2,2]` → Affiche `22`
- `matrice[i,j]` → Valeur de la cellule courante

### 6.2 Tableaux d'Enregistrements

```pascal
type
  TProduit = record
    Nom: String;
    Prix: Double;
    Stock: Integer;
  end;

var
  produits: array[1..3] of TProduit;
begin
  produits[1].Nom := 'Clavier';
  produits[1].Prix := 29.99;
  produits[1].Stock := 50;

  produits[2].Nom := 'Souris';
  produits[2].Prix := 19.99;
  produits[2].Stock := 100;

  produits[3].Nom := 'Écran';
  produits[3].Prix := 199.99;
  produits[3].Stock := 20;
  // Point d'arrêt ici
end;
```

**Inspecteur :**

```
► produits : array[1..3] of TProduit
  ► [1] : TProduit
    ├─ Nom : 'Clavier'
    ├─ Prix : 29.99
    └─ Stock : 50
  ► [2] : TProduit
    ├─ Nom : 'Souris'
    ├─ Prix : 19.99
    └─ Stock : 100
  ► [3] : TProduit
    ├─ Nom : 'Écran'
    ├─ Prix : 199.99
    └─ Stock : 20
```

**Espion utile :**
```
produits[i].Nom     // Nom du produit courant dans une boucle  
produits[i].Stock < 30  // True si stock faible  
```

### 6.3 Listes Chaînées et Structures Dynamiques

```pascal
type
  PNoeud = ^TNoeud;
  TNoeud = record
    Valeur: Integer;
    Suivant: PNoeud;
  end;

var
  premier, courant: PNoeud;
begin
  New(premier);
  premier^.Valeur := 10;
  New(premier^.Suivant);
  premier^.Suivant^.Valeur := 20;
  premier^.Suivant^.Suivant := nil;

  courant := premier;
  // Point d'arrêt ici
end;
```

**Inspecteur sur `premier` :**

```
► premier : PNoeud ($00A1B2C0)
  ├─ premier^.Valeur : 10
  └─ premier^.Suivant : PNoeud ($00A1B2D4)
     ├─ premier^.Suivant^.Valeur : 20
     └─ premier^.Suivant^.Suivant : nil
```

**Navigation :** Dépliez chaque niveau pour parcourir la liste.

**Espions :**
```
courant^.Valeur           // Valeur du nœud courant  
courant^.Suivant <> nil   // True s'il y a un suivant  
```

### 6.4 Objets avec Héritage

```pascal
type
  TAnimal = class
    Nom: String;
    Age: Integer;
  end;

  TChien = class(TAnimal)
    Race: String;
    Aboie: Boolean;
  end;

var
  monChien: TChien;
begin
  monChien := TChien.Create;
  monChien.Nom := 'Rex';
  monChien.Age := 5;
  monChien.Race := 'Labrador';
  monChien.Aboie := True;
  // Point d'arrêt ici
end;
```

**Inspecteur :**

```
► monChien : TChien ($00D3E4F0)
  ├─ Nom : 'Rex'          (hérité de TAnimal)
  ├─ Age : 5              (hérité de TAnimal)
  ├─ Race : 'Labrador'    (propre à TChien)
  └─ Aboie : True         (propre à TChien)
```

**Note :** L'inspecteur affiche à la fois les champs de la classe de base et de la classe dérivée.

---

## 7. Techniques Avancées d'Inspection

### 7.1 Inspection de Mémoire Brute

**Pour les développeurs avancés :** Voir le contenu brut de la mémoire.

**Accès (si disponible) :** Menu **Voir** → **Fenêtres de débogage** → **Dump mémoire**

**Utilisation :**
1. Entrez l'adresse mémoire (en hexadécimal)
2. Spécifiez le nombre d'octets à afficher
3. Visualisez les données brutes

**Cas d'usage :**
- Débogage de structures C importées
- Analyse de corruption mémoire
- Interfaçage avec des DLL système

**Attention :** Technique avancée, rarement nécessaire pour le développement d'applications standard.

### 7.2 Inspection de Types Personnalisés

**Types énumérés :**

```pascal
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  aujourdhui: TJour;
begin
  aujourdhui := Mercredi;
  // Point d'arrêt ici
end;
```

**Affichage :**
```
aujourdhui : Mercredi   (TJour)
```

**Ou selon version :**
```
aujourdhui : 2   (TJour)  ← Position dans l'énumération
```

**Types ensemble (Set) :**

```pascal
type
  TOption = (opLecture, opEcriture, opExecution);
  TOptions = set of TOption;

var
  permissions: TOptions;
begin
  permissions := [opLecture, opEcriture];
  // Point d'arrêt ici
end;
```

**Affichage :**
```
permissions : [opLecture, opEcriture]   (TOptions)
```

### 7.3 Formateurs Personnalisés

**Certaines versions de Lazarus permettent de configurer l'affichage des types personnalisés.**

**Exemple (configuration avancée) :**
- Afficher une date au format `dd/mm/yyyy` au lieu d'un nombre
- Afficher un montant avec le symbole € automatiquement
- Formatter des tableaux d'octets en hexadécimal

**Configuration :** Varie selon la version, consultez la documentation Lazarus.

---

## 8. Pièges Courants et Solutions

### 8.1 Valeurs "Optimisées"

**Problème :** Une variable affiche `<optimized out>` ou `<valeur optimisée>`.

**Cause :** Le compilateur a optimisé la variable (elle n'existe plus en mémoire).

**Solutions :**
1. Désactivez les optimisations : **Projet** → **Options** → **Optimisation : -O-**
2. Recompilez complètement le projet
3. Utilisez le mode Debug au lieu de Release

### 8.2 Variables avec des Caractères Spéciaux

**Problème :** Chaînes avec caractères accentués ou spéciaux mal affichés.

**Cause :** Problème d'encodage (ANSI vs UTF-8).

**Solutions :**
- Vérifiez l'encodage de votre fichier source
- Utilisez UTF-8 pour les sources : **Projet** → **Options** → **Encodage : UTF-8**
- Pour les chaînes Unicode, utilisez `UnicodeString` ou `WideString`

### 8.3 Pointeurs Dangling

**Problème :** Un pointeur affiche une adresse, mais en le dépliant, erreur ou plantage.

**Cause :** Pointeur vers une zone mémoire libérée (dangling pointer).

**Exemple :**

```pascal
var
  p: ^Integer;

procedure InitPointer;  
var  
  x: Integer;
begin
  x := 42;
  p := @x;  // p pointe vers une variable locale
end;  // Fin de la procédure : x n'existe plus !

begin
  InitPointer;
  // Point d'arrêt ici
  WriteLn(p^);  // DANGER : p pointe vers une zone mémoire libérée !
end.
```

**Détection :** L'inspecteur peut afficher des valeurs incohérentes ou `???`.

**Solution :** Ne conservez jamais un pointeur vers une variable locale après la fin de sa portée. Utilisez `New`/`Dispose` ou `GetMem`/`FreeMem` pour de la mémoire persistante.

### 8.4 Objets Non Créés

**Problème :** Un objet affiche `nil` ou cause une erreur à l'inspection.

**Cause :** Oubli du `Create`.

```pascal
var
  client: TClient;
begin
  // Oubli du client := TClient.Create;
  client.Nom := 'Dupont';  // ERREUR : client est nil !
end;
```

**Détection dans l'inspecteur :**
```
client : TClient (nil)   ⚠️
```

**Solution :** Vérifiez que l'objet a été instancié avant utilisation.

---

## 9. Multi-plateforme : Windows vs Linux

### 9.1 Affichage des Variables

**Généralement identique**, mais quelques différences mineures :

| Aspect | Windows | Linux |
|--------|---------|-------|
| **Chemins** | `C:\Dossier\fichier.txt` | `/home/user/fichier.txt` |
| **Fin de ligne** | CRLF (0D 0A) | LF (0A) |
| **Encodage par défaut** | ANSI/Windows-1252 | UTF-8 |
| **Adresses mémoire** | Format hexadécimal identique | Format hexadécimal identique |

### 9.2 Spécificités Linux

**Installation de GDB :**

Si l'inspection ne fonctionne pas sous Linux :

```bash
sudo apt update  
sudo apt install gdb  
sudo apt install fpc-source   # Sources FreePascal (recommandé)  
```

**Permissions :**

Certains déboguages système peuvent nécessiter des permissions :

```bash
# Pour déboguer des processus privilégiés
sudo gdb
```

**Note :** Rarement nécessaire pour les applications normales.

### 9.3 Encodage des Chaînes

**Windows :**
- Par défaut ANSI (Windows-1252)
- Les accents peuvent poser problème

**Linux :**
- Par défaut UTF-8
- Meilleure gestion des caractères internationaux

**Solution universelle :**
- Utilisez UTF-8 pour vos sources
- Configuration Lazarus : **Outils** → **Options** → **Éditeur de code** → **Encodage : UTF-8**

---

## 10. Cas Pratiques et Exemples Complets

### 10.1 Débogage d'une Boucle de Traitement

```pascal
procedure TraiterCommandes;  
var  
  i: Integer;
  commande: TCommande;
  total: Double;
  erreurs: Integer;
begin
  total := 0;
  erreurs := 0;

  for i := 1 to ListeCommandes.Count do
  begin
    commande := ListeCommandes[i-1];

    // Point d'arrêt ici avec condition : i = 50

    if commande.Valide then
      total := total + commande.Montant
    else
      Inc(erreurs);
  end;
end;
```

**Inspection au point d'arrêt (i = 50) :**

**Variables Locales :**
```
i         : 50  
total     : 12450.75  
erreurs   : 3  
```

**Espions ajoutés :**
```
commande.Numero         : 'CMD-000050'  
commande.Valide         : True  
commande.Montant        : 250.00  
ListeCommandes.Count    : 150  
total / i               : 249.015    (moyenne)  
```

**Analyse :** À l'itération 50, vous voyez clairement l'état global et pouvez identifier les anomalies.

### 10.2 Analyse d'une Fonction Récursive

```pascal
function Factorielle(n: Integer): Integer;  
begin  
  // Point d'arrêt ici
  if n <= 1 then
    Result := 1
  else
    Result := n * Factorielle(n - 1);
end;

begin
  WriteLn(Factorielle(5));
end.
```

**Au point d'arrêt avec n=3 :**

**Variables Locales :**
```
n      : 3  
Result : 0  (pas encore calculé)  
```

**Pile d'appels :**
```
1. Factorielle (n=3)     ← Position actuelle
2. Factorielle (n=4)
3. Factorielle (n=5)
4. Programme principal
```

**Espion :**
```
n * Factorielle(n-1)    : <en cours de calcul>
```

**Double-cliquez sur "Factorielle (n=4)"** dans la pile pour voir `n=4` dans les variables locales.

### 10.3 Débogage d'un Objet Complexe

```pascal
type
  TCommande = class
  private
    FNumero: String;
    FMontant: Double;
    FClient: TClient;
  public
    property Numero: String read FNumero write FNumero;
    property Montant: Double read FMontant write FMontant;
    property Client: TClient read FClient write FClient;
    function CalculerTTC: Double;
  end;

function TCommande.CalculerTTC: Double;  
begin  
  // Point d'arrêt ici
  Result := FMontant * 1.2;
end;

var
  cmd: TCommande;
begin
  cmd := TCommande.Create;
  cmd.Numero := 'CMD-001';
  cmd.Montant := 100.0;
  cmd.Client := TClient.Create;
  cmd.Client.Nom := 'Durand';

  WriteLn(cmd.CalculerTTC);
end;
```

**Inspecteur sur `Self` (dans la méthode) :**

```
► Self : TCommande ($00A1B2C0)
  ├─ FNumero : 'CMD-001'
  ├─ FMontant : 100.0
  └─ FClient : TClient ($00D3E4F0)
     ├─ Nom : 'Durand'
     └─ ...
```

**Espions :**
```
FMontant * 1.2              : 120.0  
Self.Client.Nom             : 'Durand'  
Self.Montant > 500          : False  
```

---

## 11. Workflow Optimal d'Inspection

### 11.1 Méthodologie en 5 Étapes

**1. Identifier la zone suspecte**
- Placez un point d'arrêt dans la fonction problématique
- Utilisez des points d'arrêt conditionnels si nécessaire

**2. Inspection rapide**
- Survolez les variables clés avec la souris
- Vérifiez les valeurs évidentes

**3. Inspection détaillée**
- Ouvrez Variables Locales pour une vue d'ensemble
- Utilisez l'Inspecteur pour les structures complexes

**4. Surveillance continue**
- Ajoutez des Espions pour les variables critiques
- Ajoutez des expressions calculées pour les métriques

**5. Évaluation et test**
- Utilisez Évaluer/Modifier pour tester des hypothèses
- Modifiez des valeurs pour tester différents scénarios

### 11.2 Checklist de Débogage

**À chaque arrêt de débogage :**

- [ ] Vérifier les paramètres d'entrée de la fonction
- [ ] Inspecter les variables juste avant l'erreur
- [ ] Comparer les valeurs attendues vs réelles
- [ ] Examiner la pile d'appels pour comprendre le contexte
- [ ] Évaluer les expressions clés
- [ ] Tester des valeurs alternatives si nécessaire

---

## 12. Astuces et Raccourcis

### 12.1 Raccourcis Clavier Essentiels

| Action | Raccourci |
|--------|-----------|
| Évaluer/Modifier | Ctrl+F7 |
| Ajouter espion | Ctrl+F5 |
| Variables locales | (via menu Voir) |
| Inspecteur | Alt+F5 |
| Inspecter sélection | Clic droit → Inspecter |

### 12.2 Astuces de Productivité

**Astuce 1 : Copier les Valeurs**

Clic droit sur une variable → **Copier la valeur** (selon version)
→ Collez dans un éditeur pour analyse ultérieure

**Astuce 2 : Espions Temporaires**

Ajoutez rapidement un espion pour un débogage ponctuel, puis supprimez-le après usage.

**Astuce 3 : Expressions "Sentinelles"**

Créez des espions qui détectent les états impossibles :

```
Tableau[i] < 0              // Devrait être toujours False  
compteur > 1000000          // Limite de sécurité  
pointeur = nil              // Devrait être initialisé  
```

**Astuce 4 : Documentation des Espions**

Commentez vos espions (dans un fichier texte) pour vous souvenir de leur utilité dans des sessions de débogage longues.

---

## 13. Résolution de Problèmes Courants

### 13.1 "Cannot evaluate expression"

**Causes :**
- Variable hors de portée
- Expression invalide
- Optimisation du compilateur

**Solutions :**
1. Vérifiez que vous êtes dans le bon contexte
2. Simplifiez l'expression
3. Désactivez les optimisations

### 13.2 Valeurs Incorrectes Affichées

**Causes :**
- Variable non initialisée
- Corruption mémoire
- Type incorrect

**Solutions :**
1. Vérifiez l'initialisation
2. Inspectez les opérations précédentes
3. Vérifiez les conversions de types

### 13.3 Fenêtres de Débogage Vides

**Causes :**
- Programme non en pause
- Informations de débogage manquantes

**Solutions :**
1. Arrêtez le programme à un point d'arrêt
2. Vérifiez les options de compilation (info de débogage activées)
3. Recompilez complètement

---

## 14. Récapitulatif

L'inspection de variables et d'expressions est une compétence fondamentale du débogage. Les outils à maîtriser :

**Niveaux d'Inspection :**
1. **Rapide** : Info-bulles (survol)
2. **Général** : Variables Locales (vue d'ensemble)
3. **Détaillé** : Inspecteur (structures complexes)
4. **Ciblé** : Espions (surveillance continue)
5. **Actif** : Évaluer/Modifier (tests interactifs)

**Règles d'Or :**
- Commencez simple (survol) puis approfondissez si nécessaire
- Utilisez des espions pour les variables critiques
- N'hésitez pas à évaluer des expressions pour comprendre
- Modifiez les valeurs pour tester différents scénarios
- Documentez vos découvertes pour référence future

**Prochaine Étape :** La section 20.4 (Profiling basique) vous apprendra à mesurer le temps d'exécution de votre code et à identifier les goulots de performance.

---

**Multi-plateforme Windows/Ubuntu** ✓  
**Formation FreePascal/Lazarus - Niveau Débutant à Intermédiaire**  

⏭️ [Profiling basique : identifier les goulots](/20-debogage-optimisation/04-profiling-basique-identifier-goulots.md)
