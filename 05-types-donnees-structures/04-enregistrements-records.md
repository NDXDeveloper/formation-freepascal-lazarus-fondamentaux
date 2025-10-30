🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 5.4 Enregistrements (Records)

## Qu'est-ce qu'un enregistrement ?

Un enregistrement (ou **record** en anglais) est une structure de données qui permet de regrouper **plusieurs informations de types différents** sous un même nom. C'est comme une fiche d'information qui contient plusieurs cases, chacune pouvant stocker un type de donnée différent.

### Analogie simple

Imaginez une fiche de renseignements pour un étudiant :
- **Nom** : une chaîne de caractères
- **Prénom** : une chaîne de caractères
- **Age** : un nombre entier
- **Moyenne** : un nombre réel
- **Redoublant** : un booléen

Au lieu de créer 5 variables séparées, on regroupe toutes ces informations dans un seul **enregistrement**.

## Pourquoi utiliser des enregistrements ?

### Sans enregistrement (compliqué)

```pascal
var
  nom1, prenom1: String;
  age1: Integer;
  moyenne1: Real;

  nom2, prenom2: String;
  age2: Integer;
  moyenne2: Real;

  // Et ainsi de suite pour chaque étudiant...
```

### Avec enregistrement (simple et organisé)

```pascal
type
  TEleve = record
    nom: String;
    prenom: String;
    age: Integer;
    moyenne: Real;
  end;

var
  eleve1, eleve2: TEleve;
```

**Avantages :**
- Code plus organisé et lisible
- Facilite la manipulation de données complexes
- Permet de passer toutes les informations en une seule fois
- Correspond mieux aux objets du monde réel

## Déclaration d'un enregistrement

### Syntaxe générale

```pascal
type
  NomType = record
    champ1: Type1;
    champ2: Type2;
    champ3: Type3;
    // ...
  end;
```

### Exemples de déclarations

```pascal
type
  // Enregistrement pour une personne
  TPersonne = record
    nom: String;
    prenom: String;
    age: Integer;
  end;

  // Enregistrement pour une date
  TDate = record
    jour: Integer;
    mois: Integer;
    annee: Integer;
  end;

  // Enregistrement pour un point en 2D
  TPoint = record
    x: Real;
    y: Real;
  end;

  // Enregistrement pour un produit
  TProduit = record
    code: String;
    designation: String;
    prix: Real;
    stock: Integer;
  end;
```

**Convention de nommage :** Le préfixe `T` (pour "Type") est couramment utilisé pour nommer les types personnalisés.

## Déclaration de variables

Une fois le type défini, on peut créer des variables :

```pascal
var
  personne1, personne2: TPersonne;
  aujourdhui: TDate;
  pointA, pointB: TPoint;
  article: TProduit;
```

## Accès aux champs d'un enregistrement

Pour accéder aux champs (les informations) d'un enregistrement, on utilise le **point** (`.`) :

```pascal
nomVariable.nomChamp
```

### Exemple complet

```pascal
program ExempleRecord;
type
  TPersonne = record
    nom: String;
    prenom: String;
    age: Integer;
  end;

var
  personne: TPersonne;
begin
  // Affectation des valeurs
  personne.nom := 'Dupont';
  personne.prenom := 'Jean';
  personne.age := 25;

  // Lecture des valeurs
  WriteLn('Nom : ', personne.nom);
  WriteLn('Prénom : ', personne.prenom);
  WriteLn('Age : ', personne.age, ' ans');

  // Modification d'un champ
  personne.age := personne.age + 1;
  WriteLn('Nouvel âge : ', personne.age, ' ans');
end.
```

**Sortie :**
```
Nom : Dupont
Prénom : Jean
Age : 25 ans
Nouvel âge : 26 ans
```

## Saisie et affichage d'un enregistrement

```pascal
program SaisiePersonne;
type
  TPersonne = record
    nom: String;
    prenom: String;
    age: Integer;
  end;

var
  personne: TPersonne;
begin
  // Saisie des informations
  WriteLn('=== Saisie des informations ===');
  Write('Nom : ');
  ReadLn(personne.nom);
  Write('Prénom : ');
  ReadLn(personne.prenom);
  Write('Age : ');
  ReadLn(personne.age);

  // Affichage récapitulatif
  WriteLn;
  WriteLn('=== Récapitulatif ===');
  WriteLn('Nom complet : ', personne.prenom, ' ', personne.nom);
  WriteLn('Age : ', personne.age, ' ans');
end.
```

## Instruction WITH (avec)

L'instruction `with` permet d'éviter de répéter le nom de la variable pour accéder aux champs :

### Sans WITH (répétitif)

```pascal
personne.nom := 'Dupont';
personne.prenom := 'Jean';
personne.age := 25;
```

### Avec WITH (plus concis)

```pascal
with personne do
begin
  nom := 'Dupont';
  prenom := 'Jean';
  age := 25;
end;
```

### Exemple complet

```pascal
program ExempleWith;
type
  TProduit = record
    code: String;
    designation: String;
    prix: Real;
    stock: Integer;
  end;

var
  article: TProduit;
begin
  with article do
  begin
    code := 'P001';
    designation := 'Clavier mécanique';
    prix := 89.99;
    stock := 15;

    WriteLn('Produit : ', designation);
    WriteLn('Code : ', code);
    WriteLn('Prix : ', prix:0:2, ' €');
    WriteLn('Stock : ', stock, ' unités');
  end;
end.
```

**Attention :** L'instruction `with` peut rendre le code moins lisible si elle est trop imbriquée. Utilisez-la avec modération.

## Copie d'enregistrements

On peut copier facilement un enregistrement complet :

```pascal
program CopieRecord;
type
  TPoint = record
    x: Real;
    y: Real;
  end;

var
  point1, point2: TPoint;
begin
  // Définir le premier point
  point1.x := 10.5;
  point1.y := 20.3;

  // Copier tous les champs en une seule instruction
  point2 := point1;

  WriteLn('Point 1 : (', point1.x:0:1, ', ', point1.y:0:1, ')');
  WriteLn('Point 2 : (', point2.x:0:1, ', ', point2.y:0:1, ')');

  // Modification de point2 n'affecte pas point1
  point2.x := 30.0;
  WriteLn('Après modification de point2 :');
  WriteLn('Point 1 : (', point1.x:0:1, ', ', point1.y:0:1, ')');
  WriteLn('Point 2 : (', point2.x:0:1, ', ', point2.y:0:1, ')');
end.
```

## Enregistrements et procédures/fonctions

### Passer un enregistrement à une procédure

```pascal
program RecordProcedure;
type
  TPersonne = record
    nom: String;
    prenom: String;
    age: Integer;
  end;

procedure AfficherPersonne(p: TPersonne);
begin
  WriteLn('--- Fiche ---');
  WriteLn('Nom : ', p.nom);
  WriteLn('Prénom : ', p.prenom);
  WriteLn('Age : ', p.age, ' ans');
  WriteLn('-------------');
end;

var
  personne: TPersonne;
begin
  personne.nom := 'Martin';
  personne.prenom := 'Sophie';
  personne.age := 30;

  AfficherPersonne(personne);
end.
```

### Modifier un enregistrement dans une procédure

Pour modifier l'enregistrement, il faut le passer **par référence** avec `var` :

```pascal
program ModifierRecord;
type
  TPersonne = record
    nom: String;
    prenom: String;
    age: Integer;
  end;

procedure Vieillir(var p: TPersonne; annees: Integer);
begin
  p.age := p.age + annees;
end;

var
  personne: TPersonne;
begin
  personne.nom := 'Durand';
  personne.prenom := 'Paul';
  personne.age := 20;

  WriteLn('Age initial : ', personne.age);
  Vieillir(personne, 5);
  WriteLn('Age après 5 ans : ', personne.age);
end.
```

### Fonction retournant un enregistrement

```pascal
program FonctionRecord;
type
  TPoint = record
    x: Real;
    y: Real;
  end;

function CreerPoint(valX, valY: Real): TPoint;
var
  p: TPoint;
begin
  p.x := valX;
  p.y := valY;
  CreerPoint := p;  // Retourner l'enregistrement
end;

function Distance(p1, p2: TPoint): Real;
begin
  Distance := Sqrt(Sqr(p2.x - p1.x) + Sqr(p2.y - p1.y));
end;

var
  pointA, pointB: TPoint;
  dist: Real;
begin
  pointA := CreerPoint(0, 0);
  pointB := CreerPoint(3, 4);

  dist := Distance(pointA, pointB);
  WriteLn('Distance entre les points : ', dist:0:2);  // 5.00
end.
```

## Comparaison d'enregistrements

En Pascal, on **ne peut pas** comparer directement deux enregistrements avec `=` :

```pascal
// ✗ ERREUR : cela ne compile pas
if personne1 = personne2 then
  WriteLn('Identiques');
```

Il faut comparer champ par champ :

```pascal
function PersonnesEgales(p1, p2: TPersonne): Boolean;
begin
  PersonnesEgales := (p1.nom = p2.nom) and
                     (p1.prenom = p2.prenom) and
                     (p1.age = p2.age);
end;
```

## Exemples pratiques

### Exemple 1 : Gestion d'une date

```pascal
program GestionDate;
type
  TDate = record
    jour: Integer;
    mois: Integer;
    annee: Integer;
  end;

function DateValide(d: TDate): Boolean;
begin
  DateValide := (d.jour >= 1) and (d.jour <= 31) and
                (d.mois >= 1) and (d.mois <= 12) and
                (d.annee > 0);
end;

procedure AfficherDate(d: TDate);
begin
  WriteLn(d.jour, '/', d.mois, '/', d.annee);
end;

var
  aujourd: TDate;
begin
  aujourd.jour := 15;
  aujourd.mois := 3;
  aujourd.annee := 2025;

  if DateValide(aujourd) then
  begin
    Write('Date valide : ');
    AfficherDate(aujourd);
  end
  else
    WriteLn('Date invalide');
end.
```

### Exemple 2 : Carnet d'adresses simple

```pascal
program CarnetAdresses;
type
  TContact = record
    nom: String;
    prenom: String;
    telephone: String;
    email: String;
  end;

procedure SaisirContact(var c: TContact);
begin
  WriteLn('=== Nouveau contact ===');
  Write('Nom : ');
  ReadLn(c.nom);
  Write('Prénom : ');
  ReadLn(c.prenom);
  Write('Téléphone : ');
  ReadLn(c.telephone);
  Write('Email : ');
  ReadLn(c.email);
end;

procedure AfficherContact(c: TContact);
begin
  WriteLn('--- Contact ---');
  WriteLn('Nom complet : ', c.prenom, ' ', c.nom);
  WriteLn('Téléphone : ', c.telephone);
  WriteLn('Email : ', c.email);
  WriteLn('---------------');
end;

var
  contact: TContact;
begin
  SaisirContact(contact);
  WriteLn;
  AfficherContact(contact);
end.
```

### Exemple 3 : Calcul de rectangle

```pascal
program Rectangle;
type
  TRectangle = record
    largeur: Real;
    hauteur: Real;
  end;

function Surface(r: TRectangle): Real;
begin
  Surface := r.largeur * r.hauteur;
end;

function Perimetre(r: TRectangle): Real;
begin
  Perimetre := 2 * (r.largeur + r.hauteur);
end;

function EstCarre(r: TRectangle): Boolean;
begin
  EstCarre := r.largeur = r.hauteur;
end;

var
  rect: TRectangle;
begin
  Write('Largeur : ');
  ReadLn(rect.largeur);
  Write('Hauteur : ');
  ReadLn(rect.hauteur);

  WriteLn('Surface : ', Surface(rect):0:2);
  WriteLn('Périmètre : ', Perimetre(rect):0:2);

  if EstCarre(rect) then
    WriteLn('C''est un carré !')
  else
    WriteLn('Ce n''est pas un carré');
end.
```

### Exemple 4 : Gestion de stock

```pascal
program GestionStock;
type
  TArticle = record
    code: String;
    designation: String;
    prixUnitaire: Real;
    quantiteStock: Integer;
  end;

procedure AjouterStock(var art: TArticle; quantite: Integer);
begin
  art.quantiteStock := art.quantiteStock + quantite;
  WriteLn('Stock ajouté. Nouveau stock : ', art.quantiteStock);
end;

function RetirerStock(var art: TArticle; quantite: Integer): Boolean;
begin
  if art.quantiteStock >= quantite then
  begin
    art.quantiteStock := art.quantiteStock - quantite;
    RetirerStock := True;
  end
  else
  begin
    WriteLn('Stock insuffisant !');
    RetirerStock := False;
  end;
end;

function ValeurStock(art: TArticle): Real;
begin
  ValeurStock := art.prixUnitaire * art.quantiteStock;
end;

var
  article: TArticle;
begin
  // Initialisation
  article.code := 'ART001';
  article.designation := 'Stylo bleu';
  article.prixUnitaire := 1.50;
  article.quantiteStock := 100;

  WriteLn('Article : ', article.designation);
  WriteLn('Prix unitaire : ', article.prixUnitaire:0:2, ' €');
  WriteLn('Stock initial : ', article.quantiteStock);
  WriteLn('Valeur du stock : ', ValeurStock(article):0:2, ' €');
  WriteLn;

  // Opérations
  AjouterStock(article, 50);

  if RetirerStock(article, 30) then
    WriteLn('Vente effectuée');

  WriteLn('Stock final : ', article.quantiteStock);
  WriteLn('Valeur finale : ', ValeurStock(article):0:2, ' €');
end.
```

## Initialisation d'enregistrements

### Initialisation champ par champ

```pascal
var
  personne: TPersonne;
begin
  personne.nom := '';
  personne.prenom := '';
  personne.age := 0;
end.
```

### Procédure d'initialisation

```pascal
procedure InitialiserPersonne(var p: TPersonne);
begin
  p.nom := '';
  p.prenom := '';
  p.age := 0;
end;
```

### Initialisation avec valeurs par défaut (FreePascal)

```pascal
type
  TProduit = record
    code: String;
    designation: String;
    prix: Real;
    stock: Integer;
  end;

var
  produit: TProduit = (
    code: 'P000';
    designation: 'Inconnu';
    prix: 0.0;
    stock: 0
  );
```

## Enregistrements vs Tableaux

| Critère | Enregistrement | Tableau |
|---------|----------------|---------|
| **Types de données** | Peut contenir différents types | Tous les éléments du même type |
| **Accès aux données** | Par nom de champ | Par indice numérique |
| **Taille** | Fixe (nombre de champs) | Peut être variable |
| **Usage** | Représenter une entité | Collection d'éléments similaires |
| **Exemple** | Fiche d'un étudiant | Liste de notes |

```pascal
// Enregistrement : données variées sur UNE personne
type
  TPersonne = record
    nom: String;
    age: Integer;
    taille: Real;
  end;

// Tableau : données similaires sur PLUSIEURS éléments
var
  ages: array[1..10] of Integer;  // Ages de 10 personnes
```

## Enregistrements constants

On peut déclarer des enregistrements constants :

```pascal
const
  ORIGINE: TPoint = (x: 0; y: 0);
  DATE_SYSTEME: TDate = (jour: 1; mois: 1; annee: 1970);
```

## Pièges courants

### 1. Oublier le point d'accès

```pascal
var
  personne: TPersonne;
begin
  personne.nom := 'Dupont';  // ✓ Correct
  nom := 'Dupont';           // ✗ Erreur : quelle variable ?
end.
```

### 2. Comparer directement deux enregistrements

```pascal
if personne1 = personne2 then  // ✗ Ne compile pas
```

Il faut créer une fonction de comparaison.

### 3. Oublier de déclarer le type

```pascal
var
  personne: record  // ✗ Possible mais déconseillé
    nom: String;
    age: Integer;
  end;
```

**Meilleure pratique :** Toujours déclarer le type dans la section `type`.

### 4. Confusion avec WITH imbriqués

```pascal
with personne1 do
  with personne2 do
    nom := 'Dupont';  // ✗ Confusion : quel nom ?
```

Évitez les `with` imbriqués.

## Quand utiliser des enregistrements ?

✓ **Utilisez des enregistrements quand :**
- Vous devez regrouper des informations liées mais de types différents
- Vous représentez une entité du monde réel (personne, produit, date, etc.)
- Vous voulez passer plusieurs informations en un seul paramètre
- Vous organisez des données complexes

✗ **N'utilisez pas d'enregistrements quand :**
- Toutes vos données sont du même type → utilisez un tableau
- Vous n'avez qu'une ou deux valeurs → des variables simples suffisent

## Résumé

Les enregistrements (records) permettent de :
- Regrouper des données de **types différents** sous un même nom
- Représenter des entités complexes (personne, produit, date, etc.)
- Organiser le code de manière logique et lisible
- Faciliter le passage de paramètres multiples

**Points clés à retenir :**
- Déclaration : `type NomType = record ... end;`
- Accès aux champs : `variable.champ`
- Instruction `with` pour simplifier l'accès
- Copie simple : `record2 := record1`
- Passage par référence avec `var` pour modification
- Comparaison champ par champ (pas d'opérateur `=` direct)

Les enregistrements sont une étape importante vers la programmation orientée objet, où ils évolueront en classes avec des méthodes associées.

⏭️ [Enregistrements imbriqués](/05-types-donnees-structures/05-enregistrements-imbriques.md)
