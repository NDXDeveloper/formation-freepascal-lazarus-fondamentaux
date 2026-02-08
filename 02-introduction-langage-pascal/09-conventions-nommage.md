🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.9 Conventions de nommage

## Qu'est-ce qu'une convention de nommage ?

Une **convention de nommage** est un ensemble de règles pour choisir les noms des éléments de votre code : variables, constantes, fonctions, procédures, types, etc.

C'est comme les règles de politesse : elles ne sont pas obligatoires pour que le code fonctionne, mais elles rendent la vie bien plus agréable pour tout le monde !

### Pourquoi c'est important

**Imaginez deux versions du même code :**

**Version 1 (sans convention) :**
```pascal
var
  x, y: integer;
  z: real;
  a: string;
begin
  x := 10;
  y := 20;
  z := x / y;
  a := 'test';
end.
```

**Version 2 (avec conventions) :**
```pascal
var
  nombreArticles: integer;
  prixTotal: integer;
  prixMoyen: real;
  nomClient: string;
begin
  nombreArticles := 10;
  prixTotal := 20;
  prixMoyen := prixTotal / nombreArticles;
  nomClient := 'Dupont';
end.
```

La seconde version se lit comme du français ! On comprend immédiatement ce que fait le code.

### Les avantages des bonnes conventions

- **Lisibilité** : le code se comprend instantanément
- **Maintenance** : plus facile de modifier le code plus tard
- **Collaboration** : les autres développeurs comprennent votre code
- **Professionnalisme** : votre code a l'air professionnel
- **Moins d'erreurs** : de bons noms évitent les confusions

## Les règles de base en Pascal

### Règles obligatoires (syntaxe)

Ces règles sont **imposées** par le langage Pascal :

1. **Commencer par une lettre** (a-z, A-Z) ou un souligné (_)
2. **Contenir uniquement** des lettres, chiffres et soulignés
3. **Pas d'espaces** ni de caractères spéciaux (@, #, €, -, etc.)
4. **Pas de mots-clés réservés** (begin, end, if, var, etc.)
5. **Longueur maximale** : 127 caractères (largement suffisant !)

**Exemples valides :**
```pascal
age
nomUtilisateur
prix_total
compteur1
_temporaire
MaClasse
```

**Exemples invalides :**
```pascal
1nombre          // Commence par un chiffre
mon-nom          // Contient un tiret
prix total       // Contient un espace
montant€         // Caractère spécial
var              // Mot-clé réservé
```

### Insensibilité à la casse

Pascal est **insensible à la casse** : il ne fait pas de différence entre majuscules et minuscules.

```pascal
var
  Age: integer;
  age: integer;    // ERREUR : c'est la même variable !
  AGE: integer;    // ERREUR : encore la même !
```

Cependant, pour la **lisibilité**, utilisez une convention cohérente :
```pascal
var
  age: integer;    // Choisissez un style
begin
  age := 25;       // Et gardez-le partout
  Age := 30;       // Fonctionne mais incohérent
  AGE := 35;       // Fonctionne mais incohérent
end.
```

## Les principales conventions de style

Il existe plusieurs styles de nommage. Choisissez-en un et **restez cohérent** !

### 1. camelCase (recommandé pour Pascal)

**Principe :** première lettre en minuscule, puis majuscule à chaque nouveau mot.

```pascal
var
  nomUtilisateur: string;
  prixTotalHT: real;
  nombreDArticles: integer;
  estConnecte: boolean;
  dateDeNaissance: string;
```

**Avantages :**
- Lisible et compact
- Style traditionnel Pascal/Delphi
- Largement utilisé dans la communauté

### 2. PascalCase (pour les types et classes)

**Principe :** majuscule à chaque mot, y compris le premier.

```pascal
type
  TPersonne = record
    Nom: string;
    Prenom: string;
  end;

  TCalculatrice = class
    procedure Additionner;
  end;
```

**Avantages :**
- Distingue visuellement les types des variables
- Convention standard pour les classes et records

### 3. snake_case (alternative acceptable)

**Principe :** tout en minuscules, mots séparés par des soulignés.

```pascal
var
  nom_utilisateur: string;
  prix_total_ht: real;
  nombre_articles: integer;
  est_connecte: boolean;
```

**Avantages :**
- Très lisible
- Populaire dans d'autres langages (Python, C)

### 4. SCREAMING_SNAKE_CASE (pour les constantes)

**Principe :** tout en majuscules, mots séparés par des soulignés.

```pascal
const
  TAUX_TVA = 20.0;
  MAX_TENTATIVES = 3;
  COULEUR_DEFAUT = 'bleu';
  PI_VALEUR = 3.14159;
```

**Avantages :**
- Les constantes sont immédiatement reconnaissables
- Convention quasi-universelle

### Comparaison des styles

```pascal
// camelCase (variables)
var
  prixArticle: real;
  nombreClients: integer;

// PascalCase (types)
type
  TProduit = record
  end;

// snake_case (alternative)
var
  prix_article: real;
  nombre_clients: integer;

// SCREAMING_SNAKE_CASE (constantes)
const
  PRIX_MINIMUM = 10.0;
  MAX_STOCK = 1000;
```

## Conventions par type d'élément

### Variables

**Style recommandé :** camelCase ou snake_case

**Principe :** nom descriptif au singulier

```pascal
var
  age: integer;                  // Simple et clair
  prixUnitaire: real;            // Précis
  nomComplet: string;            // Descriptif
  estValide: boolean;            // Question oui/non
  compteurErreurs: integer;      // Indique ce qu'on compte
```

**Bonnes pratiques :**
- Utilisez des noms complets et significatifs
- Évitez les abréviations obscures
- Les booléens commencent souvent par "est", "a", "peut"

**Mauvais exemples :**
```pascal
var
  x: integer;                    // Trop vague
  temp: string;                  // Que représente temp ?
  nb: integer;                   // Abréviation peu claire
  flag: boolean;                 // Quel flag ?
  data: string;                  // Quelle donnée ?
```

**Bons exemples :**
```pascal
var
  nombreDEtudiants: integer;     // Clair et précis
  temperatureCelsius: real;      // Inclut l'unité
  nombreTentatives: integer;     // Descriptif
  estAuthentifie: boolean;       // Question claire
  donneesUtilisateur: string;    // Précise le contenu
```

### Constantes

**Style recommandé :** SCREAMING_SNAKE_CASE

**Principe :** nom descriptif indiquant une valeur fixe

```pascal
const
  TAUX_TVA = 20.0;
  MAX_TENTATIVES = 3;
  NOM_APPLICATION = 'MonApp';
  TIMEOUT_SECONDES = 30;
  COULEUR_ERREUR = 'rouge';
  PI = 3.14159;
```

**Alternative acceptable :** PascalCase avec préfixe

```pascal
const
  cTauxTVA = 20.0;
  cMaxTentatives = 3;
  cNomApplication = 'MonApp';
```

### Procédures et fonctions

**Style recommandé :** PascalCase ou camelCase

**Principe :** verbe + complément (car elles effectuent une action)

```pascal
procedure AfficherMenu;
procedure CalculerTotal;
procedure InitialiserVariables;
procedure SauvegarderDonnees;
procedure ValiderSaisie;

function ObtenirNom: string;
function CalculerMoyenne(notes: array of real): real;
function EstPair(nombre: integer): boolean;
function ConvertirEnMajuscules(texte: string): string;
```

**Bonnes pratiques :**
- Commencez par un **verbe d'action**
- Les fonctions qui retournent un booléen commencent souvent par "Est", "A", "Peut"
- Soyez descriptif mais concis

**Mauvais exemples :**
```pascal
procedure Faire;               // Trop vague
procedure X;                   // Incompréhensible
function Get: string;          // Get quoi ?
function Calc: integer;        // Calcule quoi ?
```

**Bons exemples :**
```pascal
procedure AfficherResultat;
procedure EnregistrerClient;
function CalculerPrixTTC(prixHT: real): real;
function EstNombreValide(nombre: integer): boolean;
function ObtenirDateDuJour: string;
```

### Types personnalisés

**Style recommandé :** PascalCase avec préfixe 'T'

Le 'T' signifie "Type" et est une convention très répandue en Pascal.

```pascal
type
  TPersonne = record
    nom: string;
    prenom: string;
    age: integer;
  end;

  TArticle = record
    code: string;
    prix: real;
    stock: integer;
  end;

  TTableauEntiers = array of integer;
  TListeNoms = array[1..100] of string;
```

**Énumérations :**
```pascal
type
  TJourSemaine = (lundi, mardi, mercredi, jeudi, vendredi, samedi, dimanche);
  TCouleur = (rouge, vert, bleu, jaune);
  TEtatConnexion = (deconnecte, enCours, connecte, erreur);
```

**Classes (POO) :**
```pascal
type
  TCalculatrice = class
  private
    resultat: real;
  public
    procedure Additionner(a, b: real);
    function ObtenirResultat: real;
  end;

  TClient = class
  private
    nom: string;
    email: string;
  public
    constructor Create(leNom: string);
  end;
```

### Paramètres de fonctions

**Principe :** même style que les variables, mais souvent avec préfixe pour éviter les conflits

```pascal
procedure EnregistrerPersonne(leNom: string; lAge: integer);
function CalculerRemise(lePrix: real; lePourcentage: real): real;
procedure AfficherInfo(unNom: string; unAge: integer);
```

**Alternative :** utiliser un style légèrement différent

```pascal
procedure EnregistrerPersonne(aNom: string; aAge: integer);
function CalculerRemise(prixBase: real; tauxRemise: real): real;
```

### Variables locales vs globales

**Variables locales :** style standard camelCase

```pascal
procedure TraiterDonnees;
var
  compteur: integer;           // Variable locale
  total: real;                 // Variable locale
begin
  compteur := 0;
  total := 0.0;
end;
```

**Variables globales :** certains ajoutent un préfixe (optionnel)

```pascal
var
  gCompteurGlobal: integer;    // 'g' pour global
  gConfiguration: string;

  // Ou sans préfixe mais avec nom très explicite
  compteurGlobalApplication: integer;
```

**Note :** Évitez autant que possible les variables globales !

## Préfixes et suffixes courants

### Préfixes pour les types

```pascal
type
  TPersonne = record;          // 'T' pour Type
  PPersonne = ^TPersonne;      // 'P' pour Pointeur (Pointer)
  EErreur = class(Exception);  // 'E' pour Exception
  IInterface = interface;      // 'I' pour Interface
```

### Préfixes pour les composants (Lazarus/Delphi)

```pascal
var
  btnValider: TButton;         // 'btn' pour bouton
  edtNom: TEdit;               // 'edt' pour champ de saisie
  lblTitre: TLabel;            // 'lbl' pour label
  cbxChoix: TComboBox;         // 'cbx' pour combo box
  chkActif: TCheckBox;         // 'chk' pour case à cocher
  lstItems: TListBox;          // 'lst' pour liste
  pnlPrincipal: TPanel;        // 'pnl' pour panneau
  grdDonnees: TStringGrid;     // 'grd' pour grille
```

### Suffixes utiles

```pascal
var
  nomFichier: string;          // Indique que c'est un nom de fichier
  listeNoms: array of string;  // Indique que c'est une liste
  compteurLignes: integer;     // Indique ce qu'on compte
  prixTotal: real;             // Indique que c'est un total
  indexCourant: integer;       // Indique que c'est un index
  tailleMaximum: integer;      // Indique une limite
```

## Cas particuliers et idiomes

### Variables de boucle

**Convention :** lettres simples pour les boucles simples

```pascal
var
  i, j, k: integer;            // Pour les boucles imbriquées
  compteur: integer;           // Pour les boucles plus complexes
begin
  // Boucle simple : 'i' est acceptable
  for i := 1 to 10 do
    WriteLn(i);

  // Boucle avec signification : nom descriptif
  for numeroLigne := 1 to 100 do
    TraiterLigne(numeroLigne);
end;
```

### Variables temporaires

```pascal
var
  temp: integer;               // Pour échanges temporaires
  tampon: string;              // Buffer temporaire
  resultatTemp: real;          // Résultat intermédiaire
```

### Variables booléennes

**Principe :** formuler comme une question oui/non

```pascal
var
  estValide: boolean;          // "Est-ce valide ?"
  aReussi: boolean;            // "A-t-il réussi ?"
  peutContinuer: boolean;      // "Peut-on continuer ?"
  estConnecte: boolean;        // "Est-il connecté ?"
  doitArreter: boolean;        // "Doit-on arrêter ?"

  // Évitez les négations
  // Mauvais :
  estPasValide: boolean;       // Double négation confuse

  // Bon :
  estInvalide: boolean;        // Clair
```

### Noms de fichiers et unités

**Convention :** PascalCase sans espaces

```pascal
unit GestionClients;           // Nom d'unité
// Fichier : GestionClients.pas

unit CalculsMathematiques;     // Nom descriptif
// Fichier : CalculsMathematiques.pas

program MonApplication;        // Nom de programme
// Fichier : MonApplication.pas
```

## Exemples complets avec bonnes conventions

### Exemple 1 : Petit programme bien nommé

```pascal
{$mode objfpc}{$H+}
program CalculateurMoyenne;

const
  NOMBRE_NOTES = 3;
  NOTE_MINIMUM = 0.0;
  NOTE_MAXIMUM = 20.0;

var
  note1, note2, note3: real;
  moyenneGenerale: real;
  appreciation: string;

function CalculerMoyenne(n1, n2, n3: real): real;
begin
  { Result est la variable spéciale pour renvoyer la valeur d'une fonction }
  Result := (n1 + n2 + n3) / NOMBRE_NOTES;
end;

function ObtenirAppreciation(moyenne: real): string;
begin
  if moyenne >= 16 then
    Result := 'Très bien'
  else if moyenne >= 14 then
    Result := 'Bien'
  else if moyenne >= 12 then
    Result := 'Assez bien'
  else if moyenne >= 10 then
    Result := 'Passable'
  else
    Result := 'Insuffisant';
end;

procedure AfficherResultat(moyenne: real; appre: string);
begin
  WriteLn('Moyenne : ', moyenne:0:2, '/20');
  WriteLn('Appréciation : ', appre);
end;

begin
  WriteLn('=== CALCUL DE MOYENNE ===');

  Write('Note 1 : ');
  ReadLn(note1);

  Write('Note 2 : ');
  ReadLn(note2);

  Write('Note 3 : ');
  ReadLn(note3);

  moyenneGenerale := CalculerMoyenne(note1, note2, note3);
  appreciation := ObtenirAppreciation(moyenneGenerale);

  WriteLn;
  AfficherResultat(moyenneGenerale, appreciation);
end.
```

### Exemple 2 : Gestion d'un article avec type personnalisé

```pascal
{$mode objfpc}{$H+}
program GestionArticles;

const
  TAUX_TVA = 20.0;
  STOCK_MINIMUM = 5;

type
  TArticle = record
    code: string;
    designation: string;
    prixHT: real;
    quantiteStock: integer;
  end;

var
  article: TArticle;
  prixTTC: real;
  estEnRupture: boolean;

{ var devant le paramètre = passage par référence : les modifications
  sont répercutées sur la variable de l'appelant }
procedure InitialiserArticle(var unArticle: TArticle);
begin
  unArticle.code := 'ART001';
  unArticle.designation := 'Clavier USB';
  unArticle.prixHT := 25.00;
  unArticle.quantiteStock := 15;
end;

function CalculerPrixTTC(prixHT: real): real;
begin
  { Result est la variable spéciale pour renvoyer la valeur d'une fonction }
  Result := prixHT * (1 + TAUX_TVA / 100);
end;

function EstEnRuptureStock(quantite: integer): boolean;
begin
  Result := quantite < STOCK_MINIMUM;
end;

procedure AfficherArticle(unArticle: TArticle);
begin
  WriteLn('Code : ', unArticle.code);
  WriteLn('Désignation : ', unArticle.designation);
  WriteLn('Prix HT : ', unArticle.prixHT:0:2, ' €');
  WriteLn('Prix TTC : ', CalculerPrixTTC(unArticle.prixHT):0:2, ' €');
  WriteLn('Stock : ', unArticle.quantiteStock);

  if EstEnRuptureStock(unArticle.quantiteStock) then
    WriteLn('ATTENTION : Stock faible !');
end;

begin
  InitialiserArticle(article);
  AfficherArticle(article);
end.
```

## Conventions spécifiques Free Pascal / Lazarus

### Unités standard

Les unités de la bibliothèque standard utilisent PascalCase :

```pascal
uses
  SysUtils,         // Utilitaires système
  Classes,          // Classes de base
  Math,             // Fonctions mathématiques
  DateUtils,        // Manipulation de dates
  StrUtils;         // Utilitaires pour strings
```

### Composants LCL

Les composants Lazarus suivent aussi PascalCase avec préfixe 'T' :

```pascal
var
  MonFormulaire: TForm;
  MonBouton: TButton;
  MonEdit: TEdit;
  MaListe: TListBox;
```

## Erreurs fréquentes à éviter

### 1. Noms trop courts et cryptiques

**Mauvais :**
```pascal
var
  n: string;
  p: real;
  q: integer;
  f: boolean;
```

**Bon :**
```pascal
var
  nomClient: string;
  prixArticle: real;
  quantiteStock: integer;
  fichierExiste: boolean;
```

### 2. Noms trop longs et verbeux

**Mauvais :**
```pascal
var
  leNomCompletDuClientQuiAPasseCommande: string;
  lePrixTotalDeTousLesArticlesDansLePanier: real;
```

**Bon :**
```pascal
var
  nomClient: string;
  prixTotalPanier: real;
```

### 3. Abréviations obscures

**Mauvais :**
```pascal
var
  nbElt: integer;              // Nombre d'éléments ?
  tmpStr: string;              // Temporary string ?
  usrNm: string;               // User name ?
```

**Bon :**
```pascal
var
  nombreElements: integer;
  texteTemporaire: string;
  nomUtilisateur: string;
```

### 4. Incohérence dans le style

**Mauvais :**
```pascal
var
  nomClient: string;           // camelCase
  Prix_Article: real;          // Mélange
  QUANTITE: integer;           // Tout en majuscules
  estvalide: boolean;          // Tout attaché
```

**Bon :**
```pascal
var
  nomClient: string;
  prixArticle: real;
  quantiteStock: integer;
  estValide: boolean;
```

### 5. Utiliser le même nom pour différentes choses

**Mauvais :**
```pascal
var
  donnees: string;             // Trop générique
  donnees: integer;            // Erreur : même nom !
```

**Bon :**
```pascal
var
  donneesTexte: string;
  donneesNumeriques: integer;
```

### 6. Noms trompeurs

**Mauvais :**
```pascal
var
  compteur: string;            // Un compteur devrait être integer !
  total: boolean;              // Un total devrait être numérique !
```

**Bon :**
```pascal
var
  compteur: integer;
  total: real;
  estComplet: boolean;
```

## Check-list des bonnes pratiques

**Pour les variables :**
- ✅ Nom descriptif et complet
- ✅ camelCase ou snake_case
- ✅ Pas d'abréviation obscure
- ✅ Booléens formulés comme questions

**Pour les constantes :**
- ✅ SCREAMING_SNAKE_CASE
- ✅ Nom indiquant la nature de la constante
- ✅ Valeur évidente depuis le nom

**Pour les fonctions/procédures :**
- ✅ Commence par un verbe d'action
- ✅ PascalCase ou camelCase
- ✅ Nom clair indiquant ce qu'elle fait
- ✅ Booléens commencent par Est/A/Peut

**Pour les types :**
- ✅ PascalCase avec préfixe T
- ✅ Nom au singulier (TPersonne, pas TPersonnes)
- ✅ Descriptif du contenu

**Général :**
- ✅ Cohérence dans tout le projet
- ✅ Lisible par un humain
- ✅ Auto-documenté (le nom explique l'usage)

## Récapitulatif

**Les styles principaux :**
- **camelCase** : pour les variables et paramètres
- **PascalCase** : pour les types, classes et fonctions
- **snake_case** : alternative pour les variables
- **SCREAMING_SNAKE_CASE** : pour les constantes

**Préfixes courants :**
- **T** : Types (TPersonne, TArticle)
- **P** : Pointeurs (PPersonne)
- **E** : Exceptions (EErreurFichier)
- **I** : Interfaces (IConnexion)
- **g** : Variables globales (optionnel)

**Pour les booléens :**
- Commencer par : est, a, peut, doit
- Exemple : estValide, aReussi, peutContinuer

**Règle d'or :**
Un bon nom de variable devrait permettre de comprendre son rôle sans lire de commentaire. Si vous hésitez entre un nom court cryptique et un nom long explicite, **choisissez toujours le nom explicite**.

---

**Point clé :** Les conventions de nommage ne sont pas qu'une question d'esthétique. Elles sont essentielles pour écrire du code maintenable, lisible et professionnel. Prenez l'habitude dès maintenant de nommer correctement vos éléments : c'est un investissement qui vous fera gagner énormément de temps tout au long de votre carrière de programmeur !

⏭️ [Structures de Contrôle](/03-structures-controle/README.md)
