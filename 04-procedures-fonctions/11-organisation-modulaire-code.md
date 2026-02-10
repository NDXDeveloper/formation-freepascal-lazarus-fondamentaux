🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.11 Organisation modulaire du code

## Introduction

Un programme mal organisé devient rapidement difficile à comprendre, à maintenir et à déboguer. L'**organisation modulaire** consiste à structurer son code en **modules logiques** (procédures et fonctions bien définies) qui travaillent ensemble de manière cohérente.

## Pourquoi organiser son code ?

### Problème : code monolithique

Voici un exemple de **mauvais code** où tout est dans le programme principal :

```pascal
program Mauvais;

var
  nom, prenom, email: String;
  age: Integer;
  prix1, prix2, prix3, total, moyenne: Real;
  choix, i: Integer;
begin
  // Tout est mélangé dans le programme principal
  WriteLn('=== MENU ===');
  WriteLn('1. Inscription');
  WriteLn('2. Calculer moyenne');
  WriteLn('3. Quitter');
  Write('Choix : ');
  ReadLn(choix);

  if choix = 1 then
  begin
    Write('Nom : ');
    ReadLn(nom);
    Write('Prénom : ');
    ReadLn(prenom);
    Write('Email : ');
    ReadLn(email);
    Write('Age : ');
    ReadLn(age);
    if age < 18 then
      WriteLn('Mineur')
    else
      WriteLn('Majeur');
    WriteLn('Inscription de ', prenom, ' ', nom, ' (', email, ')');
  end
  else if choix = 2 then
  begin
    Write('Prix 1 : ');
    ReadLn(prix1);
    Write('Prix 2 : ');
    ReadLn(prix2);
    Write('Prix 3 : ');
    ReadLn(prix3);
    total := prix1 + prix2 + prix3;
    moyenne := total / 3;
    WriteLn('Moyenne : ', moyenne:0:2);
  end;
end.
```

**Problèmes :**
- ❌ Tout est dans un seul bloc
- ❌ Difficile à comprendre
- ❌ Impossible de réutiliser le code
- ❌ Difficile à tester
- ❌ Difficile à maintenir

### Solution : code modulaire

```pascal
{$mode objfpc}{$H+}
program Bon;

uses SysUtils;

// === MODULE INTERFACE UTILISATEUR ===

procedure AfficherMenu;  
begin  
  WriteLn('=== MENU ===');
  WriteLn('1. Inscription');
  WriteLn('2. Calculer moyenne');
  WriteLn('3. Quitter');
end;

function LireChoix: Integer;  
var  
  texte: String;
begin
  Write('Choix : ');
  ReadLn(texte);
  Result := StrToIntDef(texte, 0);
end;

// === MODULE INSCRIPTION ===

function LireTexte(const prompt: String): String;  
begin  
  Write(prompt, ' : ');
  ReadLn(Result);
end;

function LireAge: Integer;  
var  
  texte: String;
begin
  Write('Age : ');
  ReadLn(texte);
  Result := StrToIntDef(texte, 0);
end;

function EstMajeur(age: Integer): Boolean;  
begin  
  Result := age >= 18;
end;

procedure Inscrire;  
var  
  nom, prenom, email: String;
  age: Integer;
begin
  nom := LireTexte('Nom');
  prenom := LireTexte('Prénom');
  email := LireTexte('Email');
  age := LireAge;

  if EstMajeur(age) then
    WriteLn('Statut : Majeur')
  else
    WriteLn('Statut : Mineur');

  WriteLn(Format('Inscription de %s %s (%s)', [prenom, nom, email]));
end;

// === MODULE CALCULS ===

function LirePrix(const prompt: String): Real;  
var  
  texte: String;
begin
  Write(prompt, ' : ');
  ReadLn(texte);
  Result := StrToFloatDef(texte, 0.0);  // Convertit en réel, retourne 0.0 si invalide
end;

function CalculerMoyenne(v1, v2, v3: Real): Real;  
begin  
  Result := (v1 + v2 + v3) / 3;
end;

procedure AfficherMoyenne;  
var  
  prix1, prix2, prix3, moyenne: Real;
begin
  prix1 := LirePrix('Prix 1');
  prix2 := LirePrix('Prix 2');
  prix3 := LirePrix('Prix 3');
  moyenne := CalculerMoyenne(prix1, prix2, prix3);
  WriteLn('Moyenne : ', moyenne:0:2);
end;

// === PROGRAMME PRINCIPAL ===

var
  choix: Integer;
begin
  AfficherMenu;
  choix := LireChoix;

  case choix of
    1: Inscrire;
    2: AfficherMoyenne;
    3: WriteLn('Au revoir !');
  else
    WriteLn('Choix invalide');
  end;
end.
```

**Avantages :**
- ✅ Code organisé en modules logiques
- ✅ Chaque fonction a une responsabilité claire
- ✅ Facilement réutilisable
- ✅ Facile à tester
- ✅ Facile à maintenir

## Principes d'organisation

### 1. Principe de responsabilité unique

Chaque fonction/procédure doit avoir **une seule responsabilité** clairement définie.

```pascal
// ❌ Mauvais : fait trop de choses
procedure TraiterUtilisateur;  
begin  
  Write('Nom : ');
  ReadLn(nom);
  if nom = '' then
    nom := 'Anonyme';
  nom := UpperCase(nom);
  WriteLn('Bienvenue ', nom);
  compteur := compteur + 1;
  WriteLn('Total utilisateurs : ', compteur);
end;

// ✅ Bon : chaque fonction fait une chose
function LireNom: String;  
begin  
  Write('Nom : ');
  ReadLn(Result);
end;

function NomOuDefaut(const nom: String): String;  
begin  
  if nom = '' then
    Result := 'Anonyme'
  else
    Result := nom;
end;

procedure Saluer(const nom: String);  
begin  
  WriteLn('Bienvenue ', UpperCase(nom));
end;

procedure IncrementerCompteur;  
begin  
  compteur := compteur + 1;
end;

procedure AfficherTotal;  
begin  
  WriteLn('Total utilisateurs : ', compteur);
end;
```

### 2. Cohésion forte

Les fonctions d'un même module doivent être **fortement liées** entre elles.

```pascal
// MODULE : Gestion de géométrie
function CalculerAireRectangle(largeur, hauteur: Real): Real;  
begin  
  Result := largeur * hauteur;
end;

function CalculerPerimetreRectangle(largeur, hauteur: Real): Real;  
begin  
  Result := 2 * (largeur + hauteur);
end;

function CalculerAireCercle(rayon: Real): Real;  
const  
  PI = 3.14159;
begin
  Result := PI * rayon * rayon;
end;

// Ces trois fonctions sont cohérentes : elles parlent de géométrie
```

### 3. Couplage faible

Les modules doivent être **le moins dépendants possible** les uns des autres.

```pascal
// ❌ Mauvais : couplage fort (dépend de variables globales)
var
  utilisateurNom: String;
  utilisateurAge: Integer;

procedure AfficherUtilisateur;  
begin  
  WriteLn(utilisateurNom, ' - ', utilisateurAge, ' ans');
end;

// ✅ Bon : couplage faible (utilise des paramètres)
procedure AfficherUtilisateur(const nom: String; age: Integer);  
begin  
  WriteLn(nom, ' - ', age, ' ans');
end;
```

### 4. Niveaux d'abstraction

Séparez les **niveaux d'abstraction** : haut niveau (quoi faire) et bas niveau (comment faire).

```pascal
// Haut niveau : décrit ce qu'on fait
procedure TraiterCommande;  
var  
  articles: array of TArticle;
  total: Real;
begin
  articles := LireArticles;
  total := CalculerTotal(articles);
  AfficherFacture(articles, total);
  EnregistrerCommande(articles, total);
end;

// Bas niveau : détails d'implémentation
function CalculerTotal(const articles: array of TArticle): Real;  
var  
  i: Integer;
begin
  Result := 0;
  for i := Low(articles) to High(articles) do
    Result := Result + articles[i].Prix * articles[i].Quantite;
end;
```

## Découpage logique du code

### Structure recommandée

```pascal
{$mode objfpc}{$H+}
program MonProgramme;

uses
  SysUtils;  // Unités nécessaires

// ========================================
// SECTION 1 : CONSTANTES ET TYPES
// ========================================

const
  VERSION = '1.0';
  MAX_USERS = 100;

type
  TUtilisateur = record
    Nom: String;
    Age: Integer;
  end;

// ========================================
// SECTION 2 : VARIABLES GLOBALES (si nécessaire)
// ========================================

var
  utilisateurs: array[1..MAX_USERS] of TUtilisateur;
  nbUtilisateurs: Integer;

// ========================================
// SECTION 3 : FONCTIONS UTILITAIRES DE BAS NIVEAU
// ========================================

function EstChaineVide(const s: String): Boolean;  
begin  
  Result := Trim(s) = '';
end;

function LireEntierPositif(const prompt: String): Integer;  
var  
  texte: String;
  valeur: Integer;
begin
  repeat
    Write(prompt, ' : ');
    ReadLn(texte);
    valeur := StrToIntDef(texte, -1);
  until valeur >= 0;
  Result := valeur;
end;

// ========================================
// SECTION 4 : FONCTIONS MÉTIER
// ========================================

function CreerUtilisateur(const nom: String; age: Integer): TUtilisateur;  
begin  
  Result.Nom := nom;
  Result.Age := age;
end;

procedure AjouterUtilisateur(const user: TUtilisateur);  
begin  
  if nbUtilisateurs < MAX_USERS then
  begin
    Inc(nbUtilisateurs);
    utilisateurs[nbUtilisateurs] := user;
  end;
end;

// ========================================
// SECTION 5 : INTERFACE UTILISATEUR
// ========================================

procedure AfficherMenu;  
begin  
  WriteLn('=== MENU ===');
  WriteLn('1. Ajouter utilisateur');
  WriteLn('2. Lister utilisateurs');
  WriteLn('0. Quitter');
end;

procedure TraiterAjout;  
var  
  nom: String;
  age: Integer;
  user: TUtilisateur;
begin
  Write('Nom : ');
  ReadLn(nom);
  age := LireEntierPositif('Age');
  user := CreerUtilisateur(nom, age);
  AjouterUtilisateur(user);
  WriteLn('Utilisateur ajouté !');
end;

// ========================================
// SECTION 6 : PROGRAMME PRINCIPAL
// ========================================

var
  choix: Integer;
begin
  nbUtilisateurs := 0;

  repeat
    AfficherMenu;
    choix := LireEntierPositif('Choix');

    case choix of
      1: TraiterAjout;
      0: WriteLn('Au revoir !');
    else
      WriteLn('Choix invalide');
    end;
  until choix = 0;
end.
```

## Nommage cohérent

### Conventions de nommage pour les fonctions

| Type de fonction | Convention | Exemples |
|------------------|------------|----------|
| **Calculer** | `Calculer...` | `CalculerTotal`, `CalculerMoyenne` |
| **Vérifier** | `Est...` ou `A...` | `EstValide`, `EstVide`, `ADesEnfants` |
| **Obtenir** | `Obtenir...` ou `Get...` | `ObtenirNom`, `ObtenirAge` |
| **Modifier** | `Definir...` ou `Set...` | `DefinirNom`, `DefinirAge` |
| **Afficher** | `Afficher...` | `AfficherMenu`, `AfficherResultat` |
| **Lire** | `Lire...` | `LireNom`, `LireChoix` |
| **Créer** | `Creer...` | `CreerUtilisateur`, `CreerFacture` |
| **Action** | Verbe d'action | `Sauvegarder`, `Initialiser`, `Valider` |

### Exemples de nommage cohérent

```pascal
// Module de validation
function EstEmailValide(const email: String): Boolean;  
function EstAgeValide(age: Integer): Boolean;  
function EstNomValide(const nom: String): Boolean;  

// Module de calculs
function CalculerTotalHT(quantite: Integer; prixUnitaire: Real): Real;  
function CalculerTVA(montantHT: Real; tauxTVA: Real): Real;  
function CalculerTotalTTC(montantHT, montantTVA: Real): Real;  

// Module d'interface
procedure AfficherEntete;  
procedure AfficherLigne(const texte: String);  
procedure AfficherSeparateur;  
```

## Documentation et commentaires

### Commenter les modules

```pascal
// ============================================================================
// MODULE : GESTION DES UTILISATEURS
// Description : Fonctions pour créer, modifier et gérer les utilisateurs
// Auteur : Votre Nom
// Date : 2025-10-12
// ============================================================================

// Crée un nouvel utilisateur avec les informations fournies
// Paramètres :
//   - nom : nom complet de l'utilisateur
//   - email : adresse email valide
// Retour : un enregistrement TUtilisateur
function CreerUtilisateur(const nom, email: String): TUtilisateur;  
begin  
  Result.Nom := Trim(nom);
  Result.Email := LowerCase(Trim(email));
  Result.DateCreation := Now;
end;

// Vérifie si un utilisateur existe déjà dans la liste
// Paramètres :
//   - email : email à rechercher
// Retour : True si l'utilisateur existe, False sinon
function UtilisateurExiste(const email: String): Boolean;  
var  
  i: Integer;
begin
  Result := False;
  for i := 1 to nbUtilisateurs do
    if LowerCase(utilisateurs[i].Email) = LowerCase(email) then
    begin
      Result := True;
      Exit;
    end;
end;
```

### Commenter les algorithmes complexes

```pascal
function CalculerRemise(montant: Real; nbAchats: Integer): Real;  
begin  
  // Système de remise progressif :
  // - Moins de 5 achats : pas de remise
  // - 5 à 9 achats : 5% de remise
  // - 10 achats et plus : 10% de remise

  if nbAchats < 5 then
    Result := 0
  else if nbAchats < 10 then
    Result := montant * 0.05
  else
    Result := montant * 0.10;
end;
```

## Exemple complet : système de bibliothèque

```pascal
{$mode objfpc}{$H+}
program Bibliotheque;

uses
  SysUtils;

// ============================================================================
// TYPES ET CONSTANTES
// ============================================================================

const
  MAX_LIVRES = 100;

type
  TLivre = record
    Titre: String;
    Auteur: String;
    Annee: Integer;
    Disponible: Boolean;
  end;

var
  livres: array[1..MAX_LIVRES] of TLivre;
  nbLivres: Integer;

// ============================================================================
// MODULE : GESTION DES LIVRES
// ============================================================================

function CreerLivre(const titre, auteur: String; annee: Integer): TLivre;  
begin  
  Result.Titre := titre;
  Result.Auteur := auteur;
  Result.Annee := annee;
  Result.Disponible := True;
end;

function AjouterLivre(const livre: TLivre): Boolean;  
begin  
  if nbLivres < MAX_LIVRES then
  begin
    Inc(nbLivres);  // Inc incrémente : équivalent de nbLivres := nbLivres + 1
    livres[nbLivres] := livre;
    Result := True;
  end
  else
    Result := False;
end;

function RechercherLivre(const titre: String): Integer;  
var  
  i: Integer;
begin
  Result := -1;
  for i := 1 to nbLivres do
    if LowerCase(livres[i].Titre) = LowerCase(titre) then
    begin
      Result := i;
      Exit;
    end;
end;

procedure EmprunterLivre(index: Integer);  
begin  
  if (index > 0) and (index <= nbLivres) then
    livres[index].Disponible := False;
end;

procedure RetournerLivre(index: Integer);  
begin  
  if (index > 0) and (index <= nbLivres) then
    livres[index].Disponible := True;
end;

// ============================================================================
// MODULE : AFFICHAGE
// ============================================================================

procedure AfficherSeparateur;  
begin  
  WriteLn('------------------------------------------------');
end;

procedure AfficherEntete;  
begin  
  AfficherSeparateur;
  WriteLn('SYSTÈME DE GESTION DE BIBLIOTHÈQUE');
  AfficherSeparateur;
end;

procedure AfficherLivre(const livre: TLivre);  
var  
  statut: String;
begin
  if livre.Disponible then
    statut := 'Disponible'
  else
    statut := 'Emprunté';

  WriteLn(Format('"%s" par %s (%d) - %s',
          [livre.Titre, livre.Auteur, livre.Annee, statut]));
end;

procedure AfficherTousLesLivres;  
var  
  i: Integer;
begin
  WriteLn(Format('Total : %d livre(s)', [nbLivres]));
  AfficherSeparateur;

  for i := 1 to nbLivres do
  begin
    Write(i:2, '. ');
    AfficherLivre(livres[i]);
  end;
end;

procedure AfficherMenu;  
begin  
  WriteLn;
  WriteLn('=== MENU ===');
  WriteLn('1. Ajouter un livre');
  WriteLn('2. Lister les livres');
  WriteLn('3. Emprunter un livre');
  WriteLn('4. Retourner un livre');
  WriteLn('0. Quitter');
  Write('Votre choix : ');
end;

// ============================================================================
// MODULE : SAISIE UTILISATEUR
// ============================================================================

function LireTexte(const prompt: String): String;  
begin  
  Write(prompt, ' : ');
  ReadLn(Result);
  Result := Trim(Result);
end;

function LireEntier(const prompt: String): Integer;  
var  
  texte: String;
begin
  Write(prompt, ' : ');
  ReadLn(texte);
  Result := StrToIntDef(texte, 0);
end;

function LireChoix: Integer;  
begin  
  Result := LireEntier('Choix');
end;

// ============================================================================
// MODULE : ACTIONS MÉTIER
// ============================================================================

procedure ActionAjouterLivre;  
var  
  titre, auteur: String;
  annee: Integer;
  livre: TLivre;
begin
  WriteLn('=== AJOUTER UN LIVRE ===');
  titre := LireTexte('Titre');
  auteur := LireTexte('Auteur');
  annee := LireEntier('Année');

  livre := CreerLivre(titre, auteur, annee);

  if AjouterLivre(livre) then
    WriteLn('Livre ajouté avec succès !')
  else
    WriteLn('Erreur : bibliothèque pleine');
end;

procedure ActionListerLivres;  
begin  
  WriteLn('=== LISTE DES LIVRES ===');
  if nbLivres = 0 then
    WriteLn('Aucun livre dans la bibliothèque')
  else
    AfficherTousLesLivres;
end;

procedure ActionEmprunter;  
var  
  titre: String;
  index: Integer;
begin
  WriteLn('=== EMPRUNTER UN LIVRE ===');
  titre := LireTexte('Titre du livre');
  index := RechercherLivre(titre);

  if index = -1 then
    WriteLn('Livre non trouvé')
  else if not livres[index].Disponible then
    WriteLn('Livre déjà emprunté')
  else
  begin
    EmprunterLivre(index);
    WriteLn('Emprunt enregistré');
  end;
end;

procedure ActionRetourner;  
var  
  titre: String;
  index: Integer;
begin
  WriteLn('=== RETOURNER UN LIVRE ===');
  titre := LireTexte('Titre du livre');
  index := RechercherLivre(titre);

  if index = -1 then
    WriteLn('Livre non trouvé')
  else if livres[index].Disponible then
    WriteLn('Le livre n''est pas emprunté')
  else
  begin
    RetournerLivre(index);
    WriteLn('Retour enregistré');
  end;
end;

// ============================================================================
// PROGRAMME PRINCIPAL
// ============================================================================

var
  choix: Integer;
begin
  nbLivres := 0;
  AfficherEntete;

  repeat
    AfficherMenu;
    choix := LireChoix;
    WriteLn;

    case choix of
      1: ActionAjouterLivre;
      2: ActionListerLivres;
      3: ActionEmprunter;
      4: ActionRetourner;
      0: WriteLn('Au revoir !');
    else
      WriteLn('Choix invalide');
    end;
  until choix = 0;
end.
```

## Avantages de l'organisation modulaire

1. **Lisibilité** : code facile à comprendre
2. **Maintenance** : modifications localisées
3. **Réutilisabilité** : fonctions réutilisables dans d'autres projets
4. **Testabilité** : chaque fonction peut être testée individuellement
5. **Collaboration** : plusieurs personnes peuvent travailler sur différents modules
6. **Débogage** : plus facile de localiser les erreurs
7. **Évolutivité** : ajout de fonctionnalités facilité

## Bonnes pratiques

### 1. Taille des fonctions

Une fonction devrait faire **entre 5 et 30 lignes** idéalement. Si elle est plus longue, envisagez de la découper.

```pascal
// ❌ Trop long
procedure TraiterCommande;  
begin  
  // 100 lignes de code...
end;

// ✅ Bien découpé
procedure TraiterCommande;  
begin  
  ValiderCommande;
  CalculerTotaux;
  AppliquerRemises;
  GenererFacture;
  EnvoyerConfirmation;
end;
```

### 2. Paramètres vs variables globales

Préférez les **paramètres** aux **variables globales**.

```pascal
// ❌ Mauvais : utilise une globale
var
  resultat: Real;

procedure Calculer;  
begin  
  resultat := 10 * 20;
end;

// ✅ Bon : utilise un paramètre de retour
function Calculer: Real;  
begin  
  Result := 10 * 20;
end;
```

### 3. Ordre logique

Ordonnez vos fonctions **du bas niveau vers le haut niveau** ou **par modules fonctionnels**.

```pascal
// Ordre logique : du plus simple au plus complexe

// Niveau 1 : Fonctions de base
function EstNombrePositif(n: Integer): Boolean;

// Niveau 2 : Fonctions intermédiaires
function ValiderAge(age: Integer): Boolean;

// Niveau 3 : Fonctions de haut niveau
procedure InscrireUtilisateur;
```

### 4. Un fichier = un ensemble cohérent

Si votre programme devient trop gros, divisez-le en **unités** (modules externes) - nous verrons ça au chapitre 7.

## Points clés à retenir

1. **Organisez** votre code en modules logiques
2. Chaque fonction doit avoir une **responsabilité unique**
3. Privilégiez la **cohésion forte** et le **couplage faible**
4. Utilisez des **noms explicites** et cohérents
5. **Commentez** les modules et algorithmes complexes
6. Structurez votre code en **sections logiques**
7. Préférez les **paramètres** aux **variables globales**
8. Gardez vos fonctions **courtes** (5-30 lignes)
9. Un code bien organisé est plus **facile à maintenir**
10. La modularité facilite la **réutilisation** et les **tests**

---

**Conclusion du chapitre 4 :** Vous maîtrisez maintenant les procédures et fonctions, de leur déclaration à leur organisation. Ces concepts sont **fondamentaux** en programmation et vous serviront tout au long de votre apprentissage. Dans le chapitre 5, nous découvrirons les **types de données structurés** (tableaux, enregistrements, etc.) qui vous permettront de manipuler des données plus complexes.

⏭️ [Types de Données Structurés](/05-types-donnees-structures/README.md)
