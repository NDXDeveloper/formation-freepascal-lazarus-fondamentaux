🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 5.10 Définition de types personnalisés (Type)

## Qu'est-ce qu'un type personnalisé ?

Un type personnalisé est un **nouveau type de données** que vous créez pour répondre aux besoins spécifiques de votre programme. C'est comme créer vos propres "boîtes" sur mesure au lieu d'utiliser uniquement les boîtes standard (Integer, String, etc.).

### Analogie simple

Imaginez que vous construisez une maison avec des LEGO. Les briques de base sont les types standards (Integer, String, Boolean). Mais pour construire votre maison, vous créez des assemblages personnalisés : une fenêtre, une porte, un toit. Ces assemblages sont vos **types personnalisés** : des structures réutilisables que vous avez conçues pour votre projet.

## Pourquoi créer des types personnalisés ?

### Sans types personnalisés (code désordonné)

```pascal
program SansTypes;
var
  nom1, prenom1: String;
  age1: Integer;

  nom2, prenom2: String;
  age2: Integer;

  notes1: array[1..5] of Real;
  notes2: array[1..5] of Real;
begin
  // Code répétitif et difficile à maintenir
end.
```

### Avec types personnalisés (code organisé)

```pascal
program AvecTypes;
type
  TPersonne = record
    nom: String;
    prenom: String;
    age: Integer;
  end;

  TNotes = array[1..5] of Real;

  TEleve = record
    personne: TPersonne;
    notes: TNotes;
  end;

var
  eleve1, eleve2: TEleve;
begin
  // Code clair et maintenable
end.
```

**Avantages :**
- **Réutilisabilité** : définir une fois, utiliser partout
- **Lisibilité** : noms significatifs
- **Maintenabilité** : modifier en un seul endroit
- **Organisation** : structure logique du code
- **Documentation** : le code s'auto-documente

## La section TYPE

En Pascal, tous les types personnalisés se déclarent dans la section `type`, placée avant les variables :

```pascal
program StructureProgram;

type
  // Déclaration de tous les types personnalisés ici
  TMonType1 = ...;
  TMonType2 = ...;

var
  // Déclaration des variables ici
  variable1: TMonType1;
  variable2: TMonType2;

begin
  // Code du programme
end.
```

### Ordre de déclaration

Les types doivent être déclarés **dans l'ordre de leurs dépendances** :

```pascal
type
  // 1. Types de base en premier
  TAge = 0..120;
  TNom = String[50];

  // 2. Types simples
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

  // 3. Types qui utilisent les précédents
  TJours = set of TJour;

  // 4. Types composés
  TPersonne = record
    nom: TNom;
    age: TAge;
  end;

  // 5. Tableaux et structures complexes
  TPersonnes = array[1..100] of TPersonne;
```

## Conventions de nommage

### Règle du préfixe T

Par convention, les types personnalisés commencent par la lettre **T** (pour "Type") :

```pascal
type
  TEleve = record ...;      // ✓ Bon
  TProduit = record ...;    // ✓ Bon
  TCommande = record ...;   // ✓ Bon

  Eleve = record ...;       // ✗ Moins clair (mais valide)
```

### Noms descriptifs

Utilisez des noms qui décrivent clairement ce que représente le type :

```pascal
type
  // ✓ Bons noms
  TPersonne = record ...;
  TDateNaissance = record ...;
  TListeEleves = array ...;

  // ✗ Noms vagues
  TData = record ...;
  TInfo = record ...;
  TStuff = array ...;
```

### Singulier vs Pluriel

- **Singulier** pour un élément individuel
- **Pluriel** pour une collection

```pascal
type
  TEleve = record ...;              // Un élève
  TEleves = array[1..30] of TEleve; // Plusieurs élèves

  TProduit = record ...;            // Un produit
  TCatalogue = array of TProduit;   // Collection de produits
```

## Types de types personnalisés

Récapitulons tous les types que nous avons vus :

### 1. Alias de types simples

Donner un nouveau nom à un type existant pour plus de clarté :

```pascal
type
  TNom = String;
  TAge = Integer;
  TPrix = Real;
  TQuantite = Integer;

var
  nom: TNom;
  age: TAge;
  prix: TPrix;
```

### 2. Types intervalles

Restreindre les valeurs possibles :

```pascal
type
  TAge = 0..120;
  TNote = 0..20;
  TJourMois = 1..31;
  TMois = 1..12;
  TPourcentage = 0..100;
```

### 3. Types énumérés

Définir un ensemble de valeurs nommées :

```pascal
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TCouleur = (Rouge, Vert, Bleu, Jaune);
  TEtat = (Actif, Inactif, EnPause, Termine);
```

### 4. Types ensemble (Set)

Collections d'éléments uniques :

```pascal
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

  TOptions = set of (Option1, Option2, Option3);
```

### 5. Types tableaux

Collections ordonnées d'éléments :

```pascal
type
  TNotes = array[1..5] of Real;
  TMatrice = array[1..10, 1..10] of Integer;
  TChaine = array[1..100] of Char;
```

### 6. Types enregistrements (Record)

Structures regroupant plusieurs champs :

```pascal
type
  TPoint = record
    x: Real;
    y: Real;
  end;

  TPersonne = record
    nom: String;
    prenom: String;
    age: TAge;  // Utilise un autre type personnalisé
  end;
```

## Combinaison de types

La vraie puissance vient de la **combinaison** des différents types :

```pascal
program TypesCombines;
type
  // Types de base
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TNote = 0..20;

  // Types dérivés
  TJours = set of TJour;
  TNotes = array[1..3] of TNote;  // 3 matières

  // Types composés
  TEleve = record
    nom: String;
    prenom: String;
    notes: TNotes;
    joursPresents: TJours;
  end;

  // Collections
  TClasse = array[1..30] of TEleve;

var
  classe: TClasse;
  eleve: TEleve;
begin
  // Utilisation simple grâce aux types bien définis
  eleve.nom := 'Dupont';
  eleve.notes[1] := 15;
  eleve.joursPresents := [Lundi, Mardi, Jeudi, Vendredi];

  if Mercredi in eleve.joursPresents then
    WriteLn('Présent mercredi');
end.
```

## Réutilisabilité des types

Un type bien défini peut être utilisé à plusieurs endroits :

```pascal
type
  TAdresse = record
    rue: String;
    ville: String;
    codePostal: String;
  end;

  TPersonne = record
    nom: String;
    prenom: String;
    adresse: TAdresse;  // Réutilisation
  end;

  TEntreprise = record
    nom: String;
    siret: String;
    adresse: TAdresse;  // Même type réutilisé
  end;

  TContact = record
    personne: TPersonne;  // Réutilisation
    telephone: String;
    email: String;
  end;
```

## Exemples pratiques complets

### Exemple 1 : Système de gestion de bibliothèque

```pascal
program Bibliotheque;
type
  // Types de base
  TCategorie = (Roman, SF, Policier, Histoire, Science);
  TEtat = (Disponible, Emprunte, Perdu, EnReparation);

  // Types intervalles
  TISBNCode = String[13];
  TAnnee = 1900..2100;

  // Structures
  TLivre = record
    isbn: TISBNCode;
    titre: String;
    auteur: String;
    annee: TAnnee;
    categorie: TCategorie;
    etat: TEtat;
  end;

  TEmprunteur = record
    nom: String;
    prenom: String;
    numeroMembre: String;
  end;

  TEmprunt = record
    livre: TLivre;
    emprunteur: TEmprunteur;
    dateEmprunt: String;
    dateRetourPrevue: String;
  end;

  // Collections
  TCatalogue = array[1..1000] of TLivre;
  TListeEmprunts = array[1..100] of TEmprunt;

var
  catalogue: TCatalogue;
  emprunts: TListeEmprunts;
  nbLivres, nbEmprunts: Integer;

procedure AfficherLivre(livre: TLivre);
begin
  WriteLn('Titre : ', livre.titre);
  WriteLn('Auteur : ', livre.auteur);
  WriteLn('ISBN : ', livre.isbn);
  Write('État : ');
  case livre.etat of
    Disponible: WriteLn('Disponible');
    Emprunte: WriteLn('Emprunté');
    Perdu: WriteLn('Perdu');
    EnReparation: WriteLn('En réparation');
  end;
end;

begin
  // Exemple d'utilisation
  nbLivres := 1;
  catalogue[1].titre := 'Le Seigneur des Anneaux';
  catalogue[1].auteur := 'J.R.R. Tolkien';
  catalogue[1].isbn := '9780544003415';
  catalogue[1].annee := 1954;
  catalogue[1].categorie := SF;
  catalogue[1].etat := Disponible;

  AfficherLivre(catalogue[1]);
end.
```

### Exemple 2 : Gestion de planning

```pascal
program GestionPlanning;
type
  // Énumérés
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TTypeTache = (Reunion, Cours, Projet, Personnel);
  TPriorite = (Basse, Moyenne, Haute, Urgente);

  // Intervalles
  THeure = 0..23;
  TMinute = 0..59;

  // Structures de base
  THoraire = record
    heure: THeure;
    minute: TMinute;
  end;

  TTache = record
    titre: String;
    description: String;
    typeTache: TTypeTache;
    priorite: TPriorite;
    heureDebut: THoraire;
    heureFin: THoraire;
  end;

  // Planning journalier
  TJournee = record
    jour: TJour;
    taches: array[1..10] of TTache;
    nbTaches: Integer;
  end;

  // Planning hebdomadaire
  TPlanningHebdo = array[TJour] of TJournee;

var
  planning: TPlanningHebdo;
  tache: TTache;
  jour: TJour;

procedure InitialiserPlanning(var p: TPlanningHebdo);
var
  j: TJour;
begin
  for j := Lundi to Dimanche do
  begin
    p[j].jour := j;
    p[j].nbTaches := 0;
  end;
end;

procedure AjouterTache(var journee: TJournee; t: TTache);
begin
  if journee.nbTaches < 10 then
  begin
    journee.nbTaches := journee.nbTaches + 1;
    journee.taches[journee.nbTaches] := t;
  end;
end;

procedure AfficherTache(t: TTache);
begin
  WriteLn('  ', t.heureDebut.heure:2, ':', t.heureDebut.minute:2,
          ' - ', t.heureFin.heure:2, ':', t.heureFin.minute:2,
          ' : ', t.titre);
end;

begin
  InitialiserPlanning(planning);

  // Ajouter une tâche
  tache.titre := 'Réunion d''équipe';
  tache.typeTache := Reunion;
  tache.priorite := Haute;
  tache.heureDebut.heure := 9;
  tache.heureDebut.minute := 0;
  tache.heureFin.heure := 10;
  tache.heureFin.minute := 30;

  AjouterTache(planning[Lundi], tache);

  // Afficher le planning
  WriteLn('Planning de la semaine :');
  for jour := Lundi to Vendredi do
  begin
    WriteLn;
    Write('=== ');
    case jour of
      Lundi: Write('LUNDI');
      Mardi: Write('MARDI');
      Mercredi: Write('MERCREDI');
      Jeudi: Write('JEUDI');
      Vendredi: Write('VENDREDI');
      Samedi: Write('SAMEDI');
      Dimanche: Write('DIMANCHE');
    end;
    WriteLn(' ===');

    if planning[jour].nbTaches = 0 then
      WriteLn('  Aucune tâche')
    else
    begin
      var i: Integer;
      for i := 1 to planning[jour].nbTaches do
        AfficherTache(planning[jour].taches[i]);
    end;
  end;
end.
```

### Exemple 3 : Système de commandes

```pascal
program SystemeCommandes;
type
  // Énumérés
  TCategorieProduit = (Electronique, Vetement, Alimentaire, Livre);
  TEtatCommande = (EnAttente, Validee, EnPreparation, Expediee, Livree);
  TModePaiement = (CarteBancaire, Virement, Especes, Cheque);

  // Intervalles
  TQuantite = 1..999;
  TRemise = 0..100;  // Pourcentage

  // Structures de base
  TPrix = record
    montant: Real;
    devise: String;
  end;

  TProduit = record
    reference: String;
    designation: String;
    categorie: TCategorieProduit;
    prixUnitaire: TPrix;
    stock: Integer;
  end;

  TLigneCommande = record
    produit: TProduit;
    quantite: TQuantite;
    remise: TRemise;
  end;

  TAdresse = record
    numero: String;
    rue: String;
    codePostal: String;
    ville: String;
    pays: String;
  end;

  TClient = record
    nom: String;
    prenom: String;
    email: String;
    telephone: String;
    adresse: TAdresse;
  end;

  TCommande = record
    numero: String;
    client: TClient;
    lignes: array[1..20] of TLigneCommande;
    nbLignes: Integer;
    etat: TEtatCommande;
    modePaiement: TModePaiement;
    dateCommande: String;
  end;

function CalculerTotalLigne(ligne: TLigneCommande): Real;
var
  total, montantRemise: Real;
begin
  total := ligne.produit.prixUnitaire.montant * ligne.quantite;
  montantRemise := total * ligne.remise / 100;
  CalculerTotalLigne := total - montantRemise;
end;

function CalculerTotalCommande(commande: TCommande): Real;
var
  i: Integer;
  total: Real;
begin
  total := 0;
  for i := 1 to commande.nbLignes do
    total := total + CalculerTotalLigne(commande.lignes[i]);
  CalculerTotalCommande := total;
end;

procedure AfficherCommande(commande: TCommande);
var
  i: Integer;
begin
  WriteLn('=== COMMANDE N° ', commande.numero, ' ===');
  WriteLn('Client : ', commande.client.prenom, ' ', commande.client.nom);
  WriteLn('Email : ', commande.client.email);
  WriteLn;
  WriteLn('Articles :');

  for i := 1 to commande.nbLignes do
  begin
    with commande.lignes[i] do
    begin
      WriteLn('  ', produit.designation);
      WriteLn('    Quantité : ', quantite);
      WriteLn('    Prix unitaire : ', produit.prixUnitaire.montant:0:2, ' ',
              produit.prixUnitaire.devise);
      if remise > 0 then
        WriteLn('    Remise : ', remise, '%');
      WriteLn('    Total : ', CalculerTotalLigne(commande.lignes[i]):0:2, ' ',
              produit.prixUnitaire.devise);
    end;
    WriteLn;
  end;

  WriteLn('TOTAL COMMANDE : ', CalculerTotalCommande(commande):0:2, ' ',
          commande.lignes[1].produit.prixUnitaire.devise);

  Write('État : ');
  case commande.etat of
    EnAttente: WriteLn('En attente');
    Validee: WriteLn('Validée');
    EnPreparation: WriteLn('En préparation');
    Expediee: WriteLn('Expédiée');
    Livree: WriteLn('Livrée');
  end;
end;

var
  commande: TCommande;
begin
  // Initialisation d'une commande exemple
  commande.numero := 'CMD-2025-001';
  commande.dateCommande := '12/10/2025';
  commande.etat := EnPreparation;
  commande.modePaiement := CarteBancaire;

  // Client
  with commande.client do
  begin
    nom := 'Dupont';
    prenom := 'Marie';
    email := 'marie.dupont@example.com';
    telephone := '0123456789';
    adresse.rue := '10 rue de la Paix';
    adresse.codePostal := '75001';
    adresse.ville := 'Paris';
    adresse.pays := 'France';
  end;

  // Ligne 1
  commande.nbLignes := 1;
  with commande.lignes[1] do
  begin
    produit.reference := 'PROD-001';
    produit.designation := 'Ordinateur portable';
    produit.categorie := Electronique;
    produit.prixUnitaire.montant := 899.99;
    produit.prixUnitaire.devise := 'EUR';
    quantite := 1;
    remise := 10;
  end;

  AfficherCommande(commande);
end.
```

## Organisation des types dans un projet

### Fichier unique (petit projet)

```pascal
program PetitProjet;
type
  // Tous les types ici
  TType1 = ...;
  TType2 = ...;

var
  // Variables

begin
  // Code
end.
```

### Plusieurs unités (grand projet)

```pascal
// Fichier TypesCommuns.pas
unit TypesCommuns;

interface

type
  TAdresse = record
    rue: String;
    ville: String;
  end;

  TPersonne = record
    nom: String;
    adresse: TAdresse;
  end;

implementation

end.

// Fichier programme principal
program ProjetPrincipal;
uses TypesCommuns;

var
  personne: TPersonne;  // Type défini dans l'unité

begin
  // Code
end.
```

## Alias vs Nouveaux types

### Simple alias (compatible)

```pascal
type
  TAge1 = Integer;
  TAge2 = Integer;

var
  age1: TAge1;
  age2: TAge2;
begin
  age1 := 25;
  age2 := age1;  // ✓ OK : ce sont tous deux des Integer
end.
```

### Types distincts (FreePascal avec $MODE)

En mode objet, on peut créer des types vraiment distincts :

```pascal
{$MODE OBJFPC}
type
  TAge = type Integer;    // Nouveau type distinct
  TQuantite = type Integer;

var
  age: TAge;
  quantite: TQuantite;
begin
  age := 25;
  // quantite := age;  // ✗ Erreur : types incompatibles
  quantite := TQuantite(age);  // ✓ OK : conversion explicite
end.
```

## Conseils pour bien définir les types

### 1. Définir avant d'utiliser

Créez vos types **avant** de commencer à coder :

```pascal
// ✓ Bon : types définis en premier
type
  TPersonne = record
    nom: String;
  end;

procedure Traiter(p: TPersonne);
begin
end;
```

### 2. Noms significatifs

```pascal
// ✗ Vague
type TData = record ...;

// ✓ Clair
type TDonneesClient = record ...;
```

### 3. Granularité appropriée

Ne créez pas trop de types simples inutiles :

```pascal
// ✗ Trop granulaire
type
  TNom = String;
  TPrenom = String;
  TRue = String;
  TVille = String;

// ✓ Bon équilibre
type
  TAdresse = record
    rue: String;
    ville: String;
  end;
```

### 4. Regroupement logique

```pascal
type
  // Types de base ensemble
  TJour = (Lundi, Mardi, ...);
  TMois = (Janvier, Fevrier, ...);

  // Types dérivés ensemble
  TDate = record
    jour: Integer;
    mois: TMois;
    annee: Integer;
  end;
```

## Avantages des types personnalisés

✓ **Clarté** : Code auto-documenté
✓ **Sécurité** : Vérification de types par le compilateur
✓ **Réutilisabilité** : Définir une fois, utiliser partout
✓ **Maintenabilité** : Modification centralisée
✓ **Abstraction** : Masquer la complexité
✓ **Cohérence** : Utilisation uniforme dans tout le projet

## Résumé

Les types personnalisés permettent de :
- Créer des **structures de données adaptées** à votre domaine
- **Organiser et documenter** votre code
- Combiner différents types (énumérés, intervalles, records, etc.)
- Améliorer la **lisibilité et la maintenabilité**
- Bénéficier de la **vérification du compilateur**

**Points clés à retenir :**
- Section `type` avant les variables
- Convention : préfixe `T` pour les types
- Ordre de déclaration selon les dépendances
- Noms descriptifs et significatifs
- Combinaison de types pour structures complexes
- Réutilisation maximale des types
- Organisation logique par domaine

Les types personnalisés sont la **fondation** d'un code Pascal bien structuré. Prenez le temps de bien les concevoir : c'est un investissement qui facilite grandement le développement et la maintenance de vos programmes !

⏭️ [Pointeurs et Gestion Mémoire Basique](/06-pointeurs-gestion-memoire-basique/README.md)
