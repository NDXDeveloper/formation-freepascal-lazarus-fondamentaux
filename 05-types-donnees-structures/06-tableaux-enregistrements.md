🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 5.6 Tableaux d'enregistrements

## Qu'est-ce qu'un tableau d'enregistrements ?

Un tableau d'enregistrements combine deux concepts que vous connaissez déjà :
- **Tableau** : collection d'éléments du même type
- **Enregistrement** : structure regroupant plusieurs champs

Un tableau d'enregistrements est donc une **liste d'enregistrements**, comme une base de données simple en mémoire.

### Analogie simple

Imaginez un classeur contenant plusieurs fiches d'étudiants. Chaque fiche (enregistrement) contient les mêmes informations (nom, prénom, note), et le classeur (tableau) organise toutes ces fiches.

**Classeur d'étudiants :**
```
[1] -> Nom: Dupont,  Prénom: Marie,  Note: 15
[2] -> Nom: Martin,  Prénom: Jean,   Note: 12
[3] -> Nom: Durand,  Prénom: Sophie, Note: 18
```

## Pourquoi utiliser des tableaux d'enregistrements ?

### Sans tableau d'enregistrements

Pour gérer 3 étudiants, vous auriez besoin de :

```pascal
var
  nom1, prenom1: String;
  note1: Integer;

  nom2, prenom2: String;
  note2: Integer;

  nom3, prenom3: String;
  note3: Integer;
```

C'est lourd et ne permet pas de boucler facilement !

### Avec tableau d'enregistrements

```pascal
type
  TEleve = record
    nom: String;
    prenom: String;
    note: Integer;
  end;

var
  eleves: array[1..3] of TEleve;
```

**Avantages :**
- Code concis et organisé
- Facile à parcourir avec des boucles
- Nombre d'éléments modifiable facilement
- Opérations globales (recherche, tri) simples

## Déclaration d'un tableau d'enregistrements

### Syntaxe générale

```pascal
type
  NomEnregistrement = record
    champ1: Type1;
    champ2: Type2;
    // ...
  end;

var
  nomTableau: array[debut..fin] of NomEnregistrement;
```

### Exemples de déclarations

```pascal
type
  TEleve = record
    nom: String;
    prenom: String;
    note: Real;
  end;

  TProduit = record
    code: String;
    designation: String;
    prix: Real;
    stock: Integer;
  end;

  TContact = record
    nom: String;
    telephone: String;
    email: String;
  end;

var
  eleves: array[1..30] of TEleve;        // Classe de 30 élèves
  catalogue: array[1..100] of TProduit;  // 100 produits
  contacts: array[1..50] of TContact;    // Carnet de 50 contacts
```

## Accès aux éléments

Pour accéder à un champ d'un enregistrement dans le tableau, on combine les deux syntaxes :

```pascal
tableau[indice].champ
```

### Exemple simple

```pascal
program AccesTableauRecord;  
type  
  TEleve = record
    nom: String;
    prenom: String;
    note: Real;
  end;

var
  eleves: array[1..3] of TEleve;
begin
  // Affectation
  eleves[1].nom := 'Dupont';
  eleves[1].prenom := 'Marie';
  eleves[1].note := 15.5;

  eleves[2].nom := 'Martin';
  eleves[2].prenom := 'Jean';
  eleves[2].note := 12.0;

  eleves[3].nom := 'Durand';
  eleves[3].prenom := 'Sophie';
  eleves[3].note := 18.0;

  // Lecture
  WriteLn('Premier élève : ', eleves[1].prenom, ' ', eleves[1].nom);
  WriteLn('Sa note : ', eleves[1].note:0:1);
end.
```

**Sortie :**
```
Premier élève : Marie Dupont  
Sa note : 15.5  
```

## Parcours d'un tableau d'enregistrements

### Affichage de tous les éléments

```pascal
program ParcoursTableau;  
type  
  TEleve = record
    nom: String;
    prenom: String;
    note: Real;
  end;

var
  eleves: array[1..3] of TEleve;
  i: Integer;
begin
  // Initialisation (simplifié pour l'exemple)
  eleves[1].nom := 'Dupont';
  eleves[1].prenom := 'Marie';
  eleves[1].note := 15.5;

  eleves[2].nom := 'Martin';
  eleves[2].prenom := 'Jean';
  eleves[2].note := 12.0;

  eleves[3].nom := 'Durand';
  eleves[3].prenom := 'Sophie';
  eleves[3].note := 18.0;

  // Parcours et affichage
  WriteLn('Liste des élèves :');
  WriteLn('==================');
  for i := 1 to 3 do
  begin
    WriteLn(i, '. ', eleves[i].prenom, ' ', eleves[i].nom,
            ' - Note : ', eleves[i].note:0:1);
  end;
end.
```

**Sortie :**
```
Liste des élèves :
==================
1. Marie Dupont - Note : 15.5
2. Jean Martin - Note : 12.0
3. Sophie Durand - Note : 18.0
```

### Saisie de tous les éléments

```pascal
program SaisieTableau;  
type  
  TEleve = record
    nom: String;
    prenom: String;
    note: Real;
  end;

var
  eleves: array[1..3] of TEleve;
  i: Integer;
begin
  WriteLn('Saisie de 3 élèves');
  WriteLn('==================');

  for i := 1 to 3 do
  begin
    WriteLn('Élève ', i, ' :');
    Write('  Nom : ');
    ReadLn(eleves[i].nom);
    Write('  Prénom : ');
    ReadLn(eleves[i].prenom);
    Write('  Note : ');
    ReadLn(eleves[i].note);
    WriteLn;
  end;

  // Affichage récapitulatif
  WriteLn('Récapitulatif :');
  for i := 1 to 3 do
    WriteLn(eleves[i].prenom, ' ', eleves[i].nom, ' : ', eleves[i].note:0:1);
end.
```

## Opérations courantes

### Calcul de la moyenne

```pascal
program CalculMoyenne;  
type  
  TEleve = record
    nom: String;
    prenom: String;
    note: Real;
  end;

var
  eleves: array[1..5] of TEleve;
  i: Integer;
  somme, moyenne: Real;
begin
  // Supposons que les élèves sont déjà saisis...

  // Calcul de la somme
  somme := 0;
  for i := 1 to 5 do
    somme := somme + eleves[i].note;

  // Calcul de la moyenne
  moyenne := somme / 5;
  WriteLn('Moyenne de la classe : ', moyenne:0:2);
end.
```

### Recherche d'un élément

```pascal
program RechercheEleve;  
type  
  TEleve = record
    nom: String;
    prenom: String;
    note: Real;
  end;

var
  eleves: array[1..5] of TEleve;
  i: Integer;
  nomRecherche: String;
  trouve: Boolean;
begin
  // Supposons que les élèves sont déjà saisis...

  Write('Nom à rechercher : ');
  ReadLn(nomRecherche);

  trouve := False;
  for i := 1 to 5 do
  begin
    if eleves[i].nom = nomRecherche then
    begin
      WriteLn('Trouvé : ', eleves[i].prenom, ' ', eleves[i].nom);
      WriteLn('Note : ', eleves[i].note:0:1);
      trouve := True;
      Break;  // Arrêter la recherche
    end;
  end;

  if not trouve then
    WriteLn('Élève non trouvé');
end.
```

### Recherche du meilleur élève

```pascal
program MeilleurEleve;  
type  
  TEleve = record
    nom: String;
    prenom: String;
    note: Real;
  end;

var
  eleves: array[1..5] of TEleve;
  i, indiceMeilleur: Integer;
begin
  // Supposons que les élèves sont déjà saisis...

  // Recherche du meilleur
  indiceMeilleur := 1;
  for i := 2 to 5 do
  begin
    if eleves[i].note > eleves[indiceMeilleur].note then
      indiceMeilleur := i;
  end;

  WriteLn('Meilleur élève : ', eleves[indiceMeilleur].prenom, ' ',
          eleves[indiceMeilleur].nom);
  WriteLn('Note : ', eleves[indiceMeilleur].note:0:1);
end.
```

### Comptage selon critère

```pascal
program CompterAdmis;  
type  
  TEleve = record
    nom: String;
    prenom: String;
    note: Real;
  end;

var
  eleves: array[1..5] of TEleve;
  i, nbAdmis: Integer;
begin
  // Supposons que les élèves sont déjà saisis...

  // Compter les admis (note >= 10)
  nbAdmis := 0;
  for i := 1 to 5 do
  begin
    if eleves[i].note >= 10 then
      nbAdmis := nbAdmis + 1;
  end;

  WriteLn('Nombre d''élèves admis : ', nbAdmis, ' sur 5');
end.
```

## Tri d'un tableau d'enregistrements

### Tri par sélection (par nom)

```pascal
program TriSelection;  
type  
  TEleve = record
    nom: String;
    prenom: String;
    note: Real;
  end;

var
  eleves: array[1..5] of TEleve;
  i, j, indiceMin: Integer;
  temp: TEleve;
begin
  // Supposons que les élèves sont déjà saisis...

  // Tri par sélection (ordre alphabétique des noms)
  for i := 1 to 4 do
  begin
    indiceMin := i;
    for j := i + 1 to 5 do
    begin
      if eleves[j].nom < eleves[indiceMin].nom then
        indiceMin := j;
    end;

    // Échanger les enregistrements complets
    if indiceMin <> i then
    begin
      temp := eleves[i];
      eleves[i] := eleves[indiceMin];
      eleves[indiceMin] := temp;
    end;
  end;

  WriteLn('Liste triée par nom :');
  for i := 1 to 5 do
    WriteLn(eleves[i].nom, ' ', eleves[i].prenom);
end.
```

### Tri par notes (décroissant)

```pascal
program TriParNotes;  
type  
  TEleve = record
    nom: String;
    prenom: String;
    note: Real;
  end;

var
  eleves: array[1..5] of TEleve;
  i, j: Integer;
  temp: TEleve;
begin
  // Supposons que les élèves sont déjà saisis...

  // Tri à bulles (notes décroissantes)
  for i := 1 to 4 do
  begin
    for j := 1 to 5 - i do
    begin
      if eleves[j].note < eleves[j + 1].note then
      begin
        // Échanger
        temp := eleves[j];
        eleves[j] := eleves[j + 1];
        eleves[j + 1] := temp;
      end;
    end;
  end;

  WriteLn('Classement par notes (meilleures d''abord) :');
  for i := 1 to 5 do
    WriteLn(i, '. ', eleves[i].prenom, ' ', eleves[i].nom,
            ' : ', eleves[i].note:0:1);
end.
```

## Utilisation avec procédures et fonctions

### Procédure d'affichage

```pascal
type
  TEleve = record
    nom: String;
    prenom: String;
    note: Real;
  end;
  TTableauEleves = array[1..30] of TEleve;

procedure AfficherEleve(e: TEleve);  
begin  
  WriteLn(e.prenom, ' ', e.nom, ' - Note : ', e.note:0:1);
end;

procedure AfficherClasse(eleves: TTableauEleves; nbEleves: Integer);  
var  
  i: Integer;
begin
  WriteLn('=== Liste de la classe ===');
  for i := 1 to nbEleves do
  begin
    Write(i:2, '. ');
    AfficherEleve(eleves[i]);
  end;
  WriteLn('==========================');
end;
```

### Procédure de saisie

```pascal
procedure SaisirEleve(var e: TEleve);  
begin  
  Write('Nom : ');
  ReadLn(e.nom);
  Write('Prénom : ');
  ReadLn(e.prenom);
  Write('Note : ');
  ReadLn(e.note);
end;

procedure SaisirClasse(var eleves: TTableauEleves; var nbEleves: Integer);  
var  
  i: Integer;
begin
  Write('Nombre d''élèves : ');
  ReadLn(nbEleves);

  for i := 1 to nbEleves do
  begin
    WriteLn('Élève ', i, ' :');
    SaisirEleve(eleves[i]);
  end;
end;
```

### Fonction de recherche

```pascal
function ChercherParNom(eleves: TTableauEleves; nbEleves: Integer;
                        nom: String): Integer;
var
  i: Integer;
begin
  ChercherParNom := 0;  // 0 = non trouvé
  for i := 1 to nbEleves do
  begin
    if eleves[i].nom = nom then
    begin
      ChercherParNom := i;  // Retourner l'indice
      Exit;
    end;
  end;
end;
```

### Fonction de calcul

```pascal
function CalculerMoyenne(eleves: TTableauEleves; nbEleves: Integer): Real;  
var  
  i: Integer;
  somme: Real;
begin
  somme := 0;
  for i := 1 to nbEleves do
    somme := somme + eleves[i].note;
  CalculerMoyenne := somme / nbEleves;
end;

function CompterAdmis(eleves: TTableauEleves; nbEleves: Integer): Integer;  
var  
  i, compte: Integer;
begin
  compte := 0;
  for i := 1 to nbEleves do
    if eleves[i].note >= 10 then
      compte := compte + 1;
  CompterAdmis := compte;
end;
```

## Exemples pratiques complets

### Exemple 1 : Gestion de classe

```pascal
program GestionClasse;  
type  
  TEleve = record
    nom: String;
    prenom: String;
    note: Real;
  end;
  TClasse = array[1..30] of TEleve;

var
  eleves: TClasse;
  nbEleves: Integer;
  i: Integer;
  somme, moyenne: Real;

begin
  // Saisie du nombre d'élèves
  Write('Nombre d''élèves dans la classe : ');
  ReadLn(nbEleves);

  // Saisie des élèves
  for i := 1 to nbEleves do
  begin
    WriteLn;
    WriteLn('Élève ', i, ' :');
    Write('  Nom : ');
    ReadLn(eleves[i].nom);
    Write('  Prénom : ');
    ReadLn(eleves[i].prenom);
    Write('  Note : ');
    ReadLn(eleves[i].note);
  end;

  // Affichage de la liste
  WriteLn;
  WriteLn('=== LISTE DE LA CLASSE ===');
  for i := 1 to nbEleves do
    WriteLn(i:2, '. ', eleves[i].prenom, ' ', eleves[i].nom,
            ' : ', eleves[i].note:0:1, '/20');

  // Calcul de la moyenne
  somme := 0;
  for i := 1 to nbEleves do
    somme := somme + eleves[i].note;
  moyenne := somme / nbEleves;

  WriteLn;
  WriteLn('Moyenne de la classe : ', moyenne:0:2, '/20');
end.
```

### Exemple 2 : Catalogue de produits

```pascal
program CatalogueProduits;  
type  
  TProduit = record
    code: String;
    designation: String;
    prix: Real;
    stock: Integer;
  end;
  TCatalogue = array[1..100] of TProduit;

var
  produits: TCatalogue;
  nbProduits: Integer;
  i, indice: Integer;
  codeRecherche: String;
  trouve: Boolean;
  valeurTotale: Real;

begin
  // Pour l'exemple, initialisons quelques produits
  nbProduits := 3;

  produits[1].code := 'P001';
  produits[1].designation := 'Clavier mécanique';
  produits[1].prix := 89.99;
  produits[1].stock := 15;

  produits[2].code := 'P002';
  produits[2].designation := 'Souris optique';
  produits[2].prix := 25.50;
  produits[2].stock := 42;

  produits[3].code := 'P003';
  produits[3].designation := 'Écran 24 pouces';
  produits[3].prix := 199.00;
  produits[3].stock := 8;

  // Affichage du catalogue
  WriteLn('=== CATALOGUE ===');
  for i := 1 to nbProduits do
  begin
    WriteLn('Code : ', produits[i].code);
    WriteLn('Produit : ', produits[i].designation);
    WriteLn('Prix : ', produits[i].prix:0:2, ' €');
    WriteLn('Stock : ', produits[i].stock, ' unités');
    WriteLn('-----------------');
  end;

  // Recherche d'un produit
  Write('Code produit à rechercher : ');
  ReadLn(codeRecherche);

  trouve := False;
  for i := 1 to nbProduits do
  begin
    if produits[i].code = codeRecherche then
    begin
      WriteLn('Produit trouvé :');
      WriteLn('  ', produits[i].designation);
      WriteLn('  Prix : ', produits[i].prix:0:2, ' €');
      WriteLn('  Stock : ', produits[i].stock, ' unités');
      trouve := True;
      Break;
    end;
  end;

  if not trouve then
    WriteLn('Produit non trouvé');

  // Calcul de la valeur totale du stock
  valeurTotale := 0;
  for i := 1 to nbProduits do
    valeurTotale := valeurTotale + (produits[i].prix * produits[i].stock);

  WriteLn;
  WriteLn('Valeur totale du stock : ', valeurTotale:0:2, ' €');
end.
```

### Exemple 3 : Carnet d'adresses

```pascal
program CarnetAdresses;  
type  
  TContact = record
    nom: String;
    prenom: String;
    telephone: String;
    email: String;
  end;
  TCarnet = array[1..50] of TContact;

var
  contacts: TCarnet;
  nbContacts: Integer;
  choix, i: Integer;
  nomRecherche: String;

procedure AfficherMenu;  
begin  
  WriteLn;
  WriteLn('=== CARNET D''ADRESSES ===');
  WriteLn('1. Ajouter un contact');
  WriteLn('2. Lister tous les contacts');
  WriteLn('3. Rechercher un contact');
  WriteLn('4. Quitter');
  Write('Votre choix : ');
end;

procedure AjouterContact;  
begin  
  if nbContacts < 50 then
  begin
    nbContacts := nbContacts + 1;
    WriteLn('Nouveau contact :');
    Write('  Nom : ');
    ReadLn(contacts[nbContacts].nom);
    Write('  Prénom : ');
    ReadLn(contacts[nbContacts].prenom);
    Write('  Téléphone : ');
    ReadLn(contacts[nbContacts].telephone);
    Write('  Email : ');
    ReadLn(contacts[nbContacts].email);
    WriteLn('Contact ajouté !');
  end
  else
    WriteLn('Carnet plein !');
end;

procedure ListerContacts;  
var  
  i: Integer;
begin
  if nbContacts = 0 then
    WriteLn('Aucun contact dans le carnet')
  else
  begin
    WriteLn('=== Liste des contacts ===');
    for i := 1 to nbContacts do
    begin
      WriteLn(i, '. ', contacts[i].prenom, ' ', contacts[i].nom);
      WriteLn('   Tel : ', contacts[i].telephone);
      WriteLn('   Email : ', contacts[i].email);
      WriteLn('---');
    end;
  end;
end;

procedure RechercherContact;  
var  
  i: Integer;
  trouve: Boolean;
begin
  Write('Nom à rechercher : ');
  ReadLn(nomRecherche);

  trouve := False;
  for i := 1 to nbContacts do
  begin
    if contacts[i].nom = nomRecherche then
    begin
      WriteLn('Contact trouvé :');
      WriteLn('  ', contacts[i].prenom, ' ', contacts[i].nom);
      WriteLn('  Tel : ', contacts[i].telephone);
      WriteLn('  Email : ', contacts[i].email);
      trouve := True;
    end;
  end;

  if not trouve then
    WriteLn('Contact non trouvé');
end;

begin
  nbContacts := 0;

  repeat
    AfficherMenu;
    ReadLn(choix);

    case choix of
      1: AjouterContact;
      2: ListerContacts;
      3: RechercherContact;
      4: WriteLn('Au revoir !');
    else
      WriteLn('Choix invalide');
    end;
  until choix = 4;
end.
```

## Tableaux d'enregistrements imbriqués

On peut combiner tableaux et enregistrements imbriqués :

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
    adresse: TAdresse;  // Enregistrement imbriqué
  end;

var
  personnes: array[1..10] of TPersonne;
  i: Integer;
begin
  // Accès à un champ imbriqué
  personnes[1].nom := 'Dupont';
  personnes[1].adresse.ville := 'Paris';

  // Parcours
  for i := 1 to 10 do
    WriteLn(personnes[i].nom, ' habite ', personnes[i].adresse.ville);
end.
```

## Gestion dynamique du nombre d'éléments

Souvent, on ne remplit pas tout le tableau :

```pascal
const
  CAPACITE_MAX = 100;

type
  TProduit = record
    code: String;
    designation: String;
    prix: Real;
  end;

var
  produits: array[1..CAPACITE_MAX] of TProduit;
  nbProduits: Integer;  // Nombre réel de produits
  i: Integer;
begin
  nbProduits := 0;  // Tableau vide au départ

  // Ajouter un produit
  if nbProduits < CAPACITE_MAX then
  begin
    nbProduits := nbProduits + 1;
    // Saisir produits[nbProduits]...
  end;

  // Parcourir seulement les éléments utilisés
  for i := 1 to nbProduits do
    WriteLn(produits[i].designation);
end.
```

## Pièges courants

### 1. Oublier l'indice du tableau

```pascal
var
  eleves: array[1..5] of TEleve;
begin
  eleves.nom := 'Dupont';  // ✗ ERREUR : manque l'indice
  eleves[1].nom := 'Dupont';  // ✓ CORRECT
end.
```

### 2. Inverser tableau et champ

```pascal
eleves.nom[1] := 'Dupont';  // ✗ ERREUR : ordre incorrect  
eleves[1].nom := 'Dupont';  // ✓ CORRECT  
```

### 3. Déborder du tableau

```pascal
const
  NB_MAX = 10;
var
  eleves: array[1..NB_MAX] of TEleve;
  nbEleves: Integer;
begin
  nbEleves := 15;  // ✗ Trop grand !
  for i := 1 to nbEleves do  // Dépassement
    WriteLn(eleves[i].nom);
end.
```

**Solution :** Toujours vérifier que `nbEleves <= NB_MAX`.

### 4. Copier incorrectement

```pascal
// Pour copier un élément
eleves[2] := eleves[1];  // ✓ CORRECT : copie tout l'enregistrement

// Pour copier un tableau complet
for i := 1 to 5 do
  copie[i] := eleves[i];  // Il faut une boucle
```

## Comparaison avec d'autres structures

| Structure | Usage |
|-----------|-------|
| **Tableau simple** | Liste de valeurs du même type (nombres, noms) |
| **Enregistrement** | Plusieurs informations sur UNE entité |
| **Tableau d'enregistrements** | Plusieurs informations sur PLUSIEURS entités |

```pascal
// Tableau simple
var notes: array[1..5] of Real;  // Juste les notes

// Enregistrement
var eleve: TEleve;  // UN élève avec toutes ses infos

// Tableau d'enregistrements
var eleves: array[1..5] of TEleve;  // PLUSIEURS élèves complets
```

## Conseils pratiques

1. **Utilisez une constante** pour la taille maximale
2. **Gardez un compteur** du nombre réel d'éléments
3. **Créez des procédures** pour les opérations courantes (affichage, saisie, recherche)
4. **Vérifiez toujours** les limites du tableau
5. **Initialisez** le compteur à 0 au départ
6. **Pensez au tri** si vous devez afficher dans un ordre particulier

## Résumé

Les tableaux d'enregistrements permettent de :
- Gérer des **collections de données structurées**
- Créer des **bases de données simples** en mémoire
- Combiner la puissance des **tableaux et enregistrements**
- Effectuer des opérations globales (recherche, tri, statistiques)

**Points clés à retenir :**
- Déclaration : `array[debut..fin] of TypeEnregistrement`
- Accès : `tableau[indice].champ`
- Toujours gérer un **compteur** du nombre d'éléments réels
- Utiliser des **procédures/fonctions** pour les opérations courantes
- Vérifier les **limites** du tableau

Les tableaux d'enregistrements sont essentiels pour créer des applications de gestion (élèves, produits, contacts, etc.). C'est la base de nombreux programmes pratiques !

⏭️ [Types énumérés](/05-types-donnees-structures/07-types-enumeres.md)
