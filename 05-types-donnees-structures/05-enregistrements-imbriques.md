🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 5.5 Enregistrements imbriqués

## Qu'est-ce qu'un enregistrement imbriqué ?

Un enregistrement imbriqué est un enregistrement qui contient **d'autres enregistrements comme champs**. C'est comme une boîte qui contient d'autres boîtes, créant ainsi une structure hiérarchique.

### Analogie simple

Imaginez une fiche d'employé qui contient :
- Nom et prénom (informations simples)
- **Adresse complète** (elle-même composée de rue, ville, code postal)
- **Date de naissance** (elle-même composée de jour, mois, année)

Au lieu de mettre tous ces champs au même niveau, on peut **regrouper** les informations liées dans des sous-enregistrements.

## Pourquoi utiliser des enregistrements imbriqués ?

### Sans imbrication (structure plate)

```pascal
type
  TEmploye = record
    nom: String;
    prenom: String;
    rue: String;
    ville: String;
    codePostal: String;
    jourNaissance: Integer;
    moisNaissance: Integer;
    anneeNaissance: Integer;
  end;
```

**Problèmes :**
- Tous les champs au même niveau
- Difficulté à identifier ce qui va ensemble
- Code moins organisé

### Avec imbrication (structure hiérarchique)

```pascal
type
  TAdresse = record
    rue: String;
    ville: String;
    codePostal: String;
  end;

  TDate = record
    jour: Integer;
    mois: Integer;
    annee: Integer;
  end;

  TEmploye = record
    nom: String;
    prenom: String;
    adresse: TAdresse;        // Enregistrement imbriqué
    dateNaissance: TDate;     // Enregistrement imbriqué
  end;
```

**Avantages :**
- Structure logique et claire
- Réutilisation des types (`TAdresse` et `TDate` peuvent servir ailleurs)
- Code plus maintenable
- Correspond mieux à la réalité

## Déclaration d'enregistrements imbriqués

### Syntaxe générale

```pascal
type
  // D'abord les types de base
  TTypeA = record
    champ1: Type;
    champ2: Type;
  end;

  // Puis le type qui les contient
  TTypeB = record
    champSimple: Type;
    champRecord: TTypeA;  // Imbrication
  end;
```

**Important :** Les types utilisés doivent être déclarés **avant** d'être utilisés dans un autre type.

### Exemples de déclarations

```pascal
type
  // Types de base
  TPoint = record
    x: Real;
    y: Real;
  end;

  TDate = record
    jour: Integer;
    mois: Integer;
    annee: Integer;
  end;

  TAdresse = record
    numero: Integer;
    rue: String;
    ville: String;
    codePostal: String;
  end;

  // Types avec imbrication
  TLigne = record
    debut: TPoint;     // Point de départ
    fin: TPoint;       // Point d'arrivée
  end;

  TPersonne = record
    nom: String;
    prenom: String;
    adresse: TAdresse;        // Adresse complète
    dateNaissance: TDate;     // Date de naissance
  end;

  TRectangle = record
    coinHautGauche: TPoint;
    coinBasDroite: TPoint;
  end;
```

## Accès aux champs imbriqués

Pour accéder à un champ dans un enregistrement imbriqué, on utilise **plusieurs points** successifs :

```pascal
variable.champ1.champ2
```

### Exemple simple

```pascal
program AccesImbriques;
type
  TAdresse = record
    rue: String;
    ville: String;
    codePostal: String;
  end;

  TPersonne = record
    nom: String;
    prenom: String;
    adresse: TAdresse;
  end;

var
  personne: TPersonne;
begin
  // Accès aux champs simples
  personne.nom := 'Dupont';
  personne.prenom := 'Marie';

  // Accès aux champs imbriqués
  personne.adresse.rue := '10 rue de la Paix';
  personne.adresse.ville := 'Paris';
  personne.adresse.codePostal := '75001';

  // Affichage
  WriteLn(personne.prenom, ' ', personne.nom);
  WriteLn(personne.adresse.rue);
  WriteLn(personne.adresse.codePostal, ' ', personne.adresse.ville);
end.
```

**Sortie :**
```
Marie Dupont
10 rue de la Paix
75001 Paris
```

## Initialisation d'enregistrements imbriqués

### Méthode 1 : Champ par champ

```pascal
var
  personne: TPersonne;
begin
  personne.nom := 'Martin';
  personne.prenom := 'Jean';
  personne.adresse.rue := '5 avenue Victor Hugo';
  personne.adresse.ville := 'Lyon';
  personne.adresse.codePostal := '69000';
  personne.dateNaissance.jour := 15;
  personne.dateNaissance.mois := 3;
  personne.dateNaissance.annee := 1985;
end.
```

### Méthode 2 : Utilisation de WITH

```pascal
var
  personne: TPersonne;
begin
  with personne do
  begin
    nom := 'Martin';
    prenom := 'Jean';

    with adresse do
    begin
      rue := '5 avenue Victor Hugo';
      ville := 'Lyon';
      codePostal := '69000';
    end;

    with dateNaissance do
    begin
      jour := 15;
      mois := 3;
      annee := 1985;
    end;
  end;
end.
```

### Méthode 3 : Initialisation par sous-parties

```pascal
var
  personne: TPersonne;
  uneAdresse: TAdresse;
  uneDate: TDate;
begin
  // Préparer l'adresse
  uneAdresse.rue := '5 avenue Victor Hugo';
  uneAdresse.ville := 'Lyon';
  uneAdresse.codePostal := '69000';

  // Préparer la date
  uneDate.jour := 15;
  uneDate.mois := 3;
  uneDate.annee := 1985;

  // Assigner à la personne
  personne.nom := 'Martin';
  personne.prenom := 'Jean';
  personne.adresse := uneAdresse;
  personne.dateNaissance := uneDate;
end.
```

## Utilisation avec procédures et fonctions

### Afficher un enregistrement imbriqué

```pascal
program AffichageImbrique;
type
  TDate = record
    jour: Integer;
    mois: Integer;
    annee: Integer;
  end;

  TAdresse = record
    rue: String;
    ville: String;
    codePostal: String;
  end;

  TPersonne = record
    nom: String;
    prenom: String;
    adresse: TAdresse;
    dateNaissance: TDate;
  end;

procedure AfficherDate(d: TDate);
begin
  WriteLn(d.jour, '/', d.mois, '/', d.annee);
end;

procedure AfficherAdresse(a: TAdresse);
begin
  WriteLn(a.rue);
  WriteLn(a.codePostal, ' ', a.ville);
end;

procedure AfficherPersonne(p: TPersonne);
begin
  WriteLn('=== Fiche Personne ===');
  WriteLn('Nom : ', p.prenom, ' ', p.nom);
  Write('Né(e) le : ');
  AfficherDate(p.dateNaissance);
  WriteLn('Adresse :');
  AfficherAdresse(p.adresse);
  WriteLn('======================');
end;

var
  personne: TPersonne;
begin
  // Initialisation
  personne.nom := 'Durand';
  personne.prenom := 'Sophie';
  personne.adresse.rue := '12 boulevard Haussmann';
  personne.adresse.ville := 'Marseille';
  personne.adresse.codePostal := '13001';
  personne.dateNaissance.jour := 20;
  personne.dateNaissance.mois := 7;
  personne.dateNaissance.annee := 1990;

  AfficherPersonne(personne);
end.
```

### Modifier un sous-enregistrement

```pascal
procedure ModifierAdresse(var p: TPersonne; nouvelleVille: String);
begin
  p.adresse.ville := nouvelleVille;
end;

procedure Demenager(var p: TPersonne; a: TAdresse);
begin
  p.adresse := a;  // Remplace toute l'adresse
end;
```

### Fonctions de comparaison

```pascal
function DatesEgales(d1, d2: TDate): Boolean;
begin
  DatesEgales := (d1.jour = d2.jour) and
                 (d1.mois = d2.mois) and
                 (d1.annee = d2.annee);
end;

function AdressesEgales(a1, a2: TAdresse): Boolean;
begin
  AdressesEgales := (a1.rue = a2.rue) and
                    (a1.ville = a2.ville) and
                    (a1.codePostal = a2.codePostal);
end;
```

## Exemples pratiques

### Exemple 1 : Gestion de points géométriques

```pascal
program Geometrie;
type
  TPoint = record
    x: Real;
    y: Real;
  end;

  TLigne = record
    depart: TPoint;
    arrivee: TPoint;
  end;

  TRectangle = record
    coinHautGauche: TPoint;
    coinBasDroite: TPoint;
  end;

function Distance(p1, p2: TPoint): Real;
begin
  Distance := Sqrt(Sqr(p2.x - p1.x) + Sqr(p2.y - p1.y));
end;

function LongueurLigne(ligne: TLigne): Real;
begin
  LongueurLigne := Distance(ligne.depart, ligne.arrivee);
end;

function SurfaceRectangle(rect: TRectangle): Real;
var
  largeur, hauteur: Real;
begin
  largeur := Abs(rect.coinBasDroite.x - rect.coinHautGauche.x);
  hauteur := Abs(rect.coinBasDroite.y - rect.coinHautGauche.y);
  SurfaceRectangle := largeur * hauteur;
end;

var
  ligne: TLigne;
  rectangle: TRectangle;
begin
  // Définir une ligne
  ligne.depart.x := 0;
  ligne.depart.y := 0;
  ligne.arrivee.x := 3;
  ligne.arrivee.y := 4;

  WriteLn('Longueur de la ligne : ', LongueurLigne(ligne):0:2);

  // Définir un rectangle
  rectangle.coinHautGauche.x := 0;
  rectangle.coinHautGauche.y := 10;
  rectangle.coinBasDroite.x := 5;
  rectangle.coinBasDroite.y := 0;

  WriteLn('Surface du rectangle : ', SurfaceRectangle(rectangle):0:2);
end.
```

### Exemple 2 : Carnet d'adresses complet

```pascal
program CarnetComplet;
type
  TTelephone = record
    fixe: String;
    mobile: String;
  end;

  TAdresse = record
    rue: String;
    ville: String;
    codePostal: String;
    pays: String;
  end;

  TContact = record
    nom: String;
    prenom: String;
    email: String;
    telephones: TTelephone;
    adresse: TAdresse;
  end;

procedure SaisirContact(var c: TContact);
begin
  WriteLn('=== Nouveau contact ===');
  Write('Nom : ');
  ReadLn(c.nom);
  Write('Prénom : ');
  ReadLn(c.prenom);
  Write('Email : ');
  ReadLn(c.email);

  WriteLn('--- Téléphones ---');
  Write('Fixe : ');
  ReadLn(c.telephones.fixe);
  Write('Mobile : ');
  ReadLn(c.telephones.mobile);

  WriteLn('--- Adresse ---');
  Write('Rue : ');
  ReadLn(c.adresse.rue);
  Write('Code postal : ');
  ReadLn(c.adresse.codePostal);
  Write('Ville : ');
  ReadLn(c.adresse.ville);
  Write('Pays : ');
  ReadLn(c.adresse.pays);
end;

procedure AfficherContact(c: TContact);
begin
  WriteLn('========================');
  WriteLn('Contact : ', c.prenom, ' ', c.nom);
  WriteLn('Email : ', c.email);
  WriteLn('Téléphones :');
  WriteLn('  Fixe : ', c.telephones.fixe);
  WriteLn('  Mobile : ', c.telephones.mobile);
  WriteLn('Adresse :');
  WriteLn('  ', c.adresse.rue);
  WriteLn('  ', c.adresse.codePostal, ' ', c.adresse.ville);
  WriteLn('  ', c.adresse.pays);
  WriteLn('========================');
end;

var
  contact: TContact;
begin
  SaisirContact(contact);
  WriteLn;
  AfficherContact(contact);
end.
```

### Exemple 3 : Commande avec articles

```pascal
program GestionCommande;
type
  TDate = record
    jour: Integer;
    mois: Integer;
    annee: Integer;
  end;

  TClient = record
    nom: String;
    email: String;
  end;

  TArticle = record
    reference: String;
    designation: String;
    prixUnitaire: Real;
    quantite: Integer;
  end;

  TCommande = record
    numero: String;
    dateCommande: TDate;
    client: TClient;
    article: TArticle;
  end;

function MontantTotal(cmd: TCommande): Real;
begin
  MontantTotal := cmd.article.prixUnitaire * cmd.article.quantite;
end;

procedure AfficherCommande(cmd: TCommande);
begin
  WriteLn('===== COMMANDE N° ', cmd.numero, ' =====');
  WriteLn('Date : ', cmd.dateCommande.jour, '/',
          cmd.dateCommande.mois, '/', cmd.dateCommande.annee);
  WriteLn('Client : ', cmd.client.nom);
  WriteLn('Email : ', cmd.client.email);
  WriteLn;
  WriteLn('Article : ', cmd.article.designation);
  WriteLn('Référence : ', cmd.article.reference);
  WriteLn('Prix unitaire : ', cmd.article.prixUnitaire:0:2, ' €');
  WriteLn('Quantité : ', cmd.article.quantite);
  WriteLn;
  WriteLn('TOTAL : ', MontantTotal(cmd):0:2, ' €');
  WriteLn('=====================================');
end;

var
  commande: TCommande;
begin
  // Initialisation de la commande
  commande.numero := 'CMD-2025-001';

  with commande.dateCommande do
  begin
    jour := 15;
    mois := 3;
    annee := 2025;
  end;

  with commande.client do
  begin
    nom := 'Entreprise ABC';
    email := 'contact@abc.fr';
  end;

  with commande.article do
  begin
    reference := 'REF-123';
    designation := 'Clavier mécanique';
    prixUnitaire := 89.99;
    quantite := 5;
  end;

  AfficherCommande(commande);
end.
```

### Exemple 4 : Entreprise avec employés

```pascal
program ExempleEntreprise;
type
  TDate = record
    jour: Integer;
    mois: Integer;
    annee: Integer;
  end;

  TAdresse = record
    rue: String;
    ville: String;
    codePostal: String;
  end;

  TEmploye = record
    nom: String;
    prenom: String;
    dateEmbauche: TDate;
    salaire: Real;
  end;

  TEntreprise = record
    nom: String;
    siret: String;
    adresse: TAdresse;
    directeur: TEmploye;
  end;

function AncienneteAnnees(emp: TEmploye; dateActuelle: TDate): Integer;
begin
  AncienneteAnnees := dateActuelle.annee - emp.dateEmbauche.annee;
end;

procedure AfficherEntreprise(ent: TEntreprise);
begin
  WriteLn('===== ENTREPRISE =====');
  WriteLn('Nom : ', ent.nom);
  WriteLn('SIRET : ', ent.siret);
  WriteLn('Adresse : ', ent.adresse.rue);
  WriteLn('          ', ent.adresse.codePostal, ' ', ent.adresse.ville);
  WriteLn;
  WriteLn('Directeur : ', ent.directeur.prenom, ' ', ent.directeur.nom);
  WriteLn('Embauché le : ', ent.directeur.dateEmbauche.jour, '/',
          ent.directeur.dateEmbauche.mois, '/',
          ent.directeur.dateEmbauche.annee);
  WriteLn('Salaire : ', ent.directeur.salaire:0:2, ' €');
  WriteLn('======================');
end;

var
  entreprise: TEntreprise;
begin
  entreprise.nom := 'TechCorp';
  entreprise.siret := '12345678901234';

  entreprise.adresse.rue := '15 rue de l''Innovation';
  entreprise.adresse.ville := 'Toulouse';
  entreprise.adresse.codePostal := '31000';

  entreprise.directeur.nom := 'Leblanc';
  entreprise.directeur.prenom := 'Pierre';
  entreprise.directeur.salaire := 5000.00;
  entreprise.directeur.dateEmbauche.jour := 1;
  entreprise.directeur.dateEmbauche.mois := 1;
  entreprise.directeur.dateEmbauche.annee := 2015;

  AfficherEntreprise(entreprise);
end.
```

## Niveaux multiples d'imbrication

On peut imbriquer sur plusieurs niveaux :

```pascal
type
  TPoint = record
    x: Real;
    y: Real;
  end;

  TRectangle = record
    coinHautGauche: TPoint;
    coinBasDroite: TPoint;
  end;

  TFenetre = record
    titre: String;
    position: TRectangle;  // Contient 2 TPoint
    visible: Boolean;
  end;

var
  fenetre: TFenetre;
begin
  // Accès à 3 niveaux
  fenetre.position.coinHautGauche.x := 10;
  fenetre.position.coinHautGauche.y := 20;
end.
```

**Conseil :** Évitez de dépasser 3 niveaux d'imbrication pour garder le code lisible.

## Copie d'enregistrements imbriqués

La copie fonctionne comme pour les enregistrements simples :

```pascal
var
  personne1, personne2: TPersonne;
begin
  // Initialiser personne1
  personne1.nom := 'Dupont';
  personne1.adresse.ville := 'Paris';

  // Copie complète (tous les sous-enregistrements)
  personne2 := personne1;

  // personne2 a maintenant les mêmes valeurs
  WriteLn(personne2.nom);           // Dupont
  WriteLn(personne2.adresse.ville); // Paris

  // Copie d'un sous-enregistrement uniquement
  personne2.adresse := personne1.adresse;
end.
```

## Pièges courants

### 1. Oublier un niveau d'accès

```pascal
var
  personne: TPersonne;
begin
  personne.ville := 'Paris';  // ✗ ERREUR : ville est dans adresse
  personne.adresse.ville := 'Paris';  // ✓ CORRECT
end.
```

### 2. Ordre de déclaration des types

```pascal
type
  // ✗ ERREUR : TPersonne utilise TAdresse qui n'est pas encore déclaré
  TPersonne = record
    nom: String;
    adresse: TAdresse;
  end;

  TAdresse = record
    ville: String;
  end;
```

**Solution :** Déclarer les types de base avant les types qui les utilisent.

### 3. WITH imbriqués difficiles à lire

```pascal
with personne do
  with adresse do
    with dateNaissance do
      WriteLn(jour);  // ✗ Peu lisible, difficile à déboguer
```

**Meilleure pratique :** Limiter l'imbrication des `with` ou les éviter complètement.

### 4. Confusion entre niveaux

```pascal
var
  entreprise: TEntreprise;
begin
  // ✗ ERREUR : dateEmbauche est dans directeur, pas dans entreprise
  entreprise.dateEmbauche.annee := 2020;

  // ✓ CORRECT
  entreprise.directeur.dateEmbauche.annee := 2020;
end.
```

## Avantages et inconvénients

### Avantages  

✓ **Organisation logique** : Structure claire et hiérarchique  
✓ **Réutilisation** : Les types de base peuvent servir ailleurs  
✓ **Maintenabilité** : Plus facile à comprendre et modifier  
✓ **Modularité** : Chaque sous-partie peut avoir ses propres fonctions  
✓ **Correspond au réel** : Modélise mieux les objets du monde réel  

### Inconvénients

✗ **Complexité** : Plus difficile pour les débutants  
✗ **Verbosité** : Accès aux champs plus long (plusieurs points)  
✗ **Débogage** : Plus difficile à inspecter en profondeur  

## Quand utiliser des enregistrements imbriqués ?

✓ **Utilisez l'imbrication quand :**  
- Les données ont une structure naturellement hiérarchique  
- Vous pouvez réutiliser des types (TAdresse, TDate, etc.)  
- Vous voulez séparer logiquement les responsabilités  
- Les sous-parties ont du sens indépendamment  

✗ **Évitez l'imbrication quand :**  
- La structure devient trop profonde (> 3 niveaux)  
- Les champs sont tous indépendants  
- Cela complique inutilement le code  

## Comparaison : Plat vs Imbriqué

```pascal
// Structure PLATE
type
  TPersonnePlate = record
    nom: String;
    prenom: String;
    rue: String;
    ville: String;
    codePostal: String;
    jour: Integer;
    mois: Integer;
    annee: Integer;
  end;

// Structure IMBRIQUÉE
type
  TDate = record
    jour: Integer;
    mois: Integer;
    annee: Integer;
  end;

  TAdresse = record
    rue: String;
    ville: String;
    codePostal: String;
  end;

  TPersonneImbriquee = record
    nom: String;
    prenom: String;
    adresse: TAdresse;
    dateNaissance: TDate;
  end;
```

La version imbriquée est plus longue à déclarer mais beaucoup plus claire et réutilisable.

## Résumé

Les enregistrements imbriqués permettent de :
- Créer des **structures hiérarchiques** complexes
- **Regrouper logiquement** les données liées
- **Réutiliser** des types dans différents contextes
- Modéliser des **entités du monde réel** de manière naturelle

**Points clés à retenir :**
- Déclarer les types de base **avant** les types qui les utilisent
- Accès avec **plusieurs points** : `variable.sousRecord.champ`
- Copie complète possible : `record2 := record1`
- Limiter à **2-3 niveaux** d'imbrication maximum
- Créer des **fonctions dédiées** pour chaque sous-partie

Les enregistrements imbriqués sont un outil puissant pour structurer des données complexes de manière claire et maintenable. C'est une étape importante vers la maîtrise des structures de données avancées.

⏭️ [Tableaux d'enregistrements](/05-types-donnees-structures/06-tableaux-enregistrements.md)
