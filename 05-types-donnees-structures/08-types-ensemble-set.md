🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 5.8 Types ensemble (Set)

## Qu'est-ce qu'un type ensemble ?

Un type ensemble (ou **set** en anglais) est une structure de données qui représente un **ensemble mathématique** : une collection de valeurs **uniques** et **non ordonnées** d'un même type. C'est comme un sac où chaque élément ne peut apparaître qu'une seule fois.

### Analogie simple

Imaginez un panier de fruits. Vous pouvez dire :
- "J'ai des pommes dans mon panier" (appartenance)
- "J'ai des pommes ET des oranges" (union)
- "J'ai des pommes MAIS PAS de bananes" (différence)
- "Quels fruits ai-je en commun avec toi ?" (intersection)

Un ensemble en Pascal permet de faire exactement cela avec n'importe quel type de données.

### Caractéristiques des ensembles

- **Uniques** : chaque valeur n'apparaît qu'une fois
- **Non ordonnés** : pas de notion de premier ou dernier élément
- **Limités** : seulement 256 valeurs maximum (contrainte technique)
- **Type de base** : doit être un type ordinal (entier, caractère, énuméré)

## Pourquoi utiliser des ensembles ?

### Sans ensemble (code compliqué)

```pascal
var
  aLundi, aMardi, aMercredi, aJeudi, aVendredi: Boolean;
begin
  // Marquer les jours travaillés
  aLundi := True;
  aMardi := True;
  aMercredi := False;  // Absent mercredi
  aJeudi := True;
  aVendredi := True;

  // Vérifier si présent lundi
  if aLundi then
    WriteLn('Présent lundi');
end.
```

### Avec ensemble (code élégant)

```pascal
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

var
  joursTravailles: TJours;
begin
  joursTravailles := [Lundi, Mardi, Jeudi, Vendredi];

  if Lundi in joursTravailles then
    WriteLn('Travaille lundi');
end.
```

**Avantages :**
- Plus concis et lisible
- Opérations ensemblistes naturelles
- Gestion simple de collections
- Performance excellente

## Déclaration d'un type ensemble

### Syntaxe générale

```pascal
type
  NomTypeBase = (valeur1, valeur2, ...);
  NomTypeSet = set of NomTypeBase;
```

### Exemples de déclarations

```pascal
type
  // Ensemble de jours
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

  // Ensemble de couleurs
  TCouleur = (Rouge, Vert, Bleu, Jaune, Noir, Blanc);
  TCouleurs = set of TCouleur;

  // Ensemble de chiffres
  TChiffres = set of 0..9;

  // Ensemble de lettres
  TLettres = set of 'A'..'Z';

  // Ensemble de caractères
  TCaracteres = set of Char;
```

**Convention :** Le type ensemble est généralement le pluriel du type de base (TJour → TJours).

## Déclaration et initialisation de variables

### Déclaration

```pascal
var
  joursOuvres: TJours;
  couleursPrimaires: TCouleurs;
  chiffres: TChiffres;
```

### Initialisation avec des valeurs

```pascal
program InitEnsemble;
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

var
  joursOuvres: TJours;
  weekend: TJours;
begin
  // Ensemble avec plusieurs éléments
  joursOuvres := [Lundi, Mardi, Mercredi, Jeudi, Vendredi];

  // Ensemble avec deux éléments
  weekend := [Samedi, Dimanche];

  // Ensemble vide
  joursOuvres := [];

  // Ensemble avec un seul élément
  joursOuvres := [Lundi];
end.
```

### Notation avec crochets

Les valeurs d'un ensemble sont toujours entre **crochets** `[ ]`.

## Test d'appartenance avec IN

L'opérateur `in` vérifie si un élément appartient à un ensemble :

```pascal
program TestAppartenance;
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

var
  joursOuvres: TJours;
begin
  joursOuvres := [Lundi, Mardi, Mercredi, Jeudi, Vendredi];

  // Test d'appartenance
  if Lundi in joursOuvres then
    WriteLn('Lundi est un jour ouvré');

  if not (Samedi in joursOuvres) then
    WriteLn('Samedi n''est pas un jour ouvré');

  // Équivalent à plusieurs OR
  // Au lieu de : if (jour = Lundi) or (jour = Mardi) or (jour = Mercredi)...
  if Mercredi in joursOuvres then
    WriteLn('Mercredi est un jour ouvré');
end.
```

**Sortie :**
```
Lundi est un jour ouvré
Samedi n'est pas un jour ouvré
Mercredi est un jour ouvré
```

## Opérations sur les ensembles

### Union (+)

Combine deux ensembles (tous les éléments des deux) :

```pascal
program Union;
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

var
  semaine1, semaine2, total: TJours;
begin
  semaine1 := [Lundi, Mardi];
  semaine2 := [Mercredi, Jeudi];

  // Union : tous les éléments des deux ensembles
  total := semaine1 + semaine2;
  // total = [Lundi, Mardi, Mercredi, Jeudi]

  if Mercredi in total then
    WriteLn('Mercredi est dans l''union');
end.
```

### Intersection (*)

Éléments communs aux deux ensembles :

```pascal
program Intersection;
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

var
  jours1, jours2, communs: TJours;
begin
  jours1 := [Lundi, Mardi, Mercredi];
  jours2 := [Mardi, Mercredi, Jeudi];

  // Intersection : éléments présents dans les DEUX ensembles
  communs := jours1 * jours2;
  // communs = [Mardi, Mercredi]

  if Mardi in communs then
    WriteLn('Mardi est dans les deux ensembles');

  if not (Lundi in communs) then
    WriteLn('Lundi n''est pas dans les deux ensembles');
end.
```

### Différence (-)

Éléments du premier ensemble qui ne sont pas dans le second :

```pascal
program Difference;
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

var
  tousLesJours, weekend, joursOuvres: TJours;
begin
  tousLesJours := [Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche];
  weekend := [Samedi, Dimanche];

  // Différence : éléments du premier SAUF ceux du second
  joursOuvres := tousLesJours - weekend;
  // joursOuvres = [Lundi, Mardi, Mercredi, Jeudi, Vendredi]

  if Lundi in joursOuvres then
    WriteLn('Lundi est un jour ouvré');

  if not (Samedi in joursOuvres) then
    WriteLn('Samedi n''est pas un jour ouvré');
end.
```

### Comparaison d'ensembles

```pascal
program ComparaisonEnsembles;
type
  TChiffres = set of 0..9;

var
  ensemble1, ensemble2: TChiffres;
begin
  ensemble1 := [1, 2, 3];
  ensemble2 := [1, 2, 3];

  // Égalité
  if ensemble1 = ensemble2 then
    WriteLn('Les ensembles sont identiques');

  // Différence
  ensemble2 := [1, 2, 4];
  if ensemble1 <> ensemble2 then
    WriteLn('Les ensembles sont différents');

  // Inclusion (sous-ensemble)
  ensemble1 := [1, 2];
  ensemble2 := [1, 2, 3, 4];
  if ensemble1 <= ensemble2 then
    WriteLn('[1,2] est inclus dans [1,2,3,4]');

  // Inclusion stricte (pas d'opérateur < pour les sets en FreePascal)
  if (ensemble1 <= ensemble2) and (ensemble1 <> ensemble2) then
    WriteLn('[1,2] est strictement inclus dans [1,2,3,4]');
end.
```

## Opérations d'ajout et suppression

### Include - Ajouter un élément

```pascal
program AjouterElement;
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

var
  jours: TJours;
begin
  jours := [Lundi, Mardi];
  WriteLn('Départ : Lundi et Mardi');

  // Ajouter Mercredi
  Include(jours, Mercredi);

  if Mercredi in jours then
    WriteLn('Mercredi a été ajouté');

  // Alternative avec +
  jours := jours + [Jeudi];
end.
```

### Exclude - Retirer un élément

```pascal
program RetirerElement;
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

var
  jours: TJours;
begin
  jours := [Lundi, Mardi, Mercredi];
  WriteLn('Départ : Lundi, Mardi et Mercredi');

  // Retirer Mardi
  Exclude(jours, Mardi);

  if not (Mardi in jours) then
    WriteLn('Mardi a été retiré');

  // Alternative avec -
  jours := jours - [Mercredi];
end.
```

## Exemples pratiques

### Exemple 1 : Jours de présence

```pascal
program JoursPresence;
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

var
  joursTravail: TJours;
  joursConges: TJours;
  joursPresents: TJours;
  jour: TJour;

function NomJour(j: TJour): String;
begin
  case j of
    Lundi: NomJour := 'Lundi';
    Mardi: NomJour := 'Mardi';
    Mercredi: NomJour := 'Mercredi';
    Jeudi: NomJour := 'Jeudi';
    Vendredi: NomJour := 'Vendredi';
    Samedi: NomJour := 'Samedi';
    Dimanche: NomJour := 'Dimanche';
  end;
end;

begin
  // Définir les jours de travail normaux
  joursTravail := [Lundi, Mardi, Mercredi, Jeudi, Vendredi];

  // Jours en congé cette semaine
  joursConges := [Mercredi];

  // Jours effectivement présents = travail SAUF congés
  joursPresents := joursTravail - joursConges;

  WriteLn('Jours de présence cette semaine :');
  for jour := Lundi to Vendredi do
  begin
    if jour in joursPresents then
      WriteLn('  ', NomJour(jour));
  end;
end.
```

**Sortie :**
```
Jours de présence cette semaine :
  Lundi
  Mardi
  Jeudi
  Vendredi
```

### Exemple 2 : Validation de mot de passe

```pascal
{$mode objfpc}{$H+}
program ValidationMotDePasse;
type
  TTypeCaractere = (Minuscule, Majuscule, Chiffre, Special);
  TTypesCaracteres = set of TTypeCaractere;

function AnalyserMotDePasse(mdp: String): TTypesCaracteres;
var
  i: Integer;
  c: Char;
begin
  Result := [];  // Ensemble vide au départ

  for i := 1 to Length(mdp) do
  begin
    c := mdp[i];

    if (c >= 'a') and (c <= 'z') then
      Include(Result, Minuscule)
    else if (c >= 'A') and (c <= 'Z') then
      Include(Result, Majuscule)
    else if (c >= '0') and (c <= '9') then
      Include(Result, Chiffre)
    else
      Include(Result, Special);
  end;
end;

function MotDePasseValide(mdp: String): Boolean;
var
  types: TTypesCaracteres;
begin
  // Un mot de passe valide doit contenir au moins 3 types
  types := AnalyserMotDePasse(mdp);

  // Compter les types présents
  MotDePasseValide :=
    (Minuscule in types) and
    (Majuscule in types) and
    ((Chiffre in types) or (Special in types));
end;

var
  motDePasse: String;
  types: TTypesCaracteres;
begin
  Write('Entrez un mot de passe : ');
  ReadLn(motDePasse);

  types := AnalyserMotDePasse(motDePasse);

  WriteLn;
  WriteLn('Analyse :');
  if Minuscule in types then WriteLn('  Contient des minuscules');
  if Majuscule in types then WriteLn('  Contient des majuscules');
  if Chiffre in types then WriteLn('  Contient des chiffres');
  if Special in types then WriteLn('  Contient des caractères spéciaux');

  WriteLn;
  if MotDePasseValide(motDePasse) then
    WriteLn('Mot de passe VALIDE')
  else
    WriteLn('Mot de passe INVALIDE (doit contenir minuscules, majuscules et chiffres/spéciaux)');
end.
```

### Exemple 3 : Gestion des permissions

```pascal
program GestionPermissions;
type
  TPermission = (Lecture, Ecriture, Execution, Suppression);
  TPermissions = set of TPermission;

var
  admin, utilisateur, invite: TPermissions;

procedure AfficherPermissions(perms: TPermissions; nom: String);
begin
  WriteLn('Permissions de ', nom, ' :');
  if Lecture in perms then WriteLn('  - Lecture');
  if Ecriture in perms then WriteLn('  - Écriture');
  if Execution in perms then WriteLn('  - Exécution');
  if Suppression in perms then WriteLn('  - Suppression');
  if perms = [] then WriteLn('  - Aucune permission');
  WriteLn;
end;

function PeutModifier(perms: TPermissions): Boolean;
begin
  // Peut modifier si a lecture ET écriture
  PeutModifier := [Lecture, Ecriture] <= perms;
end;

begin
  // Définir les permissions par rôle
  admin := [Lecture, Ecriture, Execution, Suppression];
  utilisateur := [Lecture, Ecriture];
  invite := [Lecture];

  AfficherPermissions(admin, 'Administrateur');
  AfficherPermissions(utilisateur, 'Utilisateur');
  AfficherPermissions(invite, 'Invité');

  // Tests
  if PeutModifier(admin) then
    WriteLn('Admin peut modifier les fichiers');

  if PeutModifier(utilisateur) then
    WriteLn('Utilisateur peut modifier les fichiers');

  if not PeutModifier(invite) then
    WriteLn('Invité ne peut PAS modifier les fichiers');
end.
```

### Exemple 4 : Voyelles et consonnes

```pascal
program VoyellesConsonnes;
type
  TLettres = set of 'A'..'Z';

var
  voyelles: TLettres;
  phrase: String;
  i: Integer;
  c: Char;
  nbVoyelles, nbConsonnes: Integer;

begin
  // Définir les voyelles
  voyelles := ['A', 'E', 'I', 'O', 'U', 'Y'];

  Write('Entrez une phrase : ');
  ReadLn(phrase);

  nbVoyelles := 0;
  nbConsonnes := 0;

  for i := 1 to Length(phrase) do
  begin
    c := UpCase(phrase[i]);  // Convertir en majuscule

    if (c >= 'A') and (c <= 'Z') then
    begin
      if c in voyelles then
        nbVoyelles := nbVoyelles + 1
      else
        nbConsonnes := nbConsonnes + 1;
    end;
  end;

  WriteLn('Voyelles : ', nbVoyelles);
  WriteLn('Consonnes : ', nbConsonnes);
end.
```

### Exemple 5 : Planification hebdomadaire

```pascal
program PlanningHebdo;
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

  TActivite = record
    nom: String;
    jours: TJours;
  end;

var
  sport, cours, travail: TActivite;
  joursLibres: TJours;
  tousLesJours: TJours;
  jour: TJour;

function NomJour(j: TJour): String;
begin
  case j of
    Lundi: NomJour := 'Lundi';
    Mardi: NomJour := 'Mardi';
    Mercredi: NomJour := 'Mercredi';
    Jeudi: NomJour := 'Jeudi';
    Vendredi: NomJour := 'Vendredi';
    Samedi: NomJour := 'Samedi';
    Dimanche: NomJour := 'Dimanche';
  end;
end;

begin
  // Définir les activités
  sport.nom := 'Sport';
  sport.jours := [Mardi, Jeudi];

  cours.nom := 'Cours';
  cours.jours := [Lundi, Mardi, Mercredi, Jeudi, Vendredi];

  travail.nom := 'Travail';
  travail.jours := [Mercredi, Samedi];

  // Tous les jours de la semaine
  tousLesJours := [Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche];

  // Calculer les jours libres
  joursLibres := tousLesJours - (sport.jours + cours.jours + travail.jours);

  WriteLn('Planning de la semaine :');
  for jour := Lundi to Dimanche do
  begin
    Write(NomJour(jour), ' : ');

    if jour in sport.jours then Write('Sport ');
    if jour in cours.jours then Write('Cours ');
    if jour in travail.jours then Write('Travail ');
    if jour in joursLibres then Write('LIBRE');

    WriteLn;
  end;

  WriteLn;
  WriteLn('Jours complètement libres :');
  for jour := Lundi to Dimanche do
    if jour in joursLibres then
      WriteLn('  ', NomJour(jour));
end.
```

## Ensembles de caractères

Les ensembles de caractères sont très utiles pour la validation :

```pascal
program EnsemblesCaracteres;
type
  TCaracteres = set of Char;

var
  chiffres: TCaracteres;
  lettres: TCaracteres;
  voyelles: TCaracteres;
  caracteresSaisis: String;
  c: Char;
  i: Integer;

begin
  // Définir des ensembles de caractères
  chiffres := ['0'..'9'];
  lettres := ['a'..'z', 'A'..'Z'];
  voyelles := ['a', 'e', 'i', 'o', 'u', 'y', 'A', 'E', 'I', 'O', 'U', 'Y'];

  Write('Entrez du texte : ');
  ReadLn(caracteresSaisis);

  WriteLn('Analyse :');
  for i := 1 to Length(caracteresSaisis) do
  begin
    c := caracteresSaisis[i];

    Write('  "', c, '" : ');
    if c in chiffres then Write('chiffre ');
    if c in lettres then Write('lettre ');
    if c in voyelles then Write('voyelle');
    WriteLn;
  end;
end.
```

## Ensemble vide et ensemble complet

```pascal
program EnsembleVideComplet;
type
  TChiffres = set of 0..9;

var
  vide: TChiffres;
  complet: TChiffres;
begin
  // Ensemble vide
  vide := [];
  if vide = [] then
    WriteLn('L''ensemble est vide');

  // Ensemble complet
  complet := [0..9];  // Tous les chiffres de 0 à 9
  if 5 in complet then
    WriteLn('5 est dans l''ensemble complet');
end.
```

## Limitations des ensembles

### Limitation de taille

Les ensembles sont limités à **256 éléments maximum** en Pascal standard :

```pascal
type
  // ✓ OK : 10 éléments
  TChiffres = set of 0..9;

  // ✓ OK : 26 éléments
  TLettresMaj = set of 'A'..'Z';

  // ✗ ERREUR : trop d'éléments (65536)
  // TEntiers = set of Word;

  // ✓ OK : sous-ensemble restreint
  TPetitsEntiers = set of 0..255;
```

### On ne peut pas parcourir directement

Il n'y a pas de boucle `for element in ensemble` en Pascal standard. Il faut parcourir tous les éléments possibles et tester :

```pascal
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

var
  jours: TJours;
  jour: TJour;
begin
  jours := [Lundi, Mercredi, Vendredi];

  // Parcourir en testant chaque jour
  for jour := Lundi to Dimanche do
    if jour in jours then
      WriteLn('Jour présent');
end.
```

## Pièges courants

### 1. Oublier les crochets

```pascal
var
  jours: TJours;
begin
  jours := Lundi;  // ✗ ERREUR : manque les crochets
  jours := [Lundi];  // ✓ CORRECT
end.
```

### 2. Confondre IN et =

```pascal
if Lundi = jours then  // ✗ ERREUR : compare un jour à un ensemble
if Lundi in jours then  // ✓ CORRECT : teste l'appartenance
```

### 3. Tenter de parcourir directement

```pascal
// ✗ Impossible en Pascal standard
for jour in jours do
  WriteLn(jour);

// ✓ CORRECT : parcourir tous et tester
for jour := Lundi to Dimanche do
  if jour in jours then
    WriteLn(jour);
```

### 4. Dépasser la limite de 256 éléments

```pascal
type
  TGrandsNombres = set of 0..1000;  // ✗ ERREUR : trop d'éléments
  TPetitsNombres = set of 0..255;   // ✓ CORRECT : dans la limite
```

## Avantages des ensembles

✓ **Lisibilité** : Code clair et expressif  
✓ **Performance** : Opérations très rapides (manipulation de bits)  
✓ **Sécurité** : Pas de doublons possibles  
✓ **Opérations mathématiques** : Union, intersection, différence naturelles  
✓ **Tests multiples** : Remplace plusieurs `OR` par un simple `in`  

## Quand utiliser des ensembles ?

✓ **Utilisez des ensembles quand :**  
- Vous avez besoin de tester l'appartenance rapidement  
- Vous manipulez des collections sans ordre ni doublons  
- Vous faites des opérations ensemblistes (union, intersection)  
- Le nombre d'éléments possibles est ≤ 256  
- Vous travaillez avec des drapeaux (options actives/inactives)  

✗ **N'utilisez pas d'ensembles quand :**  
- Vous avez besoin de l'ordre des éléments  
- Vous devez compter les occurrences  
- Le type de base a plus de 256 valeurs possibles  
- Vous devez parcourir souvent tous les éléments  

## Résumé

Les types ensemble (Set) permettent de :
- Représenter des **collections d'éléments uniques**
- Effectuer des **opérations ensemblistes** (union, intersection, différence)
- Tester rapidement l'**appartenance** avec `in`
- Gérer des **drapeaux et options**

**Points clés à retenir :**
- Déclaration : `type NomSet = set of TypeBase;`
- Notation : crochets `[element1, element2, ...]`
- Opérateurs : `+` (union), `*` (intersection), `-` (différence)
- Test : `element in ensemble`
- Modification : `Include()`, `Exclude()`
- Limitation : **maximum 256 éléments** possibles
- Comparaison : `=`, `<>`, `<=` (inclusion)

Les ensembles sont un outil puissant pour gérer des collections logiques et effectuer des tests d'appartenance de manière élégante et performante !

⏭️ [Types intervalle](/05-types-donnees-structures/09-types-intervalle.md)
