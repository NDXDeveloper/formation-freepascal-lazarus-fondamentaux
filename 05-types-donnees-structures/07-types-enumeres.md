🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 5.7 Types énumérés

## Qu'est-ce qu'un type énuméré ?

Un type énuméré est un type de donnée personnalisé qui définit un **ensemble limité de valeurs possibles**, chacune ayant un **nom significatif**. Au lieu d'utiliser des nombres arbitraires (0, 1, 2...), on utilise des noms qui ont du sens dans le contexte du programme.

### Analogie simple

Imaginez les jours de la semaine. Au lieu de dire "jour 1", "jour 2", "jour 3", on préfère dire "lundi", "mardi", "mercredi". C'est plus clair et plus facile à comprendre. Un type énuméré permet de faire exactement cela en programmation.

## Pourquoi utiliser des types énumérés ?

### Sans type énuméré (code confus)

```pascal
var
  jourSemaine: Integer;
begin
  jourSemaine := 1;  // Que représente 1 ? Lundi ? Dimanche ?

  if jourSemaine = 5 then
    WriteLn('C''est vendredi !');  // Il faut se souvenir que 5 = vendredi
end.
```

**Problèmes :**
- Les nombres n'ont pas de sens par eux-mêmes
- Erreurs possibles (qu'est-ce qui empêche d'écrire `jourSemaine := 42` ?)
- Code difficile à lire et maintenir

### Avec type énuméré (code clair)

```pascal
type
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  jour: TJourSemaine;
begin
  jour := Vendredi;

  if jour = Vendredi then
    WriteLn('C''est vendredi !');  // Beaucoup plus clair !
end.
```

**Avantages :**
- Code **auto-documenté** et lisible
- **Protection contre les erreurs** (impossible d'assigner une valeur invalide)
- Le compilateur peut vérifier la validité
- Plus facile à maintenir

## Déclaration d'un type énuméré

### Syntaxe générale

```pascal
type
  NomType = (valeur1, valeur2, valeur3, ...);
```

### Exemples de déclarations

```pascal
type
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

  TMois = (Janvier, Fevrier, Mars, Avril, Mai, Juin,
           Juillet, Aout, Septembre, Octobre, Novembre, Decembre);

  TCouleur = (Rouge, Vert, Bleu, Jaune, Noir, Blanc);

  TDirection = (Nord, Sud, Est, Ouest);

  TEtatCommande = (EnAttente, EnPreparation, Expediee, Livree, Annulee);

  TNiveauEtude = (Primaire, College, Lycee, Universite);

  TTypeVehicule = (Voiture, Moto, Camion, Bus);
```

**Convention :**
- Le préfixe `T` indique que c'est un type
- Les valeurs commencent généralement par une majuscule
- Les valeurs sont séparées par des virgules

## Utilisation des types énumérés

### Déclaration de variables

```pascal
var
  jour: TJourSemaine;
  couleur: TCouleur;
  direction: TDirection;
```

### Affectation de valeurs

```pascal
program ExempleEnum;  
type  
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  aujourdhui: TJourSemaine;
begin
  aujourdhui := Mercredi;

  if aujourdhui = Mercredi then
    WriteLn('Nous sommes mercredi');

  aujourdhui := Vendredi;
  WriteLn('Changement : maintenant c''est vendredi');
end.
```

### Ordre des valeurs

Les valeurs d'un type énuméré ont un **ordre naturel** correspondant à leur position dans la déclaration :

```pascal
type
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  // Lundi = position 0
  // Mardi = position 1
  // Mercredi = position 2
  // etc.
```

## Comparaison de valeurs énumérées

On peut comparer les valeurs avec les opérateurs habituels :

```pascal
program ComparaisonEnum;  
type  
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  jour1, jour2: TJourSemaine;
begin
  jour1 := Mardi;
  jour2 := Jeudi;

  // Égalité
  if jour1 = Mardi then
    WriteLn('C''est mardi');

  // Différence
  if jour1 <> jour2 then
    WriteLn('Les jours sont différents');

  // Ordre (basé sur la position dans la déclaration)
  if jour1 < jour2 then
    WriteLn('Mardi vient avant jeudi');  // Vrai !

  if Samedi > Vendredi then
    WriteLn('Samedi vient après vendredi');  // Vrai !
end.
```

## Fonctions utiles pour les types énumérés

### Ord() - Position ordinale

Retourne la position numérique de la valeur (commence à 0) :

```pascal
program FonctionOrd;  
type  
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  jour: TJourSemaine;
begin
  jour := Lundi;
  WriteLn('Position de Lundi : ', Ord(jour));  // 0

  jour := Mercredi;
  WriteLn('Position de Mercredi : ', Ord(jour));  // 2

  jour := Dimanche;
  WriteLn('Position de Dimanche : ', Ord(jour));  // 6
end.
```

### Succ() - Successeur

Retourne la valeur suivante :

```pascal
program FonctionSucc;  
type  
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  jour: TJourSemaine;
begin
  jour := Mardi;
  WriteLn('Aujourd''hui : Mardi');

  jour := Succ(jour);
  WriteLn('Demain ce sera : Mercredi (valeur suivante)');

  // Attention : Succ du dernier élément provoque une erreur !
  // jour := Dimanche;
  // jour := Succ(jour);  // ERREUR à l'exécution !
end.
```

### Pred() - Prédécesseur

Retourne la valeur précédente :

```pascal
program FonctionPred;  
type  
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  jour: TJourSemaine;
begin
  jour := Jeudi;
  WriteLn('Aujourd''hui : Jeudi');

  jour := Pred(jour);
  WriteLn('Hier c''était : Mercredi (valeur précédente)');

  // Attention : Pred du premier élément provoque une erreur !
  // jour := Lundi;
  // jour := Pred(jour);  // ERREUR à l'exécution !
end.
```

### Inc() et Dec()

Incrémenter ou décrémenter une valeur énumérée :

```pascal
program IncDecEnum;  
type  
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  jour: TJourSemaine;
begin
  jour := Lundi;

  Inc(jour);  // jour devient Mardi
  Inc(jour);  // jour devient Mercredi

  Dec(jour);  // jour redevient Mardi
end.
```

## Parcourir toutes les valeurs d'un type énuméré

### Avec une boucle for

```pascal
program ParcoursEnum;  
type  
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  jour: TJourSemaine;
begin
  WriteLn('Tous les jours de la semaine :');
  for jour := Lundi to Dimanche do
  begin
    Write('Jour numéro ', Ord(jour) + 1);  // +1 pour afficher 1-7 au lieu de 0-6
    case jour of
      Lundi: WriteLn(' : Lundi');
      Mardi: WriteLn(' : Mardi');
      Mercredi: WriteLn(' : Mercredi');
      Jeudi: WriteLn(' : Jeudi');
      Vendredi: WriteLn(' : Vendredi');
      Samedi: WriteLn(' : Samedi');
      Dimanche: WriteLn(' : Dimanche');
    end;
  end;
end.
```

## Utilisation avec CASE-OF

Les types énumérés sont parfaits avec l'instruction `case-of` :

```pascal
program CaseEnum;  
type  
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  jour: TJourSemaine;
begin
  jour := Mercredi;

  case jour of
    Lundi:
      WriteLn('Début de la semaine de travail');
    Mardi, Mercredi, Jeudi:
      WriteLn('En pleine semaine');
    Vendredi:
      WriteLn('Bientôt le week-end !');
    Samedi, Dimanche:
      WriteLn('C''est le week-end !');
  end;
end.
```

### Exemple avec action selon l'état

```pascal
program GestionEtat;  
type  
  TEtatCommande = (EnAttente, EnPreparation, Expediee, Livree, Annulee);

var
  etat: TEtatCommande;

procedure TraiterCommande(e: TEtatCommande);  
begin  
  case e of
    EnAttente:
      WriteLn('Commande en attente de validation');
    EnPreparation:
      WriteLn('Préparation en cours...');
    Expediee:
      WriteLn('Commande expédiée, livraison prévue sous 48h');
    Livree:
      WriteLn('Commande livrée avec succès');
    Annulee:
      WriteLn('Commande annulée');
  end;
end;

begin
  etat := EnPreparation;
  TraiterCommande(etat);

  etat := Expediee;
  TraiterCommande(etat);
end.
```

## Types énumérés et tableaux

On peut utiliser un type énuméré comme indice de tableau :

```pascal
program TableauEnum;  
type  
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  temperatures: array[TJourSemaine] of Real;  // Tableau indexé par le type énuméré (pas par Integer)
  jour: TJourSemaine;
begin
  // Saisie des températures
  for jour := Lundi to Dimanche do
  begin
    Write('Température pour ');
    case jour of
      Lundi: Write('lundi');
      Mardi: Write('mardi');
      Mercredi: Write('mercredi');
      Jeudi: Write('jeudi');
      Vendredi: Write('vendredi');
      Samedi: Write('samedi');
      Dimanche: Write('dimanche');
    end;
    Write(' : ');
    ReadLn(temperatures[jour]);
  end;

  // Affichage
  WriteLn;
  WriteLn('Températures de la semaine :');
  for jour := Lundi to Dimanche do
    WriteLn('  ', Ord(jour) + 1, '. ', temperatures[jour]:0:1, ' °C');
end.
```

### Exemple : Comptage par catégorie

```pascal
program ComptageCategorie;  
type  
  TCouleur = (Rouge, Vert, Bleu, Jaune);

var
  compteurs: array[TCouleur] of Integer;
  couleur: TCouleur;
begin
  // Initialisation
  for couleur := Rouge to Jaune do
    compteurs[couleur] := 0;

  // Simulation de comptage
  compteurs[Rouge] := 5;
  compteurs[Vert] := 3;
  compteurs[Bleu] := 7;
  compteurs[Jaune] := 2;

  // Affichage
  WriteLn('Statistiques :');
  WriteLn('Rouge : ', compteurs[Rouge]);
  WriteLn('Vert : ', compteurs[Vert]);
  WriteLn('Bleu : ', compteurs[Bleu]);
  WriteLn('Jaune : ', compteurs[Jaune]);
end.
```

## Exemples pratiques

### Exemple 1 : Gestion de feux tricolores

```pascal
program FeuxTricolores;  
type  
  TFeu = (Rouge, Orange, Vert);

var
  etatFeu: TFeu;

procedure AfficherFeu(feu: TFeu);  
begin  
  Write('Feu ');
  case feu of
    Rouge:  WriteLn('ROUGE - STOP');
    Orange: WriteLn('ORANGE - Attention');
    Vert:   WriteLn('VERT - Passez');
  end;
end;

function FeuSuivant(feu: TFeu): TFeu;  
begin  
  case feu of
    Rouge:  FeuSuivant := Vert;
    Vert:   FeuSuivant := Orange;
    Orange: FeuSuivant := Rouge;
  end;
end;

begin
  etatFeu := Rouge;
  WriteLn('État initial :');
  AfficherFeu(etatFeu);

  WriteLn;
  WriteLn('Cycle du feu :');
  etatFeu := FeuSuivant(etatFeu);
  AfficherFeu(etatFeu);

  etatFeu := FeuSuivant(etatFeu);
  AfficherFeu(etatFeu);

  etatFeu := FeuSuivant(etatFeu);
  AfficherFeu(etatFeu);
end.
```

### Exemple 2 : Système de notation

```pascal
program SystemeNotation;  
type  
  TMention = (Insuffisant, Passable, AssezBien, Bien, TresBien, Excellent);

function ObtenirMention(note: Real): TMention;  
begin  
  if note < 10 then
    ObtenirMention := Insuffisant
  else if note < 12 then
    ObtenirMention := Passable
  else if note < 14 then
    ObtenirMention := AssezBien
  else if note < 16 then
    ObtenirMention := Bien
  else if note < 18 then
    ObtenirMention := TresBien
  else
    ObtenirMention := Excellent;
end;

procedure AfficherMention(m: TMention);  
begin  
  Write('Mention : ');
  case m of
    Insuffisant: WriteLn('Insuffisant');
    Passable:    WriteLn('Passable');
    AssezBien:   WriteLn('Assez bien');
    Bien:        WriteLn('Bien');
    TresBien:    WriteLn('Très bien');
    Excellent:   WriteLn('Excellent');
  end;
end;

var
  note: Real;
  mention: TMention;
begin
  Write('Entrez une note sur 20 : ');
  ReadLn(note);

  mention := ObtenirMention(note);
  AfficherMention(mention);
end.
```

### Exemple 3 : Gestion des saisons

```pascal
program GestionSaisons;  
type  
  TSaison = (Printemps, Ete, Automne, Hiver);
  TMois = (Janvier, Fevrier, Mars, Avril, Mai, Juin,
           Juillet, Aout, Septembre, Octobre, Novembre, Decembre);

function ObtenirSaison(mois: TMois): TSaison;  
begin  
  case mois of
    Mars, Avril, Mai:
      ObtenirSaison := Printemps;
    Juin, Juillet, Aout:
      ObtenirSaison := Ete;
    Septembre, Octobre, Novembre:
      ObtenirSaison := Automne;
    Decembre, Janvier, Fevrier:
      ObtenirSaison := Hiver;
  end;
end;

procedure DecrireSaison(s: TSaison);  
begin  
  case s of
    Printemps:
      WriteLn('C''est le printemps : les fleurs éclosent');
    Ete:
      WriteLn('C''est l''été : il fait chaud');
    Automne:
      WriteLn('C''est l''automne : les feuilles tombent');
    Hiver:
      WriteLn('C''est l''hiver : il fait froid');
  end;
end;

var
  mois: TMois;
  saison: TSaison;
begin
  mois := Juillet;
  saison := ObtenirSaison(mois);

  WriteLn('Nous sommes en juillet');
  DecrireSaison(saison);
end.
```

### Exemple 4 : Menu de navigation

```pascal
program MenuNavigation;  
type  
  TMenuPrincipal = (Nouveau, Ouvrir, Enregistrer, Quitter);

var
  choix: TMenuPrincipal;
  saisie: Integer;

procedure AfficherMenu;  
begin  
  WriteLn;
  WriteLn('=== MENU PRINCIPAL ===');
  WriteLn('1. Nouveau');
  WriteLn('2. Ouvrir');
  WriteLn('3. Enregistrer');
  WriteLn('4. Quitter');
  Write('Votre choix : ');
end;

procedure ExecuterAction(action: TMenuPrincipal);  
begin  
  case action of
    Nouveau:
      WriteLn('Création d''un nouveau fichier...');
    Ouvrir:
      WriteLn('Ouverture d''un fichier existant...');
    Enregistrer:
      WriteLn('Enregistrement du fichier...');
    Quitter:
      WriteLn('Au revoir !');
  end;
end;

begin
  repeat
    AfficherMenu;
    ReadLn(saisie);

    case saisie of
      1: choix := Nouveau;
      2: choix := Ouvrir;
      3: choix := Enregistrer;
      4: choix := Quitter;
    else
      begin
        WriteLn('Choix invalide');
        Continue;
      end;
    end;

    ExecuterAction(choix);
  until choix = Quitter;
end.
```

## Conversion entre type énuméré et String

Pascal ne convertit pas automatiquement les types énumérés en chaînes. Il faut créer des fonctions :

```pascal
program ConversionEnum;  
type  
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

function JourEnString(jour: TJourSemaine): String;  
begin  
  case jour of
    Lundi:    JourEnString := 'Lundi';
    Mardi:    JourEnString := 'Mardi';
    Mercredi: JourEnString := 'Mercredi';
    Jeudi:    JourEnString := 'Jeudi';
    Vendredi: JourEnString := 'Vendredi';
    Samedi:   JourEnString := 'Samedi';
    Dimanche: JourEnString := 'Dimanche';
  end;
end;

var
  jour: TJourSemaine;
begin
  jour := Mercredi;
  WriteLn('Aujourd''hui c''est ', JourEnString(jour));

  for jour := Lundi to Dimanche do
    WriteLn(JourEnString(jour));
end.
```

## Types énumérés dans les enregistrements

```pascal
program EnumDansRecord;  
type  
  TEtatCommande = (EnAttente, EnPreparation, Expediee, Livree, Annulee);

  TCommande = record
    numero: String;
    client: String;
    montant: Real;
    etat: TEtatCommande;  // Type énuméré dans l'enregistrement
  end;

var
  commande: TCommande;
begin
  commande.numero := 'CMD-001';
  commande.client := 'Dupont';
  commande.montant := 150.00;
  commande.etat := EnPreparation;

  WriteLn('Commande ', commande.numero);
  WriteLn('Client : ', commande.client);
  WriteLn('Montant : ', commande.montant:0:2, ' €');

  case commande.etat of
    EnAttente:      WriteLn('État : En attente');
    EnPreparation:  WriteLn('État : En préparation');
    Expediee:       WriteLn('État : Expédiée');
    Livree:         WriteLn('État : Livrée');
    Annulee:        WriteLn('État : Annulée');
  end;
end.
```

## Pièges courants

### 1. Oublier qu'on ne peut pas afficher directement

```pascal
var
  jour: TJourSemaine;
begin
  jour := Mercredi;
  WriteLn(jour);  // ✗ N'affiche pas "Mercredi" mais un nombre
end.
```

**Solution :** Créer une fonction de conversion vers String.

### 2. Dépasser les limites avec Succ/Pred

```pascal
var
  jour: TJourSemaine;
begin
  jour := Dimanche;
  jour := Succ(jour);  // ✗ ERREUR : pas de jour après Dimanche !
end.
```

**Solution :** Vérifier avant d'utiliser Succ ou Pred.

### 3. Confusion entre valeur et position

```pascal
var
  jour: TJourSemaine;
begin
  jour := 3;  // ✗ ERREUR : on ne peut pas assigner un nombre
  jour := Mercredi;  // ✓ CORRECT

  WriteLn(Ord(jour));  // ✓ Affiche 2 (position)
end.
```

### 4. Oublier une valeur dans un CASE

```pascal
type
  TCouleur = (Rouge, Vert, Bleu, Jaune);

var
  couleur: TCouleur;
begin
  couleur := Jaune;

  case couleur of
    Rouge: WriteLn('Rouge');
    Vert:  WriteLn('Vert');
    Bleu:  WriteLn('Bleu');
    // Manque Jaune !
  end;  // ⚠ Avertissement du compilateur
end.
```

**Solution :** Toujours gérer tous les cas ou ajouter un `else`.

## Avantages des types énumérés

✓ **Lisibilité** : Code auto-documenté avec des noms significatifs  
✓ **Sécurité** : Le compilateur vérifie les valeurs  
✓ **Maintenabilité** : Plus facile de modifier le code  
✓ **Performance** : Aussi rapide que des entiers  
✓ **Exhaustivité** : Le compilateur peut vérifier que tous les cas sont gérés  
✓ **Typage fort** : Impossible de mélanger des types énumérés différents  

## Quand utiliser des types énumérés ?

✓ **Utilisez des types énumérés quand :**  
- Vous avez un ensemble **fixe et limité** de valeurs possibles  
- Les valeurs ont un **nom significatif** dans votre domaine  
- Vous voulez éviter les "nombres magiques" (0, 1, 2...)  
- Vous utilisez souvent des `case-of` sur ces valeurs  
- Vous voulez une meilleure vérification du compilateur  

✗ **N'utilisez pas de types énumérés quand :**  
- L'ensemble des valeurs peut changer fréquemment  
- Vous avez besoin de valeurs numériques arbitraires  
- L'ensemble est très grand (préférez une autre structure)  

## Résumé

Les types énumérés permettent de :
- Définir des **ensembles de valeurs nommées**
- Rendre le code plus **lisible et maintenable**
- Bénéficier de la **vérification du compilateur**
- Utiliser des noms **significatifs** au lieu de nombres

**Points clés à retenir :**
- Déclaration : `type NomType = (valeur1, valeur2, ...);`
- Les valeurs ont un **ordre naturel** (position)
- Fonctions utiles : `Ord()`, `Succ()`, `Pred()`, `Inc()`, `Dec()`
- Parfaits avec `case-of` et comme indices de tableaux
- Nécessitent des **fonctions de conversion** pour l'affichage
- Le compilateur vérifie que toutes les valeurs sont gérées

Les types énumérés sont un outil puissant pour écrire du code clair et sûr. Ils font partie des bonnes pratiques de programmation en Pascal !

⏭️ [Types ensemble (Set)](/05-types-donnees-structures/08-types-ensemble-set.md)
