🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 6.7 Arbres Binaires Basics

## Introduction

Un **arbre binaire** est une structure de données hiérarchique où chaque noeud peut avoir au maximum deux enfants : un enfant gauche et un enfant droit. Cette structure est fondamentale en informatique et permet de résoudre de nombreux problèmes de manière efficace.

## Concept d'Arbre Binaire

### Analogie de l'Arbre Généalogique

Imaginez un arbre généalogique simplifié :
- La **racine** est l'ancêtre au sommet
- Chaque personne peut avoir **au maximum deux enfants**
- Les personnes sans enfants sont des **feuilles**
- On peut **descendre** dans l'arbre pour explorer les descendants

### Analogie de l'Organigramme

Comme une entreprise où :
- Le **PDG** est à la racine
- Chaque manager a **au maximum deux subordonnés directs**
- Les employés sans subordonnés sont des **feuilles**

### Représentation Visuelle

```
         50
        /  \
       /    \
      30    70
     / \    / \
    20 40  60 80
```

Dans cet arbre :
- **50** est la racine
- **30** et **70** sont les enfants de 50
- **20, 40, 60, 80** sont les feuilles (pas d'enfants)

## Terminologie Essentielle

### Vocabulaire de Base

- **Racine** : Le noeud au sommet de l'arbre
- **Noeud** : Chaque élément de l'arbre
- **Feuille** : Un noeud sans enfants
- **Enfant** : Noeud directement sous un autre noeud
- **Parent** : Noeud directement au-dessus d'un autre
- **Sous-arbre** : Un noeud et tous ses descendants
- **Hauteur** : Le nombre maximum de niveaux depuis la racine

### Exemple Annoté

```
         50    ← Racine (niveau 0, hauteur 2)
        /  \
       /    \
      30    70  ← Niveau 1
     / \    / \
    20 40  60 80 ← Niveau 2 (Feuilles)

Hauteur de l'arbre : 2  
Nombre de noeuds : 7  
Nombre de feuilles : 4  
```

## Structure d'un Noeud

### Déclaration

Un noeud d'arbre binaire contient :
1. Une **donnée** (la valeur stockée)
2. Un pointeur vers l'**enfant gauche**
3. Un pointeur vers l'**enfant droit**

```pascal
type
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: Integer;
    gauche: PNoeud;
    droite: PNoeud;
  end;
```

### Comparaison avec Liste Chaînée

| Structure | Pointeurs |
|-----------|-----------|
| Liste chaînée | 1 pointeur (suivant) |
| Arbre binaire | 2 pointeurs (gauche, droite) |

## Création d'un Arbre

### Créer un Noeud Simple

```pascal
function CreerNoeud(valeur: Integer): PNoeud;  
begin  
  New(Result);
  Result^.donnee := valeur;
  Result^.gauche := nil;
  Result^.droite := nil;
end;

var
  racine: PNoeud;
begin
  racine := CreerNoeud(50);
  WriteLn('Racine créée : ', racine^.donnee);
end;
```

### Construire un Arbre Manuellement

```pascal
var
  racine: PNoeud;
begin
  // Créer la racine
  racine := CreerNoeud(50);

  // Créer les enfants de la racine
  racine^.gauche := CreerNoeud(30);
  racine^.droite := CreerNoeud(70);

  // Créer les petits-enfants
  racine^.gauche^.gauche := CreerNoeud(20);
  racine^.gauche^.droite := CreerNoeud(40);
  racine^.droite^.gauche := CreerNoeud(60);
  racine^.droite^.droite := CreerNoeud(80);

  WriteLn('Arbre créé');
end;
```

**Résultat :**
```
         50
        /  \
       30   70
      / \   / \
     20 40 60 80
```

## Arbre Binaire de Recherche (ABR)

### Définition

Un **Arbre Binaire de Recherche** (ABR ou BST en anglais) respecte la propriété suivante :
- Tous les noeuds du **sous-arbre gauche** ont des valeurs **inférieures** au noeud parent
- Tous les noeuds du **sous-arbre droit** ont des valeurs **supérieures** au noeud parent

### Exemple d'ABR

```
         50
        /  \
       30   70    ← 30 < 50 < 70
      / \   / \
     20 40 60 80  ← 20 < 30, 40 > 30, etc.
```

**Avantage :** Recherche efficace en O(log n) dans le meilleur cas.

### Contre-exemple (Pas un ABR)

```
         50
        /  \
       70   30    ← ERREUR : 70 > 50 mais à gauche !
      / \   / \
     20 40 60 80
```

## Insertion dans un ABR

### Principe

Pour insérer une valeur :
1. Comparer avec la racine
2. Si **inférieure** : aller à gauche
3. Si **supérieure** : aller à droite
4. Répéter jusqu'à trouver une place vide
5. Créer le nouveau noeud à cet emplacement

### Fonction d'Insertion Récursive

```pascal
procedure Inserer(var racine: PNoeud; valeur: Integer);  
begin  
  // Cas de base : arbre vide ou place trouvée
  if racine = nil then
  begin
    racine := CreerNoeud(valeur);
    Exit;
  end;

  // Cas récursif : chercher la bonne place
  if valeur < racine^.donnee then
    Inserer(racine^.gauche, valeur)   // Aller à gauche
  else if valeur > racine^.donnee then
    Inserer(racine^.droite, valeur)   // Aller à droite
  else
    WriteLn('Valeur déjà présente');  // Doublon
end;

var
  arbre: PNoeud;
begin
  arbre := nil;  // Arbre vide

  Inserer(arbre, 50);
  Inserer(arbre, 30);
  Inserer(arbre, 70);
  Inserer(arbre, 20);
  Inserer(arbre, 40);
  Inserer(arbre, 60);
  Inserer(arbre, 80);

  WriteLn('Arbre construit');
end;
```

### Visualisation de l'Insertion

Insérons successivement : 50, 30, 70, 20

```
Étape 1 : Insérer 50
    50

Étape 2 : Insérer 30 (30 < 50 → gauche)
    50
   /
  30

Étape 3 : Insérer 70 (70 > 50 → droite)
    50
   /  \
  30  70

Étape 4 : Insérer 20 (20 < 50 → gauche, 20 < 30 → gauche)
    50
   /  \
  30  70
 /
20
```

### Fonction d'Insertion Itérative

Version sans récursion :

```pascal
procedure InsererIteratif(var racine: PNoeud; valeur: Integer);  
var  
  nouveau, courant, parent: PNoeud;
begin
  nouveau := CreerNoeud(valeur);

  // Cas 1 : arbre vide
  if racine = nil then
  begin
    racine := nouveau;
    Exit;
  end;

  // Cas 2 : chercher la position
  courant := racine;
  parent := nil;

  while courant <> nil do
  begin
    parent := courant;
    if valeur < courant^.donnee then
      courant := courant^.gauche
    else if valeur > courant^.donnee then
      courant := courant^.droite
    else
    begin
      Dispose(nouveau);  // Doublon, ne pas insérer
      WriteLn('Valeur déjà présente');
      Exit;
    end;
  end;

  // Attacher le nouveau noeud au parent
  if valeur < parent^.donnee then
    parent^.gauche := nouveau
  else
    parent^.droite := nouveau;
end;
```

## Parcours d'Arbres

Les arbres peuvent être parcourus de différentes manières. Chaque méthode visite tous les noeuds mais dans un ordre différent.

### 1. Parcours Préfixe (Préordre)

**Ordre :** Racine → Gauche → Droite

```pascal
procedure ParcoursPreordre(racine: PNoeud);  
begin  
  if racine = nil then
    Exit;

  Write(racine^.donnee, ' ');          // 1. Visiter la racine
  ParcoursPreordre(racine^.gauche);    // 2. Parcourir gauche
  ParcoursPreordre(racine^.droite);    // 3. Parcourir droite
end;
```

**Exemple :**
```
Arbre :    50
          /  \
         30  70
        / \
       20 40

Résultat : 50 30 20 40 70
```

**Utilisation :** Copier un arbre, expression préfixée

### 2. Parcours Infixe (Ordre Symétrique)

**Ordre :** Gauche → Racine → Droite

```pascal
procedure ParcoursInfixe(racine: PNoeud);  
begin  
  if racine = nil then
    Exit;

  ParcoursInfixe(racine^.gauche);      // 1. Parcourir gauche
  Write(racine^.donnee, ' ');          // 2. Visiter la racine
  ParcoursInfixe(racine^.droite);      // 3. Parcourir droite
end;
```

**Exemple :**
```
Arbre :    50
          /  \
         30  70
        / \
       20 40

Résultat : 20 30 40 50 70
```

**Important :** Pour un ABR, le parcours infixe donne les valeurs **dans l'ordre croissant** !

### 3. Parcours Postfixe (Postordre)

**Ordre :** Gauche → Droite → Racine

```pascal
procedure ParcoursPostordre(racine: PNoeud);  
begin  
  if racine = nil then
    Exit;

  ParcoursPostordre(racine^.gauche);   // 1. Parcourir gauche
  ParcoursPostordre(racine^.droite);   // 2. Parcourir droite
  Write(racine^.donnee, ' ');          // 3. Visiter la racine
end;
```

**Exemple :**
```
Arbre :    50
          /  \
         30  70
        / \
       20 40

Résultat : 20 40 30 70 50
```

**Utilisation :** Libérer un arbre, calculer une expression

### 4. Parcours en Largeur (Level Order)

**Ordre :** Niveau par niveau, de gauche à droite

```pascal
procedure ParcoursLargeur(racine: PNoeud);  
var  
  fileAttente: array[1..100] of PNoeud;
  debut, fin: Integer;
  courant: PNoeud;
begin
  if racine = nil then
    Exit;

  // Initialiser la file
  debut := 1;
  fin := 1;
  fileAttente[fin] := racine;

  while debut <= fin do
  begin
    courant := fileAttente[debut];
    Inc(debut);

    Write(courant^.donnee, ' ');

    if courant^.gauche <> nil then
    begin
      Inc(fin);
      fileAttente[fin] := courant^.gauche;
    end;

    if courant^.droite <> nil then
    begin
      Inc(fin);
      fileAttente[fin] := courant^.droite;
    end;
  end;
end;
```

**Exemple :**
```
Arbre :    50
          /  \
         30  70
        / \
       20 40

Résultat : 50 30 70 20 40
```

## Recherche dans un ABR

### Recherche Récursive

```pascal
function Rechercher(racine: PNoeud; valeur: Integer): Boolean;  
begin  
  // Cas de base : arbre vide
  if racine = nil then
  begin
    Result := False;
    Exit;
  end;

  // Cas de base : valeur trouvée
  if racine^.donnee = valeur then
  begin
    Result := True;
    Exit;
  end;

  // Cas récursif : chercher dans le bon sous-arbre
  if valeur < racine^.donnee then
    Result := Rechercher(racine^.gauche, valeur)
  else
    Result := Rechercher(racine^.droite, valeur);
end;

var
  arbre: PNoeud;
begin
  arbre := nil;
  Inserer(arbre, 50);
  Inserer(arbre, 30);
  Inserer(arbre, 70);

  if Rechercher(arbre, 30) then
    WriteLn('30 trouvé')
  else
    WriteLn('30 non trouvé');

  if Rechercher(arbre, 99) then
    WriteLn('99 trouvé')
  else
    WriteLn('99 non trouvé');

  LibererArbre(arbre);
end.
```

### Recherche Itérative

```pascal
function RechercherIteratif(racine: PNoeud; valeur: Integer): Boolean;  
var  
  courant: PNoeud;
begin
  courant := racine;

  while courant <> nil do
  begin
    if courant^.donnee = valeur then
    begin
      Result := True;
      Exit;
    end
    else if valeur < courant^.donnee then
      courant := courant^.gauche
    else
      courant := courant^.droite;
  end;

  Result := False;
end;
```

**Complexité :** O(h) où h est la hauteur de l'arbre
- Meilleur cas (arbre équilibré) : O(log n)
- Pire cas (arbre dégénéré) : O(n)

## Opérations Utiles

### Compter les Noeuds

```pascal
function CompterNoeuds(racine: PNoeud): Integer;  
begin  
  if racine = nil then
    Result := 0
  else
    Result := 1 + CompterNoeuds(racine^.gauche) + CompterNoeuds(racine^.droite);
end;
```

### Compter les Feuilles

```pascal
function CompterFeuilles(racine: PNoeud): Integer;  
begin  
  if racine = nil then
  begin
    Result := 0;
    Exit;
  end;

  // C'est une feuille si pas d'enfants
  if (racine^.gauche = nil) and (racine^.droite = nil) then
    Result := 1
  else
    Result := CompterFeuilles(racine^.gauche) + CompterFeuilles(racine^.droite);
end;
```

### Calculer la Hauteur

```pascal
function Hauteur(racine: PNoeud): Integer;  
var  
  hauteurGauche, hauteurDroite: Integer;
begin
  if racine = nil then
  begin
    Result := -1;  // Ou 0 selon la convention
    Exit;
  end;

  hauteurGauche := Hauteur(racine^.gauche);
  hauteurDroite := Hauteur(racine^.droite);

  if hauteurGauche > hauteurDroite then
    Result := hauteurGauche + 1
  else
    Result := hauteurDroite + 1;
end;
```

### Trouver le Minimum

Dans un ABR, le minimum est le noeud le plus à gauche :

```pascal
function TrouverMin(racine: PNoeud): Integer;  
begin  
  if racine = nil then
  begin
    WriteLn('Erreur : arbre vide');
    Result := 0;
    Exit;
  end;

  // Descendre tout à gauche
  while racine^.gauche <> nil do
    racine := racine^.gauche;

  Result := racine^.donnee;
end;
```

### Trouver le Maximum

Dans un ABR, le maximum est le noeud le plus à droite :

```pascal
function TrouverMax(racine: PNoeud): Integer;  
begin  
  if racine = nil then
  begin
    WriteLn('Erreur : arbre vide');
    Result := 0;
    Exit;
  end;

  // Descendre tout à droite
  while racine^.droite <> nil do
    racine := racine^.droite;

  Result := racine^.donnee;
end;
```

## Suppression dans un ABR

La suppression est l'opération la plus complexe. Il y a trois cas à gérer.

### Cas 1 : Noeud Feuille (Pas d'Enfants)

```pascal
// Simplement supprimer le noeud
procedure SupprimerFeuille(var noeud: PNoeud);  
begin  
  Dispose(noeud);
  noeud := nil;
end;
```

```
Avant :     50        Après :    50
           /  \                 /  \
          30  70               30  70
         /                    /
        20  ← Supprimer      (vide)
```

### Cas 2 : Noeud avec Un Seul Enfant

```pascal
// Remplacer le noeud par son enfant
procedure SupprimerUnEnfant(var noeud: PNoeud);  
var  
  temp: PNoeud;
begin
  temp := noeud;

  if noeud^.gauche <> nil then
    noeud := noeud^.gauche
  else
    noeud := noeud^.droite;

  Dispose(temp);
end;
```

```
Avant :     50           Après :    50
           /  \                    /  \
          30  70                  20  70
         /      ← Supprimer 30
        20
```

### Cas 3 : Noeud avec Deux Enfants

Stratégie : Remplacer par le successeur (minimum du sous-arbre droit) ou le prédécesseur (maximum du sous-arbre gauche).

```pascal
function TrouverMinNoeud(racine: PNoeud): PNoeud;  
begin  
  while racine^.gauche <> nil do
    racine := racine^.gauche;
  Result := racine;
end;

procedure Supprimer(var racine: PNoeud; valeur: Integer);  
var  
  temp, successeur: PNoeud;
begin
  if racine = nil then
    Exit;

  // Chercher le noeud à supprimer
  if valeur < racine^.donnee then
    Supprimer(racine^.gauche, valeur)
  else if valeur > racine^.donnee then
    Supprimer(racine^.droite, valeur)
  else
  begin
    // Noeud trouvé

    // Cas 1 : Pas d'enfant ou un seul enfant
    if racine^.gauche = nil then
    begin
      temp := racine;
      racine := racine^.droite;
      Dispose(temp);
    end
    else if racine^.droite = nil then
    begin
      temp := racine;
      racine := racine^.gauche;
      Dispose(temp);
    end
    else
    begin
      // Cas 2 : Deux enfants
      // Trouver le successeur (min du sous-arbre droit)
      successeur := TrouverMinNoeud(racine^.droite);

      // Copier la valeur du successeur
      racine^.donnee := successeur^.donnee;

      // Supprimer le successeur
      Supprimer(racine^.droite, successeur^.donnee);
    end;
  end;
end;
```

## Libération de l'Arbre

**Important :** Toujours libérer tous les noeuds à la fin !

```pascal
procedure LibererArbre(var racine: PNoeud);  
begin  
  if racine = nil then
    Exit;

  // Ordre postfixe : libérer les enfants d'abord
  LibererArbre(racine^.gauche);
  LibererArbre(racine^.droite);

  // Puis libérer la racine
  Dispose(racine);
  racine := nil;
end;

var
  arbre: PNoeud;
begin
  arbre := nil;
  Inserer(arbre, 50);
  Inserer(arbre, 30);
  Inserer(arbre, 70);

  // ... utilisation ...

  // IMPORTANT : toujours libérer !
  LibererArbre(arbre);
end;
```

**Pourquoi le parcours postfixe ?** On doit libérer les enfants avant le parent, sinon on perd l'accès aux enfants !

## Affichage Visuel d'un Arbre

### Affichage Horizontal

```pascal
procedure AfficherArbre(racine: PNoeud; profondeur: Integer);  
var  
  i: Integer;
begin
  if racine = nil then
    Exit;

  // Afficher le sous-arbre droit
  AfficherArbre(racine^.droite, profondeur + 1);

  // Afficher le noeud avec indentation
  for i := 1 to profondeur do
    Write('    ');
  WriteLn(racine^.donnee);

  // Afficher le sous-arbre gauche
  AfficherArbre(racine^.gauche, profondeur + 1);
end;

var
  arbre: PNoeud;
begin
  arbre := nil;
  Inserer(arbre, 50);
  Inserer(arbre, 30);
  Inserer(arbre, 70);
  Inserer(arbre, 20);
  Inserer(arbre, 40);

  AfficherArbre(arbre, 0);

  LibererArbre(arbre);
end.
```

**Résultat :**
```
        70
    50
            40
        30
            20
```

## Programme Complet : Gestionnaire d'ABR

```pascal
{$mode objfpc}{$H+}
program GestionnaireABR;

type
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: Integer;
    gauche: PNoeud;
    droite: PNoeud;
  end;

function CreerNoeud(valeur: Integer): PNoeud;  
begin  
  New(Result);
  Result^.donnee := valeur;
  Result^.gauche := nil;
  Result^.droite := nil;
end;

procedure Inserer(var racine: PNoeud; valeur: Integer);  
begin  
  if racine = nil then
  begin
    racine := CreerNoeud(valeur);
    Exit;
  end;

  if valeur < racine^.donnee then
    Inserer(racine^.gauche, valeur)
  else if valeur > racine^.donnee then
    Inserer(racine^.droite, valeur);
end;

procedure ParcoursInfixe(racine: PNoeud);  
begin  
  if racine = nil then
    Exit;

  ParcoursInfixe(racine^.gauche);      // 1. Parcourir gauche
  Write(racine^.donnee, ' ');          // 2. Visiter la racine
  ParcoursInfixe(racine^.droite);      // 3. Parcourir droite
end;

function Rechercher(racine: PNoeud; valeur: Integer): Boolean;  
begin  
  // Cas de base : arbre vide
  if racine = nil then
  begin
    Result := False;
    Exit;
  end;

  // Cas de base : valeur trouvée
  if racine^.donnee = valeur then
    Result := True
  // Cas récursif : chercher dans le bon sous-arbre
  else if valeur < racine^.donnee then
    Result := Rechercher(racine^.gauche, valeur)
  else
    Result := Rechercher(racine^.droite, valeur);
end;

procedure LibererArbre(var racine: PNoeud);  
begin  
  if racine = nil then
    Exit;

  LibererArbre(racine^.gauche);
  LibererArbre(racine^.droite);
  Dispose(racine);
  racine := nil;
end;

var
  arbre: PNoeud;
begin
  arbre := nil;

  WriteLn('=== Gestionnaire d''Arbre Binaire de Recherche ===');

  Inserer(arbre, 50);
  Inserer(arbre, 30);
  Inserer(arbre, 70);
  Inserer(arbre, 20);
  Inserer(arbre, 40);
  Inserer(arbre, 60);
  Inserer(arbre, 80);

  Write('Parcours infixe : ');
  ParcoursInfixe(arbre);
  WriteLn;

  if Rechercher(arbre, 40) then
    WriteLn('40 trouvé dans l''arbre')
  else
    WriteLn('40 non trouvé');

  LibererArbre(arbre);
  WriteLn('Mémoire libérée');

  ReadLn;
end.
```

## Avantages et Inconvénients

### Avantages des ABR

✅ **Recherche efficace** : O(log n) dans le meilleur cas  
✅ **Insertion/suppression** : O(log n) en moyenne  
✅ **Parcours ordonné** : Le parcours infixe donne les valeurs triées  
✅ **Structure hiérarchique** : Représente naturellement des relations  

### Inconvénients

❌ **Déséquilibre** : Peut dégénérer en liste chaînée (O(n))  
❌ **Complexité** : Plus difficile à implémenter que les listes  
❌ **Surcoût mémoire** : Deux pointeurs par noeud  

### Comparaison avec Liste Chaînée

| Opération | Liste Chaînée | ABR (équilibré) |
|-----------|---------------|-----------------|
| Recherche | O(n) | O(log n) ✅ |
| Insertion | O(1) tête ✅ | O(log n) |
| Suppression | O(n) | O(log n) |
| Tri | O(n log n) | Gratuit (infixe) ✅ |
| Mémoire | 1 pointeur/noeud | 2 pointeurs/noeud |

## Erreurs Courantes

### 1. Oublier d'Initialiser les Pointeurs

```pascal
// ✗ MAUVAIS
New(noeud);  
noeud^.donnee := 10;  
// Oubli : gauche et droite non initialisés !

// ✓ BON
New(noeud);  
noeud^.donnee := 10;  
noeud^.gauche := nil;  
noeud^.droite := nil;  
```

### 2. Libération dans le Mauvais Ordre

```pascal
// ✗ MAUVAIS (préfixe)
procedure MauvaiseLiberation(racine: PNoeud);  
begin  
  if racine = nil then Exit;
  Dispose(racine);  // Perd l'accès aux enfants !
  LibererArbre(racine^.gauche);  // Crash !
end;

// ✓ BON (postfixe)
procedure BonneLiberation(var racine: PNoeud);  
begin  
  if racine = nil then Exit;
  LibererArbre(racine^.gauche);   // Enfants d'abord
  LibererArbre(racine^.droite);
  Dispose(racine);                 // Parent ensuite
  racine := nil;
end;
```

### 3. Modifier var sans var

```pascal
// ✗ MAUVAIS
procedure Inserer(racine: PNoeud; valeur: Integer);  
begin  
  if racine = nil then
    racine := CreerNoeud(valeur);  // Ne modifie pas l'original !
end;

// ✓ BON
procedure Inserer(var racine: PNoeud; valeur: Integer);  
begin  
  if racine = nil then
    racine := CreerNoeud(valeur);  // Modifie l'original
end;
```

### 4. Déséquilibre Extrême

```pascal
// Insérer dans l'ordre croissant crée une liste !
Inserer(arbre, 10);  
Inserer(arbre, 20);  
Inserer(arbre, 30);  
Inserer(arbre, 40);  

// Résultat : arbre dégénéré
// 10
//   \
//    20
//      \
//       30
//         \
//          40
```

## Bonnes Pratiques

1. **Toujours initialiser** gauche et droite à `nil`
2. **Utiliser var** pour les paramètres qui modifient l'arbre
3. **Vérifier nil** avant tout déréférencement
4. **Libérer en postfixe** (enfants avant parents)
5. **Éviter l'insertion ordonnée** (cause le déséquilibre)
6. **Documenter** quel parcours est utilisé
7. **Tester les cas limites** (arbre vide, un seul noeud)

## Points Clés à Retenir

1. Un **arbre binaire** a au maximum **deux enfants** par noeud
2. Un **ABR** respecte : gauche < parent < droite
3. **Trois parcours principaux** : préfixe, infixe, postfixe
4. **Parcours infixe d'un ABR** = valeurs triées
5. **Recherche en O(log n)** si l'arbre est équilibré
6. **Libération en postfixe** obligatoire
7. **var** nécessaire pour modifier la racine
8. Les arbres peuvent **dégénérer** en listes (O(n))

## Prochaine Étape

Vous maîtrisez maintenant les bases des arbres binaires ! Pour aller plus loin, vous pourriez explorer :
- Les arbres AVL (arbres auto-équilibrés)
- Les arbres Rouge-Noir
- Les tas (heaps) pour les files de priorité
- Les arbres B pour les bases de données

Les arbres sont une structure fondamentale qui apparaît partout en informatique, de la compression de données aux bases de données en passant par les compilateurs !

⏭️ [Fuites mémoire et bonnes pratiques](/06-pointeurs-gestion-memoire-basique/08-fuites-memoire-bonnes-pratiques.md)
