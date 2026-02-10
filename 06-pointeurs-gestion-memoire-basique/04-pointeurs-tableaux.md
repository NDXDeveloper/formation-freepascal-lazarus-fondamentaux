🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 6.4 Pointeurs et Tableaux

## Introduction

Les tableaux et les pointeurs sont étroitement liés en programmation. Dans cette section, nous allons découvrir comment utiliser les pointeurs avec les tableaux, comment créer des tableaux dynamiques, et comprendre la relation particulière entre ces deux concepts fondamentaux.

## Rappel : Tableaux Statiques

Avant d'aborder les pointeurs, rappelons ce qu'est un tableau statique :

```pascal
var
  nombres: array[1..5] of Integer;
  i: Integer;
begin
  // Initialisation
  for i := 1 to 5 do
    nombres[i] := i * 10;

  // Affichage
  for i := 1 to 5 do
    WriteLn('Element ', i, ' : ', nombres[i]);
end;
```

**Limitations des tableaux statiques :**
- Taille fixe définie à la compilation
- Impossible de redimensionner
- Consomme de la mémoire même si peu utilisé

## Pointeur vers un Tableau Entier

### Déclaration et Utilisation de Base

Vous pouvez créer un pointeur qui pointe vers un tableau complet :

```pascal
program PointeurVersTableau;  
type  
  TTableau5 = array[1..5] of Integer;
  PTableau5 = ^TTableau5;
var
  nombres: TTableau5;
  pTableau: PTableau5;
  i: Integer;
begin
  // Initialisation du tableau
  for i := 1 to 5 do
    nombres[i] := i * 10;

  // Le pointeur pointe vers le tableau
  pTableau := @nombres;

  // Accès via le pointeur
  WriteLn('Premier élément : ', pTableau^[1]);
  WriteLn('Dernier élément : ', pTableau^[5]);

  // Modification via le pointeur
  pTableau^[3] := 999;
  WriteLn('Element 3 modifié : ', nombres[3]);  // Affiche 999
end.
```

### Visualisation en Mémoire

```
Tableau en mémoire :
┌───────────┬───────────┬─────────┐
│  Adresse  │   Index   │ Valeur  │
├───────────┼───────────┼─────────┤
│ $00001000 │ nombres[1]│   10    │
│ $00001004 │ nombres[2]│   20    │
│ $00001008 │ nombres[3]│   30    │
│ $0000100C │ nombres[4]│   40    │
│ $00001010 │ nombres[5]│   50    │
└───────────┴───────────┴─────────┘

Pointeur :
┌───────────┬──────────┬──────────┐
│  Adresse  │   Nom    │  Valeur  │
├───────────┼──────────┼──────────┤
│ $00002000 │ pTableau │ $00001000│ ← pointe vers nombres[1]
└───────────┴──────────┴──────────┘
```

## Pointeurs vers des Éléments de Tableau

### Pointeur vers un Élément Spécifique

Un pointeur peut pointer vers un seul élément d'un tableau :

```pascal
var
  notes: array[1..5] of Integer;
  pNote: ^Integer;
begin
  notes[1] := 12;
  notes[2] := 15;
  notes[3] := 18;

  // Pointer vers le deuxième élément
  pNote := @notes[2];

  WriteLn('Note pointée : ', pNote^);  // Affiche 15

  // Modification
  pNote^ := 16;
  WriteLn('Note modifiée : ', notes[2]);  // Affiche 16
end;
```

### Parcours avec Décalage

**Note avancée :** En Pascal, vous pouvez parcourir un tableau en utilisant l'arithmétique de pointeurs, mais cette technique est plus avancée et moins courante que dans d'autres langages.

```pascal
program ParcoursInc;  
var  
  nombres: array[1..5] of Integer;
  p: ^Integer;
  i: Integer;
begin
  // Initialisation
  for i := 1 to 5 do
    nombres[i] := i * 10;

  // Pointer vers le premier élément
  p := @nombres[1];

  // Afficher via le pointeur
  WriteLn('Premier : ', p^);

  // Avancer au suivant (décalage de 4 octets pour Integer)
  Inc(p);  // p pointe maintenant vers nombres[2]
  WriteLn('Deuxième : ', p^);

  Inc(p);  // p pointe vers nombres[3]
  WriteLn('Troisième : ', p^);
end.
```

**Attention :** L'arithmétique de pointeurs est dangereuse car elle peut sortir des limites du tableau. Utilisez-la avec précaution !

## Tableaux Dynamiques avec Pointeurs

### Allocation Manuelle d'un Tableau

Vous pouvez créer un tableau dynamiquement avec `New` :

```pascal
program TableauDynamiqueNew;  
type  
  TTableau5 = array[1..5] of Integer;
  PTableau5 = ^TTableau5;

var
  pTab: PTableau5;
  i: Integer;
begin
  // Allocation du tableau
  New(pTab);

  // Initialisation
  for i := 1 to 5 do
    pTab^[i] := i * 100;

  // Utilisation
  for i := 1 to 5 do
    WriteLn('Element ', i, ' : ', pTab^[i]);

  // Libération
  Dispose(pTab);
  pTab := nil;
end.
```

### Limitation de cette Approche

Le problème : la taille reste fixe (5 éléments). Pour un vrai tableau dynamique, utilisez le type `array of`.

## Tableaux Dynamiques Vrais (array of)

### Le Type "array of"

Pascal moderne offre des tableaux vraiment dynamiques avec taille variable :

```pascal
var
  nombres: array of Integer;  // Taille non spécifiée
  n, i: Integer;
begin
  Write('Combien d''éléments ? ');
  ReadLn(n);

  // Allocation dynamique
  SetLength(nombres, n);

  // Initialisation
  for i := 0 to n-1 do  // Attention : indices de 0 à n-1
    nombres[i] := i * 10;

  // Affichage
  for i := 0 to n-1 do
    WriteLn('Element ', i, ' : ', nombres[i]);

  // Libération (automatique ou manuelle)
  SetLength(nombres, 0);  // Libère la mémoire
end;
```

**Différences importantes :**
- **Indices :** Les tableaux dynamiques commencent à **0**, pas 1 !
- **SetLength :** Alloue/redimensionne le tableau
- **SetLength(tab, 0)** : Libère la mémoire
- **Automatique :** La mémoire est libérée automatiquement en fin de portée

### Avantages des array of

```pascal
program ArrayOfRedim;  
var  
  donnees: array of Integer;
begin
  // Démarrer petit
  SetLength(donnees, 5);
  donnees[0] := 100;

  // Agrandir dynamiquement
  SetLength(donnees, 10);  // Garde les 5 premiers éléments
  donnees[9] := 200;

  WriteLn('Premier : ', donnees[0]);   // 100
  WriteLn('Dernier : ', donnees[9]);   // 200
  WriteLn('Taille : ', Length(donnees));  // 10
end.
```

## Pointeurs et array of

### Pointeur vers un Tableau Dynamique

Vous pouvez aussi utiliser des pointeurs avec les tableaux dynamiques :

```pascal
type
  TIntArray = array of Integer;
  PIntArray = ^TIntArray;

var
  nombres: TIntArray;
  pNombres: PIntArray;
  i: Integer;
begin
  SetLength(nombres, 5);

  for i := 0 to 4 do
    nombres[i] := i + 1;

  pNombres := @nombres;

  // Accès via pointeur
  for i := 0 to 4 do
    WriteLn('Via pointeur : ', pNombres^[i]);
end;
```

## Allocation avec GetMem

Pour plus de contrôle, vous pouvez allouer la mémoire manuellement :

```pascal
var
  pTableau: ^Integer;
  taille, i: Integer;
begin
  taille := 10;

  // Allouer pour 10 entiers
  GetMem(pTableau, taille * SizeOf(Integer));

  try
    // Initialiser (comme un tableau)
    for i := 0 to taille-1 do
      pTableau[i] := i * 5;  // Notation tableau possible !

    // Afficher
    for i := 0 to taille-1 do
      WriteLn('Element ', i, ' : ', pTableau[i]);
  finally
    // Libérer
    FreeMem(pTableau);
    pTableau := nil;
  end;
end;
```

**Note :** `pTableau[i]` est équivalent à `(pTableau + i)^` en arithmétique de pointeurs.

## Tableaux Multidimensionnels Dynamiques

### Matrice Dynamique 2D

```pascal
program MatriceDynamique;  
type  
  TMatrice = array of array of Integer;

var
  matrice: TMatrice;
  lignes, colonnes, i, j: Integer;
begin
  lignes := 3;
  colonnes := 4;

  // Allocation
  SetLength(matrice, lignes, colonnes);  // 3 lignes × 4 colonnes

  // Initialisation
  for i := 0 to lignes-1 do
    for j := 0 to colonnes-1 do
      matrice[i][j] := i * 10 + j;

  // Affichage
  for i := 0 to lignes-1 do
  begin
    for j := 0 to colonnes-1 do
      Write(matrice[i][j]:4);
    WriteLn;
  end;

  // Libération automatique en fin de scope
end.
```

**Résultat :**
```
   0   1   2   3
  10  11  12  13
  20  21  22  23
```

## Passage de Tableaux aux Procédures

### Tableau Dynamique en Paramètre

```pascal
program PassageTableau;

procedure AfficherTableau(const tab: array of Integer);  
var  
  i: Integer;
begin
  for i := Low(tab) to High(tab) do
    WriteLn('Element ', i, ' : ', tab[i]);
end;

procedure InitialiserTableau(var tab: array of Integer; valeur: Integer);  
var  
  i: Integer;
begin
  for i := Low(tab) to High(tab) do
    tab[i] := valeur;
end;

var
  nombres: array of Integer;
begin
  SetLength(nombres, 5);

  InitialiserTableau(nombres, 100);
  AfficherTableau(nombres);
end.
```

**Avantages :**
- `const` : protège contre modification (et plus rapide)
- `var` : permet la modification
- `Low(tab)` et `High(tab)` : fonctionnent pour toutes tailles
- `array of` : accepte n'importe quelle taille

### Pointeur vers Tableau en Paramètre

```pascal
type
  PIntArray = ^TIntArray;
  TIntArray = array[0..99] of Integer;

procedure TraiterTableau(p: PIntArray; taille: Integer);  
var  
  i: Integer;
begin
  if p <> nil then
    for i := 0 to taille-1 do
      p^[i] := p^[i] * 2;
end;

var
  nombres: TIntArray;
  i: Integer;
begin
  // Initialisation
  for i := 0 to 9 do
    nombres[i] := i;

  // Traitement
  TraiterTableau(@nombres, 10);

  // Affichage
  for i := 0 to 9 do
    WriteLn(nombres[i]);  // Valeurs doublées
end;
```

## Cas Pratiques

### 1. Redimensionnement Dynamique

```pascal
program Redimensionnement;  
var  
  data: array of Integer;
  ancienneTaille, nouvelleTaille: Integer;
begin
  // Démarrer avec 5 éléments
  SetLength(data, 5);
  data[0] := 10;
  data[4] := 50;

  ancienneTaille := Length(data);
  WriteLn('Ancienne taille : ', ancienneTaille);

  // Agrandir à 10 (garde les anciennes valeurs)
  nouvelleTaille := 10;
  SetLength(data, nouvelleTaille);

  WriteLn('Nouvelle taille : ', Length(data));
  WriteLn('Premier élément conservé : ', data[0]);  // 10
  WriteLn('Dernier ancien conservé : ', data[4]);   // 50
end.
```

### 2. Copie de Tableau

```pascal
var
  source, copie: array of Integer;
  i: Integer;
begin
  SetLength(source, 5);
  for i := 0 to 4 do
    source[i] := i * 10;

  // Méthode 1 : Copie manuelle
  SetLength(copie, Length(source));
  for i := 0 to High(source) do
    copie[i] := source[i];

  // Méthode 2 : Utiliser Copy (FreePascal)
  copie := Copy(source);  // Copie complète

  WriteLn('Copie réussie : ', copie[2]);  // 20
end;
```

### 3. Tableau de Pointeurs

Créer un tableau où chaque élément est un pointeur :

```pascal
program TableauDePointeurs;  
uses SysUtils;  

type
  PPerson = ^TPerson;
  TPerson = record
    nom: String;
    age: Integer;
  end;

var
  personnes: array[1..3] of PPerson;
  i: Integer;
begin
  // Créer 3 personnes
  for i := 1 to 3 do
  begin
    New(personnes[i]);
    personnes[i]^.nom := 'Personne' + IntToStr(i);
    personnes[i]^.age := 20 + i;
  end;

  // Afficher
  for i := 1 to 3 do
    WriteLn(personnes[i]^.nom, ' : ', personnes[i]^.age, ' ans');

  // Libérer
  for i := 1 to 3 do
  begin
    Dispose(personnes[i]);
    personnes[i] := nil;
  end;
end.
```

## Comparaison des Approches

| Approche | Déclaration | Avantages | Inconvénients |
|----------|-------------|-----------|---------------|
| **Tableau statique** | `array[1..10] of Integer` | Simple, rapide | Taille fixe |
| **Pointeur + New** | `^array[1..10] of Integer` | Contrôle mémoire | Taille fixe, complexe |
| **array of** | `array of Integer` | Flexible, simple | Indices à partir de 0 |
| **GetMem** | `^Integer + GetMem` | Contrôle total | Complexe, dangereux |

**Recommandation pour débutants :** Utilisez `array of` pour les tableaux dynamiques !

## Erreurs Courantes

### 1. Confusion d'Indices

```pascal
var
  tab: array of Integer;
begin
  SetLength(tab, 5);

  // ✗ ERREUR : indices de 0 à 4, pas 1 à 5 !
  tab[5] := 100;  // Accès hors limites !

  // ✓ CORRECT
  tab[4] := 100;  // Dernier élément
end;
```

### 2. Oubli de SetLength

```pascal
var
  tab: array of Integer;
begin
  // ✗ ERREUR : tableau non alloué
  tab[0] := 100;  // Crash !

  // ✓ CORRECT
  SetLength(tab, 10);
  tab[0] := 100;
end;
```

### 3. Dépassement de Limites

```pascal
var
  pTab: ^Integer;
  i: Integer;
begin
  GetMem(pTab, 5 * SizeOf(Integer));

  // ✗ DANGER : dépasse la zone allouée
  for i := 0 to 10 do  // Seulement 0 à 4 alloués !
    pTab[i] := i;

  FreeMem(pTab);
end;
```

### 4. Fuite Mémoire avec Pointeurs

```pascal
type
  PIntArray = ^TIntArray;
  TIntArray = array[1..100] of Integer;

var
  p: PIntArray;
begin
  New(p);
  // ... utilisation ...

  // ✗ Oubli de Dispose !
end;  // Fuite de 400 octets
```

## Bonnes Pratiques

### 1. Utiliser Low() et High()

```pascal
var
  tab: array of Integer;
  i: Integer;
begin
  SetLength(tab, 10);

  // ✓ Fonctionne quelle que soit la taille
  for i := Low(tab) to High(tab) do
    tab[i] := i;
end;
```

### 2. Vérifier Length avant Accès

```pascal
var
  tab: array of Integer;
begin
  if Length(tab) > 0 then
    WriteLn('Premier : ', tab[0])
  else
    WriteLn('Tableau vide');
end;
```

### 3. Utiliser const pour Protection

```pascal
procedure Lire(const tab: array of Integer);  
begin  
  // tab ne peut pas être modifié ici
  WriteLn(tab[0]);
end;
```

### 4. Try-Finally avec GetMem

```pascal
var
  p: ^Integer;
begin
  GetMem(p, 100 * SizeOf(Integer));
  try
    // ... utilisation sécurisée ...
  finally
    FreeMem(p);  // Toujours exécuté
    p := nil;
  end;
end;
```

## Points Clés à Retenir

1. **Les tableaux dynamiques (`array of`)** sont la solution moderne et recommandée
2. **Les indices** des `array of` commencent à **0**
3. **SetLength** alloue/redimensionne, **SetLength(tab, 0)** libère
4. **Low(tab)** et **High(tab)** pour parcourir sans erreur
5. **GetMem** nécessite un calcul manuel de la taille
6. **Toujours vérifier** les limites des tableaux
7. Les **pointeurs vers tableaux** sont possibles mais plus complexes

## Prochaine Étape

Maintenant que vous maîtrisez les pointeurs et les tableaux, vous êtes prêt à découvrir les **pointeurs et enregistrements**, puis les **structures de données dynamiques** comme les listes chaînées qui combinent tous ces concepts pour créer des structures vraiment puissantes !

⏭️ [Pointeurs et enregistrements](/06-pointeurs-gestion-memoire-basique/05-pointeurs-enregistrements.md)
