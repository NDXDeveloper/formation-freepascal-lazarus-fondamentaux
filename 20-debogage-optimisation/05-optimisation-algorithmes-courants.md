🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 20.5 Optimisation des Algorithmes Courants

## Introduction

L'optimisation d'algorithmes consiste à améliorer la manière dont votre code résout un problème, pour qu'il s'exécute plus rapidement et consomme moins de ressources. Un bon algorithme peut transformer un programme qui prend des heures en un programme qui s'exécute en quelques secondes.

**Analogie simple :** Imaginez que vous devez trouver un mot dans un dictionnaire :
- **Méthode lente** : Lire chaque page depuis le début (algorithme linéaire)
- **Méthode rapide** : Ouvrir au milieu, puis ajuster selon l'ordre alphabétique (algorithme de recherche binaire)

Les deux méthodes donnent le même résultat, mais la seconde est infiniment plus rapide !

**Dans cette section, vous apprendrez à :**
- Reconnaître les algorithmes inefficaces
- Choisir les bonnes structures de données
- Optimiser les opérations courantes (recherche, tri, boucles)
- Appliquer des techniques d'optimisation pratiques
- Mesurer l'impact de vos optimisations

---

## 1. Comprendre la Complexité Algorithmique

### 1.1 Notation Big O (Simplifiée)

La notation Big O décrit comment le temps d'exécution augmente quand la taille des données augmente.

**Les complexités courantes (du meilleur au pire) :**

| Notation | Nom | Exemple | Performance |
|----------|-----|---------|-------------|
| **O(1)** | Constante | Accès à un élément de tableau | ⚡⚡⚡ Excellent |
| **O(log n)** | Logarithmique | Recherche binaire | ⚡⚡ Très bon |
| **O(n)** | Linéaire | Parcourir un tableau | ⚡ Bon |
| **O(n log n)** | Linéaire-logarithmique | Tri rapide (QuickSort) | 👍 Acceptable |
| **O(n²)** | Quadratique | Tri à bulles | ⚠️ Lent |
| **O(n³)** | Cubique | Triple boucle imbriquée | ❌ Très lent |
| **O(2ⁿ)** | Exponentielle | Problèmes combinatoires naïfs | 💀 Impraticable |

**Exemple concret avec n = 1000 éléments :**

| Complexité | Opérations | Temps approximatif |
|------------|------------|-------------------|
| O(1) | 1 | < 0,001 ms |
| O(log n) | ~10 | < 0,01 ms |
| O(n) | 1 000 | 0,1 ms |
| O(n log n) | ~10 000 | 1 ms |
| O(n²) | 1 000 000 | 100 ms |
| O(n³) | 1 000 000 000 | 100 secondes |

**Leçon :** La différence entre O(n) et O(n²) peut être énorme !

### 1.2 Visualiser la Croissance

**Pour 10 éléments :**
- O(n) : 10 opérations
- O(n²) : 100 opérations (10x plus)

**Pour 1000 éléments :**
- O(n) : 1 000 opérations
- O(n²) : 1 000 000 opérations (1000x plus !)

**Pour 10 000 éléments :**
- O(n) : 10 000 opérations
- O(n²) : 100 000 000 opérations (10 000x plus !!!)

**Conclusion :** Plus les données sont volumineuses, plus un bon algorithme fait la différence.

---

## 2. Optimisation des Recherches

### 2.1 Recherche Linéaire vs Recherche Binaire

**Problème :** Trouver un élément dans un tableau.

**Recherche Linéaire - O(n)**

```pascal
function RechercheLineaire(const tab: array of Integer; valeur: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;  // Non trouvé
  for i := Low(tab) to High(tab) do
  begin
    if tab[i] = valeur then
    begin
      Result := i;
      Exit;
    end;
  end;
end;
```

**Performance :**
- Meilleur cas : 1 opération (élément au début)
- Pire cas : n opérations (élément à la fin ou absent)
- Moyenne : n/2 opérations

**Recherche Binaire - O(log n)**

**Prérequis :** Le tableau DOIT être trié.

```pascal
function RechercheBinaire(const tab: array of Integer; valeur: Integer): Integer;
var
  gauche, droite, milieu: Integer;
begin
  Result := -1;
  gauche := Low(tab);
  droite := High(tab);

  while gauche <= droite do
  begin
    milieu := (gauche + droite) div 2;

    if tab[milieu] = valeur then
    begin
      Result := milieu;
      Exit;
    end
    else if tab[milieu] < valeur then
      gauche := milieu + 1
    else
      droite := milieu - 1;
  end;
end;
```

**Performance :**
- Meilleur cas : 1 opération (élément au milieu)
- Pire cas : log₂(n) opérations
- Exemple : Dans 1 million d'éléments → ~20 opérations maximum !

**Comparaison pratique :**

```pascal
var
  tableau: array[1..1000000] of Integer;
  i: Integer;
  debut: QWord;
begin
  // Initialiser le tableau trié
  for i := 1 to 1000000 do
    tableau[i] := i;

  // Test recherche linéaire
  debut := GetTickCount64;
  RechercheLineaire(tableau, 999999);  // Chercher le dernier
  WriteLn('Linéaire : ', GetTickCount64 - debut, ' ms');

  // Test recherche binaire
  debut := GetTickCount64;
  RechercheBinaire(tableau, 999999);
  WriteLn('Binaire : ', GetTickCount64 - debut, ' ms');
end.
```

**Résultat typique :**
```
Linéaire : 12 ms
Binaire : 0 ms
```

**Quand utiliser quoi ?**
- **Linéaire** : Tableau non trié, peu d'éléments (<100)
- **Binaire** : Tableau trié, beaucoup d'éléments

### 2.2 Utiliser des Structures de Données Efficaces

**Problème :** Vérifier si un élément existe dans une collection.

**❌ Inefficace : Recherche dans TStringList non triée - O(n)**

```pascal
var
  liste: TStringList;
begin
  liste := TStringList.Create;
  try
    // Ajouter 10 000 éléments
    for i := 1 to 10000 do
      liste.Add('Element' + IntToStr(i));

    // Recherche
    debut := GetTickCount64;
    for i := 1 to 1000 do
      if liste.IndexOf('Element5000') >= 0 then
        Inc(compteur);
    WriteLn('Temps : ', GetTickCount64 - debut, ' ms');
  finally
    liste.Free;
  end;
end;
```

**✅ Optimisé : TStringList triée - O(log n)**

```pascal
var
  liste: TStringList;
begin
  liste := TStringList.Create;
  try
    liste.Sorted := True;  // Active la recherche binaire automatique !

    for i := 1 to 10000 do
      liste.Add('Element' + IntToStr(i));

    debut := GetTickCount64;
    for i := 1 to 1000 do
      if liste.IndexOf('Element5000') >= 0 then
        Inc(compteur);
    WriteLn('Temps : ', GetTickCount64 - debut, ' ms');
  finally
    liste.Free;
  end;
end;
```

**⚡ Encore mieux : TDictionary - O(1)**

```pascal
uses Generics.Collections;

var
  dict: TDictionary<String, Integer>;
begin
  dict := TDictionary<String, Integer>.Create;
  try
    for i := 1 to 10000 do
      dict.Add('Element' + IntToStr(i), i);

    debut := GetTickCount64;
    for i := 1 to 1000 do
      if dict.ContainsKey('Element5000') then
        Inc(compteur);
    WriteLn('Temps : ', GetTickCount64 - debut, ' ms');
  finally
    dict.Free;
  end;
end;
```

**Comparaison de performances (1000 recherches dans 10 000 éléments) :**

| Méthode | Complexité | Temps | Gain |
|---------|-----------|-------|------|
| TStringList non triée | O(n) | 450 ms | - |
| TStringList triée | O(log n) | 2 ms | 225x |
| TDictionary | O(1) | 0.1 ms | 4500x |

---

## 3. Optimisation des Tris

### 3.1 Tri à Bulles (Bubble Sort) - O(n²)

**⚠️ À ÉVITER pour de grandes quantités de données**

```pascal
procedure TriBulles(var tab: array of Integer);
var
  i, j, temp: Integer;
begin
  for i := Low(tab) to High(tab) - 1 do
    for j := Low(tab) to High(tab) - 1 - i do
      if tab[j] > tab[j + 1] then
      begin
        temp := tab[j];
        tab[j] := tab[j + 1];
        tab[j + 1] := temp;
      end;
end;
```

**Performance :**
- 1 000 éléments : ~500 000 comparaisons
- 10 000 éléments : ~50 000 000 comparaisons

**Temps typique :**
```
1 000 éléments : 15 ms
10 000 éléments : 1 500 ms (1,5 seconde)
100 000 éléments : 150 000 ms (2,5 minutes !)
```

### 3.2 Tri Rapide (QuickSort) - O(n log n)

**✅ RECOMMANDÉ**

```pascal
procedure TriRapide(var tab: array of Integer; gauche, droite: Integer);
var
  i, j, pivot, temp: Integer;
begin
  if gauche >= droite then Exit;

  pivot := tab[(gauche + droite) div 2];
  i := gauche;
  j := droite;

  repeat
    while tab[i] < pivot do Inc(i);
    while tab[j] > pivot do Dec(j);

    if i <= j then
    begin
      temp := tab[i];
      tab[i] := tab[j];
      tab[j] := temp;
      Inc(i);
      Dec(j);
    end;
  until i > j;

  if gauche < j then TriRapide(tab, gauche, j);
  if i < droite then TriRapide(tab, i, droite);
end;

// Utilisation
TriRapide(monTableau, Low(monTableau), High(monTableau));
```

**Performance :**
- 1 000 éléments : ~10 000 comparaisons
- 10 000 éléments : ~130 000 comparaisons
- 100 000 éléments : ~1 660 000 comparaisons

**Temps typique :**
```
1 000 éléments : 0.3 ms
10 000 éléments : 4 ms
100 000 éléments : 50 ms
```

### 3.3 Comparaison Directe

```pascal
program ComparaisonTris;

uses SysUtils;

var
  tab1, tab2: array[1..10000] of Integer;
  i: Integer;
  debut: QWord;

procedure InitialiserTableau(var tab: array of Integer);
var i: Integer;
begin
  Randomize;
  for i := Low(tab) to High(tab) do
    tab[i] := Random(100000);
end;

begin
  // Initialiser les deux tableaux identiques
  InitialiserTableau(tab1);
  for i := Low(tab1) to High(tab1) do
    tab2[i] := tab1[i];

  // Test Tri à Bulles
  WriteLn('Test Tri à Bulles...');
  debut := GetTickCount64;
  TriBulles(tab1);
  WriteLn('Tri à Bulles : ', GetTickCount64 - debut, ' ms');

  // Test Tri Rapide
  WriteLn('Test Tri Rapide...');
  debut := GetTickCount64;
  TriRapide(tab2, Low(tab2), High(tab2));
  WriteLn('Tri Rapide : ', GetTickCount64 - debut, ' ms');

  ReadLn;
end.
```

**Résultat typique :**
```
Test Tri à Bulles...
Tri à Bulles : 1450 ms
Test Tri Rapide...
Tri Rapide : 4 ms

→ QuickSort est ~360x plus rapide !
```

### 3.4 Utiliser les Fonctions Intégrées

**FreePascal offre des fonctions de tri optimisées :**

**Pour les tableaux dynamiques d'entiers :**

```pascal
uses Classes;

var
  tableau: array of Integer;

function CompareInt(Item1, Item2: Pointer): Integer;
begin
  Result := PInteger(Item1)^ - PInteger(Item2)^;
end;

begin
  SetLength(tableau, 10000);
  // ... initialisation ...

  // Tri avec la procédure QuickSort de l'unité Classes
  QuickSort(PPointer(@tableau[0]), 0, Length(tableau) - 1,
            TListSortCompare(@CompareInt));
end;
```

> **Note :** `QuickSort` est définie dans l'unité `Classes`, pas dans `Math`.

**Pour TStringList :**

```pascal
var
  liste: TStringList;
begin
  liste := TStringList.Create;
  try
    // ... ajouter des éléments ...

    liste.Sort;  // Tri optimisé intégré
  finally
    liste.Free;
  end;
end;
```

**Avantages :**
- Code testé et optimisé
- Moins de bugs
- Maintenance facilitée

---

## 4. Optimisation des Boucles

### 4.1 Éviter les Calculs Répétitifs

**❌ Inefficace :**

```pascal
for i := 1 to Length(chaine) do  // Length() appelé à chaque itération !
  Traiter(chaine[i]);
```

**✅ Optimisé :**

```pascal
longueur := Length(chaine);  // Calcul une seule fois
for i := 1 to longueur do
  Traiter(chaine[i]);
```

### 4.2 Sortir des Boucles Tôt

**❌ Inefficace :**

```pascal
trouve := False;
for i := 1 to 1000000 do
begin
  if tableau[i] = valeur then
    trouve := True;
  // Continue même après avoir trouvé !
end;
```

**✅ Optimisé :**

```pascal
trouve := False;
for i := 1 to 1000000 do
begin
  if tableau[i] = valeur then
  begin
    trouve := True;
    Break;  // Sort immédiatement
  end;
end;
```

### 4.3 Réduire les Boucles Imbriquées

**❌ Inefficace - O(n²) :**

```pascal
// Trouver les doublons dans un tableau
for i := 1 to Length(tab) do
  for j := i + 1 to Length(tab) do
    if tab[i] = tab[j] then
      WriteLn('Doublon trouvé : ', tab[i]);
```

**✅ Optimisé - O(n) avec dictionnaire :**

```pascal
uses Generics.Collections;

var
  vus: TDictionary<Integer, Boolean>;
  element: Integer;
begin
  vus := TDictionary<Integer, Boolean>.Create;
  try
    for element in tab do
    begin
      if vus.ContainsKey(element) then
        WriteLn('Doublon trouvé : ', element)
      else
        vus.Add(element, True);
    end;
  finally
    vus.Free;
  end;
end;
```

**Performance pour 10 000 éléments :**
- Méthode O(n²) : ~500 ms
- Méthode O(n) : ~5 ms
- **Gain : 100x plus rapide**

### 4.4 Boucles à l'Envers pour Suppressions

**❌ Problématique :**

```pascal
// Supprimer les éléments pairs
for i := 0 to liste.Count - 1 do
  if liste[i] mod 2 = 0 then
    liste.Delete(i);  // Les indices changent !
```

**✅ Correct :**

```pascal
// Parcourir à l'envers
for i := liste.Count - 1 downto 0 do
  if liste[i] mod 2 = 0 then
    liste.Delete(i);  // Pas de problème d'indices
```

---

## 5. Structures de Données Efficaces

### 5.1 Choisir la Bonne Structure

**Tableau vs Liste Chaînée vs Dictionnaire**

| Opération | Tableau | Liste Chaînée | Dictionnaire |
|-----------|---------|---------------|--------------|
| Accès par index | O(1) ⚡ | O(n) ⚠️ | - |
| Recherche | O(n) | O(n) | O(1) ⚡ |
| Insertion début | O(n) | O(1) ⚡ | - |
| Insertion fin | O(1)* | O(1) ⚡ | O(1) ⚡ |
| Suppression | O(n) | O(1) ⚡ | O(1) ⚡ |

*Pour les tableaux dynamiques avec capacité

### 5.2 TStringList vs TDictionary

**Problème :** Stocker et rechercher des paires clé-valeur.

**TStringList (ancienne méthode) :**

```pascal
var
  liste: TStringList;
  index: Integer;
begin
  liste := TStringList.Create;
  try
    // Ajouter 10 000 paires
    for i := 1 to 10000 do
      liste.Values['Cle' + IntToStr(i)] := 'Valeur' + IntToStr(i);

    // Rechercher
    debut := GetTickCount64;
    for i := 1 to 1000 do
    begin
      valeur := liste.Values['Cle5000'];  // Recherche linéaire interne !
    end;
    WriteLn('TStringList : ', GetTickCount64 - debut, ' ms');
  finally
    liste.Free;
  end;
end;
```

**TDictionary (méthode moderne) :**

```pascal
uses Generics.Collections;

var
  dict: TDictionary<String, String>;
begin
  dict := TDictionary<String, String>.Create;
  try
    // Ajouter 10 000 paires
    for i := 1 to 10000 do
      dict.Add('Cle' + IntToStr(i), 'Valeur' + IntToStr(i));

    // Rechercher
    debut := GetTickCount64;
    for i := 1 to 1000 do
    begin
      if dict.TryGetValue('Cle5000', valeur) then
        ; // Trouvé instantanément
    end;
    WriteLn('TDictionary : ', GetTickCount64 - debut, ' ms');
  finally
    dict.Free;
  end;
end;
```

**Résultats :**
```
TStringList : 1200 ms
TDictionary : 1 ms

→ 1200x plus rapide !
```

### 5.3 TList Générique vs Tableau Dynamique

**TList<T> offre des avantages :**

```pascal
uses Generics.Collections;

var
  liste: TList<Integer>;
begin
  liste := TList<Integer>.Create;
  try
    // Capacité initiale pour éviter les réallocations
    liste.Capacity := 10000;

    // Ajout efficace
    for i := 1 to 10000 do
      liste.Add(i);

    // Tri intégré
    liste.Sort;

    // Recherche binaire intégrée
    if liste.BinarySearch(5000, index) then
      WriteLn('Trouvé à l''index : ', index);
  finally
    liste.Free;
  end;
end;
```

**Avantages de TList<T> :**
- Méthodes intégrées (Add, Remove, Sort, BinarySearch)
- Gestion automatique de la mémoire
- Type-safe (pas de conversion de types)

---

## 6. Optimisation des Chaînes de Caractères

### 6.1 Concaténation de Chaînes

**❌ Très inefficace - O(n²) :**

```pascal
var
  resultat: String;
  i: Integer;
begin
  resultat := '';
  for i := 1 to 10000 do
    resultat := resultat + 'Ligne ' + IntToStr(i) + #13#10;
    // Chaque += réalloue toute la chaîne !
end;
```

**Temps typique : 8 000 ms pour 10 000 lignes**

**✅ Optimisé avec TStringBuilder - O(n) :**

```pascal
uses SysUtils, Classes;

var
  sb: TStringBuilder;
  i: Integer;
begin
  sb := TStringBuilder.Create;
  try
    sb.Capacity := 200000;  // Pré-allocation

    for i := 1 to 10000 do
    begin
      sb.Append('Ligne ');
      sb.Append(i);
      sb.AppendLine;
    end;

    resultat := sb.ToString;
  finally
    sb.Free;
  end;
end;
```

**Temps typique : 15 ms pour 10 000 lignes**

**Gain : 500x plus rapide !**

**Alternative avec TStringList :**

```pascal
var
  liste: TStringList;
  i: Integer;
begin
  liste := TStringList.Create;
  try
    liste.Capacity := 10000;

    for i := 1 to 10000 do
      liste.Add('Ligne ' + IntToStr(i));

    resultat := liste.Text;  // Concaténation optimisée interne
  finally
    liste.Free;
  end;
end;
```

**Temps typique : 25 ms pour 10 000 lignes**

### 6.2 Comparaisons de Chaînes

**❌ Inefficace (sensible à la casse) :**

```pascal
if LowerCase(chaine1) = LowerCase(chaine2) then  // Double conversion !
  ...
```

**✅ Optimisé :**

```pascal
if CompareText(chaine1, chaine2) = 0 then  // Optimisé en interne
  ...
```

**Ou avec SameText :**

```pascal
if SameText(chaine1, chaine2) then  // Plus lisible
  ...
```

### 6.3 Extraire des Parties de Chaînes

**❌ Inefficace avec Copy répété :**

```pascal
for i := 1 to Length(chaine) do
  traiter := Copy(chaine, 1, i);  // Réalloue à chaque fois
```

**✅ Optimisé avec accès direct :**

```pascal
SetLength(traiter, Length(chaine));
for i := 1 to Length(chaine) do
begin
  traiter[i] := chaine[i];
  // Traitement
end;
```

---

## 7. Cas Pratiques d'Optimisation

### 7.1 Filtrage de Liste

**Problème :** Extraire les éléments qui correspondent à un critère.

**❌ Inefficace - Suppression dans la boucle :**

```pascal
procedure FiltrerPairs(liste: TList<Integer>);
var
  i: Integer;
begin
  for i := liste.Count - 1 downto 0 do
    if liste[i] mod 2 <> 0 then
      liste.Delete(i);  // Opération coûteuse
end;
```

**✅ Optimisé - Créer nouvelle liste :**

```pascal
function FiltrerPairs(liste: TList<Integer>): TList<Integer>;
var
  element: Integer;
begin
  Result := TList<Integer>.Create;
  Result.Capacity := liste.Count div 2;  // Estimation

  for element in liste do
    if element mod 2 = 0 then
      Result.Add(element);
end;
```

**Performance pour 100 000 éléments :**
- Suppression : 450 ms
- Nouvelle liste : 15 ms
- **Gain : 30x plus rapide**

### 7.2 Calcul de Statistiques

**Problème :** Calculer moyenne, min, max d'une liste.

**❌ Inefficace - Trois parcours :**

```pascal
function Moyenne(liste: TList<Integer>): Double;
var i: Integer; total: Int64;
begin
  total := 0;
  for i := 0 to liste.Count - 1 do
    total := total + liste[i];
  Result := total / liste.Count;
end;

function Minimum(liste: TList<Integer>): Integer;
var i: Integer;
begin
  Result := liste[0];
  for i := 1 to liste.Count - 1 do
    if liste[i] < Result then
      Result := liste[i];
end;

function Maximum(liste: TList<Integer>): Integer;
var i: Integer;
begin
  Result := liste[0];
  for i := 1 to liste.Count - 1 do
    if liste[i] > Result then
      Result := liste[i];
end;
```

**✅ Optimisé - Un seul parcours :**

```pascal
procedure CalculerStatistiques(liste: TList<Integer>;
                               out moyenne: Double;
                               out min, max: Integer);
var
  i: Integer;
  total: Int64;
begin
  if liste.Count = 0 then Exit;

  min := liste[0];
  max := liste[0];
  total := liste[0];

  for i := 1 to liste.Count - 1 do
  begin
    total := total + liste[i];
    if liste[i] < min then min := liste[i];
    if liste[i] > max then max := liste[i];
  end;

  moyenne := total / liste.Count;
end;
```

**Performance pour 1 000 000 d'éléments :**
- Trois parcours : 45 ms
- Un parcours : 15 ms
- **Gain : 3x plus rapide**

### 7.3 Vérification d'Unicité

**Problème :** Vérifier si tous les éléments d'une liste sont uniques.

**❌ Inefficace - O(n²) :**

```pascal
function TousUniques(liste: TList<Integer>): Boolean;
var
  i, j: Integer;
begin
  Result := True;
  for i := 0 to liste.Count - 2 do
    for j := i + 1 to liste.Count - 1 do
      if liste[i] = liste[j] then
        Exit(False);
end;
```

**✅ Optimisé - O(n) avec TDictionary :**

```pascal
function TousUniques(liste: TList<Integer>): Boolean;
var
  vus: TDictionary<Integer, Boolean>;
  element: Integer;
begin
  Result := True;
  vus := TDictionary<Integer, Boolean>.Create;
  try
    for element in liste do
    begin
      if vus.ContainsKey(element) then
        Exit(False);
      vus.Add(element, True);
    end;
  finally
    vus.Free;
  end;
end;
```

**Performance pour 10 000 éléments :**
- Méthode O(n²) : 1200 ms
- Méthode O(n) : 3 ms
- **Gain : 400x plus rapide**

### 7.4 Intersection de Deux Listes

**Problème :** Trouver les éléments communs à deux listes.

**❌ Inefficace - O(n × m) :**

```pascal
function Intersection(liste1, liste2: TList<Integer>): TList<Integer>;
var
  i, j: Integer;
begin
  Result := TList<Integer>.Create;
  for i := 0 to liste1.Count - 1 do
    for j := 0 to liste2.Count - 1 do
      if liste1[i] = liste2[j] then
      begin
        Result.Add(liste1[i]);
        Break;
      end;
end;
```

**✅ Optimisé - O(n + m) avec TDictionary :**

```pascal
function Intersection(liste1, liste2: TList<Integer>): TList<Integer>;
var
  ensemble: TDictionary<Integer, Boolean>;
  element: Integer;
begin
  Result := TList<Integer>.Create;
  ensemble := TDictionary<Integer, Boolean>.Create;
  try
    // Ajouter tous les éléments de liste1 dans le dictionnaire
    for element in liste1 do
      if not ensemble.ContainsKey(element) then
        ensemble.Add(element, True);

    // Vérifier les éléments de liste2
    for element in liste2 do
      if ensemble.ContainsKey(element) then
        Result.Add(element);
  finally
    ensemble.Free;
  end;
end;
```

**Performance (listes de 10 000 éléments chacune) :**
- Méthode O(n×m) : 2500 ms
- Méthode O(n+m) : 8 ms
- **Gain : 312x plus rapide**

---

## 8. Optimisation Mémoire

### 8.1 Réutiliser les Objets

**❌ Inefficace - Créer/Détruire répétitivement :**

```pascal
for i := 1 to 100000 do
begin
  liste := TStringList.Create;
  liste.Add('Element');
  Traiter(liste);
  liste.Free;
end;
```

**✅ Optimisé - Réutiliser :**

```pascal
liste := TStringList.Create;
try
  for i := 1 to 100000 do
  begin
    liste.Clear;
    liste.Add('Element');
    Traiter(liste);
  end;
finally
  liste.Free;
end;
```

**Gain :** Évite 100 000 allocations/désallocations.

### 8.2 Pré-allocation de Capacité

**❌ Sans pré-allocation :**

```pascal
liste := TList<Integer>.Create;
for i := 1 to 100000 do
  liste.Add(i);  // Réallocations multiples
```

**✅ Avec pré-allocation :**

```pascal
liste := TList<Integer>.Create;
liste.Capacity := 100000;  // Alloue une seule fois
for i := 1 to 100000 do
  liste.Add(i);
```

**Performance :**
- Sans pré-allocation : 45 ms (+ ~20 réallocations)
- Avec pré-allocation : 12 ms (1 seule allocation)
- **Gain : 3-4x plus rapide**

### 8.3 Libération de Grandes Structures

**Pour TStringList et TList :**

```pascal
// Si vous savez que vous avez terminé avec de grandes données
liste.Clear;
liste.Capacity := 0;  // Libère la mémoire allouée
```

---

## 9. Techniques Avancées

### 9.1 Mémorisation (Memoization)

**Technique :** Stocker les résultats de calculs coûteux pour les réutiliser.

**Exemple : Fibonacci naïf - O(2ⁿ) :**

```pascal
function Fibonacci(n: Integer): Int64;
begin
  if n <= 1 then
    Result := n
  else
    Result := Fibonacci(n - 1) + Fibonacci(n - 2);
end;

// Fibonacci(40) prend plusieurs secondes !
```

**Fibonacci avec mémorisation - O(n) :**

```pascal
var
  FibCache: TDictionary<Integer, Int64>;

function FibonacciMemo(n: Integer): Int64;
begin
  if n <= 1 then
    Result := n
  else if FibCache.ContainsKey(n) then
    Result := FibCache[n]  // Résultat déjà calculé
  else
  begin
    Result := FibonacciMemo(n - 1) + FibonacciMemo(n - 2);
    FibCache.Add(n, Result);  // Stocker pour plus tard
  end;
end;

// Initialisation
FibCache := TDictionary<Integer, Int64>.Create;
try
  WriteLn(FibonacciMemo(40));  // Instantané !
finally
  FibCache.Free;
end;
```

**Performance :**
- Sans mémorisation : 2500 ms
- Avec mémorisation : 0.5 ms
- **Gain : 5000x plus rapide !**

### 9.2 Traitement par Lots (Batching)

**Pour les opérations de base de données :**

**❌ Requête par requête :**

```pascal
for i := 1 to 1000 do
begin
  Query.SQL.Text := 'INSERT INTO Table VALUES (' + IntToStr(i) + ')';
  Query.ExecSQL;  // 1000 allers-retours réseau !
end;
```

**✅ Par lots :**

```pascal
Query.SQL.Text := 'INSERT INTO Table VALUES (:valeur)';
Query.Prepare;

SQLTransaction.Active := True;
try
  for i := 1 to 1000 do
  begin
    Query.ParamByName('valeur').AsInteger := i;
    Query.ExecSQL;
  end;
  SQLTransaction.Commit;
except
  SQLTransaction.Rollback;
  raise;
end;
```

**Encore mieux - Insertion multiple :**

```pascal
sql := 'INSERT INTO Table VALUES ';
for i := 1 to 1000 do
begin
  if i > 1 then sql := sql + ',';
  sql := sql + '(' + IntToStr(i) + ')';
end;
Query.SQL.Text := sql;
Query.ExecSQL;  // Une seule requête !
```

**Performance :**
- Requête par requête : 5000 ms
- Transaction : 500 ms (10x)
- Insertion multiple : 50 ms (100x)

### 9.3 Lazy Loading (Chargement Paresseux)

**Principe :** Ne charger les données que quand elles sont nécessaires.

```pascal
type
  TDataManager = class
  private
    FDonnees: TList<Integer>;
    FChargeees: Boolean;
    procedure ChargerDonnees;
  public
    function GetDonnees: TList<Integer>;
    property Donnees: TList<Integer> read GetDonnees;
  end;

procedure TDataManager.ChargerDonnees;
begin
  if FChargeees then Exit;  // Déjà chargées

  WriteLn('Chargement des données...');
  // ... chargement coûteux ...

  FChargeees := True;
end;

function TDataManager.GetDonnees: TList<Integer>;
begin
  if not FChargeees then
    ChargerDonnees;
  Result := FDonnees;
end;
```

**Avantage :** Les données ne sont chargées que si on les utilise vraiment.

---

## 10. Multi-plateforme : Considérations

### 10.1 Différences de Performance

**Windows vs Linux :**
- Allocations mémoire généralement plus rapides sous Linux
- Opérations fichier peuvent varier
- Threads ont des performances différentes

**Toujours profiler sur les deux plateformes !**

### 10.2 Code Portable Optimisé

**Utiliser les fonctions portables :**

```pascal
// ✅ Portable
uses SysUtils;
chemin := IncludeTrailingPathDelimiter(repertoire) + fichier;

// ❌ Spécifique Windows
chemin := repertoire + '\' + fichier;
```

**Optimisations spécifiques par plateforme :**

```pascal
{$IFDEF WINDOWS}
  // Utiliser QueryPerformanceCounter pour haute précision
{$ENDIF}

{$IFDEF LINUX}
  // Utiliser clock_gettime
{$ENDIF}
```

---

## 11. Checklist d'Optimisation

### 11.1 Avant d'Optimiser

- [ ] Profiler pour identifier les vrais goulots
- [ ] Mesurer les performances actuelles
- [ ] Définir des objectifs de performance réalistes
- [ ] Vérifier que le code est correct (pas de bugs)

### 11.2 Pendant l'Optimisation

- [ ] Ne traiter qu'un goulot à la fois
- [ ] Garder une copie du code original
- [ ] Commenter les optimisations
- [ ] Tester après chaque changement
- [ ] Re-mesurer les performances

### 11.3 Après l'Optimisation

- [ ] Vérifier que le code produit les mêmes résultats
- [ ] Mesurer le gain réel
- [ ] Documenter les optimisations
- [ ] Évaluer le compromis lisibilité/performance
- [ ] Tester sur différentes plateformes

---

## 12. Pièges à Éviter

### 12.1 Optimisation Prématurée

**❌ N'optimisez pas sans preuve :**
```pascal
// "Je pense que cette fonction est lente, je vais l'optimiser"
```

**✅ Profilez d'abord :**
```pascal
// Mesure : cette fonction prend 0,01% du temps
// → Ne pas optimiser, se concentrer ailleurs
```

### 12.2 Sur-optimisation

**Exemple :**
```pascal
// Code original (lisible)
if (x > 0) and (x < 100) then
  Traiter(x);

// "Optimisé" (illisible, gain négligeable)
if (x - 1) and $FFFFFF80 = 0 then
  Traiter(x);
```

**Règle :** Ne sacrifiez pas la lisibilité pour un gain inférieur à 10%.

### 12.3 Ignorer les Constantes

**La notation Big O ignore les constantes, mais elles comptent !**

```pascal
// O(n) mais avec constante élevée
for i := 1 to n do
  CalculComplexe(i);  // Prend 10 ms par appel

// O(n log n) mais avec constante faible
TriRapide(tableau);   // Tri optimisé en assembleur
```

Pour de petites valeurs de n (<1000), l'algorithme O(n log n) peut être plus rapide que l'O(n) avec constante élevée.

---

## 13. Récapitulatif

### 13.1 Règles d'Or

1. **Profilez avant d'optimiser** - Identifiez les vrais goulots
2. **Choisissez le bon algorithme** - O(n log n) vs O(n²) fait toute la différence
3. **Utilisez les bonnes structures de données** - TDictionary vs tableau
4. **Évitez les opérations répétitives** - Calculez une fois, réutilisez
5. **Mesurez les résultats** - Vérifiez que l'optimisation fonctionne

### 13.2 Gains Typiques

**Optimisations ayant le plus d'impact :**

| Optimisation | Gain typique | Complexité |
|--------------|--------------|------------|
| O(n²) → O(n log n) | 100-1000x | ⭐⭐⭐ |
| Recherche linéaire → binaire | 100-1000x | ⭐⭐⭐ |
| TStringList → TDictionary | 100-500x | ⭐⭐ |
| Concaténation → TStringBuilder | 100-500x | ⭐⭐ |
| Calculs répétés → cache | 10-100x | ⭐⭐ |
| Pré-allocation | 2-5x | ⭐ |
| Optimisations de boucles | 1.5-3x | ⭐ |

### 13.3 Tableau de Décision Rapide

**Quelle structure utiliser ?**

| Besoin | Structure Recommandée | Raison |
|--------|---------------------|---------|
| Accès par index | Tableau / TList<T> | O(1) |
| Recherche par clé | TDictionary | O(1) |
| Ordre d'insertion | TList<T> | Simple |
| Ordre trié | TList<T> + Sort | O(n log n) |
| Ensemble mathématique | TDictionary ou Set | O(1) |
| File FIFO | TQueue<T> | O(1) |
| Pile LIFO | TStack<T> | O(1) |

---

## 14. Pour Aller Plus Loin

### 14.1 Ressources

**Livres et Documentation :**
- "Introduction to Algorithms" (CLRS)
- FreePascal Documentation : https://www.freepascal.org/docs.html
- Lazarus Wiki : https://wiki.freepascal.org/

**Pratiquer :**
- Implémenter différents algorithmes de tri
- Résoudre des problèmes sur des plateformes comme Project Euler
- Comparer les performances de vos implémentations

### 14.2 Sujets Avancés

**Pour approfondir :**
- Structures de données avancées (arbres AVL, B-trees)
- Algorithmes de graphes (Dijkstra, A*)
- Programmation dynamique
- Algorithmes parallèles et multi-thread
- Optimisations CPU (cache-friendly code)

---

## Conclusion

L'optimisation d'algorithmes est un équilibre entre :
- **Performance** : Vitesse d'exécution
- **Lisibilité** : Code compréhensible
- **Maintenabilité** : Facilité de modification

**La formule gagnante :**
1. Écrivez du code correct et lisible AVANT tout
2. Profilez pour identifier les goulots
3. Optimisez uniquement ce qui compte (80/20)
4. Mesurez vos gains
5. Documentez vos changements

**Citation finale :**
> "Premature optimization is the root of all evil, but wise optimization is the path to excellence."

**Prochaine Étape :** La section 20.6 (Gestion efficace de la mémoire) explorera comment optimiser l'utilisation de la mémoire de vos programmes.

---

**Multi-plateforme Windows/Ubuntu** ✓
**Formation FreePascal/Lazarus - Niveau Débutant à Intermédiaire**

⏭️ [Gestion efficace de la mémoire](/20-debogage-optimisation/06-gestion-efficace-memoire.md)
