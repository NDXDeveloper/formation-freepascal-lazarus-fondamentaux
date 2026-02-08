🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.7 Surcharge de procédures/fonctions

## Introduction

Parfois, on veut créer plusieurs versions d'une même procédure ou fonction qui font la même chose, mais avec des paramètres différents. La **surcharge** (ou **overloading** en anglais) permet de donner le même nom à plusieurs procédures/fonctions ayant des paramètres différents.

## Le problème sans surcharge

Imaginons que vous voulez afficher différents types de données :

```pascal
procedure AfficherEntier(n: Integer);
begin
  WriteLn('Valeur : ', n);
end;

procedure AfficherReel(r: Real);
begin
  WriteLn('Valeur : ', r:0:2);
end;

procedure AfficherTexte(s: String);
begin
  WriteLn('Valeur : ', s);
end;

begin
  AfficherEntier(42);
  AfficherReel(3.14);
  AfficherTexte('Bonjour');
end.
```

**Problème :** Il faut se souvenir de trois noms différents alors qu'on fait conceptuellement la même chose : afficher une valeur.

## La solution : surcharge de procédures/fonctions

Avec la surcharge, on peut utiliser le **même nom** pour toutes ces procédures :

```pascal
procedure Afficher(n: Integer); overload;
begin
  WriteLn('Valeur : ', n);
end;

procedure Afficher(r: Real); overload;
begin
  WriteLn('Valeur : ', r:0:2);
end;

procedure Afficher(s: String); overload;
begin
  WriteLn('Valeur : ', s);
end;

begin
  Afficher(42);        // Appelle la version Integer
  Afficher(3.14);      // Appelle la version Real
  Afficher('Bonjour'); // Appelle la version String
end.
```

**Avantage :** Un seul nom à retenir, le compilateur choisit automatiquement la bonne version selon le type de paramètre !

## Qu'est-ce que la surcharge ?

La **surcharge** permet de créer plusieurs procédures ou fonctions avec :
- **Le même nom**
- **Des paramètres différents** (type, nombre ou ordre)

Le compilateur choisit automatiquement la version appropriée selon les arguments fournis lors de l'appel.

## Syntaxe

Pour surcharger une procédure/fonction, ajoutez la directive `overload` après la déclaration :

```pascal
procedure NomProcedure(param1: Type1); overload;
procedure NomProcedure(param1: Type2); overload;
procedure NomProcedure(param1: Type1; param2: Type2); overload;

function NomFonction(param1: Type1): TypeRetour; overload;
function NomFonction(param1: Type2): TypeRetour; overload;
```

**Important :** Toutes les versions surchargées doivent avoir la directive `overload`.

## Règles de surcharge

Pour que deux procédures/fonctions puissent être surchargées, elles doivent différer par :

### 1. Le nombre de paramètres

```pascal
procedure Calculer(a: Integer); overload;
begin
  WriteLn('Résultat : ', a);
end;

procedure Calculer(a, b: Integer); overload;
begin
  WriteLn('Résultat : ', a + b);
end;

procedure Calculer(a, b, c: Integer); overload;
begin
  WriteLn('Résultat : ', a + b + c);
end;

begin
  Calculer(5);        // Version avec 1 paramètre
  Calculer(5, 3);     // Version avec 2 paramètres
  Calculer(5, 3, 2);  // Version avec 3 paramètres
end.
```

### 2. Le type des paramètres

```pascal
function Doubler(n: Integer): Integer; overload;
begin
  Result := n * 2;
end;

function Doubler(r: Real): Real; overload;
begin
  Result := r * 2;
end;

function Doubler(s: String): String; overload;
begin
  Result := s + s;
end;

begin
  WriteLn(Doubler(5));           // 10
  WriteLn(Doubler(2.5):0:1);     // 5.0
  WriteLn(Doubler('Ha'));        // HaHa
end.
```

### 3. L'ordre des paramètres (si types différents)

```pascal
procedure Afficher(texte: String; nombre: Integer); overload;
begin
  WriteLn(texte, ' : ', nombre);
end;

procedure Afficher(nombre: Integer; texte: String); overload;
begin
  WriteLn(nombre, ' : ', texte);
end;

begin
  Afficher('Age', 25);      // Version String, Integer
  Afficher(25, 'Age');      // Version Integer, String
end.
```

### ❌ Ce qui ne fonctionne PAS pour différencier

- **Uniquement le type de retour** (pour les fonctions)
- **Uniquement les noms de paramètres**
- **Uniquement const/var/valeur**

```pascal
// ❌ ERREUR : même signature (seul le type de retour diffère)
function Obtenir(n: Integer): Integer; overload;
function Obtenir(n: Integer): String; overload;

// ❌ ERREUR : même signature (seuls les noms diffèrent)
procedure Test(a: Integer); overload;
procedure Test(b: Integer); overload;

// ❌ ERREUR : même signature (seul const/var diffère)
procedure Modifier(n: Integer); overload;
procedure Modifier(var n: Integer); overload;
```

## Exemples pratiques

### 1. Fonction Minimum avec différents types

```pascal
function Min(a, b: Integer): Integer; overload;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function Min(a, b: Real): Real; overload;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function Min(a, b, c: Integer): Integer; overload;
begin
  Result := Min(Min(a, b), c);  // Réutilise la version à 2 paramètres
end;

begin
  WriteLn('Min(5, 3) = ', Min(5, 3));              // 3
  WriteLn('Min(2.7, 4.1) = ', Min(2.7, 4.1):0:1); // 2.7
  WriteLn('Min(5, 3, 8) = ', Min(5, 3, 8));       // 3
end.
```

### 2. Procédure de dessin avec options

```pascal
procedure DessinerLigne(longueur: Integer); overload;
var
  i: Integer;
begin
  for i := 1 to longueur do
    Write('-');
  WriteLn;
end;

procedure DessinerLigne(longueur: Integer; caractere: Char); overload;
var
  i: Integer;
begin
  for i := 1 to longueur do
    Write(caractere);
  WriteLn;
end;

procedure DessinerLigne(longueur: Integer; caractere: Char; repetitions: Integer); overload;
var
  i: Integer;
begin
  for i := 1 to repetitions do
    DessinerLigne(longueur, caractere);
end;

begin
  DessinerLigne(20);              // --------------------
  DessinerLigne(15, '=');         // ===============
  DessinerLigne(10, '*', 3);      // Trois lignes de **********
end.
```

### 3. Conversion de types

```pascal
uses SysUtils;

function VersTexte(n: Integer): String; overload;
begin
  Result := IntToStr(n);
end;

function VersTexte(r: Real): String; overload;
begin
  Result := FloatToStr(r);
end;

function VersTexte(b: Boolean): String; overload;
begin
  if b then
    Result := 'Vrai'
  else
    Result := 'Faux';
end;

begin
  WriteLn(VersTexte(42));        // '42'
  WriteLn(VersTexte(3.14));      // '3.14'
  WriteLn(VersTexte(True));      // 'Vrai'
end.
```

### 4. Création de rectangles

```pascal
procedure CreerRectangle(largeur, hauteur: Integer); overload;
begin
  WriteLn('Rectangle de ', largeur, 'x', hauteur);
end;

procedure CreerRectangle(cote: Integer); overload;
begin
  WriteLn('Carré de ', cote, 'x', cote);
  CreerRectangle(cote, cote);  // Appelle la version avec 2 paramètres
end;

procedure CreerRectangle(largeur, hauteur: Integer; couleur: String); overload;
begin
  WriteLn('Rectangle de ', largeur, 'x', hauteur, ' en ', couleur);
end;

begin
  CreerRectangle(10, 5);           // Rectangle
  CreerRectangle(7);               // Carré
  CreerRectangle(8, 6, 'rouge');   // Rectangle coloré
end.
```

### 5. Recherche dans différentes structures

```pascal
uses StrUtils;

function Rechercher(const texte: String; const motif: String): Integer; overload;
begin
  Result := Pos(motif, texte);
end;

function Rechercher(const texte: String; const motif: String;
                   depart: Integer): Integer; overload;
begin
  Result := PosEx(motif, texte, depart);
end;

function Rechercher(const texte: String; const motif: Char): Integer; overload;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(texte) do
    if texte[i] = motif then
    begin
      Result := i;
      Exit;
    end;
end;

var
  position: Integer;
begin
  position := Rechercher('Bonjour le monde', 'monde');  // 12
  position := Rechercher('Bonjour le monde', 'o', 5);   // 5
  position := Rechercher('Bonjour', 'o');               // 2
end.
```

## Surcharge vs Paramètres par défaut

Ces deux fonctionnalités peuvent sembler similaires mais ont des usages différents :

### Paramètres par défaut

```pascal
procedure Afficher(texte: String; largeur: Integer = 20;
                  centrer: Boolean = False);
begin
  // ...
end;

begin
  Afficher('Test');
  Afficher('Test', 30);
  Afficher('Test', 30, True);
end;
```

**Avantages :**
- Un seul bloc de code à maintenir
- Syntaxe plus simple

**Inconvénients :**
- Paramètres doivent être du même type "conceptuel"
- Ne peut gérer des comportements très différents

### Surcharge

```pascal
procedure Afficher(n: Integer); overload;
procedure Afficher(r: Real); overload;
procedure Afficher(s: String); overload;
```

**Avantages :**
- Peut gérer des types complètement différents
- Chaque version peut avoir une logique différente
- Plus flexible

**Inconvénients :**
- Plus de code à écrire et maintenir
- Peut être plus complexe

### Quand utiliser quoi ?

**Utilisez les paramètres par défaut** quand :
- Les paramètres supplémentaires sont des **options** pour la même opération
- La logique reste fondamentalement la même

**Utilisez la surcharge** quand :
- Vous traitez des **types différents**
- La logique change significativement selon les paramètres
- Vous voulez une interface simple pour différents cas d'usage

## Combinaison : surcharge + paramètres par défaut

On peut combiner les deux techniques :

```pascal
procedure Logger(message: String; niveau: String = 'INFO'); overload;
begin
  WriteLn('[', niveau, '] ', message);
end;

procedure Logger(code: Integer; niveau: String = 'ERROR'); overload;
begin
  WriteLn('[', niveau, '] Code erreur : ', code);
end;

begin
  Logger('Démarrage');                    // [INFO] Démarrage
  Logger('Attention', 'WARN');            // [WARN] Attention
  Logger(404);                            // [ERROR] Code erreur : 404
  Logger(200, 'SUCCESS');                 // [SUCCESS] Code erreur : 200
end.
```

## Exemple complet : calculatrice

```pascal
{$mode objfpc}{$H+}
program Calculatrice;

// overload : permet plusieurs fonctions avec le même nom mais des paramètres différents
function Additionner(a, b: Integer): Integer; overload;
begin
  Result := a + b;
end;

// Addition avec 3 nombres
function Additionner(a, b, c: Integer): Integer; overload;
begin
  Result := a + b + c;
end;

// Addition de réels
function Additionner(a, b: Real): Real; overload;
begin
  Result := a + b;
end;

// Multiplication
function Multiplier(a, b: Integer): Integer; overload;
begin
  Result := a * b;
end;

function Multiplier(a, b: Real): Real; overload;
begin
  Result := a * b;
end;

// Affichage du résultat
procedure AfficherResultat(operation: String; resultat: Integer); overload;
begin
  WriteLn(operation, ' = ', resultat);
end;

procedure AfficherResultat(operation: String; resultat: Real); overload;
begin
  WriteLn(operation, ' = ', resultat:0:2);
end;

var
  resInt: Integer;
  resReal: Real;
begin
  resInt := Additionner(5, 3);
  AfficherResultat('5 + 3', resInt);

  resInt := Additionner(5, 3, 2);
  AfficherResultat('5 + 3 + 2', resInt);

  resReal := Additionner(2.5, 3.7);
  AfficherResultat('2.5 + 3.7', resReal);

  resInt := Multiplier(4, 7);
  AfficherResultat('4 × 7', resInt);

  resReal := Multiplier(2.5, 4.0);
  AfficherResultat('2.5 × 4.0', resReal);
end.
```

**Résultat :**
```
5 + 3 = 8
5 + 3 + 2 = 10
2.5 + 3.7 = 6.20
4 × 7 = 28
2.5 × 4.0 = 10.00
```

## Erreurs courantes à éviter

### 1. Oublier la directive overload

```pascal
procedure Test(n: Integer);  // ❌ Manque overload
begin
  WriteLn(n);
end;

procedure Test(s: String);   // ❌ Manque overload
begin
  WriteLn(s);
end;

// ✅ CORRECT
procedure Test(n: Integer); overload;
procedure Test(s: String); overload;
```

### 2. Signatures ambiguës

```pascal
procedure Traiter(n: Integer); overload;
procedure Traiter(n: LongInt); overload;  // ❌ Trop similaire

var
  x: Integer;
begin
  Traiter(x);  // Quelle version appeler ? Ambiguïté !
end;
```

### 3. Surcharge excessive

```pascal
// ⚠️ Mauvaise pratique : trop de versions
procedure Afficher(n: Integer); overload;
procedure Afficher(n: LongInt); overload;
procedure Afficher(n: Int64); overload;
procedure Afficher(n: Byte); overload;
procedure Afficher(n: Word); overload;
procedure Afficher(n: Cardinal); overload;

// ✅ Mieux : une seule version générique
procedure Afficher(n: Int64);
```

### 4. Confusion entre surcharge et redéfinition

```pascal
// Surcharge : même nom, paramètres différents
procedure Test(n: Integer); overload;
procedure Test(s: String); overload;  // ✅ Surcharge

// Redéfinition : déclaration multiple identique
procedure Autre(n: Integer);
procedure Autre(n: Integer);  // ❌ ERREUR : redéfinition
```

## Bonnes pratiques

1. **Cohérence sémantique** : toutes les versions surchargées doivent faire conceptuellement la même chose
   ```pascal
   // ✅ BON : toutes calculent un carré
   function Carre(n: Integer): Integer; overload;
   function Carre(r: Real): Real; overload;

   // ❌ MAUVAIS : fonctions différentes
   function Calcul(n: Integer): Integer; overload;  // Calcule le carré
   function Calcul(r: Real): Real; overload;        // Calcule la racine carrée
   ```

2. **Limitation du nombre** : ne créez pas trop de versions (3-5 maximum recommandé)

3. **Documentation** : commentez les différences entre les versions si ce n'est pas évident

4. **Réutilisation** : faites appel aux autres versions quand c'est logique
   ```pascal
   function Max(a, b: Integer): Integer; overload;
   function Max(a, b, c: Integer): Integer; overload;
   begin
     Result := Max(Max(a, b), c);  // Réutilise la version à 2 paramètres
   end;
   ```

5. **Nommage explicite** : si les versions font des choses vraiment différentes, utilisez plutôt des noms différents

## Points clés à retenir

1. La **surcharge** permet d'utiliser le **même nom** pour plusieurs procédures/fonctions
2. Directive obligatoire : **overload**
3. Les versions doivent différer par le **nombre**, le **type** ou l'**ordre** des paramètres
4. Le compilateur choisit **automatiquement** la version appropriée
5. **Avantage** : interface simplifiée, un seul nom à retenir
6. **Attention** : ne pas abuser, maintenir la cohérence sémantique
7. Peut être combinée avec les **paramètres par défaut**
8. Différent de la **redéfinition** de méthodes (vu plus tard en POO)

---

**Prochaine étape :** Dans la section 4.8, nous explorerons les **variables locales vs globales** et leur portée (scope) dans les procédures et fonctions.

⏭️ [Variables locales vs globales](/04-procedures-fonctions/08-variables-locales-vs-globales.md)
