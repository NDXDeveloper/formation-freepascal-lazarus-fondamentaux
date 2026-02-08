🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.10 Fonctions prédéfinies utiles

## Introduction

Pascal et FreePascal fournissent une grande bibliothèque de **fonctions prédéfinies** (aussi appelées **fonctions standard** ou **built-in**) qui permettent de réaliser des opérations courantes sans avoir à les programmer soi-même. Ces fonctions sont immédiatement disponibles et testées, ce qui vous fait gagner du temps.

## Fonctions mathématiques

### Fonctions de base

#### Abs - Valeur absolue

Retourne la valeur absolue (sans le signe).

```pascal
function Abs(x: Integer): Integer;
function Abs(x: Real): Real;
```

**Exemples :**
```pascal
WriteLn(Abs(-5));      // 5
WriteLn(Abs(5));       // 5
WriteLn(Abs(-3.14));   // 3.14
WriteLn(Abs(0));       // 0
```

#### Sqr - Carré

Retourne le carré d'un nombre (x²).

```pascal
function Sqr(x: Integer): Integer;
function Sqr(x: Real): Real;
```

**Exemples :**
```pascal
WriteLn(Sqr(5));       // 25
WriteLn(Sqr(3.0));     // 9.0
WriteLn(Sqr(-4));      // 16
```

#### Sqrt - Racine carrée

Retourne la racine carrée d'un nombre.

```pascal
function Sqrt(x: Real): Real;
```

**Exemples :**
```pascal
WriteLn(Sqrt(25):0:0);     // 5
WriteLn(Sqrt(16):0:0);     // 4
WriteLn(Sqrt(2):0:5);      // 1.41421
WriteLn(Sqrt(100):0:0);    // 10
```

**⚠️ Attention :** Ne pas utiliser Sqrt avec un nombre négatif (erreur d'exécution).

### Fonctions trigonométriques

Ces fonctions travaillent en **radians** (pas en degrés).

#### Sin, Cos, Tan - Trigonométrie

```pascal
function Sin(x: Real): Real;  // Sinus
function Cos(x: Real): Real;  // Cosinus
function Tan(x: Real): Real;  // Tangente (dans l'unité Math)
```

**Exemples :**
```pascal
uses Math;

var
  angle: Real;
begin
  angle := 0;  // 0 radian = 0 degré
  WriteLn('Sin(0) = ', Sin(angle):0:2);  // 0.00
  WriteLn('Cos(0) = ', Cos(angle):0:2);  // 1.00

  angle := Pi / 2;  // 90 degrés
  WriteLn('Sin(π/2) = ', Sin(angle):0:2);  // 1.00
end.
```

#### Conversion degrés ↔ radians

```pascal
uses Math;

function DegreesVersRadians(degres: Real): Real;
begin
  Result := degres * Pi / 180;
end;

function RadiansVersDegrees(radians: Real): Real;
begin
  Result := radians * 180 / Pi;
end;

// Ou utiliser les fonctions de l'unité Math
WriteLn(DegToRad(90):0:5);   // π/2 ≈ 1.5708
WriteLn(RadToDeg(Pi):0:1);   // 180.0
```

### Fonctions exponentielles et logarithmes

#### Exp - Exponentielle

Retourne e^x (e à la puissance x).

```pascal
function Exp(x: Real): Real;
```

**Exemples :**
```pascal
WriteLn(Exp(0):0:0);      // 1 (e^0 = 1)
WriteLn(Exp(1):0:5);      // 2.71828 (e^1 = e)
WriteLn(Exp(2):0:5);      // 7.38906 (e^2)
```

#### Ln - Logarithme naturel

Retourne le logarithme naturel (base e).

```pascal
function Ln(x: Real): Real;
```

**Exemples :**
```pascal
WriteLn(Ln(1):0:0);       // 0 (ln(1) = 0)
WriteLn(Ln(Exp(1)):0:5);  // 1.00000 (ln(e) = 1)
WriteLn(Ln(10):0:5);      // 2.30259
```

#### Power - Puissance (unité Math)

Calcule x^y (x à la puissance y).

```pascal
uses Math;

function Power(base, exposant: Real): Real;
```

**Exemples :**
```pascal
uses Math;

WriteLn(Power(2, 3):0:0);     // 8 (2³)
WriteLn(Power(10, 2):0:0);    // 100 (10²)
WriteLn(Power(5, 0.5):0:5);   // 2.23607 (√5)
WriteLn(Power(2, -1):0:2);    // 0.50 (1/2)
```

### Fonctions d'arrondi

#### Round - Arrondir

Arrondit au nombre entier le plus proche.

```pascal
function Round(x: Real): Int64;
```

**Exemples :**
```pascal
WriteLn(Round(3.4));    // 3
WriteLn(Round(3.5));    // 4
WriteLn(Round(3.6));    // 4
WriteLn(Round(-2.5));   // -2 (arrondi vers le pair le plus proche)
WriteLn(Round(-2.6));   // -3
```

#### Trunc - Tronquer

Supprime la partie décimale (arrondi vers zéro).

```pascal
function Trunc(x: Real): Int64;
```

**Exemples :**
```pascal
WriteLn(Trunc(3.9));    // 3
WriteLn(Trunc(3.1));    // 3
WriteLn(Trunc(-3.9));   // -3
WriteLn(Trunc(-3.1));   // -3
```

#### Frac - Partie fractionnaire

Retourne la partie décimale d'un nombre.

```pascal
function Frac(x: Real): Real;
```

**Exemples :**
```pascal
WriteLn(Frac(3.14):0:2);    // 0.14
WriteLn(Frac(5.75):0:2);    // 0.75
WriteLn(Frac(-2.8):0:1);    // -0.8
WriteLn(Frac(7):0:0);       // 0
```

#### Int - Partie entière

Retourne la partie entière (comme Trunc mais retourne un Real).

```pascal
function Int(x: Real): Real;
```

**Exemples :**
```pascal
WriteLn(Int(3.14):0:0);     // 3
WriteLn(Int(-5.7):0:0);     // -5
```

### Fonctions Min/Max (unité Math)

```pascal
uses Math;

function Min(a, b: Integer): Integer;
function Max(a, b: Integer): Integer;
function Min(a, b: Real): Real;
function Max(a, b: Real): Real;
```

**Exemples :**
```pascal
uses Math;

WriteLn(Min(5, 3));         // 3
WriteLn(Max(5, 3));         // 5
WriteLn(Min(2.5, 7.8):0:1); // 2.5
WriteLn(Max(2.5, 7.8):0:1); // 7.8
```

## Fonctions de manipulation de chaînes

### Length - Longueur

Retourne le nombre de caractères dans une chaîne.

```pascal
function Length(const s: String): Integer;
```

**Exemples :**
```pascal
WriteLn(Length('Bonjour'));      // 7
WriteLn(Length(''));             // 0
WriteLn(Length('A'));            // 1
WriteLn(Length('Pascal 123'));   // 10
```

### Copy - Extraire une sous-chaîne

Extrait une partie d'une chaîne.

```pascal
function Copy(const s: String; index, count: Integer): String;
```

**Paramètres :**
- `s` : la chaîne source
- `index` : position de départ (commence à 1)
- `count` : nombre de caractères à extraire

**Exemples :**
```pascal
var
  texte: String;
begin
  texte := 'Bonjour Pascal';

  WriteLn(Copy(texte, 1, 7));    // 'Bonjour'
  WriteLn(Copy(texte, 9, 6));    // 'Pascal'
  WriteLn(Copy(texte, 1, 3));    // 'Bon'
  WriteLn(Copy(texte, 5, 4));    // 'jour'
end.
```

### Pos - Position d'une sous-chaîne

Recherche une sous-chaîne et retourne sa position (ou 0 si non trouvée).

```pascal
function Pos(const subStr, str: String): Integer;
```

**Exemples :**
```pascal
var
  texte: String;
begin
  texte := 'Bonjour le monde';

  WriteLn(Pos('le', texte));        // 9
  WriteLn(Pos('monde', texte));     // 12
  WriteLn(Pos('o', texte));         // 2 (première occurrence)
  WriteLn(Pos('xyz', texte));       // 0 (pas trouvé)
end.
```

### Delete - Supprimer une partie

Supprime une partie d'une chaîne (modification in-place).

```pascal
procedure Delete(var s: String; index, count: Integer);
```

**Exemples :**
```pascal
var
  texte: String;
begin
  texte := 'Bonjour le monde';
  Delete(texte, 9, 3);  // Supprime ' le'
  WriteLn(texte);       // 'Bonjour monde'

  texte := 'ABCDEF';
  Delete(texte, 2, 2);  // Supprime 'BC'
  WriteLn(texte);       // 'ADEF'
end.
```

### Insert - Insérer une chaîne

Insère une chaîne dans une autre (modification in-place).

```pascal
procedure Insert(const source: String; var dest: String; index: Integer);
```

**Exemples :**
```pascal
var
  texte: String;
begin
  texte := 'Bonjour monde';
  Insert('le ', texte, 9);
  WriteLn(texte);       // 'Bonjour le monde'

  texte := 'AC';
  Insert('B', texte, 2);
  WriteLn(texte);       // 'ABC'
end.
```

### UpCase - Majuscule (un caractère)

Convertit un caractère en majuscule.

```pascal
function UpCase(c: Char): Char;
```

**Exemples :**
```pascal
WriteLn(UpCase('a'));   // 'A'
WriteLn(UpCase('z'));   // 'Z'
WriteLn(UpCase('A'));   // 'A'
WriteLn(UpCase('5'));   // '5' (pas de changement)
```

### UpperCase et LowerCase - Majuscules/minuscules (chaîne)

Convertit toute une chaîne.

```pascal
uses SysUtils;

function UpperCase(const s: String): String;
function LowerCase(const s: String): String;
```

**Exemples :**
```pascal
uses SysUtils;

WriteLn(UpperCase('bonjour'));      // 'BONJOUR'
WriteLn(LowerCase('PASCAL'));       // 'pascal'
WriteLn(UpperCase('Test 123'));     // 'TEST 123'
```

### Trim - Supprimer espaces début/fin

```pascal
uses SysUtils;

function Trim(const s: String): String;      // Début et fin
function TrimLeft(const s: String): String;  // Début seulement
function TrimRight(const s: String): String; // Fin seulement
```

**Exemples :**
```pascal
uses SysUtils;

WriteLn('[' + Trim('  Bonjour  ') + ']');      // '[Bonjour]'
WriteLn('[' + TrimLeft('  Test') + ']');       // '[Test]'
WriteLn('[' + TrimRight('Test  ') + ']');      // '[Test]'
```

## Fonctions de conversion

### IntToStr et StrToInt - Entier ↔ Chaîne

```pascal
uses SysUtils;

function IntToStr(value: Integer): String;
function StrToInt(const s: String): Integer;
```

**Exemples :**
```pascal
uses SysUtils;

var
  nombre: Integer;
  texte: String;
begin
  texte := IntToStr(42);
  WriteLn('Texte : ', texte);     // 'Texte : 42'

  nombre := StrToInt('123');
  WriteLn('Nombre : ', nombre);   // 'Nombre : 123'

  // nombre := StrToInt('abc');   // ❌ ERREUR d'exécution
end.
```

### FloatToStr et StrToFloat - Réel ↔ Chaîne

```pascal
uses SysUtils;

function FloatToStr(value: Real): String;
function StrToFloat(const s: String): Real;
```

**Exemples :**
```pascal
uses SysUtils;

var
  nombre: Real;
  texte: String;
begin
  texte := FloatToStr(3.14);
  WriteLn(texte);                 // '3.14'

  nombre := StrToFloat('2.718');
  WriteLn(nombre:0:3);            // 2.718
end.
```

### Format - Formatage avancé

Fonction puissante pour formater des chaînes.

```pascal
uses SysUtils;

function Format(const fmt: String; const args: array of const): String;
```

**Exemples :**
```pascal
uses SysUtils;

var
  nom: String;
  age: Integer;
  taille: Real;
begin
  nom := 'Jean';
  age := 25;
  taille := 1.75;

  WriteLn(Format('Nom : %s', [nom]));                       // Nom : Jean
  WriteLn(Format('Age : %d ans', [age]));                   // Age : 25 ans
  WriteLn(Format('Taille : %.2f m', [taille]));             // Taille : 1.75 m
  WriteLn(Format('%s a %d ans et mesure %.2f m',
          [nom, age, taille]));
  // Jean a 25 ans et mesure 1.75 m
end.
```

**Spécificateurs courants :**
- `%s` : chaîne de caractères
- `%d` : entier
- `%f` : réel (floating point)
- `%.2f` : réel avec 2 décimales
- `%10d` : entier sur 10 caractères

### Chr et Ord - Caractère ↔ Code ASCII

```pascal
function Chr(code: Byte): Char;
function Ord(c: Char): Byte;
```

**Exemples :**
```pascal
WriteLn(Chr(65));          // 'A'
WriteLn(Chr(97));          // 'a'
WriteLn(Chr(48));          // '0'

WriteLn(Ord('A'));         // 65
WriteLn(Ord('a'));         // 97
WriteLn(Ord('0'));         // 48
```

## Fonctions de génération aléatoire

### Random - Nombre aléatoire

```pascal
function Random: Real;            // Entre 0.0 et 1.0
function Random(max: Integer): Integer;  // Entre 0 et max-1
```

**Exemples :**
```pascal
var
  i: Integer;
begin
  Randomize;  // ⚠️ IMPORTANT : initialiser le générateur

  // Nombre entre 0.0 et 1.0
  WriteLn(Random:0:5);

  // Nombre entre 0 et 9
  for i := 1 to 5 do
    Write(Random(10), ' ');
  WriteLn;

  // Nombre entre 1 et 6 (dé)
  for i := 1 to 5 do
    Write(Random(6) + 1, ' ');
  WriteLn;

  // Nombre entre 10 et 20
  WriteLn(Random(11) + 10);
end.
```

### Randomize - Initialiser le générateur

**⚠️ TRÈS IMPORTANT :** Toujours appeler `Randomize` au début du programme, sinon vous obtiendrez toujours les mêmes nombres "aléatoires" !

```pascal
begin
  Randomize;  // À faire une seule fois au début
  WriteLn(Random(100));
end.
```

## Fonctions de dates et heures

### Now - Date et heure actuelles

```pascal
uses SysUtils;

function Now: TDateTime;
```

**Exemples :**
```pascal
uses SysUtils;

var
  maintenant: TDateTime;
begin
  maintenant := Now;
  WriteLn(DateTimeToStr(maintenant));
  // Exemple : 12/10/2025 14:30:25
end.
```

### Date et Time - Date/heure séparées

```pascal
uses SysUtils;

function Date: TDateTime;  // Date sans l'heure
function Time: TDateTime;  // Heure sans la date
```

**Exemples :**
```pascal
uses SysUtils;

WriteLn('Date : ', DateToStr(Date));      // 12/10/2025
WriteLn('Heure : ', TimeToStr(Time));     // 14:30:25
```

### Extraction de composants

```pascal
uses SysUtils;

procedure DecodeDate(date: TDateTime; var annee, mois, jour: Word);
procedure DecodeTime(time: TDateTime; var heure, minute, seconde, milli: Word);
```

**Exemples :**
```pascal
uses SysUtils;

var
  annee, mois, jour: Word;
  heure, minute, seconde, milli: Word;
begin
  DecodeDate(Now, annee, mois, jour);
  WriteLn(Format('Date : %d/%d/%d', [jour, mois, annee]));

  DecodeTime(Now, heure, minute, seconde, milli);
  WriteLn(Format('Heure : %d:%d:%d', [heure, minute, seconde]));
end.
```

### FormatDateTime - Formatage personnalisé

```pascal
uses SysUtils;

function FormatDateTime(const fmt: String; dt: TDateTime): String;
```

**Exemples :**
```pascal
uses SysUtils;

var
  dt: TDateTime;
begin
  dt := Now;

  WriteLn(FormatDateTime('dd/mm/yyyy', dt));           // 12/10/2025
  WriteLn(FormatDateTime('hh:nn:ss', dt));             // 14:30:25
  WriteLn(FormatDateTime('dddd dd mmmm yyyy', dt));    // Sunday 12 October 2025
  WriteLn(FormatDateTime('dd/mm/yyyy hh:nn', dt));     // 12/10/2025 14:30
end.
```

**Codes de format courants :**
- `dd` : jour (01-31)
- `mm` : mois (01-12)
- `yyyy` : année (4 chiffres)
- `hh` : heure (00-23)
- `nn` : minute (00-59)
- `ss` : seconde (00-59)
- `dddd` : nom du jour
- `mmmm` : nom du mois

## Fonctions de test/vérification

### Odd - Nombre impair ?

```pascal
function Odd(x: Integer): Boolean;
```

**Exemples :**
```pascal
WriteLn(Odd(5));    // True
WriteLn(Odd(6));    // False
WriteLn(Odd(0));    // False
WriteLn(Odd(-3));   // True
```

### Fonctions IsXXX (unité SysUtils)

```pascal
uses SysUtils;

function IsDelimiter(const delimiters, s: String; index: Integer): Boolean;
```

**Exemple :**
```pascal
uses SysUtils;

var
  texte: String;
begin
  texte := 'un,deux,trois';
  WriteLn(IsDelimiter(',', texte, 3));   // True (position 3 = ',')
  WriteLn(IsDelimiter(',', texte, 1));   // False
end.
```

## Fonctions de contrôle

### Inc et Dec - Incrémenter/décrémenter

```pascal
procedure Inc(var x: Integer);              // Incrémente de 1
procedure Inc(var x: Integer; delta: Integer);  // Incrémente de delta
procedure Dec(var x: Integer);              // Décrémente de 1
procedure Dec(var x: Integer; delta: Integer);  // Décrémente de delta
```

**Exemples :**
```pascal
var
  compteur: Integer;
begin
  compteur := 10;

  Inc(compteur);      // compteur = 11
  Inc(compteur, 5);   // compteur = 16

  Dec(compteur);      // compteur = 15
  Dec(compteur, 3);   // compteur = 12

  WriteLn(compteur);  // 12
end.
```

### Succ et Pred - Successeur/prédécesseur

```pascal
function Succ(x: Ordinal): Ordinal;
function Pred(x: Ordinal): Ordinal;
```

**Exemples :**
```pascal
WriteLn(Succ(5));      // 6
WriteLn(Pred(5));      // 4

WriteLn(Succ('A'));    // 'B'
WriteLn(Pred('C'));    // 'B'

WriteLn(Succ(True));   // (erreur si déjà au max)
WriteLn(Pred(False));  // (erreur si déjà au min)
```

## Exemple complet : mini-calculatrice

```pascal
program MiniCalculatrice;

uses
  SysUtils, Math;

procedure AfficherMenu;
begin
  WriteLn('=== CALCULATRICE ===');
  WriteLn('1. Addition');
  WriteLn('2. Multiplication');
  WriteLn('3. Puissance');
  WriteLn('4. Racine carrée');
  WriteLn('5. Arrondir');
  WriteLn('6. Nombre aléatoire');
  WriteLn('0. Quitter');
  WriteLn('====================');
end;

var
  choix: Integer;
  a, b, resultat: Real;
  texte: String;
begin
  Randomize;  // Initialise le générateur aléatoire (à appeler une seule fois)

  repeat
    AfficherMenu;
    Write('Votre choix : ');
    ReadLn(texte);
    choix := StrToIntDef(texte, -1);  // Convertit en entier, retourne -1 si invalide

    case choix of
      1: begin
           Write('Premier nombre : ');
           ReadLn(a);
           Write('Deuxième nombre : ');
           ReadLn(b);
           resultat := a + b;
           WriteLn('Résultat : ', Format('%.2f', [resultat]));
         end;

      2: begin
           Write('Premier nombre : ');
           ReadLn(a);
           Write('Deuxième nombre : ');
           ReadLn(b);
           resultat := a * b;
           WriteLn('Résultat : ', Format('%.2f', [resultat]));
         end;

      3: begin
           Write('Base : ');
           ReadLn(a);
           Write('Exposant : ');
           ReadLn(b);
           resultat := Power(a, b);
           WriteLn('Résultat : ', Format('%.2f', [resultat]));
         end;

      4: begin
           Write('Nombre : ');
           ReadLn(a);
           if a >= 0 then
           begin
             resultat := Sqrt(a);
             WriteLn('Résultat : ', Format('%.5f', [resultat]));
           end
           else
             WriteLn('Erreur : nombre négatif');
         end;

      5: begin
           Write('Nombre : ');
           ReadLn(a);
           WriteLn('Arrondi : ', Round(a));
           WriteLn('Tronqué : ', Trunc(a));
         end;

      6: begin
           WriteLn('Nombre aléatoire entre 1 et 100 : ', Random(100) + 1);
         end;

      0: WriteLn('Au revoir !');

      else
        WriteLn('Choix invalide');
    end;

    WriteLn;
  until choix = 0;
end.
```

## Résumé des unités importantes

| Unité | Description | Fonctions principales |
|-------|-------------|----------------------|
| **System** | Incluse automatiquement | WriteLn, ReadLn, Length, Pos, Copy, Chr, Ord, Inc, Dec |
| **SysUtils** | Utilitaires système | IntToStr, StrToInt, Format, UpperCase, Trim, Now, Date |
| **Math** | Fonctions mathématiques | Power, Min, Max, DegToRad, RadToDeg, Floor, Ceil |
| **StrUtils** | Utilitaires chaînes | AnsiReplaceStr, DupeString, ReverseString |

## Bonnes pratiques

1. **Toujours utiliser Randomize** avant Random
2. **Vérifier les conversions** : utiliser StrToIntDef au lieu de StrToInt
3. **Inclure les bonnes unités** : `uses SysUtils, Math;`
4. **Documenter** les fonctions utilisées si peu connues
5. **Tester les limites** : Sqrt avec négatif, division par zéro, etc.
6. **Préférer les fonctions standard** plutôt que recoder

## Points clés à retenir

1. Pascal fournit de **nombreuses fonctions prédéfinies** utiles
2. Les fonctions de base sont dans l'unité **System** (automatique)
3. Ajoutez **SysUtils** pour les conversions et dates
4. Ajoutez **Math** pour les fonctions mathématiques avancées
5. Utilisez **Format** pour un formatage élégant
6. N'oubliez pas **Randomize** avant d'utiliser Random
7. Les fonctions standard sont **testées et optimisées**
8. Consultez la **documentation** pour découvrir plus de fonctions
9. Les fonctions **Int**, **Trunc**, **Round** sont différentes
10. **UpperCase/LowerCase** nécessitent l'unité SysUtils

---

**Prochaine étape :** Dans la section 4.11, nous verrons comment **organiser modulairement le code** en créant des procédures et fonctions bien structurées pour faciliter la maintenance et la réutilisation.

⏭️ [Organisation modulaire du code](/04-procedures-fonctions/11-organisation-modulaire-code.md)
