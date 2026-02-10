🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.3 Systèmes de numération et représentation des données

## Introduction

Dans cette section, nous allons explorer comment les ordinateurs représentent et stockent les informations. Comprendre les systèmes de numération est essentiel pour saisir le fonctionnement interne des programmes et optimiser votre code.

## Les systèmes de numération

### Le système décimal (base 10)

C'est le système que nous utilisons au quotidien.

**Caractéristiques :**
- Utilise 10 chiffres : 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
- Chaque position représente une puissance de 10

**Exemple : décomposition du nombre 2543**
```
2543 = (2 × 10³) + (5 × 10²) + (4 × 10¹) + (3 × 10⁰)
     = (2 × 1000) + (5 × 100) + (4 × 10) + (3 × 1)
     = 2000 + 500 + 40 + 3
     = 2543
```

Chaque position a un poids qui est une puissance de 10 :
- Position 1 (unités) : 10⁰ = 1
- Position 2 (dizaines) : 10¹ = 10
- Position 3 (centaines) : 10² = 100
- Position 4 (milliers) : 10³ = 1000

### Le système binaire (base 2)

C'est le système utilisé par les ordinateurs.

**Caractéristiques :**
- Utilise seulement 2 chiffres : 0 et 1
- Chaque chiffre est appelé un **bit** (binary digit)
- Chaque position représente une puissance de 2

**Pourquoi le binaire ?**
Les ordinateurs utilisent l'électricité, qui a deux états :
- Courant passe = 1
- Courant ne passe pas = 0

**Exemple : le nombre binaire 1011**
```
1011 = (1 × 2³) + (0 × 2²) + (1 × 2¹) + (1 × 2⁰)
     = (1 × 8) + (0 × 4) + (1 × 2) + (1 × 1)
     = 8 + 0 + 2 + 1
     = 11 en décimal
```

Puissances de 2 à connaître :
```
2⁰ = 1
2¹ = 2
2² = 4
2³ = 8
2⁴ = 16
2⁵ = 32
2⁶ = 64
2⁷ = 128
2⁸ = 256
2⁹ = 512
2¹⁰ = 1024
```

### Conversion décimal → binaire

**Méthode des divisions successives par 2 :**

Convertissons 13 en binaire :
```
13 ÷ 2 = 6 reste 1  ← bit de poids faible (droite)
6 ÷ 2 = 3 reste 0
3 ÷ 2 = 1 reste 1
1 ÷ 2 = 0 reste 1  ← bit de poids fort (gauche)

Résultat : 1101
```

**Vérification :**
```
1101 = (1×8) + (1×4) + (0×2) + (1×1) = 8 + 4 + 0 + 1 = 13 ✓
```

### Conversion binaire → décimal

**Méthode des puissances de 2 :**

Convertissons 10110 en décimal :
```
Position:  4    3    2    1    0  
Bit:       1    0    1    1    0  
Poids:    16    8    4    2    1  

Calcul: (1×16) + (0×8) + (1×4) + (1×2) + (0×1)
      = 16 + 0 + 4 + 2 + 0
      = 22
```

### Le système hexadécimal (base 16)

Le système hexadécimal est très utilisé en informatique comme notation compacte du binaire.

**Caractéristiques :**
- Utilise 16 symboles : 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F
- A = 10, B = 11, C = 12, D = 13, E = 14, F = 15
- Chaque position représente une puissance de 16
- Préfixe courant : `0x` ou `$` (en Pascal)

**Pourquoi l'hexadécimal ?**
4 bits = 1 chiffre hexadécimal, donc c'est plus compact que le binaire :
```
Binaire:      1111 0101 1010 0011  
Hexadécimal:   F    5    A    3  
```

**Correspondance binaire-hexadécimal :**
```
Décimal | Binaire | Hexadécimal
   0    |  0000   |      0
   1    |  0001   |      1
   2    |  0010   |      2
   3    |  0011   |      3
   4    |  0100   |      4
   5    |  0101   |      5
   6    |  0110   |      6
   7    |  0111   |      7
   8    |  1000   |      8
   9    |  1001   |      9
  10    |  1010   |      A
  11    |  1011   |      B
  12    |  1100   |      C
  13    |  1101   |      D
  14    |  1110   |      E
  15    |  1111   |      F
```

**Exemple : 2A3F en hexadécimal**
```
2A3F = (2 × 16³) + (10 × 16²) + (3 × 16¹) + (15 × 16⁰)
     = (2 × 4096) + (10 × 256) + (3 × 16) + (15 × 1)
     = 8192 + 2560 + 48 + 15
     = 10815 en décimal
```

### Le système octal (base 8)

Moins utilisé aujourd'hui, mais encore présent dans certains contextes.

**Caractéristiques :**
- Utilise 8 chiffres : 0, 1, 2, 3, 4, 5, 6, 7
- Chaque position représente une puissance de 8
- 3 bits = 1 chiffre octal
- Préfixe courant : `0` (en début de nombre)

**Exemple : 157 en octal**
```
157 (octal) = (1 × 8²) + (5 × 8¹) + (7 × 8⁰)
            = (1 × 64) + (5 × 8) + (7 × 1)
            = 64 + 40 + 7
            = 111 en décimal
```

## Représentation des nombres entiers

### Entiers non signés (positifs uniquement)

Sur 8 bits (1 octet), on peut représenter de 0 à 255 :
```
00000000 = 0
00000001 = 1
00000010 = 2
...
11111110 = 254
11111111 = 255
```

**Formule :** avec n bits, on peut représenter 2ⁿ valeurs différentes.

**Exemples :**
- 8 bits : 2⁸ = 256 valeurs (0 à 255)
- 16 bits : 2¹⁶ = 65 536 valeurs (0 à 65 535)
- 32 bits : 2³² = 4 294 967 296 valeurs (0 à 4 294 967 295)

### Entiers signés (positifs et négatifs)

Pour représenter des nombres négatifs, on utilise le **complément à deux**.

**Principe :**
- Le bit de poids fort (le plus à gauche) indique le signe
- 0 = positif, 1 = négatif

**Sur 8 bits signés :**
- Plage : -128 à +127
- `0111 1111` = +127 (le plus grand)
- `0000 0000` = 0
- `1000 0000` = -128 (le plus petit)

**Comment représenter -5 sur 8 bits ?**

Méthode du complément à deux :
1. Écrire 5 en binaire : `0000 0101`
2. Inverser tous les bits : `1111 1010`
3. Ajouter 1 : `1111 1011`

Donc -5 = `1111 1011`

**Vérification :**
Si on additionne 5 et -5 :
```
  0000 0101  (5)
+ 1111 1011  (-5)
-----------
  0000 0000  (0) ✓
```

### Dépassement de capacité (overflow)

Que se passe-t-il si on dépasse la capacité ?

**Exemple sur 8 bits non signés :**
```
  1111 1111  (255)
+        1  (+1)
-----------
  0000 0000  (0) ← Le résultat déborde !
```

C'est ce qu'on appelle un **dépassement de capacité** ou **overflow**. Le 9ème bit est perdu.

**Attention :** En programmation, il faut toujours choisir le type de variable adapté à la plage de valeurs attendue.

## Représentation des nombres à virgule

### Nombres à virgule fixe

Simple mais limité : on fixe le nombre de décimales.

**Exemple :** sur 16 bits avec 2 décimales
- Le nombre 123.45 est stocké comme 12345
- On divise mentalement par 100

### Nombres à virgule flottante (standard IEEE 754)

C'est la méthode la plus courante, utilisée pour les types `Real`, `Single`, `Double`.

**Principe :** notation scientifique en binaire
```
Nombre = Signe × Mantisse × 2^Exposant
```

**Format simple précision (32 bits) :**
```
| Signe | Exposant |    Mantisse    |
|  1 bit|  8 bits  |    23 bits     |
```

**Format double précision (64 bits) :**
```
| Signe | Exposant |    Mantisse    |
|  1 bit| 11 bits  |    52 bits     |
```

**Exemple simplifié :** représenter 6.5
1. En binaire : 6.5 = 110.1
2. Normaliser : 1.101 × 2²
3. Stocker : signe (0), exposant (2), mantisse (101)

**Limitations importantes :**
- Précision limitée (environ 7 chiffres pour `Single`, 15 pour `Double`)
- Certains nombres décimaux ne peuvent pas être représentés exactement
- Exemple : 0.1 n'a pas de représentation exacte en binaire

**Conséquence :** éviter les comparaisons d'égalité strictes avec les flottants
```pascal
// Mauvais
if (x = 0.1) then ...

// Bon
if (Abs(x - 0.1) < 0.0001) then ...
```

## Représentation des caractères

### La table ASCII

ASCII (American Standard Code for Information Interchange) est le standard historique.

**Caractéristiques :**
- Utilise 7 bits (128 caractères)
- Extension 8 bits (256 caractères)

**Principales plages ASCII :**
```
Décimal | Hexadécimal | Caractère
  0-31  |   00-1F     | Caractères de contrôle
   32   |     20      | Espace
  48-57 |   30-39     | Chiffres '0' à '9'
  65-90 |   41-5A     | Lettres majuscules 'A' à 'Z'
 97-122 |   61-7A     | Lettres minuscules 'a' à 'z'
```

**Exemples de codes ASCII :**
```
'A' = 65 (décimal) = 41 (hexa) = 01000001 (binaire)
'B' = 66
'a' = 97
'0' = 48  ← Le caractère '0', pas le nombre 0
' ' = 32  (espace)
```

**Caractères de contrôle courants :**
```
0   = NUL (Null)
7   = BEL (Beep)
8   = BS  (Backspace)
9   = TAB (Tabulation)
10  = LF  (Line Feed - nouvelle ligne Unix/Linux)
13  = CR  (Carriage Return - retour chariot)
27  = ESC (Escape)
```

### ASCII étendu

Les codes 128-255 varient selon les pages de code :
- Page 437 : IBM PC original
- Page 850 : Europe occidentale
- Page 1252 : Windows Europe occidentale

**Problème :** incompatibilité entre systèmes

### Unicode et UTF-8

Unicode est le standard moderne pour représenter tous les alphabets du monde.

**Caractéristiques :**
- Peut représenter plus d'un million de caractères
- Inclut tous les alphabets : latin, cyrillique, arabe, chinois, emoji, etc.
- UTF-8 est l'encodage le plus courant

**UTF-8 :**
- Utilise 1 à 4 octets par caractère
- Compatible avec ASCII (les 128 premiers caractères sont identiques)
- Taille variable selon le caractère

**Exemples UTF-8 :**
```
'A' = 1 octet  : 01000001
'é' = 2 octets : 11000011 10101001
'€' = 3 octets : 11100010 10000010 10101100
'😀' = 4 octets : 11110000 10011111 10011000 10000000
```

**En Pascal/Lazarus :**
- Type `AnsiString` : encodage ANSI/ASCII
- Type `UTF8String` : encodage UTF-8
- Type `UnicodeString` : encodage UTF-16

## Représentation des chaînes de caractères

### Chaînes de longueur fixe

**ShortString en Pascal :**
- Longueur maximale : 255 caractères
- Le premier octet contient la longueur
- Les caractères suivent

```
Exemple : 'Bonjour'
[7]['B']['o']['n']['j']['o']['u']['r'][...octets inutilisés...]
```

### Chaînes de longueur variable

**String (AnsiString/UnicodeString) :**
- Longueur dynamique
- Gestion automatique de la mémoire
- Terminées par un caractère nul (#0) pour compatibilité C

### Chaînes nulles (C-Style)

En langage C et dans les API système :
- Tableau de caractères
- Terminé par le caractère nul (code 0)
- Pas de stockage de la longueur

```
'Hello' en C : ['H']['e']['l']['l']['o'][0]
```

## Représentation des booléens

Un booléen ne peut avoir que deux valeurs : VRAI ou FAUX.

**Stockage :**
- Théoriquement : 1 bit suffit
- En pratique : souvent 1 octet (8 bits) pour des raisons d'adressage

**En Pascal :**
- Type `Boolean` : 1 octet
- `False` = 0
- `True` = 1 (mais toute valeur non nulle peut être considérée comme vraie)

## Les unités de mesure en informatique

### Bits et octets

```
1 bit       = 0 ou 1
1 octet     = 8 bits
1 mot       = 2 octets (16 bits) ou 4 octets (32 bits) selon l'architecture
1 double mot = 4 octets (32 bits)
1 quad mot  = 8 octets (64 bits)
```

### Préfixes de quantité

**Préfixes binaires (base 2) :**
```
1 Kio (Kibioctet) = 1024 octets     = 2¹⁰ octets
1 Mio (Mébioctet) = 1024 Kio        = 2²⁰ octets
1 Gio (Gibioctet) = 1024 Mio        = 2³⁰ octets
1 Tio (Tébioctet) = 1024 Gio        = 2⁴⁰ octets
```

**Préfixes décimaux (base 10) :**
```
1 Ko (Kilooctet) = 1000 octets      = 10³ octets
1 Mo (Mégaoctet) = 1000 Ko          = 10⁶ octets
1 Go (Gigaoctet) = 1000 Mo          = 10⁹ octets
1 To (Téraoctet) = 1000 Go          = 10¹² octets
```

**Attention :** Confusion courante !
- Fabricants de disques durs : utilisent les préfixes décimaux
- Systèmes d'exploitation : utilisent souvent les préfixes binaires
- Un disque de "1 To" affiche environ 931 Gio dans Windows

## L'ordre des octets (Endianness)

Quand un nombre prend plusieurs octets, dans quel ordre sont-ils stockés ?

### Little Endian (Intel x86, AMD64)

L'octet de poids faible est stocké en premier.

**Exemple : le nombre 0x12345678 (305 419 896)**
```
Adresse :  0x00    0x01    0x02    0x03  
Valeur  :  0x78    0x56    0x34    0x12  
```

### Big Endian (Motorola, PowerPC, réseau)

L'octet de poids fort est stocké en premier.

**Exemple : le même nombre 0x12345678**
```
Adresse :  0x00    0x01    0x02    0x03  
Valeur  :  0x12    0x34    0x56    0x78  
```

**Impact :** Important lors de la communication réseau ou de la lecture de fichiers binaires créés sur d'autres systèmes.

## Opérations sur les bits

### Opérateurs bit à bit

Ces opérateurs travaillent directement sur la représentation binaire :

**AND (ET logique) - `and`**
```
  1010
& 1100
------
  1000
```
Résultat : 1 seulement si les deux bits valent 1.

**OR (OU logique) - `or`**
```
  1010
| 1100
------
  1110
```
Résultat : 1 si au moins un des deux bits vaut 1.

**XOR (OU exclusif) - `xor`**
```
  1010
^ 1100
------
  0110
```
Résultat : 1 si les deux bits sont différents.

**NOT (complément) - `not`**
```
~ 1010
------
  0101
```
Inverse tous les bits.

### Décalages de bits

**Décalage à gauche (SHL - Shift Left)**
```
0011 shl 1 = 0110  (3 × 2 = 6, équivaut à multiplier par 2)
0011 shl 2 = 1100  (3 × 4 = 12, équivaut à multiplier par 4)
```

**Attention :** si le bit de poids fort est perdu lors du décalage (dépassement de capacité), le résultat ne correspond plus à une simple multiplication.

**Décalage à droite (SHR - Shift Right)**
```
1100 shr 1 = 0110  (équivaut à diviser par 2)
1100 shr 2 = 0011  (équivaut à diviser par 4)
```

**Utilisations :**
- Optimisation (multiplication/division par puissances de 2)
- Extraction de bits spécifiques
- Manipulation de drapeaux (flags)

## Application pratique : les couleurs RVB

Les couleurs sur écran sont souvent représentées en RVB (Rouge, Vert, Bleu).

**Format 24 bits (True Color) :**
```
| Rouge  | Vert   | Bleu   |
| 8 bits | 8 bits | 8 bits |
```

Chaque composante va de 0 à 255.

**Exemples en hexadécimal :**
```
#FF0000 = Rouge pur    (255,   0,   0)
#00FF00 = Vert pur     (  0, 255,   0)
#0000FF = Bleu pur     (  0,   0, 255)
#FFFFFF = Blanc        (255, 255, 255)
#000000 = Noir         (  0,   0,   0)
#FF00FF = Magenta      (255,   0, 255)
#FFFF00 = Jaune        (255, 255,   0)
#808080 = Gris         (128, 128, 128)
```

## Conclusion

Vous avez maintenant une compréhension solide des systèmes de numération et de la représentation des données en informatique.

**Points clés à retenir :**
- Les ordinateurs utilisent le système binaire (base 2)
- L'hexadécimal (base 16) est une notation compacte du binaire
- Les entiers peuvent être signés ou non signés
- Les nombres à virgule utilisent la notation IEEE 754
- Les caractères sont codés en ASCII ou Unicode (UTF-8)
- Chaque type de donnée a une représentation spécifique en mémoire
- Les opérations bit à bit permettent de manipuler directement les bits

Cette compréhension vous sera utile pour :
- Choisir les bons types de données
- Comprendre les limites et la précision
- Optimiser votre code
- Déboguer des problèmes subtils
- Travailler avec des fichiers binaires et des protocoles réseau

Dans la section suivante, nous aborderons la logique booléenne et les tables de vérité, qui sont essentielles pour comprendre les conditions et les tests dans la programmation.

⏭️ [Logique booléenne et tables de vérité](/01-prerequis-bases-programmation/04-logique-booleenne-tables-verite.md)
