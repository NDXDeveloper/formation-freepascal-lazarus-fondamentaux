🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 5.3 Chaînes de caractères (String, ShortString)

## Qu'est-ce qu'une chaîne de caractères ?

Une chaîne de caractères (ou "string" en anglais) est une suite de caractères qui forme un texte. C'est comme si vous aligniez plusieurs variables de type `Char` pour créer des mots, des phrases ou des textes complets.

### Exemples de chaînes

```
"Bonjour"
"Pascal"
"123 rue de la Paix"
"alice@example.com"
"Né le 15/03/1990"
```

En Pascal, une chaîne est entourée de **guillemets simples** : `'Bonjour'`

## Les différents types de chaînes en Pascal

Pascal propose plusieurs types de chaînes, chacun avec ses particularités :

### 1. String (chaîne longue)

Le type `String` sans précision de taille peut contenir jusqu'à **2 Go de texte** (en pratique, limité par la mémoire disponible).

```pascal
var
  nom: String;
  phrase: String;
```

**Avantages :**
- Taille dynamique (s'adapte automatiquement)
- Gestion automatique de la mémoire
- Idéal pour la plupart des usages modernes

### 2. ShortString (chaîne courte)

Le type `ShortString` est limité à **255 caractères maximum**. C'est l'ancien type de chaîne de Turbo Pascal.

```pascal
var
  prenom: ShortString;
  ville: String[50];  // Exactement 50 caractères max
```

**Caractéristiques :**
- Taille fixe en mémoire
- Plus rapide pour de petites chaînes
- Compatible avec l'ancien code Pascal
- Premier octet stocke la longueur

### Quelle chaîne choisir ?

Pour débuter et dans la plupart des cas, **utilisez `String`** tout simplement. C'est le choix moderne et le plus pratique.

```pascal
var
  texte: String;  // ✓ Recommandé pour les débutants
```

## Déclaration et initialisation

### Déclaration simple

```pascal
var
  nom: String;
  prenom: String;
  age: String;
```

### Initialisation lors de la déclaration

```pascal
var
  message: String = 'Bonjour tout le monde !';
  titre: String = 'Formation Pascal';
```

### Initialisation dans le code

```pascal
program InitChaines;
var
  nom, prenom, complet: String;
begin
  nom := 'Dupont';
  prenom := 'Jean';
  complet := prenom + ' ' + nom;  // Concaténation

  WriteLn(complet);  // Affiche : Jean Dupont
end.
```

## Opérations de base sur les chaînes

### 1. Concaténation (assemblage)

L'opérateur `+` permet de coller plusieurs chaînes ensemble :

```pascal
program Concatenation;
var
  prenom, nom, phrase: String;
begin
  prenom := 'Marie';
  nom := 'Martin';

  // Assemblage de chaînes
  phrase := 'Bonjour ' + prenom + ' ' + nom + ' !';
  WriteLn(phrase);  // Bonjour Marie Martin !

  // On peut aussi mélanger avec des nombres
  phrase := prenom + ' a ' + '25' + ' ans';
  WriteLn(phrase);  // Marie a 25 ans
end.
```

### 2. Longueur d'une chaîne

La fonction `Length()` retourne le nombre de caractères :

```pascal
program LongueurChaine;
var
  texte: String;
  taille: Integer;
begin
  texte := 'Bonjour';
  taille := Length(texte);
  WriteLn('Le mot "', texte, '" contient ', taille, ' caractères');
  // Affiche : Le mot "Bonjour" contient 7 caractères

  // Chaîne vide
  texte := '';
  WriteLn('Longueur de la chaîne vide : ', Length(texte));  // 0
end.
```

### 3. Accès à un caractère

Comme un tableau, on peut accéder à chaque caractère individuellement :

```pascal
program AccesCaractere;
var
  mot: String;
  i: Integer;
begin
  mot := 'Pascal';

  // Accès au premier caractère
  WriteLn('Premier caractère : ', mot[1]);  // P

  // Accès au dernier caractère
  WriteLn('Dernier caractère : ', mot[Length(mot)]);  // l

  // Parcours de tous les caractères
  for i := 1 to Length(mot) do
    WriteLn('Caractère ', i, ' : ', mot[i]);
end.
```

**Important :** En Pascal, les indices des chaînes commencent à **1** (pas à 0 comme dans certains langages).

### 4. Modification d'un caractère

```pascal
program ModificationCaractere;
var
  mot: String;
begin
  mot := 'Pascal';
  WriteLn('Avant : ', mot);

  mot[1] := 'B';  // On change le P en B
  WriteLn('Après : ', mot);  // Bascal
end.
```

## Fonctions importantes pour les chaînes

### Copy() - Extraire une portion

```pascal
Copy(chaine, position, longueur)
```

```pascal
program ExempleCopy;
var
  texte, extrait: String;
begin
  texte := 'Bonjour tout le monde';

  // Extraire à partir de la position 1, 7 caractères
  extrait := Copy(texte, 1, 7);
  WriteLn(extrait);  // Bonjour

  // Extraire "monde"
  extrait := Copy(texte, 17, 5);
  WriteLn(extrait);  // monde

  // Si on demande trop de caractères, ça s'arrête à la fin
  extrait := Copy(texte, 1, 100);
  WriteLn(extrait);  // Bonjour tout le monde
end.
```

### Pos() - Chercher dans une chaîne

```pascal
Pos(sous_chaine, chaine)  // Retourne la position (0 si non trouvé)
```

```pascal
program ExemplePos;
var
  texte: String;
  position: Integer;
begin
  texte := 'Pascal est un langage génial';

  // Chercher "langage"
  position := Pos('langage', texte);
  if position > 0 then
    WriteLn('Trouvé à la position : ', position)  // 15
  else
    WriteLn('Non trouvé');

  // Chercher quelque chose qui n'existe pas
  position := Pos('Python', texte);
  if position = 0 then
    WriteLn('Python non trouvé dans le texte');
end.
```

### Delete() - Supprimer des caractères

```pascal
Delete(chaine, position, nombre)
```

```pascal
program ExempleDelete;
var
  texte: String;
begin
  texte := 'Bonjour tout le monde';
  WriteLn('Avant : ', texte);

  // Supprimer "tout le " (9 caractères à partir de la position 9)
  Delete(texte, 9, 8);
  WriteLn('Après : ', texte);  // Bonjour monde
end.
```

### Insert() - Insérer du texte

```pascal
Insert(sous_chaine, chaine, position)
```

```pascal
program ExempleInsert;
var
  texte: String;
begin
  texte := 'Bonjour monde';
  WriteLn('Avant : ', texte);

  // Insérer "le " à la position 9
  Insert('le ', texte, 9);
  WriteLn('Après : ', texte);  // Bonjour le monde
end.
```

### UpCase() et LowerCase() - Changer la casse

```pascal
program ExempleCasse;
var
  texte: String;
begin
  texte := 'Pascal';

  WriteLn('Original : ', texte);
  WriteLn('Majuscules : ', UpCase(texte));    // PASCAL
  WriteLn('Minuscules : ', LowerCase(texte)); // pascal
end.
```

**Note :** `UpCase()` en standard Pascal ne fonctionne que sur un `Char`. Pour une chaîne complète, utilisez `UpperCase()` et `LowerCase()` de l'unité `SysUtils`.

### Trim() - Supprimer les espaces

```pascal
program ExempleTrim;
uses SysUtils;
var
  texte: String;
begin
  texte := '   Bonjour   ';

  WriteLn('[', texte, ']');        // [   Bonjour   ]
  WriteLn('[', Trim(texte), ']');  // [Bonjour]
end.
```

## Comparaison de chaînes

### Opérateurs de comparaison

```pascal
program ComparaisonChaines;
var
  nom1, nom2: String;
begin
  nom1 := 'Alice';
  nom2 := 'Bob';

  // Égalité
  if nom1 = nom2 then
    WriteLn('Les noms sont identiques')
  else
    WriteLn('Les noms sont différents');

  // Ordre alphabétique
  if nom1 < nom2 then
    WriteLn(nom1, ' vient avant ', nom2);  // Alice vient avant Bob

  // Différence
  if nom1 <> nom2 then
    WriteLn('Les noms ne sont pas les mêmes');
end.
```

**Attention :** Par défaut, la comparaison est **sensible à la casse** ('Alice' ≠ 'alice').

### Comparaison insensible à la casse

```pascal
program ComparaisonInsensible;
uses SysUtils;
var
  mot1, mot2: String;
begin
  mot1 := 'Pascal';
  mot2 := 'PASCAL';

  // Comparaison normale (sensible)
  if mot1 = mot2 then
    WriteLn('Identiques')
  else
    WriteLn('Différents');  // Différents

  // Comparaison insensible à la casse
  if CompareText(mot1, mot2) = 0 then
    WriteLn('Identiques (insensible)');  // Identiques (insensible)
end.
```

## Conversion entre String et nombres

### Nombre vers String

```pascal
program NombreVersString;
uses SysUtils;
var
  nombre: Integer;
  texte: String;
begin
  nombre := 42;

  // Méthode 1 : IntToStr
  texte := IntToStr(nombre);
  WriteLn('Texte : ', texte);

  // Méthode 2 : Str
  Str(nombre, texte);
  WriteLn('Texte : ', texte);

  // Pour les réels
  texte := FloatToStr(3.14);
  WriteLn('Pi : ', texte);
end.
```

### String vers nombre

```pascal
program StringVersNombre;
uses SysUtils;
var
  texte: String;
  nombre: Integer;
  valeur: Real;
  code: Integer;
begin
  // Méthode 1 : StrToInt (peut générer une exception)
  texte := '123';
  nombre := StrToInt(texte);
  WriteLn('Nombre : ', nombre);

  // Méthode 2 : Val (plus sûre)
  Val(texte, nombre, code);
  if code = 0 then
    WriteLn('Conversion réussie : ', nombre)
  else
    WriteLn('Erreur à la position : ', code);

  // Pour les réels
  valeur := StrToFloat('3.14');
  WriteLn('Valeur : ', valeur:0:2);
end.
```

## Exemples pratiques

### Exemple 1 : Inverser une chaîne

```pascal
program InverserChaine;
var
  texte, inverse: String;
  i: Integer;
begin
  Write('Entrez un mot : ');
  ReadLn(texte);

  inverse := '';
  for i := Length(texte) downto 1 do
    inverse := inverse + texte[i];

  WriteLn('Inversé : ', inverse);
end.
```

### Exemple 2 : Compter les voyelles

```pascal
program CompterVoyelles;
var
  texte: String;
  i, compte: Integer;
  c: Char;
begin
  Write('Entrez une phrase : ');
  ReadLn(texte);

  compte := 0;
  for i := 1 to Length(texte) do
  begin
    c := UpCase(texte[i]);  // Convertir en majuscule
    if (c = 'A') or (c = 'E') or (c = 'I') or (c = 'O') or
       (c = 'U') or (c = 'Y') then
      compte := compte + 1;
  end;

  WriteLn('Nombre de voyelles : ', compte);
end.
```

### Exemple 3 : Vérifier si c'est un palindrome

```pascal
program Palindrome;
var
  texte: String;
  i, j: Integer;
  estPalindrome: Boolean;
begin
  Write('Entrez un mot : ');
  ReadLn(texte);

  estPalindrome := True;
  j := Length(texte);

  for i := 1 to Length(texte) div 2 do
  begin
    if texte[i] <> texte[j] then
    begin
      estPalindrome := False;
      Break;
    end;
    j := j - 1;
  end;

  if estPalindrome then
    WriteLn(texte, ' est un palindrome')
  else
    WriteLn(texte, ' n''est pas un palindrome');
end.
```

### Exemple 4 : Extraire le prénom et nom

```pascal
program ExtraireNoms;
var
  nomComplet, prenom, nom: String;
  posEspace: Integer;
begin
  Write('Entrez prénom et nom : ');
  ReadLn(nomComplet);

  // Trouver la position de l'espace
  posEspace := Pos(' ', nomComplet);

  if posEspace > 0 then
  begin
    // Extraire le prénom (avant l'espace)
    prenom := Copy(nomComplet, 1, posEspace - 1);

    // Extraire le nom (après l'espace)
    nom := Copy(nomComplet, posEspace + 1, Length(nomComplet));

    WriteLn('Prénom : ', prenom);
    WriteLn('Nom : ', nom);
  end
  else
    WriteLn('Format incorrect : espace manquant');
end.
```

### Exemple 5 : Remplacer un mot

```pascal
program RemplacerMot;
uses SysUtils;
var
  texte, ancien, nouveau: String;
  position: Integer;
begin
  texte := 'J''aime Python. Python est génial.';
  ancien := 'Python';
  nouveau := 'Pascal';

  WriteLn('Texte original : ', texte);

  // Remplacer toutes les occurrences
  repeat
    position := Pos(ancien, texte);
    if position > 0 then
    begin
      Delete(texte, position, Length(ancien));
      Insert(nouveau, texte, position);
    end;
  until position = 0;

  WriteLn('Texte modifié : ', texte);
end.
```

## Chaînes multiligne

Pour créer des chaînes sur plusieurs lignes :

```pascal
program ChainesMultiligne;
var
  message: String;
begin
  // Méthode 1 : Concaténation
  message := 'Première ligne' + #13#10 +
             'Deuxième ligne' + #13#10 +
             'Troisième ligne';
  WriteLn(message);

  // Méthode 2 : Utiliser la constante système
  message := 'Ligne 1' + LineEnding + 'Ligne 2';
  WriteLn(message);
end.
```

**Codes spéciaux :**
- `#13#10` : Retour chariot + Saut de ligne (Windows)
- `#10` : Saut de ligne (Linux)
- `LineEnding` : Adaptatif selon la plateforme (recommandé)

## Caractères spéciaux

```pascal
program CaracteresSpeciaux;
begin
  WriteLn('Guillemet simple : ''');      // Pour afficher '
  WriteLn('Tabulation :'#9'texte');      // #9 = tabulation
  WriteLn('Sonnerie'#7);                 // #7 = bip sonore
  WriteLn('Symbole Euro : '#8364);       // Code Unicode
end.
```

## Différences String vs ShortString

### Limitations de ShortString

```pascal
program DifferenceTypes;
var
  court: ShortString;
  long: String;
begin
  // ShortString : max 255 caractères
  court := 'Texte court';
  // court := 'Texte très très très long...' (+ de 255 car) -> ERREUR !

  // String : pas de limite pratique
  long := 'Texte extrêmement long sans limite...';

  WriteLn('ShortString : ', Length(court));
  WriteLn('String : ', Length(long));
end.
```

### Pourquoi ShortString existe encore ?

- Compatibilité avec l'ancien code
- Taille fixe en mémoire (prévisible)
- Légèrement plus rapide pour de petites chaînes
- Utile pour certains protocoles binaires

## Pièges courants

### 1. Oubli des guillemets simples

```pascal
texte := "Bonjour";  // ✗ ERREUR : utiliser des guillemets simples
texte := 'Bonjour';  // ✓ CORRECT
```

### 2. Indices hors limites

```pascal
var
  mot: String;
begin
  mot := 'Bonjour';
  WriteLn(mot[10]);  // ✗ ERREUR : le mot n'a que 7 caractères
end.
```

### 3. Confusion entre Char et String

```pascal
var
  c: Char;
  s: String;
begin
  c := 'A';      // ✓ Un seul caractère
  s := 'Texte';  // ✓ Plusieurs caractères
  c := 'AB';     // ✗ ERREUR : trop de caractères pour un Char
end.
```

### 4. Oublier uses SysUtils

Beaucoup de fonctions utiles (IntToStr, FloatToStr, etc.) nécessitent :

```pascal
uses SysUtils;
```

## Résumé des fonctions principales

| Fonction | Description | Exemple |
|----------|-------------|---------|
| `Length(s)` | Longueur de la chaîne | `Length('Bonjour')` → 7 |
| `Copy(s, pos, len)` | Extraire une portion | `Copy('Pascal', 1, 3)` → 'Pas' |
| `Pos(sub, s)` | Position d'une sous-chaîne | `Pos('ca', 'Pascal')` → 4 |
| `Delete(s, pos, len)` | Supprimer des caractères | Modifie directement `s` |
| `Insert(sub, s, pos)` | Insérer du texte | Modifie directement `s` |
| `UpperCase(s)` | Convertir en majuscules | `UpperCase('abc')` → 'ABC' |
| `LowerCase(s)` | Convertir en minuscules | `LowerCase('ABC')` → 'abc' |
| `Trim(s)` | Retirer espaces début/fin | `Trim('  ok  ')` → 'ok' |
| `IntToStr(n)` | Nombre → String | `IntToStr(42)` → '42' |
| `StrToInt(s)` | String → Nombre | `StrToInt('42')` → 42 |

## Conseils pratiques

1. **Utilisez `String`** pour la plupart de vos besoins
2. **Vérifiez toujours la longueur** avant d'accéder aux caractères
3. **Utilisez `SysUtils`** pour accéder aux fonctions avancées
4. **Testez les conversions** String ↔ Nombre pour éviter les erreurs
5. **Préférez `Length()`** à compter manuellement

## Résumé

Les chaînes de caractères sont essentielles en programmation :
- `String` : type moderne, taille dynamique, idéal pour débuter
- `ShortString` : type ancien, limité à 255 caractères
- Nombreuses fonctions disponibles pour manipuler le texte
- Conversion facile entre chaînes et nombres
- Indices commencent à 1 en Pascal

Maîtriser les chaînes vous permettra de créer des programmes interactifs et de traiter du texte efficacement !

⏭️ [Enregistrements (Records)](/05-types-donnees-structures/04-enregistrements-records.md)
