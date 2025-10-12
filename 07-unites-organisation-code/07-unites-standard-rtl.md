🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 7.7 Unités standard du RTL

## Qu'est-ce que le RTL ?

Le **RTL** (Run-Time Library) est la **bibliothèque d'exécution** de FreePascal. C'est une collection d'unités prêtes à l'emploi qui fournissent des fonctions pour les tâches courantes de programmation.

### Analogie : La boîte à outils

Imaginez que vous construisez une maison :
- **Sans RTL** : Vous devez fabriquer vous-même chaque outil (marteau, scie, tournevis)
- **Avec RTL** : Vous avez une boîte à outils complète, vous prenez simplement ce dont vous avez besoin

Le RTL vous évite de réinventer la roue et vous permet de vous concentrer sur la logique de votre programme.

## Les unités fondamentales

### System - L'unité invisible

L'unité `System` est **automatiquement** incluse dans tous les programmes. Vous n'avez pas besoin de l'ajouter dans la clause `uses`.

Elle contient les fonctions de base :
- `WriteLn`, `ReadLn`
- `Inc`, `Dec`
- `Length`, `SetLength`
- `New`, `Dispose`
- Les opérateurs de base
- Les types primitifs

```pascal
program SansUses;

begin
  // Pas besoin de "uses System"
  WriteLn('Hello World');  // ✅ Fonctionne automatiquement
  Inc(x);                  // ✅ Fonctionne automatiquement
end.
```

## SysUtils - Utilitaires système

L'unité **SysUtils** est probablement l'unité la plus utilisée après `System`. Elle contient des fonctions essentielles pour :

### 1. Manipulation de chaînes

```pascal
uses
  SysUtils;

var
  texte, maj, min: String;
  i: Integer;

begin
  texte := '  Bonjour le Monde  ';

  // Conversion de casse
  maj := UpperCase(texte);     // '  BONJOUR LE MONDE  '
  min := LowerCase(texte);     // '  bonjour le monde  '

  // Suppression des espaces
  texte := Trim(texte);        // 'Bonjour le Monde'

  // Conversion en entier
  i := StrToInt('123');        // 123

  // Conversion d'entier en chaîne
  texte := IntToStr(456);      // '456'

  // Formatage
  texte := Format('J''ai %d ans', [25]);  // 'J'ai 25 ans'
end.
```

### 2. Gestion des fichiers et répertoires

```pascal
uses
  SysUtils;

var
  chemin, fichier: String;

begin
  // Obtenir le répertoire courant
  chemin := GetCurrentDir;
  WriteLn('Répertoire : ', chemin);

  // Vérifier l'existence d'un fichier
  if FileExists('data.txt') then
    WriteLn('Le fichier existe');

  // Vérifier l'existence d'un répertoire
  if DirectoryExists('donnees') then
    WriteLn('Le répertoire existe');

  // Créer un répertoire
  if not DirectoryExists('temp') then
    CreateDir('temp');

  // Supprimer un fichier
  if FileExists('ancien.txt') then
    DeleteFile('ancien.txt');

  // Extraire le nom du fichier d'un chemin
  fichier := ExtractFileName('/home/user/data.txt');  // 'data.txt'
end.
```

### 3. Gestion des dates et heures

```pascal
uses
  SysUtils;

var
  maintenant: TDateTime;
  dateStr: String;

begin
  // Obtenir la date et l'heure actuelles
  maintenant := Now;
  WriteLn('Maintenant : ', DateTimeToStr(maintenant));

  // Obtenir seulement la date
  maintenant := Date;
  WriteLn('Date : ', DateToStr(maintenant));

  // Obtenir seulement l'heure
  maintenant := Time;
  WriteLn('Heure : ', TimeToStr(maintenant));

  // Formater une date
  dateStr := FormatDateTime('dd/mm/yyyy hh:nn:ss', Now);
  WriteLn(dateStr);  // Ex: 13/10/2025 14:30:45
end.
```

### 4. Gestion des exceptions

```pascal
uses
  SysUtils;

var
  nombre: Integer;

begin
  try
    nombre := StrToInt('abc');  // Erreur : pas un nombre
  except
    on E: EConvertError do
      WriteLn('Erreur de conversion : ', E.Message);
  end;
end.
```

## Classes - Collections et listes

L'unité **Classes** fournit des classes de base pour stocker et organiser des données.

### TStringList - Liste de chaînes

```pascal
uses
  Classes, SysUtils;

var
  liste: TStringList;
  i: Integer;

begin
  liste := TStringList.Create;
  try
    // Ajouter des éléments
    liste.Add('Pomme');
    liste.Add('Banane');
    liste.Add('Orange');

    // Accéder aux éléments
    WriteLn('Premier : ', liste[0]);  // 'Pomme'
    WriteLn('Nombre : ', liste.Count);  // 3

    // Parcourir la liste
    for i := 0 to liste.Count - 1 do
      WriteLn(i, ': ', liste[i]);

    // Trier
    liste.Sort;

    // Sauvegarder dans un fichier
    liste.SaveToFile('liste.txt');

    // Charger depuis un fichier
    liste.LoadFromFile('liste.txt');

    // Rechercher
    if liste.IndexOf('Banane') >= 0 then
      WriteLn('Banane trouvée !');

  finally
    liste.Free;  // Toujours libérer !
  end;
end.
```

### TList - Liste d'objets

```pascal
uses
  Classes;

var
  liste: TList;
  valeur: Integer;

begin
  liste := TList.Create;
  try
    // Ajouter des pointeurs (attention : type non sécurisé)
    liste.Add(Pointer(10));
    liste.Add(Pointer(20));
    liste.Add(Pointer(30));

    // Récupérer
    valeur := Integer(liste[0]);  // 10

    WriteLn('Nombre d''éléments : ', liste.Count);
  finally
    liste.Free;
  end;
end.
```

### TStream - Flux de données

Les streams sont utilisés pour lire et écrire des données de manière uniforme (fichiers, mémoire, réseau).

```pascal
uses
  Classes, SysUtils;

var
  fichier: TFileStream;
  texte: String;
  buffer: array[0..255] of Byte;

begin
  // Écriture dans un fichier
  fichier := TFileStream.Create('data.bin', fmCreate);
  try
    texte := 'Hello World';
    fichier.Write(texte[1], Length(texte));
  finally
    fichier.Free;
  end;

  // Lecture depuis un fichier
  fichier := TFileStream.Create('data.bin', fmOpenRead);
  try
    fichier.Read(buffer, fichier.Size);
  finally
    fichier.Free;
  end;
end.
```

## Math - Fonctions mathématiques

L'unité **Math** fournit des fonctions mathématiques avancées.

```pascal
uses
  Math;

var
  x, y: Real;

begin
  // Constantes
  WriteLn('Pi = ', Pi);  // 3.14159...

  // Fonctions de base
  x := Sqrt(16);          // Racine carrée = 4
  x := Power(2, 3);       // 2^3 = 8
  x := Sqr(5);            // 5^2 = 25

  // Fonctions trigonométriques
  x := Sin(Pi / 2);       // Sinus = 1
  x := Cos(0);            // Cosinus = 1
  x := Tan(Pi / 4);       // Tangente ≈ 1

  // Fonctions d'arrondi
  x := Ceil(3.2);         // Arrondi supérieur = 4
  x := Floor(3.8);        // Arrondi inférieur = 3
  x := Round(3.5);        // Arrondi classique = 4

  // Valeur absolue
  x := Abs(-5.7);         // 5.7

  // Maximum et minimum
  x := Max(10, 20);       // 20
  x := Min(10, 20);       // 10

  // Logarithmes et exponentielles
  x := Ln(2.718281828);   // Logarithme naturel ≈ 1
  x := Exp(1);            // e^1 ≈ 2.718
  x := Log10(100);        // Logarithme base 10 = 2

  // Fonctions statistiques
  x := Mean([1, 2, 3, 4, 5]);  // Moyenne = 3
  x := Sum([1, 2, 3, 4, 5]);   // Somme = 15
end.
```

## StrUtils - Manipulation avancée de chaînes

L'unité **StrUtils** complète SysUtils avec des fonctions supplémentaires.

```pascal
uses
  SysUtils, StrUtils;

var
  texte, resultat: String;

begin
  texte := 'Bonjour le monde';

  // Extraire la gauche ou la droite
  resultat := LeftStr(texte, 7);    // 'Bonjour'
  resultat := RightStr(texte, 5);   // 'monde'

  // Extraire le milieu
  resultat := MidStr(texte, 9, 2);  // 'le'

  // Inverser une chaîne
  resultat := ReverseString(texte);  // 'ednom el ruojnoB'

  // Remplacer du texte
  resultat := StringReplace(texte, 'monde', 'Pascal', [rfReplaceAll]);
  // 'Bonjour le Pascal'

  // Compter les occurrences
  WriteLn('Nombre de "o" : ', AnsiContainsStr(texte, 'o'));

  // Répéter une chaîne
  resultat := DupeString('Ha', 3);  // 'HaHaHa'

  // Remplir avec des caractères
  resultat := AddChar('*', 'Test', 10);  // 'Test******'
end.
```

## DateUtils - Manipulation avancée de dates

L'unité **DateUtils** offre des fonctions puissantes pour travailler avec les dates.

```pascal
uses
  SysUtils, DateUtils;

var
  date1, date2: TDateTime;
  jours, heures: Integer;

begin
  date1 := Now;

  // Ajouter des jours, mois, années
  date2 := IncDay(date1, 7);      // +7 jours
  date2 := IncMonth(date1, 2);    // +2 mois
  date2 := IncYear(date1, 1);     // +1 an

  // Calculer des différences
  date2 := EncodeDate(2025, 12, 31);
  jours := DaysBetween(date1, date2);
  heures := HoursBetween(date1, date2);

  WriteLn('Jours jusqu''à fin d''année : ', jours);

  // Début et fin de période
  date2 := StartOfTheDay(date1);   // Début de journée (00:00:00)
  date2 := EndOfTheMonth(date1);   // Fin du mois

  // Vérifications
  if IsLeapYear(2024) then
    WriteLn('2024 est une année bissextile');

  // Extraire des composants
  WriteLn('Année : ', YearOf(date1));
  WriteLn('Mois : ', MonthOf(date1));
  WriteLn('Jour : ', DayOf(date1));
  WriteLn('Heure : ', HourOf(date1));
end.
```

## CRT - Interface console (texte et couleurs)

L'unité **CRT** permet de contrôler la console texte (couleurs, positionnement).

```pascal
uses
  CRT;

begin
  ClrScr;  // Effacer l'écran

  // Changer les couleurs
  TextColor(Yellow);
  TextBackground(Blue);
  WriteLn('Texte jaune sur fond bleu');

  // Restaurer les couleurs par défaut
  NormVideo;

  // Positionner le curseur
  GotoXY(10, 5);  // Colonne 10, ligne 5
  WriteLn('Texte positionné');

  // Attendre une touche
  WriteLn('Appuyez sur une touche...');
  ReadKey;
end.
```

**Note :** L'unité CRT fonctionne mieux dans des applications console pures.

## Types - Définitions de types avancés

L'unité **Types** fournit des types de données utiles.

```pascal
uses
  Types;

var
  point: TPoint;
  rect: TRect;
  tableau: TIntegerDynArray;

begin
  // Point 2D
  point.X := 10;
  point.Y := 20;

  // Rectangle
  rect := Rect(0, 0, 100, 50);  // x1, y1, x2, y2

  // Tableau dynamique d'entiers
  SetLength(tableau, 5);
  tableau[0] := 10;
  tableau[1] := 20;
end.
```

## Variants - Variables de type variable

L'unité **Variants** permet d'utiliser le type `Variant` qui peut contenir différents types.

```pascal
uses
  Variants;

var
  v: Variant;

begin
  v := 10;              // Entier
  WriteLn(v);

  v := 'Hello';         // Chaîne
  WriteLn(v);

  v := 3.14;            // Réel
  WriteLn(v);

  v := True;            // Booléen
  WriteLn(v);
end.
```

**Attention :** Les Variants sont pratiques mais moins performants et moins sûrs que les types statiques.

## Tableau récapitulatif des unités importantes

| Unité | Usage principal | Exemples de fonctions |
|-------|----------------|----------------------|
| **System** | Base (automatique) | WriteLn, Inc, Length |
| **SysUtils** | Utilitaires système | UpperCase, FileExists, Now, IntToStr |
| **Classes** | Collections et objets | TStringList, TList, TStream |
| **Math** | Mathématiques | Sin, Cos, Sqrt, Max, Min |
| **StrUtils** | Chaînes avancées | LeftStr, RightStr, ReverseString |
| **DateUtils** | Dates avancées | IncDay, DaysBetween, YearOf |
| **CRT** | Console texte | ClrScr, TextColor, GotoXY |
| **Types** | Types de données | TPoint, TRect, tableaux dynamiques |
| **Variants** | Types variables | Variant |

## Comment découvrir d'autres unités ?

### 1. Documentation officielle
Consultez la documentation FreePascal : https://www.freepascal.org/docs.html

### 2. Lazarus IDE
Dans Lazarus, utilisez **F1** sur un nom d'unité ou de fonction pour accéder à l'aide.

### 3. Exploration du code source
Les unités du RTL sont dans le répertoire d'installation de FreePascal, généralement :
- **Windows :** `C:\lazarus\fpc\3.x.x\source\rtl\`
- **Linux :** `/usr/share/fpcsrc/3.x.x/rtl/`

### 4. Auto-complétion
Dans Lazarus, tapez le début d'un nom et appuyez sur **Ctrl+Espace** pour voir les suggestions.

## Bonnes pratiques

### 1. N'incluez que ce dont vous avez besoin

```pascal
// ❌ Mauvais - trop d'unités
uses
  SysUtils, Classes, Math, StrUtils, DateUtils, Types, Variants;

// ✅ Bon - seulement le nécessaire
uses
  SysUtils;  // Pour UpperCase et IntToStr
```

### 2. Groupez logiquement les unités

```pascal
uses
  // Unités système
  SysUtils, Classes,

  // Unités mathématiques
  Math,

  // Unités personnelles
  MonUnite;
```

### 3. Consultez la documentation

Avant de créer votre propre fonction, vérifiez si elle n'existe pas déjà dans le RTL !

### 4. Préférez le RTL aux bibliothèques externes

Le RTL est :
- ✅ Toujours disponible
- ✅ Bien testé
- ✅ Multi-plateforme
- ✅ Documenté

## Résumé

- Le **RTL** est la bibliothèque standard de FreePascal
- **System** est incluse automatiquement
- **SysUtils** est l'unité la plus utilisée (chaînes, fichiers, dates)
- **Classes** fournit des collections (TStringList, TList)
- **Math** contient les fonctions mathématiques
- **StrUtils** et **DateUtils** complètent SysUtils
- De nombreuses autres unités existent pour des besoins spécifiques
- Consultez la documentation pour découvrir toutes les possibilités
- N'incluez que les unités dont vous avez réellement besoin

Le RTL vous évite de réinventer la roue et accélère considérablement votre développement !

Dans la prochaine section, nous verrons comment créer vos propres bibliothèques réutilisables.

⏭️ [Création de bibliothèques réutilisables](/07-unites-organisation-code/08-creation-bibliotheques-reutilisables.md)
