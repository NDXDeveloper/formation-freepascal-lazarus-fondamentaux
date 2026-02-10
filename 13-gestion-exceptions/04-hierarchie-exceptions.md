🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.4 Hiérarchie des exceptions

## Introduction

En programmation orientée objet, les classes peuvent hériter d'autres classes, formant ainsi une **hiérarchie**. Les exceptions en FreePascal suivent ce même principe : toutes les exceptions descendent d'une classe mère commune. Comprendre cette hiérarchie est essentiel pour bien gérer les erreurs dans vos programmes.

## Qu'est-ce qu'une hiérarchie de classes ?

Avant de parler des exceptions, faisons une analogie simple avec le monde animal :

```
Être vivant
  │
  ├─► Animal
  │     │
  │     ├─► Mammifère
  │     │     │
  │     │     ├─► Chien
  │     │     ├─► Chat
  │     │     └─► Humain
  │     │
  │     └─► Oiseau
  │           │
  │           ├─► Aigle
  │           └─► Moineau
  │
  └─► Végétal
```

Dans cette hiérarchie :
- Un **Chien** est un **Mammifère**
- Un **Mammifère** est un **Animal**
- Un **Animal** est un **Être vivant**

Chaque niveau hérite des caractéristiques du niveau supérieur et ajoute ses propres spécificités.

## La hiérarchie des exceptions en FreePascal

Les exceptions suivent exactement le même principe :

```
TObject (classe de base de tout objet)
  │
  └─► Exception (classe de base de toutes les exceptions)
        │
        ├─► EIntError (erreurs d'entier)
        │     │
        │     └─► EDivByZero (division entière par zéro)
        │
        ├─► EMathError (erreurs mathématiques flottantes)
        │     │
        │     ├─► EZeroDivide (division flottante par zéro)
        │     ├─► EOverflow (dépassement de capacité)
        │     └─► EInvalidOp (opération invalide)
        │
        ├─► EConvertError (erreurs de conversion)
        │
        ├─► EInOutError (erreurs d'entrée/sortie fichier)
        │
        ├─► EStreamError (erreurs de flux - unit Classes)
        │     │
        │     └─► EFOpenError (erreur d'ouverture de fichier)
        │
        └─► EAccessViolation (accès mémoire invalide)
```

## La classe Exception : la mère de toutes les exceptions

La classe `Exception` est la classe de base dont héritent toutes les autres exceptions. Elle contient les fonctionnalités communes à toutes les erreurs :

### Propriétés principales

```pascal
Exception
  │
  ├─► Message: String      // Le message d'erreur
  ├─► HelpContext: Integer // Contexte d'aide (rarement utilisé)
  └─► ClassName: String    // Le nom de la classe
```

### Exemple d'utilisation

```pascal
try
  // Code à risque
  StrToInt('abc');
except
  on E: Exception do
  begin
    WriteLn('Classe : ', E.ClassName);    // Affiche : EConvertError
    WriteLn('Message : ', E.Message);      // Affiche : 'abc' is not a valid integer
  end;
end;
```

## Principe fondamental : capturer du plus spécifique au plus général

Quand vous capturez des exceptions, l'ordre est **crucial**. FreePascal teste les classes d'exceptions dans l'ordre où vous les écrivez.

### La règle d'or

**Toujours capturer les exceptions spécifiques AVANT les exceptions générales.**

### Exemple correct

```pascal
try
  resultat := numerateur div denominateur;
except
  on E: EDivByZero do
    WriteLn('Erreur : division par zéro');
  on E: EMathError do
    WriteLn('Erreur mathématique générale');
  on E: Exception do
    WriteLn('Erreur générale');
end;
```

### Exemple incorrect

```pascal
try
  resultat := numerateur div denominateur;
except
  on E: Exception do
    WriteLn('Erreur générale');  // ⚠️ Capture TOUT !
  on E: EDivByZero do
    WriteLn('Division par zéro');  // ⚠️ Ce code ne sera JAMAIS exécuté !
end;
```

**Pourquoi ?** Parce que `EDivByZero` hérite de `Exception`. Donc quand vous capturez `Exception` en premier, toutes les exceptions sont capturées, y compris `EDivByZero`.

## Comprendre l'héritage des exceptions

### Relation "est un"

Grâce à l'héritage :
- `EDivByZero` **est une** `EMathError`
- `EMathError` **est une** `Exception`
- Donc `EDivByZero` **est une** `Exception`

### Conséquence pratique

```pascal
try
  x := 10 div 0;  // Lève EDivByZero
except
  on E: EMathError do
    WriteLn('Capturé comme EMathError');  // ✓ Fonctionne !
end;

try
  x := 10 div 0;  // Lève EDivByZero
except
  on E: Exception do
    WriteLn('Capturé comme Exception');  // ✓ Fonctionne aussi !
end;

try
  x := 10 div 0;  // Lève EDivByZero
except
  on E: EConvertError do
    WriteLn('Capturé comme EConvertError');  // ✗ Ne fonctionne PAS !
end;
```

## Les principales branches de la hiérarchie

### 1. Erreurs mathématiques (EMathError)

Toutes les erreurs liées aux calculs mathématiques :

```pascal
Exception
  ├─► EIntError              // Erreurs d'entier
  │     └─► EDivByZero        // Division entière par zéro (10 div 0)
  │
  └─► EMathError             // Erreurs mathématiques flottantes
        ├─► EZeroDivide        // Division flottante par zéro (10.0 / 0.0)
        ├─► EOverflow         // Nombre trop grand
        ├─► EUnderflow        // Nombre trop petit
        └─► EInvalidOp        // Opération invalide
```

**Note :** `EDivByZero` (sous `EIntError`) concerne la division entière (`div`), tandis que `EZeroDivide` (sous `EMathError`) concerne la division flottante (`/`).

**Exemple d'utilisation :**

```pascal
try
  resultat := CalculComplexe(a, b, c);
except
  on E: EDivByZero do
    WriteLn('Division par zéro dans le calcul');
  on E: EOverflow do
    WriteLn('Le résultat est trop grand');
  on E: EMathError do
    WriteLn('Erreur mathématique non spécifiée');
end;
```

### 2. Erreurs de conversion (EConvertError)

Erreurs lors de la conversion entre types de données :

```pascal
Exception
  └─► EConvertError
```

**Exemple :**

```pascal
try
  nombre := StrToInt(saisie);
  date := StrToDate(dateTexte);
except
  on E: EConvertError do
    WriteLn('Conversion impossible : ', E.Message);
end;
```

### 3. Erreurs d'entrée/sortie (EInOutError)

Erreurs liées aux opérations de lecture/écriture :

```pascal
Exception
  ├─► EInOutError            // Erreurs I/O Pascal (AssignFile, Reset...)
  │
  └─► EStreamError           // Erreurs de flux (unit Classes)
        ├─► EFOpenError        // Impossible d'ouvrir un fichier
        ├─► EFCreateError      // Impossible de créer un fichier
        ├─► EReadError         // Erreur de lecture
        └─► EWriteError        // Erreur d'écriture
```

**Note :** `EInOutError` et `EStreamError` sont deux branches séparées sous `Exception`. `EInOutError` est levée par les routines Pascal classiques (`AssignFile`, `Reset`...), tandis que `EStreamError` et ses descendants sont levés par les classes de flux (`TFileStream`, `TStringList.LoadFromFile`...).

**Exemple :**

```pascal
try
  AssignFile(f, 'donnees.txt');
  Reset(f);
except
  on E: EInOutError do
    WriteLn('Erreur d''accès au fichier : ', E.Message);
end;
```

### 4. Erreurs de limites (ERangeError)

Erreurs quand on accède à un index invalide :

```pascal
Exception
  └─► ERangeError
```

**Exemple :**

```pascal
var
  tableau: array[1..10] of Integer;
  i: Integer;
begin
  try
    i := 15;
    tableau[i] := 100;  // Lève ERangeError
  except
    on E: ERangeError do
      WriteLn('Index hors limites : ', i);
  end;
end;
```

### 5. Erreurs mémoire (EAccessViolation)

Erreurs lors de l'accès à la mémoire :

```pascal
Exception
  └─► EAccessViolation
```

Ces erreurs sont graves et indiquent souvent un bug dans votre programme (pointeur invalide, objet déjà libéré, etc.).

## Hiérarchie complète (version détaillée)

Voici une représentation plus complète de la hiérarchie :

```
Exception
│
├─► EAbort                    // Opération annulée (silencieuse)
│
├─► EHeapException           // Problèmes de tas mémoire
│     ├─► EOutOfMemory         // Mémoire insuffisante
│     └─► EInvalidPointer      // Pointeur invalide
│
├─► EExternal                // Exceptions matérielles/système
│     │
│     ├─► EIntError            // Erreurs d'entier
│     │     ├─► EDivByZero       // Division entière par zéro
│     │     ├─► ERangeError      // Index hors limites
│     │     └─► EIntOverflow     // Dépassement d'entier
│     │
│     ├─► EMathError           // Erreurs mathématiques flottantes
│     │     ├─► EInvalidOp       // Opération invalide
│     │     ├─► EZeroDivide      // Division flottante par zéro
│     │     ├─► EOverflow        // Dépassement flottant
│     │     └─► EUnderflow       // Sous-capacité flottante
│     │
│     ├─► EAccessViolation     // Violation d'accès mémoire
│     ├─► EPrivilege           // Instruction privilégiée
│     ├─► EControlC            // Ctrl+C pressé
│     ├─► EStackOverflow       // Pile débordée
│     └─► EExternalException   // Exception externe (OS)
│
├─► EConvertError            // Erreurs de conversion
│
├─► EInOutError              // Erreurs I/O Pascal classique
│
├─► EStreamError             // Erreurs de flux (unit Classes)
│     ├─► EFCreateError        // Création de fichier impossible
│     ├─► EFOpenError          // Ouverture de fichier impossible
│     ├─► EReadError           // Erreur de lecture
│     └─► EWriteError          // Erreur d'écriture
│
├─► EVariantError            // Erreurs de variant
│
└─► EAssertionFailed         // Assertion échouée
```

## Choisir la bonne classe lors de la capture

### Stratégie 1 : Capture spécifique

Utilisez cette stratégie quand vous voulez réagir différemment selon le type d'erreur :

```pascal
try
  nombre := StrToInt(saisie);
  resultat := 100 div nombre;
  liste[resultat] := valeur;
except
  on E: EConvertError do
    WriteLn('La saisie n''est pas un nombre valide');
  on E: EDivByZero do
    WriteLn('Impossible de diviser par zéro');
  on E: ERangeError do
    WriteLn('Le résultat dépasse les limites du tableau');
end;
```

### Stratégie 2 : Capture par catégorie

Utilisez cette stratégie quand plusieurs erreurs d'une même famille peuvent être gérées de la même façon :

```pascal
try
  // Opérations mathématiques complexes
  resultat := CalculerFormule(a, b, c, d);
except
  on E: EMathError do
    WriteLn('Erreur de calcul : ', E.Message);
end;
```

### Stratégie 3 : Capture générale avec clause else

Utilisez cette stratégie pour combiner captures spécifiques et générale :

```pascal
try
  TraiterDonnees;
except
  on E: EConvertError do
    WriteLn('Données invalides');
  on E: EDivByZero do
    WriteLn('Division par zéro');
  else
    WriteLn('Erreur inattendue');  // Toutes les autres exceptions
end;
```

## Exemple complet : traitement de fichier avec gestion fine

```pascal
procedure TraiterFichierAvecGestionFine(const nomFichier: String);  
var  
  f: TextFile;
  ligne: String;
  nombre: Integer;
begin
  try
    // Tentative d'ouverture
    AssignFile(f, nomFichier);
    Reset(f);

    try
      // Lecture et traitement
      while not EOF(f) do
      begin
        ReadLn(f, ligne);
        nombre := StrToInt(ligne);
        WriteLn('Nombre lu : ', nombre);
      end;
    finally
      CloseFile(f);
    end;

  except
    // Gestion des erreurs spécifiques d'I/O
    on E: EFOpenError do
      WriteLn('Impossible d''ouvrir le fichier "', nomFichier, '" (introuvable ou droits insuffisants)');

    on E: EInOutError do
      WriteLn('Erreur de lecture du fichier : ', E.Message);

    // Gestion des erreurs de conversion
    on E: EConvertError do
      WriteLn('Le fichier contient des données non numériques');

    // Toutes les autres erreurs
    on E: Exception do
      WriteLn('Erreur inattendue : ', E.ClassName, ' - ', E.Message);
  end;
end;
```

## Tester le type d'exception dynamiquement

Parfois, vous voulez vérifier le type d'une exception sans utiliser plusieurs blocs `on` :

### Avec l'opérateur IS

```pascal
try
  // Code à risque
except
  on E: Exception do
  begin
    if E is EDivByZero then
      WriteLn('Division par zéro')
    else if E is EMathError then
      WriteLn('Erreur mathématique')
    else
      WriteLn('Autre erreur');
  end;
end;
```

**Note :** Cette approche est moins élégante que les multiples blocs `on`, mais peut être utile dans certains cas.

## Bonnes pratiques

### 1. Ne capturez que ce que vous pouvez gérer

```pascal
// ✗ MAUVAIS : capture tout sans discrimination
try
  TraiterDonnees;
except
  on E: Exception do
    WriteLn('Erreur');  // Trop vague !
end;

// ✓ BON : capture seulement les erreurs gérables
try
  TraiterDonnees;
except
  on E: EConvertError do
    WriteLn('Données invalides, veuillez vérifier le format');
  on E: EInOutError do
    WriteLn('Problème d''accès aux fichiers');
  // Laisser les autres erreurs se propager
end;
```

### 2. Respectez l'ordre spécifique → général

```pascal
// ✓ CORRECT
try
  // ...
except
  on E: EDivByZero do      // Plus spécifique
    HandleDivByZero;
  on E: EMathError do       // Moins spécifique
    HandleMathError;
  on E: Exception do        // Plus général
    HandleGenericError;
end;
```

### 3. Documentez vos exceptions

Quand vous créez des fonctions, documentez les exceptions qu'elles peuvent lever :

```pascal
/// Convertit une chaîne en entier
/// @param texte La chaîne à convertir
/// @returns L'entier correspondant
/// @raises EConvertError si la conversion est impossible
function ConvertirTexte(const texte: String): Integer;  
begin  
  Result := StrToInt(texte);  // Peut lever EConvertError
end;
```

## Visualisation : arbre de décision

```
Une exception est levée
        │
        ▼
    try-except ?
        │
        ├─► Non ──► Exception remonte au niveau supérieur
        │
        └─► Oui
              │
              ▼
          on E: EDivByZero ?
              │
              ├─► Oui ──► Gérer EDivByZero ──► Fin
              │
              └─► Non
                    │
                    ▼
                on E: EMathError ?
                    │
                    ├─► Oui ──► Gérer EMathError ──► Fin
                    │
                    └─► Non
                          │
                          ▼
                      on E: Exception ?
                          │
                          ├─► Oui ──► Gérer Exception ──► Fin
                          │
                          └─► Non ──► Exception remonte
```

## Conclusion

La hiérarchie des exceptions est un outil puissant qui permet :

- Une gestion fine et précise des erreurs
- Un code plus lisible et maintenable
- Une organisation logique des différents types d'erreurs
- Une réutilisation du code de gestion d'erreurs

En comprenant bien cette hiérarchie, vous pouvez :
- Choisir le bon niveau de capture (spécifique ou général)
- Organiser vos blocs `except` dans le bon ordre
- Créer vos propres classes d'exceptions qui s'intègrent naturellement

---

**Points clés à retenir :**

- Toutes les exceptions héritent de la classe `Exception`
- L'héritage crée une relation "est un" (EDivByZero **est une** EMathError)
- Toujours capturer du plus spécifique au plus général
- Ne capturez que les exceptions que vous pouvez réellement gérer
- La hiérarchie permet une gestion flexible et précise des erreurs
- Les principales familles : EMathError, EConvertError, EInOutError, ERangeError
- Utilisez `E.ClassName` pour identifier le type exact d'une exception

⏭️ [Exceptions personnalisées](/13-gestion-exceptions/05-exceptions-personnalisees.md)
