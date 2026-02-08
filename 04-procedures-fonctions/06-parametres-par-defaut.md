🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.6 Paramètres par défaut

## Introduction

Parfois, certains paramètres d'une procédure ou fonction ont souvent la même valeur. Les **paramètres par défaut** (aussi appelés **paramètres optionnels**) permettent de définir des valeurs par défaut, rendant ces paramètres facultatifs lors de l'appel.

## Le problème sans paramètres par défaut

Imaginons une procédure pour afficher un message avec un séparateur :

```pascal
procedure AfficherMessage(texte: String; caractere: Char; longueur: Integer);
var
  i: Integer;
begin
  for i := 1 to longueur do
    Write(caractere);
  WriteLn;
  WriteLn(texte);
  for i := 1 to longueur do
    Write(caractere);
  WriteLn;
end;

begin
  AfficherMessage('Bienvenue', '=', 20);
  AfficherMessage('Menu', '=', 20);
  AfficherMessage('Titre', '=', 20);
  AfficherMessage('Section', '=', 20);
end.
```

**Problème :** On utilise toujours `'='` et `20`. On doit quand même les écrire à chaque appel !

## La solution : paramètres par défaut

En définissant des valeurs par défaut, ces paramètres deviennent optionnels :

```pascal
procedure AfficherMessage(texte: String; caractere: Char = '='; longueur: Integer = 20);
var
  i: Integer;
begin
  for i := 1 to longueur do
    Write(caractere);
  WriteLn;
  WriteLn(texte);
  for i := 1 to longueur do
    Write(caractere);
  WriteLn;
end;

begin
  AfficherMessage('Bienvenue');              // Utilise '=' et 20
  AfficherMessage('Menu', '-');              // Utilise '-' et 20
  AfficherMessage('Titre', '*', 30);         // Tout personnalisé
end.
```

**Avantage :** Code plus court et plus lisible !

## Syntaxe

### Déclaration

```pascal
procedure NomProcedure(param1: Type; param2: Type = valeurDefaut);
function NomFonction(param1: Type; param2: Type = valeurDefaut): TypeRetour;
```

### Exemples de déclarations

```pascal
// Un paramètre par défaut
procedure Saluer(nom: String; formule: String = 'Bonjour');

// Plusieurs paramètres par défaut
procedure ConfigurerAffichage(titre: String; largeur: Integer = 80; hauteur: Integer = 25);

// Fonction avec paramètre par défaut
function Puissance(base: Real; exposant: Integer = 2): Real;
```

## Règles importantes

### 1. Les paramètres par défaut doivent être à la fin

```pascal
// ❌ ERREUR : paramètre par défaut avant paramètre obligatoire
procedure Mauvais(a: Integer = 10; b: Integer);

// ✅ CORRECT : paramètres par défaut à la fin
procedure Correct(b: Integer; a: Integer = 10);
```

### 2. On ne peut omettre que les derniers paramètres

```pascal
procedure Test(a: Integer; b: Integer = 5; c: Integer = 10);

begin
  Test(1);        // ✅ OK : a=1, b=5, c=10
  Test(1, 3);     // ✅ OK : a=1, b=3, c=10
  Test(1, 3, 7);  // ✅ OK : a=1, b=3, c=7

  // Test(1, , 7);  // ❌ ERREUR : on ne peut pas "sauter" b
end;
```

### 3. Types de valeurs par défaut autorisés

On peut utiliser :
- **Constantes numériques** : `10`, `3.14`, `-5`
- **Constantes caractères** : `'A'`, `'='`
- **Constantes chaînes** : `'Bonjour'`, `''`
- **Constantes booléennes** : `True`, `False`
- **Expressions constantes simples** : `10 * 2`, `'Pré' + 'nom'`

```pascal
procedure Exemple(
  n: Integer = 100;
  c: Char = 'X';
  s: String = 'Défaut';
  b: Boolean = True;
  r: Real = 3.14
);
```

### 4. Pas de variables ou d'appels de fonction

```pascal
var
  valeur: Integer = 10;

// ❌ ERREUR : on ne peut pas utiliser une variable
procedure Mauvais(n: Integer = valeur);

// ❌ ERREUR : on ne peut pas appeler une fonction
procedure Mauvais2(n: Integer = CalculerValeur());

// ✅ CORRECT : constante uniquement
procedure Correct(n: Integer = 10);
```

## Exemples pratiques

### 1. Affichage avec formatage optionnel

```pascal
uses SysUtils;

procedure AfficherNombre(nombre: Integer; largeur: Integer = 5; remplissage: Char = ' ');
var
  texte: String;
  i: Integer;
begin
  texte := IntToStr(nombre);

  // Ajouter le remplissage à gauche
  for i := Length(texte) + 1 to largeur do
    Write(remplissage);

  WriteLn(texte);
end;

begin
  AfficherNombre(42);           // "   42"
  AfficherNombre(42, 6);        // "    42"
  AfficherNombre(42, 6, '0');   // "000042"
end.
```

### 2. Fonction de calcul avec valeur par défaut

```pascal
function Puissance(base: Real; exposant: Integer = 2): Real;
var
  i: Integer;
  resultat: Real;
begin
  resultat := 1;
  for i := 1 to exposant do
    resultat := resultat * base;
  Result := resultat;
end;

begin
  WriteLn('3² = ', Puissance(3):0:0);           // 9 (exposant = 2)
  WriteLn('3³ = ', Puissance(3, 3):0:0);        // 27
  WriteLn('2⁴ = ', Puissance(2, 4):0:0);        // 16
  WriteLn('5² = ', Puissance(5):0:0);           // 25 (exposant = 2)
end.
```

### 3. Procédure de journalisation (logging)

```pascal
procedure Log(message: String; niveau: String = 'INFO'; afficherDate: Boolean = True);
begin
  if afficherDate then
    Write('[2025-10-12] ');

  Write('[', niveau, '] ');
  WriteLn(message);
end;

begin
  Log('Application démarrée');                    // [2025-10-12] [INFO] Application démarrée
  Log('Attention : mémoire faible', 'WARN');      // [2025-10-12] [WARN] Attention : mémoire faible
  Log('Erreur critique', 'ERROR', False);         // [ERROR] Erreur critique
end.
```

### 4. Fonction de recherche dans du texte

```pascal
uses SysUtils;

function Rechercher(const texte: String; const motif: String;
                   sensibleCasse: Boolean = False): Integer;
var
  texteRecherche, motifRecherche: String;
begin
  if sensibleCasse then
  begin
    texteRecherche := texte;
    motifRecherche := motif;
  end
  else
  begin
    texteRecherche := LowerCase(texte);
    motifRecherche := LowerCase(motif);
  end;

  Result := Pos(motifRecherche, texteRecherche);
end;

var
  position: Integer;
begin
  position := Rechercher('Bonjour le Monde', 'monde');
  // Trouve "Monde" même avec casse différente (défaut = False)

  position := Rechercher('Bonjour le Monde', 'monde', True);
  // Ne trouve pas (casse différente)
end.
```

### 5. Création de séparateurs visuels

```pascal
procedure AfficherSeparateur(longueur: Integer = 40;
                            caractere: Char = '-';
                            retourLigne: Boolean = True);
var
  i: Integer;
begin
  for i := 1 to longueur do
    Write(caractere);

  if retourLigne then
    WriteLn;
end;

begin
  WriteLn('Début du rapport');
  AfficherSeparateur;                    // ----------------------------------------

  WriteLn('Section importante');
  AfficherSeparateur(40, '=');          // ========================================

  Write('Progression : ');
  AfficherSeparateur(20, '#', False);   // ####################
  WriteLn(' 100%');
end.
```

## Combinaison avec d'autres types de paramètres

On peut combiner paramètres normaux, const, var et par défaut :

```pascal
uses SysUtils;

procedure TraiterDonnees(
  const source: String;           // const, obligatoire
  var resultat: String;           // var, obligatoire
  prefixe: String = '[DATA] ';    // par défaut
  majuscules: Boolean = False     // par défaut
);
begin
  if majuscules then
    resultat := prefixe + UpperCase(source)
  else
    resultat := prefixe + source;
end;

var
  sortie: String;
begin
  TraiterDonnees('test', sortie);
  WriteLn(sortie);  // [DATA] test

  TraiterDonnees('test', sortie, '>>> ', True);
  WriteLn(sortie);  // >>> TEST
end.
```

## Cas d'usage typiques

### 1. Configuration avec valeurs courantes

```pascal
procedure CreerFichier(nomFichier: String;
                      extension: String = '.txt';
                      encodage: String = 'UTF-8');
begin
  WriteLn('Création : ', nomFichier, extension);
  WriteLn('Encodage : ', encodage);
end;

begin
  CreerFichier('document');                  // .txt, UTF-8
  CreerFichier('image', '.jpg');             // .jpg, UTF-8
  CreerFichier('data', '.csv', 'ASCII');     // .csv, ASCII
end.
```

### 2. Options d'affichage

```pascal
procedure AfficherTableau(const titre: String;
                         largeur: Integer = 50;
                         centrer: Boolean = True;
                         bordure: Boolean = True);
var
  espaces, i: Integer;
begin
  if bordure then
  begin
    for i := 1 to largeur do
      Write('=');
    WriteLn;
  end;

  if centrer then
  begin
    espaces := (largeur - Length(titre)) div 2;
    for i := 1 to espaces do
      Write(' ');
  end;

  WriteLn(titre);

  if bordure then
  begin
    for i := 1 to largeur do
      Write('=');
    WriteLn;
  end;
end;

begin
  AfficherTableau('RAPPORT MENSUEL');
  AfficherTableau('Note simple', 30, False, False);
end.
```

### 3. Fonctions mathématiques

```pascal
uses Math;

function Arrondir(valeur: Real; decimales: Integer = 2): Real;
var
  multiplicateur: Real;
begin
  multiplicateur := Power(10, decimales);
  Result := Round(valeur * multiplicateur) / multiplicateur;
end;

begin
  WriteLn(Arrondir(3.14159):0:2);        // 3.14
  WriteLn(Arrondir(3.14159, 3):0:3);     // 3.142
  WriteLn(Arrondir(3.14159, 0):0:0);     // 3
end.
```

## Exemple complet : système de notification

```pascal
{$mode objfpc}{$H+}
program SystemeNotification;

type
  TNiveauUrgence = (Basse, Normale, Haute, Critique);  // Type énuméré : liste de valeurs nommées

procedure EnvoyerNotification(
  const destinataire: String;
  const message: String;
  urgence: TNiveauUrgence = Normale;
  afficherTimestamp: Boolean = True;
  sonore: Boolean = False
);
var
  urgenceTexte: String;
begin
  // Déterminer le texte d'urgence
  case urgence of
    Basse:     urgenceTexte := 'INFO';
    Normale:   urgenceTexte := 'NORMAL';
    Haute:     urgenceTexte := 'IMPORTANT';
    Critique:  urgenceTexte := 'CRITIQUE';
  end;

  // Afficher timestamp si demandé
  if afficherTimestamp then
    Write('[2025-10-12 14:30] ');

  // Afficher la notification
  WriteLn('À: ', destinataire);
  WriteLn('Niveau: ', urgenceTexte);
  WriteLn('Message: ', message);

  // Son si demandé
  if sonore then
    WriteLn('*BEEP*');

  WriteLn('---');
end;

begin
  // Notification simple
  EnvoyerNotification('admin@example.com', 'Système démarré');

  // Notification importante
  EnvoyerNotification('admin@example.com', 'Mise à jour disponible', Haute);

  // Notification critique avec son
  EnvoyerNotification('admin@example.com', 'Erreur système !', Critique, True, True);

  // Sans timestamp
  EnvoyerNotification('user@example.com', 'Bienvenue', Basse, False);
end.
```

## Avantages des paramètres par défaut

1. **Code plus court** : pas besoin de répéter les valeurs courantes
2. **Flexibilité** : on peut personnaliser quand nécessaire
3. **Évolution facile** : ajouter des options sans casser le code existant
4. **Lisibilité** : les valeurs par défaut documentent l'usage courant
5. **Moins d'erreurs** : moins de paramètres à se rappeler

## Inconvénients et précautions

1. **Complexité cachée** : trop de paramètres par défaut peut rendre le code difficile à comprendre
2. **Maintenance** : changer une valeur par défaut affecte tous les appels
3. **Ordre des paramètres** : on ne peut pas "sauter" un paramètre optionnel
4. **Compatibilité** : attention lors de la modification de signatures existantes

## Erreurs courantes à éviter

### 1. Paramètre par défaut avant obligatoire

```pascal
// ❌ ERREUR
procedure Mauvais(a: Integer = 5; b: Integer);

// ✅ CORRECT
procedure Correct(b: Integer; a: Integer = 5);
```

### 2. Utiliser une variable comme valeur par défaut

```pascal
const
  VALEUR_DEFAUT = 10;

var
  maVariable: Integer;

// ❌ ERREUR : variable
procedure Mauvais(n: Integer = maVariable);

// ✅ CORRECT : constante
procedure Correct(n: Integer = VALEUR_DEFAUT);
```

### 3. Trop de paramètres par défaut

```pascal
// ⚠️ Mauvaise pratique : trop complexe
procedure Configurer(a: Integer = 1; b: Integer = 2; c: Integer = 3;
                    d: Integer = 4; e: Integer = 5; f: Integer = 6);

// ✅ Mieux : utiliser un enregistrement ou séparer en plusieurs procédures
```

### 4. Confusion dans l'ordre des paramètres

```pascal
procedure Test(nom: String; age: Integer = 18; ville: String = 'Paris');

begin
  // Si on veut changer ville mais garder age par défaut ?
  Test('Jean', , 'Lyon');  // ❌ ERREUR : impossible de "sauter" age
  Test('Jean', 18, 'Lyon'); // ✅ SOLUTION : répéter la valeur par défaut
end;
```

## Bonnes pratiques

1. **Valeurs sensées** : choisissez des valeurs par défaut qui correspondent aux cas d'usage les plus fréquents
2. **Pas trop nombreux** : limitez le nombre de paramètres par défaut (3-4 maximum)
3. **Documentation** : commentez pourquoi vous avez choisi ces valeurs par défaut
4. **Cohérence** : utilisez les mêmes conventions dans tout votre code
5. **Ordre logique** : placez les paramètres les plus souvent personnalisés en premier

```pascal
// ✅ Bon exemple
procedure AfficherTexte(
  const texte: String;           // Toujours nécessaire
  centrer: Boolean = False;      // Option fréquente
  couleur: Integer = 15          // Option rare
);
```

## Points clés à retenir

1. Les paramètres par défaut rendent certains paramètres **optionnels**
2. Syntaxe : `nomParam: Type = valeurDefaut`
3. Les paramètres par défaut doivent être **à la fin** de la liste
4. On peut omettre les derniers paramètres, **pas les intermédiaires**
5. Seules les **constantes** peuvent être valeurs par défaut
6. Avantages : code plus court, plus flexible, plus lisible
7. Attention à ne pas **abuser** : trop de paramètres par défaut = complexité
8. Utile pour : options de configuration, paramètres rarement modifiés

---

**Prochaine étape :** Dans la section 4.7, nous découvrirons la **surcharge de procédures/fonctions** qui permet de créer plusieurs versions d'une même procédure/fonction avec différents paramètres.

⏭️ [Surcharge de procédures/fonctions](/04-procedures-fonctions/07-surcharge-procedures-fonctions.md)
