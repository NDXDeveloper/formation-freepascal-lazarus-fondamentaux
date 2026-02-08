🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.7 Gestion des erreurs simples

## Introduction

Un bon programme ne se contente pas de fonctionner quand tout va bien. Il doit aussi **prévoir les erreurs** et les gérer de manière appropriée. Que se passe-t-il si l'utilisateur entre une valeur invalide ? Si un calcul cause une division par zéro ? Si un fichier n'existe pas ?

La gestion des erreurs consiste à **anticiper les problèmes** et à y réagir de manière contrôlée.

**Analogie de la vie quotidienne :**
- Vérifier qu'il reste du carburant avant de partir en voyage
- S'assurer qu'une porte est déverrouillée avant d'essayer de l'ouvrir
- Confirmer qu'un aliment n'est pas périmé avant de le manger

## Pourquoi gérer les erreurs ?

### Programme sans gestion d'erreurs

```pascal
program SansGestionErreurs;
var
  a, b: Integer;
  resultat: Real;
begin
  Write('Entrez le premier nombre : ');
  ReadLn(a);
  Write('Entrez le deuxième nombre : ');
  ReadLn(b);

  resultat := a / b;  // ❌ Et si b = 0 ?
  WriteLn('Résultat : ', resultat:0:2);
end.
```

**Problèmes :**
- Division par zéro → plantage du programme
- Pas de message d'erreur clair
- Expérience utilisateur désastreuse

### Programme avec gestion d'erreurs

```pascal
program AvecGestionErreurs;
var
  a, b: Integer;
  resultat: Real;
begin
  Write('Entrez le premier nombre : ');
  ReadLn(a);
  Write('Entrez le deuxième nombre : ');
  ReadLn(b);

  // ✓ Vérification avant le calcul
  if b = 0 then
    WriteLn('ERREUR : Division par zéro impossible !')
  else
  begin
    resultat := a / b;
    WriteLn('Résultat : ', resultat:0:2);
  end;
end.
```

**Avantages :**
- Programme robuste
- Message d'erreur clair
- Pas de plantage

## Types d'erreurs courantes

### 1. Erreurs de saisie utilisateur

L'utilisateur peut entrer n'importe quoi : des valeurs hors limites, des types incorrects, etc.

### 2. Erreurs de calcul

Division par zéro, dépassement de capacité, racine carrée d'un nombre négatif, etc.

### 3. Erreurs logiques

Indices de tableau hors limites, conditions impossibles, etc.

### 4. Erreurs de ressources

Fichier introuvable, mémoire insuffisante, etc.

## Validation des entrées utilisateur

### Validation simple

```pascal
program ValidationAgeSimple;
var
  age: Integer;
begin
  Write('Entrez votre âge : ');
  ReadLn(age);

  if (age < 0) or (age > 150) then
  begin
    WriteLn('ERREUR : Âge invalide !');
    WriteLn('L''âge doit être entre 0 et 150.');
  end
  else
    WriteLn('Âge accepté : ', age, ' ans');
end.
```

### Validation avec boucle

```pascal
program ValidationAvecBoucle;
var
  age: Integer;
  valide: Boolean;
begin
  valide := False;

  WriteLn('Veuillez entrer votre âge (0-150)');

  while not valide do
  begin
    Write('Âge : ');
    ReadLn(age);

    if (age < 0) or (age > 150) then
      WriteLn('❌ Âge invalide. Réessayez.')
    else
    begin
      WriteLn('✓ Âge accepté');
      valide := True;
    end;
  end;

  WriteLn('Vous avez ', age, ' ans.');
end.
```

### Validation avec REPEAT-UNTIL

```pascal
program ValidationRepeatNombre;
var
  nombre: Integer;
begin
  WriteLn('Entrez un nombre positif :');

  repeat
    Write('Nombre : ');
    ReadLn(nombre);

    if nombre <= 0 then
      WriteLn('❌ Le nombre doit être positif !')
    else
      WriteLn('✓ Nombre accepté : ', nombre);
  until nombre > 0;
end.
```

### Validation multiple

```pascal
program ValidationMultiple;
var
  note: Real;
  erreur: String;
begin
  WriteLn('Entrez une note (0.0 à 20.0) :');

  repeat
    erreur := '';  // Pas d'erreur par défaut
    Write('Note : ');
    ReadLn(note);

    // Vérifications multiples
    if note < 0 then
      erreur := 'La note ne peut pas être négative'
    else if note > 20 then
      erreur := 'La note ne peut pas dépasser 20'
    // Trunc supprime les décimales sans arrondir ; cette astuce détecte plus d'1 décimale
    else if (note * 10) <> Trunc(note * 10) then
      erreur := 'Utilisez maximum 1 décimale (ex: 15.5)';

    if erreur <> '' then
      WriteLn('❌ ERREUR : ', erreur)
    else
      WriteLn('✓ Note valide : ', note:0:1, '/20');
  until erreur = '';
end.
```

## Vérification avant traitement

### Division sécurisée

```pascal
program DivisionSecurisee;
var
  a, b: Real;
  resultat: Real;
begin
  Write('Dividende : ');
  ReadLn(a);
  Write('Diviseur : ');
  ReadLn(b);

  if b = 0 then
  begin
    WriteLn('═══════════════════════════');
    WriteLn('   ERREUR : Division par 0');
    WriteLn('═══════════════════════════');
    WriteLn('Le diviseur ne peut pas être zéro.');
  end
  else
  begin
    resultat := a / b;
    WriteLn('Résultat : ', a:0:2, ' / ', b:0:2, ' = ', resultat:0:2);
  end;
end.
```

### Racine carrée sécurisée

```pascal
program RacineCarreeSecurisee;
var
  nombre, racine: Real;
begin
  Write('Entrez un nombre : ');
  ReadLn(nombre);

  if nombre < 0 then
  begin
    WriteLn('ERREUR : Impossible de calculer la racine carrée d''un nombre négatif.');
    WriteLn('Conseil : Utilisez un nombre positif ou nul.');
  end
  else
  begin
    racine := Sqrt(nombre);
    WriteLn('La racine carrée de ', nombre:0:2, ' est ', racine:0:4);
  end;
end.
```

### Accès tableau sécurisé

```pascal
program AccesTableauSecurise;
const
  TAILLE = 10;
var
  tableau: array[1..TAILLE] of Integer;
  i, indice: Integer;
begin
  // Remplissage
  for i := 1 to TAILLE do
    tableau[i] := i * 10;

  WriteLn('Tableau de ', TAILLE, ' éléments');
  Write('Entrez l''indice à afficher (1-', TAILLE, ') : ');
  ReadLn(indice);

  if (indice < 1) or (indice > TAILLE) then
  begin
    WriteLn('ERREUR : Indice hors limites !');
    WriteLn('Les indices valides sont de 1 à ', TAILLE);
  end
  else
    WriteLn('Valeur à l''indice ', indice, ' : ', tableau[indice]);
end.
```

## Messages d'erreur efficaces

### Mauvais messages d'erreur

```pascal
// ❌ Trop vague
WriteLn('Erreur');

// ❌ Trop technique
WriteLn('ERR_INVALID_INPUT_003');

// ❌ Pas constructif
WriteLn('Vous avez fait une erreur');
```

### Bons messages d'erreur

```pascal
// ✓ Clair et précis
WriteLn('ERREUR : Le nombre doit être entre 1 et 100');

// ✓ Constructif avec solution
WriteLn('ERREUR : Division par zéro impossible.');
WriteLn('Conseil : Utilisez un diviseur différent de zéro.');

// ✓ Contexte et indication
WriteLn('ERREUR : Âge invalide (', age, ')');
WriteLn('L''âge doit être un nombre positif entre 0 et 150.');
```

### Structure d'un bon message d'erreur

```pascal
program BonMessageErreur;
var
  temperature: Real;
begin
  Write('Température en Celsius : ');
  ReadLn(temperature);

  if temperature < -273.15 then
  begin
    WriteLn('╔════════════════════════════════════╗');
    WriteLn('║         ERREUR DÉTECTÉE            ║');
    WriteLn('╚════════════════════════════════════╝');
    WriteLn;
    WriteLn('Problème : Température physiquement impossible');
    WriteLn('Valeur entrée : ', temperature:0:2, '°C');
    WriteLn('Limite minimale : -273.15°C (zéro absolu)');
    WriteLn;
    WriteLn('Action : Veuillez entrer une température valide.');
  end
  else
    WriteLn('Température acceptée : ', temperature:0:2, '°C');
end.
```

## Valeurs de retour pour signaler les erreurs

### Utilisation de booléens

```pascal
program ValidationAvecBooleen;

function ValiderEmail(email: String): Boolean;
begin
  // Validation simplifiée
  // Pos retourne la position (1-based) ou 0 si non trouvé
  ValiderEmail := (Pos('@', email) > 0) and (Pos('.', email) > 0);
end;

var
  email: String;
begin
  Write('Entrez votre email : ');
  ReadLn(email);

  if ValiderEmail(email) then
    WriteLn('✓ Email valide')
  else
  begin
    WriteLn('✗ Email invalide');
    WriteLn('Un email doit contenir @ et un point');
  end;
end.
```

### Utilisation de codes d'erreur

```pascal
program ValidationAvecCode;

function ValiderMotDePasse(mdp: String): Integer;
var
  i: Integer;
  contientChiffre: Boolean;
begin
  // Retourne 0 si OK, sinon code d'erreur
  if Length(mdp) < 8 then
    ValiderMotDePasse := 1  // Trop court
  else
  begin
    contientChiffre := False;
    for i := 1 to Length(mdp) do
      if mdp[i] in ['0'..'9'] then  // in : test d'appartenance à un ensemble de caractères
        contientChiffre := True;

    if not contientChiffre then
      ValiderMotDePasse := 2  // Pas de chiffre
    else
      ValiderMotDePasse := 0;  // OK
  end;
end;

var
  motDePasse: String;
  codeErreur: Integer;
begin
  Write('Créez un mot de passe : ');
  ReadLn(motDePasse);

  codeErreur := ValiderMotDePasse(motDePasse);

  case codeErreur of
    0: WriteLn('✓ Mot de passe accepté');
    1: WriteLn('✗ Le mot de passe doit contenir au moins 8 caractères');
    2: WriteLn('✗ Le mot de passe doit contenir au moins un chiffre');
  else
    WriteLn('✗ Erreur inconnue');
  end;
end.
```

### Valeurs sentinelles

```pascal
program RechercheAvecSentinelle;
const
  TAILLE = 10;
  NON_TROUVE = -1;  // Valeur sentinelle
var
  tableau: array[1..TAILLE] of Integer;
  i, recherche, position: Integer;
begin
  // Remplissage
  for i := 1 to TAILLE do
    tableau[i] := i * 5;

  Write('Nombre à rechercher : ');
  ReadLn(recherche);

  // Recherche
  position := NON_TROUVE;
  for i := 1 to TAILLE do
  begin
    if tableau[i] = recherche then
    begin
      position := i;
      break;
    end;
  end;

  // Vérification du résultat
  if position = NON_TROUVE then
    WriteLn('✗ Nombre non trouvé')
  else
    WriteLn('✓ Nombre trouvé à la position ', position);
end.
```

## Stratégies de gestion d'erreurs

### 1. Prévention (le meilleur choix)

```pascal
// Empêcher l'erreur de se produire
if diviseur <> 0 then
  resultat := dividende / diviseur;
```

### 2. Détection et message

```pascal
// Détecter et informer
if diviseur = 0 then
  WriteLn('ERREUR : Division par zéro')
else
  resultat := dividende / diviseur;
```

### 3. Demande de correction

```pascal
// Demander une nouvelle valeur
repeat
  Write('Diviseur : ');
  ReadLn(diviseur);
  if diviseur = 0 then
    WriteLn('Veuillez entrer un nombre non nul');
until diviseur <> 0;
```

### 4. Valeur par défaut

```pascal
// Utiliser une valeur de secours
if diviseur = 0 then
begin
  WriteLn('ATTENTION : Division par zéro détectée');
  WriteLn('Utilisation de la valeur par défaut (1)');
  diviseur := 1;
end;
resultat := dividende / diviseur;
```

## Exemples pratiques

### Calculatrice robuste

```pascal
program CalculatriceRobuste;
var
  a, b, resultat: Real;
  operation: Char;
  erreur: Boolean;
begin
  WriteLn('═══════════════════════════');
  WriteLn('   CALCULATRICE ROBUSTE');
  WriteLn('═══════════════════════════');
  WriteLn;

  erreur := False;

  Write('Premier nombre : ');
  ReadLn(a);
  Write('Opération (+, -, *, /) : ');
  ReadLn(operation);
  Write('Deuxième nombre : ');
  ReadLn(b);
  WriteLn;

  case operation of
    '+': resultat := a + b;
    '-': resultat := a - b;
    '*': resultat := a * b;
    '/':
      begin
        if b = 0 then
        begin
          WriteLn('ERREUR : Division par zéro impossible !');
          erreur := True;
        end
        else
          resultat := a / b;
      end;
  else
    begin
      WriteLn('ERREUR : Opération inconnue !');
      WriteLn('Opérations valides : +, -, *, /');
      erreur := True;
    end;
  end;

  if not erreur then
  begin
    WriteLn('═══════════════════════════');
    WriteLn('Calcul : ', a:0:2, ' ', operation, ' ', b:0:2);
    WriteLn('Résultat : ', resultat:0:2);
    WriteLn('═══════════════════════════');
  end;
end.
```

### Système de notation

```pascal
program SystemeNotation;
var
  note: Real;
  mention: String;
  valide: Boolean;
begin
  WriteLn('═══ SYSTÈME DE NOTATION ═══');
  WriteLn;

  valide := False;

  repeat
    Write('Entrez la note (0-20) : ');
    ReadLn(note);

    // Validation
    if note < 0 then
      WriteLn('❌ La note ne peut pas être négative')
    else if note > 20 then
      WriteLn('❌ La note ne peut pas dépasser 20')
    else
      valide := True;
  until valide;

  // Détermination de la mention
  if note < 10 then
    mention := 'Insuffisant'
  else if note < 12 then
    mention := 'Passable'
  else if note < 14 then
    mention := 'Assez bien'
  else if note < 16 then
    mention := 'Bien'
  else
    mention := 'Très bien';

  WriteLn;
  WriteLn('═══════════════════════════');
  WriteLn('Note : ', note:0:1, '/20');
  WriteLn('Mention : ', mention);
  WriteLn('═══════════════════════════');
end.
```

### Gestion de stock

```pascal
program GestionStock;
const
  STOCK_INITIAL = 100;
  STOCK_MIN = 10;
var
  stock, quantite: Integer;
  operation: Char;
  continuer: Boolean;
begin
  stock := STOCK_INITIAL;
  continuer := True;

  WriteLn('═══ GESTION DE STOCK ═══');
  WriteLn('Stock initial : ', stock, ' unités');
  WriteLn;

  while continuer do
  begin
    WriteLn('Stock actuel : ', stock, ' unités');

    // Alerte stock faible
    if stock <= STOCK_MIN then
    begin
      WriteLn('⚠️  ALERTE : Stock faible !');
      WriteLn('   Recommandation : Réapprovisionner');
    end;

    WriteLn;
    WriteLn('A - Ajouter au stock');
    WriteLn('R - Retirer du stock');
    WriteLn('Q - Quitter');
    Write('Choix : ');
    ReadLn(operation);
    WriteLn;

    case UpCase(operation) of  // UpCase convertit un Char en majuscule (un seul caractère)
      'A':
        begin
          repeat
            Write('Quantité à ajouter : ');
            ReadLn(quantite);

            if quantite <= 0 then
              WriteLn('❌ La quantité doit être positive')
            else
            begin
              stock := stock + quantite;
              WriteLn('✓ ', quantite, ' unités ajoutées');
            end;
          until quantite > 0;
        end;

      'R':
        begin
          repeat
            Write('Quantité à retirer : ');
            ReadLn(quantite);

            if quantite <= 0 then
              WriteLn('❌ La quantité doit être positive')
            else if quantite > stock then
            begin
              WriteLn('❌ Stock insuffisant !');
              WriteLn('   Stock disponible : ', stock, ' unités');
            end;
          until (quantite > 0) and (quantite <= stock);

          stock := stock - quantite;
          WriteLn('✓ ', quantite, ' unités retirées');
        end;

      'Q':
        begin
          WriteLn('Stock final : ', stock, ' unités');
          continuer := False;
        end;

    else
      WriteLn('❌ Choix invalide');
    end;

    WriteLn;
  end;
end.
```

### Conversion de température

```pascal
program ConversionTemperatureRobuste;
const
  ZERO_ABSOLU = -273.15;
var
  temperature, resultat: Real;
  choix: Integer;
  erreur: Boolean;
begin
  WriteLn('═══ CONVERSION DE TEMPÉRATURE ═══');
  WriteLn;
  WriteLn('1. Celsius → Fahrenheit');
  WriteLn('2. Fahrenheit → Celsius');
  WriteLn('3. Celsius → Kelvin');
  WriteLn;

  // Validation du choix
  repeat
    Write('Votre choix (1-3) : ');
    ReadLn(choix);
    if (choix < 1) or (choix > 3) then
      WriteLn('❌ Choix invalide. Entrez 1, 2 ou 3.');
  until (choix >= 1) and (choix <= 3);

  Write('Température : ');
  ReadLn(temperature);

  erreur := False;

  case choix of
    1:  // Celsius → Fahrenheit
      begin
        if temperature < ZERO_ABSOLU then
        begin
          WriteLn('❌ ERREUR : Température impossible');
          WriteLn('   Le zéro absolu est ', ZERO_ABSOLU:0:2, '°C');
          erreur := True;
        end
        else
        begin
          resultat := (temperature * 9/5) + 32;
          WriteLn('✓ ', temperature:0:2, '°C = ', resultat:0:2, '°F');
        end;
      end;

    2:  // Fahrenheit → Celsius
      begin
        resultat := (temperature - 32) * 5/9;
        if resultat < ZERO_ABSOLU then
        begin
          WriteLn('❌ ERREUR : Température impossible');
          WriteLn('   Le zéro absolu est -459.67°F');
          erreur := True;
        end
        else
          WriteLn('✓ ', temperature:0:2, '°F = ', resultat:0:2, '°C');
      end;

    3:  // Celsius → Kelvin
      begin
        if temperature < ZERO_ABSOLU then
        begin
          WriteLn('❌ ERREUR : Température impossible');
          WriteLn('   Le zéro absolu est ', ZERO_ABSOLU:0:2, '°C');
          erreur := True;
        end
        else
        begin
          resultat := temperature + 273.15;
          WriteLn('✓ ', temperature:0:2, '°C = ', resultat:0:2, 'K');
        end;
      end;
  end;

  if erreur then
    WriteLn('⚠️  Veuillez réessayer avec une température valide.');
end.
```

### Gestion de compte bancaire

```pascal
program CompteBancaire;
var
  solde, montant: Real;
  operation: Char;
  continuer: Boolean;
const
  SOLDE_MIN = 0.0;
  DECOUVERT_MAX = 500.0;
begin
  solde := 1000.0;
  continuer := True;

  WriteLn('═══════════════════════════════');
  WriteLn('   GESTION COMPTE BANCAIRE');
  WriteLn('═══════════════════════════════');
  WriteLn;

  while continuer do
  begin
    WriteLn('Solde actuel : ', solde:0:2, ' €');

    // Avertissements
    if solde < 0 then
      WriteLn('⚠️  Compte à découvert')
    else if solde < 100 then
      WriteLn('⚠️  Solde faible');

    WriteLn;
    WriteLn('D - Déposer');
    WriteLn('R - Retirer');
    WriteLn('Q - Quitter');
    Write('Opération : ');
    ReadLn(operation);
    WriteLn;

    case UpCase(operation) of  // UpCase convertit un Char en majuscule (un seul caractère)
      'D':
        begin
          repeat
            Write('Montant à déposer : ');
            ReadLn(montant);

            if montant <= 0 then
              WriteLn('❌ Le montant doit être positif')
            else
            begin
              solde := solde + montant;
              WriteLn('✓ Dépôt effectué');
              WriteLn('  Nouveau solde : ', solde:0:2, ' €');
            end;
          until montant > 0;
        end;

      'R':
        begin
          repeat
            Write('Montant à retirer : ');
            ReadLn(montant);

            if montant <= 0 then
              WriteLn('❌ Le montant doit être positif')
            else if (solde - montant) < -DECOUVERT_MAX then
            begin
              WriteLn('❌ Opération refusée');
              WriteLn('   Découvert maximum : ', DECOUVERT_MAX:0:2, ' €');
              WriteLn('   Solde disponible : ', (solde + DECOUVERT_MAX):0:2, ' €');
            end;
          until (montant > 0) and ((solde - montant) >= -DECOUVERT_MAX);

          solde := solde - montant;
          WriteLn('✓ Retrait effectué');
          WriteLn('  Nouveau solde : ', solde:0:2, ' €');

          if solde < 0 then
            WriteLn('  ⚠️  Vous êtes maintenant à découvert');
        end;

      'Q':
        begin
          WriteLn('═══════════════════════════════');
          WriteLn('Solde final : ', solde:0:2, ' €');
          WriteLn('Merci de votre visite !');
          WriteLn('═══════════════════════════════');
          continuer := False;
        end;

    else
      WriteLn('❌ Opération invalide');
    end;

    WriteLn;
  end;
end.
```

## Liste de vérification

Avant de considérer votre programme comme terminé, vérifiez :

### ✓ Entrées utilisateur
- [ ] Toutes les entrées sont validées
- [ ] Les limites min/max sont vérifiées
- [ ] Les types de données sont corrects
- [ ] Les valeurs impossibles sont rejetées

### ✓ Calculs
- [ ] Division par zéro impossible
- [ ] Dépassements de capacité prévenus
- [ ] Opérations mathématiques valides

### ✓ Accès aux données
- [ ] Indices de tableaux vérifiés
- [ ] Pointeurs non nuls
- [ ] Fichiers existants avant lecture

### ✓ Messages
- [ ] Messages d'erreur clairs
- [ ] Suggestions de correction
- [ ] Contexte fourni

### ✓ Expérience utilisateur
- [ ] Pas de plantage possible
- [ ] Possibilité de réessayer
- [ ] Feedback approprié

## Erreurs courantes

### 1. Ne pas valider les entrées

```pascal
// ❌ Pas de validation
ReadLn(age);
resultat := 100 / age;  // Et si age = 0 ?

// ✓ Avec validation
repeat
  ReadLn(age);
  if age <= 0 then
    WriteLn('Âge invalide');
until age > 0;
```

### 2. Messages d'erreur vagues

```pascal
// ❌ Trop vague
WriteLn('Erreur');

// ✓ Précis et utile
WriteLn('ERREUR : La note doit être entre 0 et 20');
WriteLn('Vous avez entré : ', note:0:1);
```

### 3. Oublier les cas limites

```pascal
// ❌ Oublie le cas n = 0
for i := 1 to n do
  Write(i, ' ');

// ✓ Gère le cas n = 0
if n <= 0 then
  WriteLn('Aucun élément à afficher')
else
  for i := 1 to n do
    Write(i, ' ');
```

### 4. Ne pas tester les conditions

```pascal
// ❌ Assume que le fichier existe
Assign(f, 'data.txt');
Reset(f);

// ✓ Vérifie l'existence (simplifié)
if FileExists('data.txt') then
begin
  Assign(f, 'data.txt');
  Reset(f);
end
else
  WriteLn('ERREUR : Fichier introuvable');
```

### 5. Trop de validations imbriquées

```pascal
// ❌ Difficile à lire
if x > 0 then
  if y > 0 then
    if z > 0 then
      // code

// ✓ Plus clair
if (x > 0) and (y > 0) and (z > 0) then
  // code
else
  WriteLn('Toutes les valeurs doivent être positives');
```

## Bonnes pratiques

### 1. Valider tôt

```pascal
// Validez dès la saisie, pas plus tard
repeat
  Write('Âge : ');
  ReadLn(age);
until (age >= 0) and (age <= 150);
```

### 2. Messages constructifs

```pascal
// Dites ce qui ne va pas ET comment corriger
WriteLn('❌ Note invalide : ', note:0:1);
WriteLn('✓ La note doit être entre 0 et 20');
WriteLn('⚙️  Conseil : Utilisez des décimales (ex: 15.5)');
```

### 3. Utiliser des constantes

```pascal
const
  AGE_MIN = 0;
  AGE_MAX = 150;
begin
  if (age < AGE_MIN) or (age > AGE_MAX) then
    WriteLn('Âge doit être entre ', AGE_MIN, ' et ', AGE_MAX);
end;
```

### 4. Fonction de validation

```pascal
function EstAgeValide(age: Integer): Boolean;
begin
  EstAgeValide := (age >= 0) and (age <= 150);
end;

// Utilisation
if not EstAgeValide(age) then
  WriteLn('Âge invalide');
```

### 5. Documenter les erreurs possibles

```pascal
// Cette procédure peut échouer si :
// - diviseur = 0 (division par zéro)
// - résultat trop grand (dépassement)
procedure Diviser(a, b: Real);
begin
  if b = 0 then
    WriteLn('ERREUR : Division par zéro')
  else
    WriteLn('Résultat : ', a / b:0:2);
end;
```

## Résumé

La gestion des erreurs simples consiste à **prévoir et gérer** les situations problématiques :

### Principes clés
- **Valider** toutes les entrées utilisateur
- **Vérifier** les conditions avant traitement
- **Prévenir** plutôt que corriger
- **Informer** clairement l'utilisateur

### Techniques essentielles
- Boucles de validation (repeat/while)
- Vérifications conditionnelles (if)
- Messages d'erreur explicites
- Valeurs sentinelles
- Codes de retour

### Structure d'un bon message d'erreur
1. **Nature** du problème
2. **Valeur** problématique
3. **Cause** de l'erreur
4. **Solution** ou conseil

### À retenir
- Un bon programme ne plante jamais
- Les erreurs doivent être anticipées
- L'utilisateur doit comprendre le problème
- Offrez toujours une possibilité de correction

**Note importante :** Cette section couvre la gestion d'erreurs **simple** avec des techniques de base. Plus tard dans la formation, vous découvrirez la gestion d'**exceptions** (try-except), qui est une approche plus avancée et puissante pour gérer les erreurs complexes.

⏭️ [Validation des entrées utilisateur](/03-structures-controle/08-validation-entrees-utilisateur.md)
