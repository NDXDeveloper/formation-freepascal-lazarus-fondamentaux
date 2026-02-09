🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 5.9 Types intervalle

## Qu'est-ce qu'un type intervalle ?

Un type intervalle (ou **subrange** en anglais) est un type qui représente un **sous-ensemble contigu** d'un type ordinal existant. C'est comme dire "je ne veux que les nombres entre 1 et 10" ou "seulement les jours de lundi à vendredi".

### Analogie simple

Imaginez un thermomètre médical. Il ne mesure pas toutes les températures possibles (-273°C à des milliers de degrés), mais seulement une plage utile : disons de 35°C à 42°C. C'est exactement ce que fait un type intervalle : il restreint les valeurs possibles à une plage définie.

## Pourquoi utiliser des types intervalles ?

### Sans type intervalle

```pascal
var
  agePersonne: Integer;
  noteExamen: Integer;
  jourMois: Integer;
begin
  agePersonne := -5;      // ✗ Valeur absurde mais acceptée
  noteExamen := 150;      // ✗ Note impossible mais acceptée
  jourMois := 45;         // ✗ Jour inexistant mais accepté
end.
```

**Problèmes :**
- Aucune vérification des valeurs
- Erreurs possibles difficiles à détecter
- Pas de documentation sur les valeurs attendues

### Avec type intervalle

```pascal
type
  TAge = 0..120;
  TNote = 0..20;
  TJourMois = 1..31;

var
  agePersonne: TAge;
  noteExamen: TNote;
  jourMois: TJourMois;
begin
  agePersonne := 25;      // ✓ OK
  noteExamen := 15;       // ✓ OK
  jourMois := 15;         // ✓ OK

  // agePersonne := -5;   // ✗ Erreur détectée par le compilateur
  // noteExamen := 150;   // ✗ Erreur détectée
end.
```

**Avantages :**
- **Sécurité** : le compilateur vérifie les valeurs
- **Documentation** : le code exprime les contraintes
- **Détection précoce** des erreurs
- **Lisibilité** améliorée

## Déclaration d'un type intervalle

### Syntaxe générale

```pascal
type
  NomType = ValeurMin..ValeurMax;
```

Les valeurs min et max doivent être de même type ordinal (Integer, Char, énuméré, etc.).

### Exemples de déclarations

```pascal
type
  // Intervalles d'entiers
  TAge = 0..120;
  TNote = 0..20;
  TJourMois = 1..31;
  TMois = 1..12;
  TPourcentage = 0..100;
  TChiffre = 0..9;

  // Intervalles de caractères
  TChiffreChar = '0'..'9';
  TLettreMajuscule = 'A'..'Z';
  TLettreMinuscule = 'a'..'z';

  // Intervalles basés sur des énumérés
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJourOuvre = Lundi..Vendredi;
  TWeekend = Samedi..Dimanche;
```

**Règles importantes :**
- La valeur minimale doit être **inférieure ou égale** à la valeur maximale
- Les deux bornes doivent être du **même type de base**
- Le type de base doit être **ordinal** (pas de Real)

## Utilisation des types intervalles

### Déclaration de variables

```pascal
program ExempleIntervalle;
type
  TAge = 0..120;
  TNote = 0..20;

var
  age: TAge;
  note: TNote;
begin
  // Affectations valides
  age := 25;
  note := 15;

  WriteLn('Age : ', age);
  WriteLn('Note : ', note);

  // Ces affectations généreraient des erreurs à l'exécution
  // age := 200;   // Erreur : hors de la plage 0..120
  // note := 50;   // Erreur : hors de la plage 0..20
end.
```

### Intervalles dans les tableaux

Les types intervalles sont très utiles comme indices de tableaux :

```pascal
program TableauIntervalle;
type
  TJourMois = 1..31;
  TMois = 1..12;

var
  temperaturesJour: array[TJourMois] of Real;
  preciptationsMois: array[TMois] of Real;
  jour: TJourMois;
  mois: TMois;
begin
  // Saisie des températures du mois
  for jour := 1 to 31 do
  begin
    Write('Température du jour ', jour, ' : ');
    ReadLn(temperaturesJour[jour]);
  end;

  // Les indices sont automatiquement restreints
  // temperaturesJour[35] := 20;  // Erreur à la compilation
end.
```

### Intervalles dans les paramètres

```pascal
type
  TNote = 0..20;
  TMention = (Insuffisant, Passable, AssezBien, Bien, TresBien);

function CalculerMention(note: TNote): TMention;
begin
  if note < 10 then
    CalculerMention := Insuffisant
  else if note < 12 then
    CalculerMention := Passable
  else if note < 14 then
    CalculerMention := AssezBien
  else if note < 16 then
    CalculerMention := Bien
  else
    CalculerMention := TresBien;
end;

var
  note: TNote;
  mention: TMention;
begin
  Write('Entrez une note (0-20) : ');
  ReadLn(note);  // Si l'utilisateur entre 25, erreur à l'exécution

  mention := CalculerMention(note);
end.
```

## Intervalles basés sur des types énumérés

C'est une utilisation très puissante des intervalles :

```pascal
program IntervalleEnumere;
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJourOuvre = Lundi..Vendredi;
  TWeekend = Samedi..Dimanche;

var
  jourTravail: TJourOuvre;
  jourRepos: TWeekend;
begin
  // Valeurs autorisées pour jourTravail : Lundi à Vendredi uniquement
  jourTravail := Mardi;     // ✓ OK
  jourTravail := Vendredi;  // ✓ OK
  // jourTravail := Samedi; // ✗ Erreur : Samedi n'est pas dans Lundi..Vendredi

  // Valeurs autorisées pour jourRepos : Samedi ou Dimanche uniquement
  jourRepos := Samedi;      // ✓ OK
  jourRepos := Dimanche;    // ✓ OK
  // jourRepos := Lundi;    // ✗ Erreur : Lundi n'est pas dans Samedi..Dimanche
end.
```

## Intervalles de caractères

Très utiles pour valider les entrées :

```pascal
{$mode objfpc}{$H+}
{$R+}  // Active la vérification des intervalles à l'exécution
program IntervalleCaracteres;
type
  TChiffre = '0'..'9';
  TLettreMajuscule = 'A'..'Z';
  TLettreMinuscule = 'a'..'z';

function EstChiffre(c: Char): Boolean;
var
  chiffre: TChiffre;
begin
  try
    chiffre := c;
    EstChiffre := True;
  except
    EstChiffre := False;
  end;
end;

function EstLettre(c: Char): Boolean;
begin
  EstLettre := ((c >= 'A') and (c <= 'Z')) or
               ((c >= 'a') and (c <= 'z'));
end;

var
  caractere: Char;
begin
  Write('Entrez un caractère : ');
  ReadLn(caractere);

  if EstChiffre(caractere) then
    WriteLn('C''est un chiffre')
  else if EstLettre(caractere) then
    WriteLn('C''est une lettre')
  else
    WriteLn('C''est un caractère spécial');
end.
```

## Exemples pratiques

### Exemple 1 : Gestion de dates

```pascal
program GestionDates;
type
  TJour = 1..31;
  TMois = 1..12;
  TAnnee = 1900..2100;

  TDate = record
    jour: TJour;
    mois: TMois;
    annee: TAnnee;
  end;

function DateValide(d: TDate): Boolean;
const
  JoursParMois: array[1..12] of Integer =
    (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
var
  joursMax: Integer;
begin
  joursMax := JoursParMois[d.mois];

  // Année bissextile : février a 29 jours
  if (d.mois = 2) and ((d.annee mod 4 = 0) and
     ((d.annee mod 100 <> 0) or (d.annee mod 400 = 0))) then
    joursMax := 29;

  DateValide := d.jour <= joursMax;
end;

procedure AfficherDate(d: TDate);
const
  NomsMois: array[1..12] of String =
    ('Janvier', 'Février', 'Mars', 'Avril', 'Mai', 'Juin',
     'Juillet', 'Août', 'Septembre', 'Octobre', 'Novembre', 'Décembre');
begin
  WriteLn(d.jour, ' ', NomsMois[d.mois], ' ', d.annee);
end;

var
  date: TDate;
begin
  WriteLn('Entrez une date :');
  Write('  Jour (1-31) : ');
  ReadLn(date.jour);
  Write('  Mois (1-12) : ');
  ReadLn(date.mois);
  Write('  Année (1900-2100) : ');
  ReadLn(date.annee);

  if DateValide(date) then
  begin
    Write('Date valide : ');
    AfficherDate(date);
  end
  else
    WriteLn('Date invalide !');
end.
```

### Exemple 2 : Système de notation

```pascal
program SystemeNotation;
type
  TNoteSur20 = 0..20;
  TNoteSur100 = 0..100;
  TPourcentage = 0..100;

function ConvertirNote20Vers100(note: TNoteSur20): TNoteSur100;
begin
  ConvertirNote20Vers100 := note * 5;
end;

function ConvertirNote100Vers20(note: TNoteSur100): TNoteSur20;
begin
  ConvertirNote100Vers20 := note div 5;
end;

function CalculerPourcentage(note, total: Integer): TPourcentage;
begin
  if total > 0 then
    CalculerPourcentage := Round((note * 100) / total)
  else
    CalculerPourcentage := 0;
end;

var
  noteSur20: TNoteSur20;
  noteSur100: TNoteSur100;
  pourcentage: TPourcentage;
begin
  Write('Entrez une note sur 20 : ');
  ReadLn(noteSur20);

  noteSur100 := ConvertirNote20Vers100(noteSur20);
  WriteLn('Note sur 100 : ', noteSur100);

  pourcentage := CalculerPourcentage(noteSur20, 20);
  WriteLn('Pourcentage : ', pourcentage, '%');
end.
```

### Exemple 3 : Gestion d'heures

```pascal
{$mode objfpc}{$H+}
program GestionHeures;
type
  THeure = 0..23;
  TMinute = 0..59;
  TSeconde = 0..59;

  THoraire = record
    heure: THeure;
    minute: TMinute;
    seconde: TSeconde;
  end;

procedure AfficherHoraire(h: THoraire);
begin
  // Afficher avec zéros de remplissage
  if h.heure < 10 then Write('0');
  Write(h.heure);
  Write(':');
  if h.minute < 10 then Write('0');
  Write(h.minute);
  Write(':');
  if h.seconde < 10 then Write('0');
  WriteLn(h.seconde);
end;

function AjouterSecondes(h: THoraire; sec: Integer): THoraire;
var
  totalSecondes: Integer;
begin
  // Convertir en secondes totales
  totalSecondes := h.heure * 3600 + h.minute * 60 + h.seconde + sec;

  // Gérer le dépassement de 24h
  totalSecondes := totalSecondes mod 86400;  // 86400 = 24 * 60 * 60

  // Reconvertir
  Result.heure := totalSecondes div 3600;
  totalSecondes := totalSecondes mod 3600;
  Result.minute := totalSecondes div 60;
  Result.seconde := totalSecondes mod 60;

  AjouterSecondes := Result;
end;

var
  maintenant: THoraire;
  plusTard: THoraire;
begin
  WriteLn('Entrez l''heure actuelle :');
  Write('  Heure (0-23) : ');
  ReadLn(maintenant.heure);
  Write('  Minute (0-59) : ');
  ReadLn(maintenant.minute);
  Write('  Seconde (0-59) : ');
  ReadLn(maintenant.seconde);

  Write('Heure actuelle : ');
  AfficherHoraire(maintenant);

  plusTard := AjouterSecondes(maintenant, 3600);  // +1 heure
  Write('Dans 1 heure : ');
  AfficherHoraire(plusTard);
end.
```

### Exemple 4 : Gestion d'un ascenseur

```pascal
program GestionAscenseur;
type
  TEtage = -2..50;  // Du sous-sol -2 au 50e étage

var
  etageActuel: TEtage;
  etageDestination: TEtage;
  i: TEtage;

procedure DeplacerVers(destination: TEtage);
begin
  WriteLn('Déplacement de l''étage ', etageActuel, ' vers l''étage ', destination);

  if destination > etageActuel then
  begin
    WriteLn('Monte...');
    for i := etageActuel + 1 to destination do
    begin
      WriteLn('  Étage ', i);
      // Simulation d'attente
    end;
  end
  else if destination < etageActuel then
  begin
    WriteLn('Descend...');
    for i := etageActuel - 1 downto destination do
    begin
      WriteLn('  Étage ', i);
      // Simulation d'attente
    end;
  end
  else
    WriteLn('Déjà à l''étage demandé');

  etageActuel := destination;
  WriteLn('Arrivé à l''étage ', etageActuel);
end;

begin
  etageActuel := 0;  // Départ au rez-de-chaussée

  WriteLn('=== ASCENSEUR ===');
  WriteLn('Étage actuel : ', etageActuel);

  Write('Quel étage souhaitez-vous (-2 à 50) ? ');
  ReadLn(etageDestination);

  DeplacerVers(etageDestination);
end.
```

### Exemple 5 : Code PIN

```pascal
program CodePIN;
type
  TChiffre = 0..9;
  TCodePIN = array[1..4] of TChiffre;

var
  code: TCodePIN;
  tentative: TCodePIN;
  i: Integer;
  correct: Boolean;

procedure SaisirCode(var c: TCodePIN);
var
  i: Integer;
begin
  WriteLn('Entrez un code à 4 chiffres :');
  for i := 1 to 4 do
  begin
    Write('  Chiffre ', i, ' (0-9) : ');
    ReadLn(c[i]);
  end;
end;

function CodesIdentiques(c1, c2: TCodePIN): Boolean;
var
  i: Integer;
begin
  CodesIdentiques := True;
  for i := 1 to 4 do
  begin
    if c1[i] <> c2[i] then
    begin
      CodesIdentiques := False;
      Break;
    end;
  end;
end;

begin
  WriteLn('=== CRÉATION DE CODE PIN ===');
  SaisirCode(code);

  WriteLn;
  WriteLn('=== VÉRIFICATION ===');
  SaisirCode(tentative);

  if CodesIdentiques(code, tentative) then
    WriteLn('✓ Code correct !')
  else
    WriteLn('✗ Code incorrect');
end.
```

## Vérification des bornes

### Mode strict

Par défaut, FreePascal peut désactiver la vérification des bornes pour des raisons de performance. Pour activer la vérification :

```pascal
{$R+}  // Active la vérification des intervalles (Range checking)

program VerificationBornes;
type
  TNote = 0..20;

var
  note: TNote;
begin
  note := 15;  // OK
  note := 25;  // Erreur à l'exécution si {$R+}
end.
```

**Conseil :** Activez toujours `{$R+}` pendant le développement.

## Conversion entre types

### Conversion explicite

```pascal
program ConversionTypes;
type
  TPetitNombre = 1..10;
  TGrandNombre = 1..100;

var
  petit: TPetitNombre;
  grand: TGrandNombre;
  entier: Integer;
begin
  petit := 5;

  // Conversion implicite (compatible)
  grand := petit;  // OK : 1..10 est inclus dans 1..100

  // Conversion depuis Integer (nécessite précaution)
  entier := 7;
  petit := entier;  // OK si entier est dans 1..10

  // Attention aux débordements
  entier := 50;
  // petit := entier;  // Erreur si {$R+} : 50 n'est pas dans 1..10
end.
```

## Intervalles et compatibilité

```pascal
type
  TAge = 0..120;
  TAnneeNaissance = 1900..2024;

var
  age: TAge;
  annee: TAnneeNaissance;
begin
  age := 25;
  // annee := age;  // ✗ ERREUR : types incompatibles (intervalles différents)

  // Il faut passer par Integer
  annee := 2000 - Integer(age);
end.
```

## Pièges courants

### 1. Oublier la vérification des bornes

```pascal
type
  TNote = 0..20;

var
  note: TNote;
  saisie: Integer;
begin
  Write('Note : ');
  ReadLn(saisie);

  // ✗ Risqué : pas de vérification
  note := saisie;

  // ✓ Meilleur : vérifier avant
  if (saisie >= 0) and (saisie <= 20) then
    note := saisie
  else
    WriteLn('Note invalide');
end.
```

### 2. Confusion entre intervalle et ensemble

```pascal
type
  TChiffre = 0..9;  // Intervalle : tous les nombres de 0 à 9
  TChiffres = set of 0..9;  // Ensemble : collection de chiffres

var
  chiffre: TChiffre;
  chiffres: TChiffres;
begin
  chiffre := 5;  // Une seule valeur
  chiffres := [1, 3, 5, 7];  // Plusieurs valeurs

  // if chiffre in [1, 3, 5, 7] then  // Teste si chiffre vaut 1, 3, 5 ou 7
end.
```

### 3. Intervalle vide

```pascal
type
  TIntervalle = 10..5;  // ✗ ERREUR : min > max
  TIntervalle = 5..10;  // ✓ CORRECT
```

### 4. Type de base incorrect

```pascal
type
  // TIntervalle = 1.5..10.5;  // ✗ ERREUR : Real n'est pas ordinal
  TIntervalle = 1..10;         // ✓ CORRECT : Integer est ordinal
```

## Avantages des types intervalles

✓ **Sécurité** : Détection des erreurs de valeurs  
✓ **Documentation** : Le code exprime les contraintes  
✓ **Lisibilité** : Intention claire du programmeur  
✓ **Vérification** : Le compilateur aide à trouver les bugs  
✓ **Optimisation** : Peut réduire l'espace mémoire nécessaire  
✓ **Maintenance** : Facilite les modifications futures  

## Quand utiliser des types intervalles ?

✓ **Utilisez des types intervalles quand :**
- Les valeurs ont une plage naturelle limitée (âge, note, mois, etc.)
- Vous voulez documenter les contraintes dans le code
- Vous souhaitez une vérification automatique
- Vous utilisez des indices de tableaux
- Vous définissez des paramètres de fonction

✗ **Évitez les types intervalles quand :**
- La plage peut changer fréquemment
- Les valeurs peuvent vraiment être quelconques
- La performance est critique et la vérification trop coûteuse
- Le type de base n'est pas ordinal

## Résumé

Les types intervalle permettent de :
- Définir des **sous-ensembles contigus** de types ordinaux
- Restreindre les **valeurs possibles** d'une variable
- Améliorer la **sécurité** et la **lisibilité** du code
- Bénéficier de la **vérification du compilateur**

**Points clés à retenir :**
- Déclaration : `type NomType = ValeurMin..ValeurMax;`
- Types de base : **ordinaux uniquement** (Integer, Char, énuméré)
- Bornes : min ≤ max obligatoire
- Vérification : activer avec `{$R+}`
- Compatible avec tableaux, paramètres, enregistrements
- Basés sur énumérés : très puissant (ex: `Lundi..Vendredi`)

Les types intervalles sont un excellent outil pour écrire du code plus sûr et plus expressif. Ils font partie des bonnes pratiques en Pascal !

⏭️ [Définition de types personnalisés (Type)](/05-types-donnees-structures/10-definition-types-personnalises.md)
