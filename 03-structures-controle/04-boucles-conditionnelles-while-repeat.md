🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.4 Boucles conditionnelles (while-do, repeat-until)

## Introduction

Dans la section précédente, nous avons vu la boucle `for` qui répète des instructions un nombre **connu à l'avance** de fois. Mais que faire si vous ne savez pas combien de fois répéter ? Par exemple :
- "Continue à demander un mot de passe jusqu'à ce qu'il soit correct"
- "Lis des nombres jusqu'à ce que l'utilisateur entre 0"
- "Cherche dans une liste jusqu'à trouver l'élément recherché"

Pour ces situations, nous utilisons les **boucles conditionnelles** : `while-do` et `repeat-until`.

## Différence fondamentale avec FOR

| Boucle FOR | Boucles WHILE/REPEAT |
|------------|----------------------|
| Nombre de répétitions **connu** | Nombre de répétitions **inconnu** |
| "Répète 10 fois" | "Répète tant que..." |
| Compteur automatique | Condition à gérer manuellement |

## La boucle WHILE-DO

### Concept

La boucle `while` signifie "**tant que**". Elle dit : "Tant que la condition est vraie, continue à répéter les instructions".

**Analogie de la vie quotidienne :**
- "Tant qu'il reste du café dans la tasse, continue à boire"
- "Tant que le feu est rouge, attends"
- "Tant qu'il y a des questions, réponds"

### Syntaxe de base

```pascal
while condition do
  instruction;
```

Pour plusieurs instructions :

```pascal
while condition do
begin
  instruction1;
  instruction2;
  instruction3;
end;
```

### Premier exemple simple

```pascal
program PremierWhile;
var
  compteur: Integer;
begin
  compteur := 1;
  { while verifie la condition AVANT chaque iteration :
    si elle est fausse des le depart, le corps n'est jamais execute }
  while compteur <= 5 do
  begin
    WriteLn('Compteur = ', compteur);
    compteur := compteur + 1;
  end;
  WriteLn('Fin de la boucle');
end.
```

**Résultat :**
```
Compteur = 1
Compteur = 2
Compteur = 3
Compteur = 4
Compteur = 5
Fin de la boucle
```

### Comment fonctionne WHILE

1. **Évaluation** : La condition est testée
2. **Si VRAIE** : Les instructions sont exécutées, puis retour à l'étape 1
3. **Si FAUSSE** : La boucle se termine, le programme continue après

**Schéma du fonctionnement :**
```
┌─────────────────┐
│ Début de while  │
└────────┬────────┘
         │
         ▼
    ┌─────────┐
    │Condition│───Non──► Continue après while
    └────┬────┘
         │Oui
         ▼
    ┌────────────┐
    │Instructions│
    └────┬───────┘
         │
         └──────► Retour au test de condition
```

### Point crucial : WHILE peut ne JAMAIS s'exécuter

Si la condition est fausse dès le début, la boucle ne s'exécute jamais :

```pascal
program WhileJamaisExecute;
var
  i: Integer;
begin
  i := 10;
  while i < 5 do
  begin
    WriteLn('Ce message ne s''affichera jamais !');
    i := i + 1;
  end;
  WriteLn('i vaut toujours ', i);
end.
```

**Résultat :**
```
i vaut toujours 10
```

### Exemple : Validation d'entrée

```pascal
program ValidationAgeWhile;
var
  age: Integer;
begin
  age := -1;
  while (age < 0) or (age > 120) do
  begin
    Write('Entrez votre âge (0-120) : ');
    ReadLn(age);
    if (age < 0) or (age > 120) then
      WriteLn('Âge invalide ! Réessayez.');
  end;
  WriteLn('Âge accepté : ', age, ' ans');
end.
```

### Exemple : Menu avec choix

```pascal
program MenuSimple;
var
  choix: Integer;
begin
  choix := 0;
  while choix <> 4 do
  begin
    WriteLn;
    WriteLn('=== MENU ===');
    WriteLn('1. Option A');
    WriteLn('2. Option B');
    WriteLn('3. Option C');
    WriteLn('4. Quitter');
    Write('Votre choix : ');
    ReadLn(choix);
    WriteLn;
    case choix of
      1: WriteLn('Vous avez choisi l''option A');
      2: WriteLn('Vous avez choisi l''option B');
      3: WriteLn('Vous avez choisi l''option C');
      4: WriteLn('Au revoir !');
    else
      WriteLn('Choix invalide !');
    end;
  end;
end.
```

## La boucle REPEAT-UNTIL

### Concept

La boucle `repeat` signifie "**répète jusqu'à ce que**". Elle dit : "Répète les instructions jusqu'à ce que la condition soit vraie".

**Analogie de la vie quotidienne :**
- "Lance le dé jusqu'à obtenir un 6"
- "Essaie de démarrer la voiture jusqu'à ce qu'elle démarre"
- "Cherche jusqu'à ce que tu trouves"

### Syntaxe de base

```pascal
repeat
  instruction1;
  instruction2;
  instruction3;
until condition;
```

**Important :** Notez qu'il n'y a **pas besoin** de `begin-end` avec `repeat-until` !

### Premier exemple simple

```pascal
program PremierRepeat;
var
  compteur: Integer;
begin
  compteur := 1;
  repeat
    WriteLn('Compteur = ', compteur);
    compteur := compteur + 1;
  until compteur > 5;  { until = "arrete quand c'est vrai" (logique inversee par rapport a while) }
  WriteLn('Fin de la boucle');
end.
```

**Résultat :**
```
Compteur = 1
Compteur = 2
Compteur = 3
Compteur = 4
Compteur = 5
Fin de la boucle
```

### Comment fonctionne REPEAT-UNTIL

1. **Exécution** : Les instructions sont exécutées
2. **Évaluation** : La condition est testée
3. **Si FAUSSE** : Retour à l'étape 1
4. **Si VRAIE** : La boucle se termine

**Schéma du fonctionnement :**
```
┌─────────────────┐
│ Début de repeat │
└────────┬────────┘
         │
         ▼
    ┌────────────┐
    │Instructions│
    └────┬───────┘
         │
         ▼
    ┌─────────┐
    │Condition│───Non──► Retour aux instructions
    └────┬────┘
         │Oui
         ▼
    Continue après repeat
```

### Point crucial : REPEAT s'exécute TOUJOURS au moins une fois

Contrairement à `while`, `repeat` exécute toujours ses instructions au moins une fois :

```pascal
program RepeatToujoursUneFois;
var
  i: Integer;
begin
  i := 10;
  { repeat execute le corps AVANT de tester la condition :
    le message s'affiche au moins une fois, meme si i > 5 des le depart }
  repeat
    WriteLn('Ce message s''affiche quand même !');
    WriteLn('i = ', i);
  until i > 5;
  WriteLn('Fin de la boucle');
end.
```

**Résultat :**
```
Ce message s'affiche quand même !
i = 10
Fin de la boucle
```

### Exemple : Validation d'entrée avec REPEAT

```pascal
program ValidationRepeatAge;
var
  age: Integer;
begin
  { Pas besoin d'initialiser age a une valeur invalide :
    repeat execute le corps au moins une fois, donc ReadLn s'execute d'abord }
  repeat
    Write('Entrez votre âge (0-120) : ');
    ReadLn(age);
    if (age < 0) or (age > 120) then
      WriteLn('Âge invalide ! Réessayez.');
  until (age >= 0) and (age <= 120);
  { Comparer avec la version while : while utilise (age < 0) or (age > 120)
    pour continuer, repeat utilise (age >= 0) and (age <= 120) pour s'arreter }
  WriteLn('Âge accepté : ', age, ' ans');
end.
```

Notez que ce code est plus court qu'avec `while` car nous n'avons pas besoin d'initialiser `age` avec une valeur invalide.

## WHILE vs REPEAT : Les différences clés

| Caractéristique | WHILE-DO | REPEAT-UNTIL |
|----------------|----------|--------------|
| **Test de condition** | Au début | À la fin |
| **Exécution minimale** | 0 fois | 1 fois |
| **Condition** | Continue si VRAIE | S'arrête si VRAIE |
| **BEGIN-END** | Nécessaire pour plusieurs instructions | Pas nécessaire |
| **Utilisation typique** | Peut ne pas s'exécuter | Doit s'exécuter au moins une fois |

### Exemple comparatif

```pascal
program ComparaisonWhileRepeat;
var
  i: Integer;
begin
  WriteLn('=== Avec WHILE ===');
  i := 10;
  while i < 5 do
  begin
    WriteLn('WHILE : i = ', i);
    i := i + 1;
  end;
  WriteLn('WHILE : Aucune exécution car condition fausse au départ');
  WriteLn;
  WriteLn('=== Avec REPEAT ===');
  i := 10;
  repeat
    WriteLn('REPEAT : i = ', i);
    i := i + 1;
  until i >= 5;
  WriteLn('REPEAT : Exécuté une fois malgré la condition vraie');
end.
```

### Quand utiliser WHILE ?

- Quand la boucle **peut ne pas s'exécuter du tout**
- Quand vous devez tester la condition **avant** d'agir
- Exemple : "Tant qu'il y a des données à lire"

### Quand utiliser REPEAT ?

- Quand la boucle **doit s'exécuter au moins une fois**
- Quand vous devez agir **avant** de tester
- Exemple : "Demande à l'utilisateur jusqu'à avoir une réponse valide"

## Boucles infinies et comment les éviter

### Qu'est-ce qu'une boucle infinie ?

Une boucle qui ne se termine jamais car la condition ne change jamais.

### Boucle infinie avec WHILE

```pascal
// DANGER ! Boucle infinie !
var
  i: Integer;
begin
  i := 1;
  while i <= 10 do
  begin
    WriteLn(i);
    // ERREUR : On oublie d'incrémenter i !
  end;
end.
```

Cette boucle affiche "1" indéfiniment car `i` reste toujours à 1.

### Boucle infinie avec REPEAT

```pascal
// DANGER ! Boucle infinie !
var
  i: Integer;
begin
  i := 1;
  repeat
    WriteLn(i);
    // ERREUR : On oublie d'incrémenter i !
  until i > 10;
end.
```

### Comment éviter les boucles infinies

**Règle d'or :** Assurez-vous que quelque chose dans la boucle **modifie la condition** !

```pascal
// CORRECT
var
  i: Integer;
begin
  i := 1;
  while i <= 10 do
  begin
    WriteLn(i);
    i := i + 1;  // ✓ La condition change !
  end;
end.
```

### Boucles infinies intentionnelles

Parfois, on veut une boucle infinie (serveurs, jeux) :

```pascal
// Boucle infinie avec condition de sortie
while True do
begin
  // Traitement
  if conditionDeSortie then
    Break;  // Sort de la boucle
end;
```

## Exemples pratiques

### Jeu de devinette

```pascal
program JeuDevinette;
var
  nombreSecret, proposition, tentatives: Integer;
begin
  Randomize;   { Initialise le generateur aleatoire (a appeler une seule fois) }
  nombreSecret := Random(100) + 1;  { Random(100) donne 0..99, donc +1 donne 1..100 }
  tentatives := 0;
  WriteLn('=== JEU DE DEVINETTE ===');
  WriteLn('J''ai choisi un nombre entre 1 et 100.');
  WriteLn('Essayez de le deviner !');
  WriteLn;
  repeat
    Write('Votre proposition : ');
    ReadLn(proposition);
    tentatives := tentatives + 1;
    if proposition < nombreSecret then
      WriteLn('C''est plus !')
    else if proposition > nombreSecret then
      WriteLn('C''est moins !')
    else
      WriteLn('Bravo ! Vous avez trouvé en ', tentatives, ' tentative(s) !');
    WriteLn;
  until proposition = nombreSecret;
end.
```

### Calcul de moyenne avec sentinelle

```pascal
program CalculMoyenneSentinelle;
var
  nombre: Real;
  somme: Real;
  compteur: Integer;
  moyenne: Real;
begin
  somme := 0;
  compteur := 0;
  WriteLn('=== CALCUL DE MOYENNE ===');
  WriteLn('Entrez des nombres (0 pour terminer)');
  WriteLn;
  repeat
    Write('Nombre ', compteur + 1, ' : ');
    ReadLn(nombre);
    if nombre <> 0 then
    begin
      somme := somme + nombre;
      compteur := compteur + 1;
    end;
  until nombre = 0;
  WriteLn;
  if compteur > 0 then
  begin
    moyenne := somme / compteur;
    WriteLn('Nombre de valeurs : ', compteur);
    WriteLn('Somme : ', somme:0:2);
    WriteLn('Moyenne : ', moyenne:0:2);
  end
  else
    WriteLn('Aucune valeur saisie.');
end.
```

### Lecture de fichier ligne par ligne

```pascal
program LectureFichier;
var
  fichier: TextFile;
  ligne: String;
  numeroLigne: Integer;
begin
  AssignFile(fichier, 'exemple.txt');

  try
    Reset(fichier);
    numeroLigne := 0;

    // Utilisation de WHILE car le fichier peut être vide
    while not Eof(fichier) do
    begin
      ReadLn(fichier, ligne);
      numeroLigne := numeroLigne + 1;
      WriteLn('Ligne ', numeroLigne, ' : ', ligne);
    end;

    CloseFile(fichier);
    WriteLn;
    WriteLn('Total : ', numeroLigne, ' ligne(s)');
  except
    WriteLn('Erreur lors de la lecture du fichier.');
  end;

  ReadLn;
end.
```

### Système de connexion

```pascal
program SystemeConnexion;
const
  MAX_TENTATIVES = 3;
  MOT_DE_PASSE_CORRECT = 'pascal2024';
var
  motDePasse: String;
  tentatives: Integer;
  connecte: Boolean;
begin
  tentatives := 0;
  connecte := False;
  WriteLn('=== CONNEXION ===');
  WriteLn;
  { not connecte : l'operateur not inverse un Boolean (True -> False, False -> True) }
  while (tentatives < MAX_TENTATIVES) and (not connecte) do
  begin
    tentatives := tentatives + 1;
    Write('Mot de passe (tentative ', tentatives, '/', MAX_TENTATIVES, ') : ');
    ReadLn(motDePasse);
    if motDePasse = MOT_DE_PASSE_CORRECT then
    begin
      connecte := True;
      WriteLn;
      WriteLn('Connexion réussie !');
      WriteLn('Bienvenue dans le système.');
    end
    else
    begin
      if tentatives < MAX_TENTATIVES then
        WriteLn('Mot de passe incorrect. Il vous reste ',
                MAX_TENTATIVES - tentatives, ' tentative(s).')
      else
        WriteLn('Accès refusé. Nombre maximum de tentatives atteint.');
    end;
    WriteLn;
  end;
end.
```

### Recherche dans une liste

```pascal
program RechercheNombre;
const
  TAILLE = 10;
var
  nombres: array[1..TAILLE] of Integer;
  i, recherche: Integer;
  trouve: Boolean;
begin
  WriteLn('Entrez ', TAILLE, ' nombres :');
  for i := 1 to TAILLE do
  begin
    Write('Nombre ', i, ' : ');
    ReadLn(nombres[i]);
  end;
  WriteLn;
  Write('Nombre à rechercher : ');
  ReadLn(recherche);
  i := 1;
  trouve := False;
  while (i <= TAILLE) and (not trouve) do
  begin
    if nombres[i] = recherche then
      trouve := True
    else
      i := i + 1;
  end;
  WriteLn;
  if trouve then
    WriteLn('Nombre ', recherche, ' trouvé à la position ', i)
  else
    WriteLn('Nombre ', recherche, ' non trouvé');
end.
```

## Boucles imbriquées

Vous pouvez imbriquer des boucles conditionnelles comme avec `for` :

### WHILE imbriqués

```pascal
program WhileImbriques;
var
  i, j: Integer;
begin
  i := 1;
  while i <= 3 do
  begin
    WriteLn('Ligne ', i, ' :');
    j := 1;
    while j <= 4 do
    begin
      Write(j, ' ');
      j := j + 1;
    end;
    WriteLn;
    i := i + 1;
  end;
end.
```

**Résultat :**
```
Ligne 1 :
1 2 3 4
Ligne 2 :
1 2 3 4
Ligne 3 :
1 2 3 4
```

### Mélange de types de boucles

```pascal
program MelangeBoucles;
var
  continuer: Char;
  i: Integer;
begin
  repeat
    WriteLn('Affichage des nombres de 1 à 5 :');
    for i := 1 to 5 do
      Write(i, ' ');
    WriteLn;
    WriteLn;
    Write('Continuer ? (O/N) : ');
    ReadLn(continuer);
    WriteLn;
  until (continuer = 'N') or (continuer = 'n');
  WriteLn('Au revoir !');
end.
```

## Erreurs courantes

### 1. Oublier de modifier la condition

```pascal
// ERREUR ! Boucle infinie
var
  i: Integer;
begin
  i := 1;
  while i <= 10 do
  begin
    WriteLn(i);
    // Oubli : i := i + 1;
  end;
end.
```

**Solution :** Toujours modifier la variable de la condition dans la boucle.

### 2. Confondre WHILE et REPEAT

```pascal
// ERREUR de logique
repeat
  WriteLn('Message');
until i < 10;  // Continue tant que i < 10 ? NON !
// REPEAT s'arrête quand la condition est VRAIE

// CORRECT
repeat
  WriteLn('Message');
until i >= 10;  // S'arrête quand i >= 10
```

### 3. Condition inversée dans REPEAT

```pascal
// Avec WHILE : continue tant que age est invalide
while (age < 0) or (age > 120) do

// Avec REPEAT : s'arrête quand age est valide
repeat
until (age >= 0) and (age <= 120)  // Attention à la logique inversée !
```

### 4. Oublier BEGIN-END avec WHILE

```pascal
// ERREUR ! Seule la première instruction est dans la boucle
while i < 10 do
  WriteLn(i);
  i := i + 1;  // ❌ Ceci est APRÈS la boucle !

// CORRECT
while i < 10 do
begin
  WriteLn(i);
  i := i + 1;
end;
```

### 5. Point-virgule avant UNTIL

```pascal
// ATTENTION à la syntaxe
repeat
  WriteLn('Message');
  i := i + 1;  // Point-virgule ici OK
until i > 10;  // Pas de point-virgule avant until
```

## Bonnes pratiques

### 1. Choix de la boucle appropriée

```pascal
// Utiliser FOR quand on connaît le nombre d'itérations
for i := 1 to 10 do

// Utiliser WHILE quand la condition est testée avant
while not Eof(fichier) do

// Utiliser REPEAT pour les validations d'entrée
repeat
  ReadLn(valeur);
until valeur > 0;
```

### 2. Éviter les conditions complexes

```pascal
// MOINS BON
while (a > 0) and (b < 10) and ((c = 5) or (d <> 3)) do

// MEILLEUR
continuer := (a > 0) and (b < 10) and ((c = 5) or (d <> 3));
while continuer do
begin
  // instructions
  continuer := (a > 0) and (b < 10) and ((c = 5) or (d <> 3));
end;
```

### 3. Utiliser des drapeaux (flags)

```pascal
var
  termine, trouve: Boolean;
begin
  termine := False;
  trouve := False;

  while (not termine) and (not trouve) do
  begin
    // logique
    if conditionTrouve then
      trouve := True;
    if conditionFin then
      termine := True;
  end;
end;
```

### 4. Commentaires pour les boucles complexes

```pascal
// Recherche du premier nombre premier après n
while not EstPremier(nombre) do
begin
  nombre := nombre + 1;
  // Continue jusqu'à trouver un nombre premier
end;
```

### 5. Initialisation correcte

```pascal
// Toujours initialiser avant WHILE
i := 0;  // Important !
while i < 10 do
begin
  WriteLn(i);
  i := i + 1;
end;
```

## Exemple récapitulatif complet

```pascal
program GestionCompteClient;
var
  solde, montant: Real;
  choix: Integer;
  continuer: Boolean;
  tentatives: Integer;
  codePin, codeCorrect: String;
begin
  continuer := True;
  codeCorrect := '1234';
  solde := 1000.00;
  WriteLn('================================');
  WriteLn('   DISTRIBUTEUR AUTOMATIQUE   ');
  WriteLn('================================');
  WriteLn;
  tentatives := 0;
  repeat
    tentatives := tentatives + 1;
    Write('Entrez votre code PIN : ');
    ReadLn(codePin);
    if codePin <> codeCorrect then
    begin
      WriteLn('Code incorrect !');
      if tentatives >= 3 then
      begin
        WriteLn('Carte bloquée. Contactez votre banque.');
        continuer := False;
      end;
    end;
  until (codePin = codeCorrect) or (tentatives >= 3);
  while continuer and (codePin = codeCorrect) do
  begin
    WriteLn;
    WriteLn('================================');
    WriteLn('Solde actuel : ', solde:0:2, ' euros');
    WriteLn('================================');
    WriteLn('1. Consulter le solde');
    WriteLn('2. Retirer de l''argent');
    WriteLn('3. Déposer de l''argent');
    WriteLn('4. Quitter');
    WriteLn;
    Write('Votre choix : ');
    ReadLn(choix);
    WriteLn;
    case choix of
      1:
        begin
          WriteLn('--- CONSULTATION ---');
          WriteLn('Votre solde est de : ', solde:0:2, ' euros');
        end;
      2:
        begin
          WriteLn('--- RETRAIT ---');
          repeat
            Write('Montant à retirer : ');
            ReadLn(montant);
            if montant <= 0 then
              WriteLn('Le montant doit être positif !')
            else if montant > solde then
              WriteLn('Solde insuffisant ! (Solde : ', solde:0:2, ' euros)');
          until (montant > 0) and (montant <= solde);
          solde := solde - montant;
          WriteLn('Retrait effectué avec succès !');
          WriteLn('Nouveau solde : ', solde:0:2, ' euros');
        end;
      3:
        begin
          WriteLn('--- DÉPÔT ---');
          repeat
            Write('Montant à déposer : ');
            ReadLn(montant);
            if montant <= 0 then
              WriteLn('Le montant doit être positif !')
            else
            begin
              solde := solde + montant;
              WriteLn('Dépôt effectué avec succès !');
              WriteLn('Nouveau solde : ', solde:0:2, ' euros');
            end;
          until montant > 0;
        end;
      4:
        begin
          WriteLn('Merci d''avoir utilisé nos services.');
          WriteLn('Au revoir !');
          continuer := False;
        end;
    else
      WriteLn('Choix invalide. Veuillez réessayer.');
    end;
  end;
  WriteLn;
  WriteLn('================================');
end.
```

## Tableau récapitulatif des trois boucles

| Critère | FOR | WHILE | REPEAT |
|---------|-----|-------|--------|
| **Nombre d'itérations** | Connu | Inconnu | Inconnu |
| **Test de condition** | - | Avant | Après |
| **Exécution minimale** | 0 fois | 0 fois | 1 fois |
| **BEGIN-END requis** | Si plusieurs instructions | Si plusieurs instructions | Non |
| **Usage typique** | Comptages | Lecture fichiers | Validation entrées |
| **Incrémentation** | Automatique | Manuelle | Manuelle |

## Résumé

Les boucles conditionnelles permettent de répéter des instructions quand le nombre de répétitions est inconnu :

### WHILE-DO
- **Syntaxe** : `while condition do`
- Teste la condition **avant** d'exécuter
- Peut s'exécuter **0 fois**
- Continue **tant que** la condition est **vraie**
- Utiliser quand la boucle peut ne pas s'exécuter

### REPEAT-UNTIL
- **Syntaxe** : `repeat ... until condition`
- Teste la condition **après** avoir exécuté
- S'exécute **au moins 1 fois**
- S'arrête **quand** la condition devient **vraie**
- Pas besoin de BEGIN-END
- Utiliser pour les validations d'entrée

### Points importants
- Toujours faire évoluer la condition dans la boucle
- Attention aux boucles infinies
- Choisir la bonne boucle selon le besoin
- WHILE et REPEAT ont des logiques de condition inversées

Ces boucles sont essentielles pour créer des programmes interactifs et robustes qui s'adaptent aux actions de l'utilisateur !

⏭️ [Instructions break et continue](/03-structures-controle/05-instructions-break-continue.md)
