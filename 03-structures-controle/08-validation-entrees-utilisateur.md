🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.8 Validation des entrées utilisateur

## Introduction

La validation des entrées utilisateur est l'une des compétences les plus importantes en programmation. Les utilisateurs peuvent (intentionnellement ou non) entrer n'importe quoi : des valeurs hors limites, des caractères invalides, des formats incorrects... Un programme robuste doit **toujours** vérifier et valider les données avant de les utiliser.

**Le principe fondamental :** Ne faites JAMAIS confiance aux entrées utilisateur. Validez tout, tout le temps.

**Analogie :** C'est comme un contrôle de sécurité à l'aéroport. Même si 99% des passagers sont en règle, on vérifie chacun d'eux systématiquement.

## Pourquoi valider les entrées ?

### Sans validation

```pascal
program SansValidation;  
var  
  age: Integer;
begin
  Write('Âge : ');
  ReadLn(age);
  WriteLn('Dans 10 ans, vous aurez ', age + 10, ' ans');
end.
```

**Problèmes possibles :**
- L'utilisateur entre -50 → résultat absurde
- L'utilisateur entre 500 → résultat impossible
- L'utilisateur tape des lettres → plantage du programme

### Avec validation

```pascal
program AvecValidation;  
var  
  age: Integer;
begin
  repeat
    Write('Âge (0-150) : ');
    ReadLn(age);
    if (age < 0) or (age > 150) then
      WriteLn('⚠️  Âge invalide. Réessayez.');
  until (age >= 0) and (age <= 150);

  WriteLn('✓ Dans 10 ans, vous aurez ', age + 10, ' ans');
end.
```

**Avantages :**
- Programme robuste
- Pas de valeurs absurdes
- Meilleure expérience utilisateur

## Types de validation

### 1. Validation de plage (range)

Vérifier qu'une valeur est dans un intervalle acceptable.

```pascal
program ValidationPlage;  
var  
  note: Integer;
begin
  WriteLn('Entrez une note (0-20) :');

  repeat
    Write('Note : ');
    ReadLn(note);

    if (note < 0) or (note > 20) then
      WriteLn('❌ La note doit être entre 0 et 20');
  until (note >= 0) and (note <= 20);

  WriteLn('✓ Note enregistrée : ', note, '/20');
end.
```

### 2. Validation de type

Vérifier que la donnée est du bon type (nombre, lettre, etc.).

```pascal
program ValidationType;  
var  
  choix: Char;
begin
  WriteLn('Choisissez une option (A/B/C) :');

  repeat
    Write('Choix : ');
    ReadLn(choix);
    choix := UpCase(choix);  // Conversion en majuscule

    if not (choix in ['A', 'B', 'C']) then
      WriteLn('❌ Veuillez entrer A, B ou C');
  until choix in ['A', 'B', 'C'];

  WriteLn('✓ Option ', choix, ' sélectionnée');
end.
```

### 3. Validation de format

Vérifier que les données respectent un format spécifique.

```pascal
program ValidationFormat;  
var  
  codePostal: String;
  i: Integer;
  valide: Boolean;
begin
  WriteLn('Entrez un code postal français (5 chiffres) :');

  repeat
    Write('Code postal : ');
    ReadLn(codePostal);

    valide := True;

    // Vérifier la longueur
    if Length(codePostal) <> 5 then
      valide := False
    else
    begin
      // Vérifier que tous sont des chiffres
      for i := 1 to 5 do
      begin
        if not (codePostal[i] in ['0'..'9']) then
        begin
          valide := False;
          break;
        end;
      end;
    end;

    if not valide then
      WriteLn('❌ Le code postal doit contenir exactement 5 chiffres');
  until valide;

  WriteLn('✓ Code postal accepté : ', codePostal);
end.
```

### 4. Validation de cohérence

Vérifier que les données sont logiques entre elles.

```pascal
program ValidationCoherence;  
var  
  dateNaissance, dateEmbauche: Integer;
begin
  WriteLn('Saisie d''informations employé :');

  repeat
    Write('Année de naissance : ');
    ReadLn(dateNaissance);
    if (dateNaissance < 1900) or (dateNaissance > 2024) then
      WriteLn('❌ Année invalide');
  until (dateNaissance >= 1900) and (dateNaissance <= 2024);

  repeat
    Write('Année d''embauche : ');
    ReadLn(dateEmbauche);

    if dateEmbauche < dateNaissance then
      WriteLn('❌ L''embauche ne peut pas être avant la naissance !')
    else if (dateEmbauche - dateNaissance) < 16 then
      WriteLn('❌ L''employé doit avoir au moins 16 ans à l''embauche')
    else if dateEmbauche > 2024 then
      WriteLn('❌ Date dans le futur impossible');
  until (dateEmbauche >= dateNaissance + 16) and (dateEmbauche <= 2024);

  WriteLn('✓ Données validées');
end.
```

## Techniques de validation

### Technique 1 : Validation avec REPEAT-UNTIL

La technique la plus simple et la plus utilisée pour les débutants.

```pascal
// Structure de base
repeat
  // 1. Demander la saisie
  Write('Valeur : ');
  ReadLn(valeur);

  // 2. Vérifier et afficher erreur si besoin
  if PasValide(valeur) then
    WriteLn('❌ Erreur...');
until EstValide(valeur);
```

**Exemple :**

```pascal
program ValidationRepeatPourcentage;  
var  
  pourcentage: Integer;
begin
  repeat
    Write('Pourcentage (0-100) : ');
    ReadLn(pourcentage);

    if (pourcentage < 0) or (pourcentage > 100) then
      WriteLn('❌ Doit être entre 0 et 100');
  until (pourcentage >= 0) and (pourcentage <= 100);

  WriteLn('✓ Pourcentage : ', pourcentage, '%');
end.
```

### Technique 2 : Validation avec WHILE et drapeau

Plus flexible, permet de compter les tentatives.

```pascal
program ValidationWhile;  
var  
  valeur: Integer;
  valide: Boolean;
  tentatives: Integer;
const
  MAX_TENTATIVES = 3;
begin
  valide := False;
  tentatives := 0;

  while (not valide) and (tentatives < MAX_TENTATIVES) do
  begin
    tentatives := tentatives + 1;
    Write('Tentative ', tentatives, '/', MAX_TENTATIVES, ' - Valeur (1-10) : ');
    ReadLn(valeur);

    if (valeur >= 1) and (valeur <= 10) then
    begin
      valide := True;
      WriteLn('✓ Valeur acceptée');
    end
    else
      WriteLn('❌ Invalide');
  end;

  if not valide then
    WriteLn('Nombre maximum de tentatives atteint');
end.
```

### Technique 3 : Fonction de validation

Rendre le code réutilisable et plus clair.

```pascal
program ValidationFonction;

function EstNombreEntier(s: String): Boolean;  
var  
  i: Integer;
begin
  EstNombreEntier := True;

  if Length(s) = 0 then
  begin
    EstNombreEntier := False;
    exit;  // exit quitte immédiatement la fonction (comme return en C/Java)
  end;

  for i := 1 to Length(s) do
  begin
    if not (s[i] in ['0'..'9']) then
    begin
      EstNombreEntier := False;
      exit;
    end;
  end;
end;

var
  saisie: String;
begin
  repeat
    Write('Entrez un nombre entier : ');
    ReadLn(saisie);

    if not EstNombreEntier(saisie) then
      WriteLn('❌ Ce n''est pas un nombre entier valide');
  until EstNombreEntier(saisie);

  WriteLn('✓ Nombre valide : ', saisie);
end.
```

### Technique 4 : Validation avec procédure

Encapsuler toute la logique de validation.

```pascal
program ValidationProcedure;

procedure LireEntierDansIntervalle(message: String; min, max: Integer; var resultat: Integer);  
begin  
  repeat
    Write(message, ' (', min, '-', max, ') : ');
    ReadLn(resultat);

    if (resultat < min) or (resultat > max) then
      WriteLn('❌ Valeur hors limites');
  until (resultat >= min) and (resultat <= max);
end;

var
  age, note: Integer;
begin
  LireEntierDansIntervalle('Âge', 0, 150, age);
  WriteLn('✓ Âge : ', age);

  LireEntierDansIntervalle('Note', 0, 20, note);
  WriteLn('✓ Note : ', note);
end.
```

## Validation de nombres

### Nombres entiers dans un intervalle

```pascal
program ValidationEntierIntervalle;  
var  
  jour: Integer;
begin
  repeat
    Write('Jour du mois (1-31) : ');
    ReadLn(jour);

    if (jour < 1) or (jour > 31) then
    begin
      WriteLn('❌ Jour invalide');
      WriteLn('   Les jours vont de 1 à 31');
    end;
  until (jour >= 1) and (jour <= 31);

  WriteLn('✓ Jour : ', jour);
end.
```

### Nombres décimaux positifs

```pascal
program ValidationDecimalPositif;  
var  
  prix: Real;
begin
  repeat
    Write('Prix en euros : ');
    ReadLn(prix);

    if prix < 0 then
      WriteLn('❌ Le prix ne peut pas être négatif')
    else if prix > 1000000 then
      WriteLn('❌ Prix trop élevé (max 1 000 000 €)');
  until (prix >= 0) and (prix <= 1000000);

  WriteLn('✓ Prix : ', prix:0:2, ' €');
end.
```

### Nombres avec précision

```pascal
program ValidationPrecision;  
var  
  note: Real;
  valide: Boolean;
begin
  repeat
    Write('Note (0.0 à 20.0, max 1 décimale) : ');
    ReadLn(note);

    valide := True;

    if (note < 0) or (note > 20) then
    begin
      WriteLn('❌ Note hors limites');
      valide := False;
    end
    // Trunc supprime les décimales sans arrondir ; cette astuce détecte plus d'1 décimale
    else if (note * 10) <> Trunc(note * 10) then
    begin
      WriteLn('❌ Maximum 1 décimale (ex: 15.5)');
      valide := False;
    end;
  until valide;

  WriteLn('✓ Note : ', note:0:1, '/20');
end.
```

## Validation de caractères et chaînes

### Validation d'un seul caractère

```pascal
program ValidationCaractere;  
var  
  reponse: Char;
begin
  repeat
    Write('Continuer ? (O/N) : ');
    ReadLn(reponse);
    reponse := UpCase(reponse);

    if not (reponse in ['O', 'N']) then
      WriteLn('❌ Répondez par O (Oui) ou N (Non)');
  until reponse in ['O', 'N'];

  if reponse = 'O' then
    WriteLn('✓ Continuation...')
  else
    WriteLn('✓ Arrêt');
end.
```

### Validation de chaîne non vide

```pascal
program ValidationNonVide;  
uses  
  SysUtils;
var
  nom: String;
begin
  repeat
    Write('Nom : ');
    ReadLn(nom);

    // Supprimer les espaces au début et à la fin
    nom := Trim(nom);

    if Length(nom) = 0 then
      WriteLn('❌ Le nom ne peut pas être vide');
  until Length(nom) > 0;

  WriteLn('✓ Bonjour ', nom, ' !');
end.
```

### Validation de longueur

```pascal
program ValidationLongueur;  
var  
  motDePasse: String;
const
  LONGUEUR_MIN = 8;
  LONGUEUR_MAX = 20;
begin
  repeat
    Write('Mot de passe (', LONGUEUR_MIN, '-', LONGUEUR_MAX, ' caractères) : ');
    ReadLn(motDePasse);

    if Length(motDePasse) < LONGUEUR_MIN then
      WriteLn('❌ Trop court (min ', LONGUEUR_MIN, ' caractères)')
    else if Length(motDePasse) > LONGUEUR_MAX then
      WriteLn('❌ Trop long (max ', LONGUEUR_MAX, ' caractères)');
  until (Length(motDePasse) >= LONGUEUR_MIN) and
        (Length(motDePasse) <= LONGUEUR_MAX);

  WriteLn('✓ Mot de passe accepté');
end.
```

### Validation de format de chaîne

```pascal
program ValidationEmail;  
var  
  email: String;
  posArobase, posPoint: Integer;
  valide: Boolean;
begin
  repeat
    Write('Email : ');
    ReadLn(email);

    valide := True;
    posArobase := Pos('@', email);

    // Vérifications basiques
    if Length(email) < 5 then
    begin
      WriteLn('❌ Email trop court');
      valide := False;
    end
    else if posArobase = 0 then
    begin
      WriteLn('❌ L''email doit contenir @');
      valide := False;
    end
    else if posArobase = 1 then
    begin
      WriteLn('❌ L''email ne peut pas commencer par @');
      valide := False;
    end
    else
    begin
      // Vérifier qu'il y a un point après le @
      // Copy(chaine, debut, longueur) extrait une sous-chaîne
      posPoint := Pos('.', Copy(email, posArobase, Length(email)));
      if posPoint = 0 then
      begin
        WriteLn('❌ L''email doit avoir un point après @');
        valide := False;
      end;
    end;
  until valide;

  WriteLn('✓ Email accepté : ', email);
end.
```

## Validation de choix multiples

### Menu avec validation

```pascal
program MenuValidation;  
var  
  choix: Integer;
begin
  WriteLn('═══ MENU PRINCIPAL ═══');
  WriteLn('1. Nouveau');
  WriteLn('2. Ouvrir');
  WriteLn('3. Enregistrer');
  WriteLn('4. Quitter');
  WriteLn;

  repeat
    Write('Votre choix (1-4) : ');
    ReadLn(choix);

    if (choix < 1) or (choix > 4) then
      WriteLn('❌ Veuillez choisir entre 1 et 4');
  until (choix >= 1) and (choix <= 4);

  WriteLn('✓ Option ', choix, ' sélectionnée');
end.
```

### Choix parmi des options textuelles

```pascal
program ChoixTextuels;  
uses  
  SysUtils;
var
  couleur: String;
  valide: Boolean;
begin
  WriteLn('Couleurs disponibles : rouge, vert, bleu');

  repeat
    Write('Votre couleur : ');
    ReadLn(couleur);
    couleur := LowerCase(couleur);  // LowerCase (SysUtils) agit sur String, UpCase sur Char seul

    valide := (couleur = 'rouge') or (couleur = 'vert') or (couleur = 'bleu');

    if not valide then
      WriteLn('❌ Couleur non disponible');
  until valide;

  WriteLn('✓ Couleur choisie : ', couleur);
end.
```

## Validation combinée

### Plusieurs critères simultanés

```pascal
program ValidationCombinee;  
var  
  code: String;
  i: Integer;
  nbChiffres, nbLettres: Integer;
  valide: Boolean;
const
  LONGUEUR_CODE = 6;
begin
  WriteLn('Créez un code de ', LONGUEUR_CODE, ' caractères');
  WriteLn('(doit contenir au moins 1 chiffre et 1 lettre)');
  WriteLn;

  repeat
    Write('Code : ');
    ReadLn(code);

    valide := True;
    nbChiffres := 0;
    nbLettres := 0;

    // Vérifier la longueur
    if Length(code) <> LONGUEUR_CODE then
    begin
      WriteLn('❌ Le code doit faire exactement ', LONGUEUR_CODE, ' caractères');
      valide := False;
    end
    else
    begin
      // Compter chiffres et lettres
      for i := 1 to Length(code) do
      begin
        if code[i] in ['0'..'9'] then
          nbChiffres := nbChiffres + 1
        else if code[i] in ['A'..'Z', 'a'..'z'] then
          nbLettres := nbLettres + 1;
      end;

      if nbChiffres = 0 then
      begin
        WriteLn('❌ Le code doit contenir au moins 1 chiffre');
        valide := False;
      end;

      if nbLettres = 0 then
      begin
        WriteLn('❌ Le code doit contenir au moins 1 lettre');
        valide := False;
      end;

      if (nbChiffres + nbLettres) <> LONGUEUR_CODE then
      begin
        WriteLn('❌ Le code ne doit contenir que des chiffres et des lettres');
        valide := False;
      end;
    end;
  until valide;

  WriteLn('✓ Code valide : ', code);
end.
```

### Validation de date

```pascal
program ValidationDate;  
var  
  jour, mois, annee: Integer;
  joursMax: Integer;
  valide: Boolean;
begin
  WriteLn('Entrez une date :');

  // Validation du jour
  repeat
    Write('Jour (1-31) : ');
    ReadLn(jour);
  until (jour >= 1) and (jour <= 31);

  // Validation du mois
  repeat
    Write('Mois (1-12) : ');
    ReadLn(mois);
  until (mois >= 1) and (mois <= 12);

  // Validation de l'année
  repeat
    Write('Année (1900-2100) : ');
    ReadLn(annee);
  until (annee >= 1900) and (annee <= 2100);

  // Validation de cohérence jour/mois
  valide := True;

  case mois of
    1, 3, 5, 7, 8, 10, 12: joursMax := 31;
    4, 6, 9, 11: joursMax := 30;
    2:  // Février
      begin
        // Année bissextile (simplifiée)
        if (annee mod 4 = 0) and ((annee mod 100 <> 0) or (annee mod 400 = 0)) then
          joursMax := 29
        else
          joursMax := 28;
      end;
  else
    joursMax := 31;
  end;

  if jour > joursMax then
  begin
    WriteLn('❌ ERREUR : Ce mois n''a que ', joursMax, ' jours');
    valide := False;
  end;

  if valide then
  begin
    WriteLn('✓ Date valide : ', jour, '/', mois, '/', annee);
    if (mois = 2) and (joursMax = 29) then
      WriteLn('  (', annee, ' est une année bissextile)');
  end;
end.
```

## Exemples pratiques complets

### Formulaire d'inscription

```pascal
program FormulaireInscription;  
uses  
  SysUtils;
var
  nom, prenom, email, telephone: String;
  age: Integer;
  i, posArobase: Integer;
  valide: Boolean;
begin
  WriteLn('═══════════════════════════════');
  WriteLn('   FORMULAIRE D''INSCRIPTION');
  WriteLn('═══════════════════════════════');
  WriteLn;

  // Nom
  repeat
    Write('Nom : ');
    ReadLn(nom);
    nom := Trim(nom);

    if Length(nom) < 2 then
      WriteLn('❌ Le nom doit contenir au moins 2 caractères');
  until Length(nom) >= 2;

  // Prénom
  repeat
    Write('Prénom : ');
    ReadLn(prenom);
    prenom := Trim(prenom);

    if Length(prenom) < 2 then
      WriteLn('❌ Le prénom doit contenir au moins 2 caractères');
  until Length(prenom) >= 2;

  // Âge
  repeat
    Write('Âge : ');
    ReadLn(age);

    if (age < 18) or (age > 100) then
      WriteLn('❌ Vous devez avoir entre 18 et 100 ans');
  until (age >= 18) and (age <= 100);

  // Email
  repeat
    Write('Email : ');
    ReadLn(email);
    email := Trim(email);
    valide := True;

    posArobase := Pos('@', email);
    if (posArobase = 0) or (posArobase = 1) or (posArobase = Length(email)) then
    begin
      WriteLn('❌ Format d''email invalide');
      valide := False;
    end;
  until valide;

  // Téléphone
  repeat
    Write('Téléphone (10 chiffres) : ');
    ReadLn(telephone);
    valide := True;

    if Length(telephone) <> 10 then
    begin
      WriteLn('❌ Le numéro doit contenir 10 chiffres');
      valide := False;
    end
    else
    begin
      for i := 1 to 10 do
      begin
        if not (telephone[i] in ['0'..'9']) then
        begin
          WriteLn('❌ Le numéro ne doit contenir que des chiffres');
          valide := False;
          break;
        end;
      end;
    end;
  until valide;

  // Récapitulatif
  WriteLn;
  WriteLn('═══════════════════════════════');
  WriteLn('   RÉCAPITULATIF');
  WriteLn('═══════════════════════════════');
  WriteLn('Nom : ', nom);
  WriteLn('Prénom : ', prenom);
  WriteLn('Âge : ', age, ' ans');
  WriteLn('Email : ', email);
  WriteLn('Téléphone : ', telephone);
  WriteLn('═══════════════════════════════');
  WriteLn('✓ Inscription validée !');
end.
```

### Système de paiement

```pascal
program SystemePaiement;  
var  
  montant, montantPaye, rendu: Real;
  valide: Boolean;
begin
  WriteLn('═══════════════════════════');
  WriteLn('   SYSTÈME DE PAIEMENT');
  WriteLn('═══════════════════════════');
  WriteLn;

  // Montant de l'achat
  repeat
    Write('Montant de l''achat : ');
    ReadLn(montant);

    if montant <= 0 then
      WriteLn('❌ Le montant doit être positif')
    else if montant > 10000 then
      WriteLn('❌ Montant trop élevé (max 10 000 €)');
  until (montant > 0) and (montant <= 10000);

  WriteLn;
  WriteLn('Montant à payer : ', montant:0:2, ' €');
  WriteLn;

  // Montant payé
  repeat
    Write('Montant payé : ');
    ReadLn(montantPaye);
    valide := True;

    if montantPaye < 0 then
    begin
      WriteLn('❌ Le montant ne peut pas être négatif');
      valide := False;
    end
    else if montantPaye < montant then
    begin
      WriteLn('❌ Montant insuffisant');
      WriteLn('   Il manque ', (montant - montantPaye):0:2, ' €');
      valide := False;
    end;
  until valide;

  // Calcul du rendu
  rendu := montantPaye - montant;

  WriteLn;
  WriteLn('═══════════════════════════');
  WriteLn('Montant payé : ', montantPaye:0:2, ' €');
  if rendu > 0 then
    WriteLn('Rendu : ', rendu:0:2, ' €')
  else
    WriteLn('Paiement exact');
  WriteLn('✓ Transaction terminée');
  WriteLn('═══════════════════════════');
end.
```

### Configuration de profil utilisateur

```pascal
program ConfigurationProfil;  
var  
  pseudo: String;
  avatar: Integer;
  notifications: Char;
  i: Integer;
  valide: Boolean;
begin
  WriteLn('═══════════════════════════════');
  WriteLn('   CONFIGURATION DU PROFIL');
  WriteLn('═══════════════════════════════');
  WriteLn;

  // Pseudo
  repeat
    Write('Pseudo (3-15 caractères, lettres et chiffres uniquement) : ');
    ReadLn(pseudo);
    valide := True;

    if (Length(pseudo) < 3) or (Length(pseudo) > 15) then
    begin
      WriteLn('❌ Le pseudo doit contenir entre 3 et 15 caractères');
      valide := False;
    end
    else
    begin
      for i := 1 to Length(pseudo) do
      begin
        if not (pseudo[i] in ['A'..'Z', 'a'..'z', '0'..'9']) then
        begin
          WriteLn('❌ Le pseudo ne doit contenir que des lettres et chiffres');
          valide := False;
          break;
        end;
      end;
    end;
  until valide;

  // Choix d'avatar
  WriteLn;
  WriteLn('Avatars disponibles :');
  WriteLn('1. 😊 Souriant');
  WriteLn('2. 😎 Cool');
  WriteLn('3. 🤓 Geek');
  WriteLn('4. 🐱 Chat');
  WriteLn('5. 🦊 Renard');
  WriteLn;

  repeat
    Write('Choisissez votre avatar (1-5) : ');
    ReadLn(avatar);

    if (avatar < 1) or (avatar > 5) then
      WriteLn('❌ Veuillez choisir entre 1 et 5');
  until (avatar >= 1) and (avatar <= 5);

  // Notifications
  WriteLn;
  repeat
    Write('Activer les notifications ? (O/N) : ');
    ReadLn(notifications);
    notifications := UpCase(notifications);

    if not (notifications in ['O', 'N']) then
      WriteLn('❌ Répondez par O (Oui) ou N (Non)');
  until notifications in ['O', 'N'];

  // Récapitulatif
  WriteLn;
  WriteLn('═══════════════════════════════');
  WriteLn('   PROFIL CONFIGURÉ');
  WriteLn('═══════════════════════════════');
  WriteLn('Pseudo : ', pseudo);
  Write('Avatar : ');
  case avatar of
    1: WriteLn('😊 Souriant');
    2: WriteLn('😎 Cool');
    3: WriteLn('🤓 Geek');
    4: WriteLn('🐱 Chat');
    5: WriteLn('🦊 Renard');
  end;
  Write('Notifications : ');
  if notifications = 'O' then
    WriteLn('Activées')
  else
    WriteLn('Désactivées');
  WriteLn('═══════════════════════════════');
  WriteLn('✓ Configuration enregistrée !');
end.
```

## Feedback utilisateur

### Messages progressifs

```pascal
// ❌ Pas de feedback
repeat
  ReadLn(valeur);
until (valeur >= 1) and (valeur <= 100);

// ✓ Avec feedback
repeat
  Write('Valeur (1-100) : ');
  ReadLn(valeur);

  if valeur < 1 then
    WriteLn('❌ Trop petit (minimum : 1)')
  else if valeur > 100 then
    WriteLn('❌ Trop grand (maximum : 100)')
  else
    WriteLn('✓ Valeur acceptée');
until (valeur >= 1) and (valeur <= 100);
```

### Compteur de tentatives

```pascal
program CompteurTentatives;  
var  
  code: Integer;
  tentative: Integer;
const
  CODE_SECRET = 1234;
  MAX_TENTATIVES = 3;
begin
  WriteLn('Entrez le code secret :');

  tentative := 0;
  repeat
    tentative := tentative + 1;
    Write('Tentative ', tentative, '/', MAX_TENTATIVES, ' : ');
    ReadLn(code);

    if code <> CODE_SECRET then
    begin
      WriteLn('❌ Code incorrect');
      if tentative < MAX_TENTATIVES then
        WriteLn('   Il vous reste ', MAX_TENTATIVES - tentative, ' tentative(s)');
    end;
  until (code = CODE_SECRET) or (tentative >= MAX_TENTATIVES);

  if code = CODE_SECRET then
    WriteLn('✓ Accès autorisé')
  else
    WriteLn('✗ Accès refusé - Trop de tentatives');
end.
```

## Erreurs courantes

### 1. Validation trop stricte

```pascal
// ❌ Trop strict
if nom <> 'Dupont' then
  WriteLn('Erreur');

// ✓ Validation raisonnable
if Length(nom) < 2 then
  WriteLn('Nom trop court');
```

### 2. Oublier de trim les espaces

```pascal
// ❌ Les espaces comptent
if nom = '' then
  WriteLn('Vide');

// ✓ Suppression des espaces
nom := Trim(nom);  
if nom = '' then  
  WriteLn('Vide');
```

### 3. Validation après utilisation

```pascal
// ❌ Utilise avant de valider
resultat := a / b;  
if b = 0 then  
  WriteLn('Erreur');

// ✓ Valide avant d'utiliser
if b = 0 then
  WriteLn('Erreur')
else
  resultat := a / b;
```

### 4. Messages d'erreur inutiles

```pascal
// ❌ Message inutile
if valeur < 0 then
  WriteLn('Erreur');

// ✓ Message explicatif
if valeur < 0 then
  WriteLn('❌ La valeur doit être positive (vous avez entré ', valeur, ')');
```

### 5. Pas de feedback positif

```pascal
// ❌ Rien quand c'est bon
repeat
  ReadLn(valeur);
  if valeur < 0 then
    WriteLn('Invalide');
until valeur >= 0;

// ✓ Confirmation
repeat
  ReadLn(valeur);
  if valeur < 0 then
    WriteLn('❌ Invalide')
  else
    WriteLn('✓ Valeur acceptée');
until valeur >= 0;
```

## Bonnes pratiques

### 1. Utiliser des constantes

```pascal
const
  AGE_MIN = 0;
  AGE_MAX = 150;
begin
  if (age < AGE_MIN) or (age > AGE_MAX) then
    WriteLn('Âge invalide : ', AGE_MIN, '-', AGE_MAX);
end;
```

### 2. Extraire en fonctions

```pascal
function EstEmailValide(email: String): Boolean;  
begin  
  // Logique de validation
  EstEmailValide := (Pos('@', email) > 0);
end;
```

### 3. Messages clairs et constructifs

```pascal
WriteLn('❌ Mot de passe invalide');  
WriteLn('   • Minimum 8 caractères');  
WriteLn('   • Au moins 1 chiffre');  
WriteLn('   • Au moins 1 lettre');  
```

### 4. Donner des exemples

```pascal
WriteLn('Format : JJ/MM/AAAA');  
WriteLn('Exemple : 25/12/2024');  
```

### 5. Confirmer les saisies valides

```pascal
WriteLn('✓ Email accepté : ', email);  
WriteLn('✓ Téléphone accepté : ', telephone);  
```

## Résumé

La validation des entrées utilisateur est essentielle pour créer des programmes robustes :

### Types de validation
- **Plage** : vérifier les limites min/max
- **Type** : s'assurer du bon type de données
- **Format** : respecter une structure spécifique
- **Cohérence** : logique entre plusieurs données

### Techniques
- **REPEAT-UNTIL** : la plus simple et courante
- **WHILE** : plus flexible, permet de compter
- **Fonctions** : code réutilisable
- **Procédures** : encapsulation complète

### Principes clés
- Ne **jamais** faire confiance aux entrées
- Valider **avant** d'utiliser les données
- Donner des **messages clairs**
- Offrir la **possibilité de réessayer**
- **Confirmer** les saisies valides

### Messages efficaces
- ❌ Indiquer l'erreur
- ℹ️ Expliquer pourquoi
- ✓ Confirmer quand c'est bon
- 💡 Donner des exemples

La validation est la première ligne de défense contre les erreurs. Un programme bien validé = un programme robuste et professionnel !

⏭️ [Débogage pas à pas](/03-structures-controle/09-debogage-pas-a-pas.md)
