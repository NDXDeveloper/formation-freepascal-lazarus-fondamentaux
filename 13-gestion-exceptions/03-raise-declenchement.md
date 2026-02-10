🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.3 Raise et déclenchement

## Introduction

Nous avons vu comment **capturer** les exceptions avec `try-except`. Maintenant, nous allons apprendre à **déclencher** (ou **lever**) nos propres exceptions avec le mot-clé `raise`. C'est un outil puissant qui vous permet de signaler qu'une situation anormale s'est produite dans votre code.

## Le mot-clé Raise

Le mot-clé `raise` permet de déclencher volontairement une exception. On dit qu'on "lève" ou "lance" une exception.

### Syntaxe de base

```pascal
raise ClasseException.Create('Message d''erreur');
```

### Exemple simple

```pascal
procedure DiviserParDeux(nombre: Integer);  
begin  
  if nombre mod 2 <> 0 then
    raise Exception.Create('Le nombre doit être pair !');

  WriteLn('Résultat : ', nombre div 2);
end;
```

Lorsque `raise` est exécuté :
1. L'exécution normale s'arrête immédiatement
2. Une exception est créée avec le message fourni
3. Le programme cherche un bloc `except` pour la gérer
4. Si aucun bloc n'est trouvé, le programme s'arrête avec un message d'erreur

## Pourquoi lever des exceptions ?

### 1. Valider les données

Plutôt que de continuer avec des données invalides, il vaut mieux signaler le problème immédiatement.

```pascal
procedure DefinirAge(age: Integer);  
begin  
  if age < 0 then
    raise Exception.Create('L''âge ne peut pas être négatif');

  if age > 150 then
    raise Exception.Create('L''âge semble irréaliste');

  // Si on arrive ici, l'âge est valide
  WriteLn('Âge défini : ', age);
end;
```

### 2. Signaler l'impossibilité d'effectuer une opération

```pascal
function LireFichierConfiguration: String;  
var  
  f: TextFile;
begin
  AssignFile(f, 'config.ini');

  if not FileExists('config.ini') then
    raise Exception.Create('Fichier de configuration introuvable');

  Reset(f);
  ReadLn(f, Result);
  CloseFile(f);
end;
```

### 3. Vérifier les préconditions

```pascal
procedure RetirerArgent(montant: Double);  
begin  
  if montant <= 0 then
    raise Exception.Create('Le montant doit être positif');

  if montant > SoldeCompte then
    raise Exception.Create('Solde insuffisant');

  // Effectuer le retrait
  SoldeCompte := SoldeCompte - montant;
end;
```

## Les classes d'exceptions standard

FreePascal fournit plusieurs classes d'exceptions prédéfinies. Vous pouvez les utiliser avec `raise` :

### Exception générique

```pascal
raise Exception.Create('Une erreur s''est produite');
```

La classe `Exception` est la classe de base. Utilisez-la quand aucune classe spécifique ne convient.

### Exceptions mathématiques

```pascal
// Division par zéro
raise EDivByZero.Create('Division par zéro détectée');

// Dépassement de capacité
raise EOverflow.Create('Le nombre est trop grand');

// Erreur de calcul
raise EMathError.Create('Opération mathématique invalide');
```

### Exceptions de conversion

```pascal
// Conversion impossible
raise EConvertError.Create('Impossible de convertir "abc" en nombre');
```

### Exceptions d'accès aux données

```pascal
// Index hors limites
raise ERangeError.Create('Index en dehors des limites du tableau');

// Liste vide
raise EListError.Create('Impossible d''accéder à une liste vide');
```

### Exceptions de fichiers et I/O

```pascal
// Erreur de lecture/écriture
raise EInOutError.Create('Erreur lors de l''accès au fichier');

// Erreur de flux
raise EStreamError.Create('Erreur lors de la manipulation du flux');
```

## Lever une exception avec format

Parfois, vous voulez inclure des valeurs dans votre message d'erreur. Utilisez `Format` :

```pascal
procedure VerifierAge(age: Integer);  
begin  
  if (age < 18) or (age > 65) then
    raise Exception.CreateFmt(
      'Âge invalide : %d. Doit être entre 18 et 65.',
      [age]
    );
end;
```

### Exemples avec Format

```pascal
// Plusieurs valeurs
raise Exception.CreateFmt(
  'Division impossible : %d / %d',
  [numerateur, denominateur]
);

// Avec des chaînes
raise Exception.CreateFmt(
  'Fichier "%s" introuvable dans le répertoire "%s"',
  [nomFichier, repertoire]
);

// Mélange de types
raise Exception.CreateFmt(
  'L''utilisateur "%s" a échoué %d fois',
  [nomUtilisateur, nombreEchecs]
);
```

## Re-lever une exception

Parfois, vous voulez capturer une exception, faire quelque chose (comme logger l'erreur), puis la re-lancer pour qu'elle soit gérée à un niveau supérieur.

### Syntaxe : Raise seul

```pascal
try
  // Code à risque
except
  on E: Exception do
  begin
    // Faire quelque chose avec l'exception
    WriteLn('Erreur détectée : ', E.Message);

    // Re-lever la même exception
    raise;
  end;
end;
```

**Important :** Utilisez `raise;` (sans argument) pour re-lever l'exception actuelle.

### Exemple pratique : logging et re-propagation

```pascal
procedure TraiterFichier(const nomFichier: String);  
begin  
  try
    // Traitement du fichier
    LireDonnees(nomFichier);
    AnalyserContenu;
    SauvegarderResultat;
  except
    on E: Exception do
    begin
      // Logger l'erreur pour diagnostic
      LoggerErreur(Format('Erreur dans TraiterFichier(%s): %s',
                          [nomFichier, E.Message]));

      // Re-lever l'exception pour que l'appelant puisse la gérer
      raise;
    end;
  end;
end;
```

## Lever une exception différente

Vous pouvez aussi capturer une exception et en lever une autre, plus appropriée :

```pascal
function ChargerConfiguration: TConfiguration;  
begin  
  try
    Result := TConfiguration.Create;
    Result.LoadFromFile('config.xml');
  except
    on E: EFileNotFoundException do
      raise Exception.Create(
        'Impossible de démarrer l''application : configuration manquante'
      );
    on E: EXMLError do
      raise Exception.Create(
        'Fichier de configuration corrompu, veuillez le réinstaller'
      );
  end;
end;
```

Ceci est utile pour :
- Traduire une erreur technique en message compréhensible pour l'utilisateur
- Masquer les détails d'implémentation
- Fournir un contexte supplémentaire

## Exemple complet : validation d'un formulaire

Voici un exemple réaliste combinant plusieurs concepts :

```pascal
procedure ValiderFormulaireInscription(
  const nom, email: String;
  age: Integer
);
begin
  // Validation du nom
  if Trim(nom) = '' then
    raise Exception.Create('Le nom ne peut pas être vide');

  if Length(nom) < 2 then
    raise Exception.Create('Le nom doit contenir au moins 2 caractères');

  // Validation de l'email
  if Pos('@', email) = 0 then
    raise Exception.Create('L''adresse email est invalide');

  // Validation de l'âge
  if age < 18 then
    raise Exception.CreateFmt(
      'Vous devez avoir au moins 18 ans (âge actuel : %d)',
      [age]
    );

  if age > 120 then
    raise Exception.CreateFmt(
      'L''âge %d semble incorrect',
      [age]
    );

  // Si on arrive ici, tout est valide
  WriteLn('Inscription validée pour ', nom);
end;

// Utilisation
begin
  try
    ValiderFormulaireInscription('', 'test@example.com', 25);
  except
    on E: Exception do
      WriteLn('Erreur de validation : ', E.Message);
  end;
end;
```

**Résultat :**
```
Erreur de validation : Le nom ne peut pas être vide
```

## Quand NE PAS lever d'exception

Les exceptions doivent être réservées aux situations **exceptionnelles**. Ne les utilisez pas pour :

### 1. Le flux de contrôle normal

```pascal
// ✗ MAUVAIS
function TrouverElement(valeur: Integer): Boolean;  
begin  
  try
    // Recherche...
    raise Exception.Create('Pas trouvé');
  except
    Result := False;
  end;
end;

// ✓ BON
function TrouverElement(valeur: Integer): Boolean;  
begin  
  // Recherche...
  Result := False;  // Simple retour de valeur
end;
```

### 2. Les situations prévisibles

```pascal
// ✗ MAUVAIS
function Diviser(a, b: Integer): Integer;  
begin  
  // Lever une exception à chaque fois que b = 0
  if b = 0 then
    raise EDivByZero.Create('Division par zéro');
  Result := a div b;
end;

// ✓ BON
function Diviser(a, b: Integer; out resultat: Integer): Boolean;  
begin  
  if b = 0 then
  begin
    Result := False;  // Retour simple
    Exit;
  end;
  resultat := a div b;
  Result := True;
end;
```

### 3. Les validations simples

Si vous pouvez vérifier une condition simplement, faites-le plutôt que de lever une exception :

```pascal
// Pour les cas simples
if FileExists(nomFichier) then
  TraiterFichier(nomFichier)
else
  WriteLn('Fichier introuvable');
```

## Hiérarchie et cascade d'exceptions

Les exceptions peuvent se propager à travers plusieurs niveaux de procédures :

```pascal
procedure NiveauProfond;  
begin  
  raise Exception.Create('Erreur au niveau profond');
end;

procedure NiveauIntermediaire;  
begin  
  WriteLn('Avant appel niveau profond');
  NiveauProfond;  // Lève une exception
  WriteLn('Après appel - jamais exécuté');
end;

procedure NiveauSuperieur;  
begin  
  try
    WriteLn('Début du traitement');
    NiveauIntermediaire;
    WriteLn('Fin du traitement - jamais exécuté');
  except
    on E: Exception do
      WriteLn('Exception capturée : ', E.Message);
  end;
  WriteLn('Après le try-except');
end;
```

**Résultat :**
```
Début du traitement  
Avant appel niveau profond  
Exception capturée : Erreur au niveau profond  
Après le try-except  
```

## Bonnes pratiques

### 1. Messages d'erreur clairs et utiles

```pascal
// ✗ MAUVAIS
raise Exception.Create('Erreur');

// ✓ BON
raise Exception.Create('Impossible d''ouvrir le fichier "donnees.txt" : vérifiez qu''il existe et que vous avez les droits d''accès');
```

### 2. Utiliser la classe d'exception appropriée

```pascal
// ✗ MOINS BON
if diviseur = 0 then
  raise Exception.Create('Division par zéro');

// ✓ MIEUX
if diviseur = 0 then
  raise EDivByZero.Create('Division par zéro');
```

### 3. Lever tôt, capturer tard

Levez les exceptions dès que vous détectez un problème, mais ne les capturez que là où vous pouvez faire quelque chose d'utile.

```pascal
// ✓ BON
function OuvrirFichier(const nom: String): TextFile;  
begin  
  if not FileExists(nom) then
    raise Exception.Create('Fichier introuvable : ' + nom);  // Lever tôt

  // ...
end;

procedure TraiterDonnees;  
begin  
  try
    // ... appels multiples
  except
    // Capturer tard, au niveau où on peut gérer
    on E: Exception do
      AfficherMessageUtilisateur(E.Message);
  end;
end;
```

### 4. Ne pas masquer les informations

```pascal
// ✗ MAUVAIS
try
  TraiterFichier(nom);
except
  raise Exception.Create('Erreur');  // Perte d'information !
end;

// ✓ BON
try
  TraiterFichier(nom);
except
  on E: Exception do
    raise Exception.Create('Erreur lors du traitement de ' + nom + ': ' + E.Message);
end;
```

## Schéma récapitulatif du flux

```
Fonction A
   │
   ├─► Appelle Fonction B
   │      │
   │      ├─► Appelle Fonction C
   │      │      │
   │      │      └─► raise Exception ──────┐
   │      │                                │
   │      │      (pas de try-except)       │
   │      └────────────────────────────────┤
   │                                       │
   │      (pas de try-except)              │
   └───────────────────────────────────────┤
                                           │
          try-except dans le programme     │
          principal capture ◄──────────────┘
```

## Conclusion

Le mot-clé `raise` vous permet de créer des programmes robustes qui détectent et signalent les erreurs de manière explicite :

- Utilisez `raise` pour signaler les situations anormales
- Choisissez des classes d'exceptions appropriées
- Fournissez des messages clairs et informatifs
- Re-levez les exceptions avec `raise;` quand nécessaire
- N'utilisez pas les exceptions pour le flux de contrôle normal

Combinez `raise` avec les blocs `try-except` que nous avons vus précédemment, et vous aurez tous les outils pour gérer efficacement les erreurs dans vos programmes.

---

**Points clés à retenir :**

- `raise` déclenche volontairement une exception
- Syntaxe : `raise ClasseException.Create('Message')`
- Utilisez `CreateFmt` pour inclure des valeurs dans le message
- `raise;` (sans argument) re-lève l'exception actuelle
- Les exceptions doivent être réservées aux situations exceptionnelles
- Toujours fournir des messages d'erreur clairs et utiles
- Choisir la classe d'exception la plus spécifique possible
- Les exceptions se propagent automatiquement vers les niveaux supérieurs

⏭️ [Hiérarchie des exceptions](/13-gestion-exceptions/04-hierarchie-exceptions.md)
