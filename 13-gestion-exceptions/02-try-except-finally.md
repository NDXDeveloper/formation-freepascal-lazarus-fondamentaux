🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.2 Try-except-finally

## Introduction

Maintenant que nous comprenons ce qu'est une exception, voyons comment les gérer concrètement en Pascal. Le langage nous offre trois mots-clés puissants : **try**, **except** et **finally**. Ces mots-clés nous permettent de contrôler ce qui se passe quand une erreur survient dans notre programme.

## Le bloc Try-Except

### Syntaxe de base

La structure la plus simple pour gérer les exceptions est le bloc `try-except` :

```pascal
try
  // Code qui peut causer une erreur
except
  // Code exécuté si une erreur se produit
end;
```

### Fonctionnement

1. Le programme essaie d'exécuter le code dans la section `try`
2. Si tout se passe bien, la section `except` est ignorée
3. Si une exception se produit, l'exécution saute immédiatement à la section `except`

### Premier exemple concret

```pascal
var
  nombre: Integer;
  texte: String;
begin
  texte := 'abc';

  try
    nombre := StrToInt(texte);  // Ceci va causer une erreur !
    WriteLn('Le nombre est : ', nombre);
  except
    WriteLn('Erreur : impossible de convertir le texte en nombre');
  end;

  WriteLn('Le programme continue normalement');
end;
```

**Résultat :**
```
Erreur : impossible de convertir le texte en nombre  
Le programme continue normalement  
```

Sans le `try-except`, le programme se serait arrêté brutalement avec un message d'erreur système.

## Capturer des types d'exceptions spécifiques

Toutes les exceptions ne sont pas identiques. FreePascal nous permet de réagir différemment selon le type d'erreur.

### Syntaxe avec gestion spécifique

```pascal
try
  // Code à risque
except
  on E: ETypeException1 do
    // Gestion de ce type d'exception
  on E: ETypeException2 do
    // Gestion d'un autre type
  else
    // Gestion de toutes les autres exceptions
end;
```

### Exemple avec plusieurs types d'exceptions

```pascal
var
  nombre, diviseur, resultat: Integer;
  texte: String;
begin
  try
    Write('Entrez un nombre : ');
    ReadLn(texte);
    nombre := StrToInt(texte);  // Peut lever EConvertError

    Write('Diviser par : ');
    ReadLn(texte);
    diviseur := StrToInt(texte);  // Peut lever EConvertError

    resultat := nombre div diviseur;  // Peut lever EDivByZero
    WriteLn('Résultat : ', resultat);

  except
    on E: EConvertError do
      WriteLn('Erreur de conversion : ', E.Message);
    on E: EDivByZero do
      WriteLn('Erreur : division par zéro impossible !');
    else
      WriteLn('Une erreur inattendue s''est produite');
  end;
end;
```

### Accès au message d'erreur

Remarquez la variable `E` dans `on E: EConvertError do`. Cette variable contient des informations sur l'exception :

- `E.Message` : le message décrivant l'erreur
- `E.ClassName` : le nom de la classe de l'exception

## Le bloc Try-Finally

Le bloc `try-finally` a un objectif différent : il garantit que du code sera exécuté, **qu'une erreur se produise ou non**.

### Syntaxe

```pascal
try
  // Code principal
finally
  // Code exécuté dans TOUS les cas
end;
```

### Quand utiliser Try-Finally ?

Le `try-finally` est essentiel pour **libérer des ressources** : fichiers, mémoire, connexions réseau, etc.

### Exemple : gestion de fichier

```pascal
var
  f: TextFile;
begin
  AssignFile(f, 'donnees.txt');
  Reset(f);  // Ouvre le fichier

  try
    // Lecture du fichier
    while not EOF(f) do
    begin
      // Traitement ligne par ligne
    end;
  finally
    CloseFile(f);  // Ferme le fichier DANS TOUS LES CAS
  end;
end;
```

Même si une erreur se produit pendant la lecture, le fichier sera fermé correctement grâce au `finally`.

### Exemple : création d'objet

```pascal
var
  liste: TStringList;
begin
  liste := TStringList.Create;
  try
    liste.Add('Ligne 1');
    liste.Add('Ligne 2');
    liste.SaveToFile('sortie.txt');
  finally
    liste.Free;  // Libère la mémoire DANS TOUS LES CAS
  end;
end;
```

**Important :** Sans le `finally`, si une erreur survient entre `Create` et `Free`, la mémoire ne serait jamais libérée (fuite mémoire).

## Combiner Try-Except et Try-Finally

Parfois, nous avons besoin des deux : gérer les erreurs ET libérer des ressources. Il existe deux façons de le faire.

### Méthode 1 : Imbrication

```pascal
var
  f: TextFile;
begin
  AssignFile(f, 'donnees.txt');

  try
    Reset(f);
    try
      // Lecture et traitement
      while not EOF(f) do
      begin
        // ...
      end;
    finally
      CloseFile(f);  // Toujours fermer le fichier
    end;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;
end;
```

### Méthode 2 : Try-Finally puis Try-Except (englobant)

On peut aussi placer le `try-finally` à l'intérieur d'un `try-except` englobant :

```pascal
var
  f: TextFile;
  ligne: String;
begin
  try
    AssignFile(f, 'donnees.txt');
    Reset(f);

    try
      while not EOF(f) do
      begin
        ReadLn(f, ligne);
        WriteLn(ligne);
      end;
    finally
      CloseFile(f);  // Toujours fermer le fichier
    end;
  except
    on E: Exception do
      WriteLn('Erreur de lecture : ', E.Message);
  end;
end;
```

**Important :** En FreePascal, on ne peut **pas** combiner `except` et `finally` dans un seul bloc `try`. Il faut toujours les imbriquer. La forme `try...except...finally...end` n'existe pas.

**Ordre d'exécution :**
1. Le code dans le `try` interne s'exécute
2. Le bloc `finally` s'exécute dans tous les cas (libération des ressources)
3. Si une erreur survient, le bloc `except` externe la capture

## Exemple complet : téléchargement de fichier

Voici un exemple réaliste combinant tout ce que nous avons vu :

```pascal
procedure TelechargerFichier(const URL, NomFichier: String);  
var  
  client: TFPHttpClient;
  flux: TFileStream;
begin
  client := TFPHttpClient.Create(nil);
  flux := nil;

  try
    try
      flux := TFileStream.Create(NomFichier, fmCreate);
      client.Get(URL, flux);
      WriteLn('Téléchargement réussi !');
    except
      on E: EHTTPClient do
        WriteLn('Erreur réseau : ', E.Message);
      on E: EStreamError do
        WriteLn('Erreur d''écriture du fichier : ', E.Message);
      else
        WriteLn('Erreur inattendue durant le téléchargement');
    end;
  finally
    flux.Free;    // Libère le flux de fichier
    client.Free;  // Libère le client HTTP
  end;
end;
```

## Propagation des exceptions

Si vous ne gérez pas une exception, elle "remonte" automatiquement au niveau supérieur.

```pascal
procedure NiveauBas;  
begin  
  // Lève une exception, pas de try-except
  raise Exception.Create('Erreur au niveau bas');
end;

procedure NiveauMoyen;  
begin  
  // Appelle NiveauBas, pas de try-except
  NiveauBas;
end;

procedure NiveauHaut;  
begin  
  try
    NiveauMoyen;
  except
    on E: Exception do
      WriteLn('Exception capturée en haut : ', E.Message);
  end;
end;
```

L'exception traverse `NiveauMoyen` et est finalement capturée dans `NiveauHaut`.

## Bonnes pratiques

### 1. Toujours libérer les ressources avec Finally

```pascal
// ✓ BON
objet := TMonObjet.Create;  
try  
  objet.FaireTravail;
finally
  objet.Free;
end;

// ✗ MAUVAIS
objet := TMonObjet.Create;  
objet.FaireTravail;  
objet.Free;  // Ne sera pas appelé si une erreur survient  
```

### 2. Capturer les exceptions spécifiques d'abord

```pascal
// ✓ BON
try
  // ...
except
  on E: EDivByZero do
    // Gestion spécifique
  on E: Exception do
    // Gestion générale
end;

// ✗ MAUVAIS
try
  // ...
except
  on E: Exception do
    // Capture TOUT, les cas spécifiques ne seront jamais atteints
  on E: EDivByZero do
    // Ce code ne sera jamais exécuté !
end;
```

### 3. Ne pas capturer silencieusement

```pascal
// ✗ TRÈS MAUVAIS
try
  // Code à risque
except
  // Rien ! L'erreur est ignorée
end;

// ✓ BON
try
  // Code à risque
except
  on E: Exception do
  begin
    LoggerErreur(E.Message);  // Au moins enregistrer l'erreur
    // ou informer l'utilisateur
  end;
end;
```

### 4. Finally s'exécute même avec Exit

```pascal
procedure Test;  
begin  
  WriteLn('Début');
  try
    WriteLn('Dans try');
    Exit;  // Sort de la procédure
    WriteLn('Après Exit - jamais exécuté');
  finally
    WriteLn('Dans finally - TOUJOURS exécuté');
  end;
  WriteLn('Fin - jamais exécuté');
end;
```

**Résultat :**
```
Début  
Dans try  
Dans finally - TOUJOURS exécuté  
```

## Différences entre Except et Finally

| Aspect | Except | Finally |
|--------|--------|---------|
| Objectif | Gérer les erreurs | Libérer les ressources |
| Exécution | Seulement si erreur | Toujours |
| Peut capturer l'exception | Oui | Non |
| Typique pour | Validation, logging | Fermeture fichiers, Free |

## Schéma récapitulatif

```
Début du programme
    │
    ├─► try
    │     │
    │     ├─► Code normal
    │     │     │
    │     │     ├─► Pas d'erreur ──────────┐
    │     │     │                          │
    │     │     └─► ERREUR !               │
    │     │              │                 │
    │     │              ▼                 │
    │     └─► except                       │
    │           │                          │
    │           └─► Gestion erreur ────────┤
    │                                      │
    └─► finally ◄─────────────────────────┘
          │
          └─► Nettoyage (toujours exécuté)
               │
               ▼
         Suite du programme
```

## Conclusion

Les blocs `try-except-finally` sont des outils essentiels pour créer des programmes robustes :

- **try-except** : pour gérer les erreurs et empêcher le programme de crasher
- **try-finally** : pour garantir la libération des ressources
- **try-except-finally** : pour combiner gestion d'erreurs et nettoyage

Avec ces structures, vous pouvez anticiper les problèmes, informer l'utilisateur de manière claire, et garantir que votre programme ne laisse pas de ressources non libérées.

---

**Points clés à retenir :**

- `try-except` capture et gère les exceptions
- `try-finally` garantit l'exécution de code de nettoyage
- Le bloc `finally` s'exécute TOUJOURS (erreur ou non, même avec Exit)
- On peut capturer des types d'exceptions spécifiques avec `on E: TypeException do`
- Toujours libérer les ressources (fichiers, objets) dans un bloc `finally`
- Une exception non capturée remonte aux niveaux supérieurs
- Ne jamais ignorer silencieusement les exceptions

⏭️ [Raise et déclenchement](/13-gestion-exceptions/03-raise-declenchement.md)
