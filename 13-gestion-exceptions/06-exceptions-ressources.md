🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.6 Exceptions et ressources

## Introduction

L'un des aspects les plus critiques de la gestion des exceptions est de s'assurer que les **ressources** sont correctement libérées, même quand une erreur survient. Une mauvaise gestion peut entraîner des fuites mémoire, des fichiers non fermés, des connexions réseau bloquées... bref, des problèmes sérieux pour votre application.

## Qu'est-ce qu'une ressource ?

Une **ressource** est un élément dont votre programme a besoin temporairement et qu'il doit libérer après utilisation.

### Les types de ressources courants

#### 1. Fichiers
```pascal
var
  f: TextFile;
begin
  AssignFile(f, 'donnees.txt');
  Reset(f);      // Ouvre le fichier (acquiert la ressource)
  // ... utilisation
  CloseFile(f);  // Ferme le fichier (libère la ressource)
end;
```

#### 2. Objets et mémoire
```pascal
var
  liste: TStringList;
begin
  liste := TStringList.Create;  // Alloue la mémoire (acquiert)
  // ... utilisation
  liste.Free;                    // Libère la mémoire (libère)
end;
```

#### 3. Connexions réseau
```pascal
var
  client: TFPHttpClient;
begin
  client := TFPHttpClient.Create(nil);  // Crée la connexion
  // ... utilisation
  client.Free;                          // Ferme la connexion
end;
```

#### 4. Connexions base de données
```pascal
var
  connexion: TSQLConnection;
begin
  connexion := TSQLConnection.Create(nil);
  connexion.Open;     // Ouvre la connexion
  // ... requêtes
  connexion.Close;    // Ferme la connexion
  connexion.Free;     // Libère l'objet
end;
```

#### 5. Verrous et sections critiques
```pascal
var
  section: TCriticalSection;
begin
  section.Enter;    // Acquiert le verrou
  // ... code protégé
  section.Leave;    // Libère le verrou
end;
```

### Pourquoi libérer les ressources ?

Chaque ressource non libérée est un problème :

| Ressource | Conséquence si non libérée |
|-----------|----------------------------|
| Fichier | Fichier bloqué, impossible à supprimer ou modifier |
| Mémoire | Fuite mémoire, crash si accumulation |
| Connexion DB | Épuisement du pool de connexions |
| Connexion réseau | Sockets bloqués, limite système atteinte |
| Verrous | Deadlock, blocage de l'application |

## Le problème : exceptions et ressources

Que se passe-t-il si une exception survient avant la libération ?

### Exemple du problème

```pascal
procedure LireFichier(const nom: String);
var
  f: TextFile;
  ligne: String;
begin
  AssignFile(f, nom);
  Reset(f);

  ReadLn(f, ligne);
  ProcesserLigne(ligne);  // ⚠️ Si une exception survient ici...

  CloseFile(f);  // ⚠️ ...cette ligne ne sera JAMAIS exécutée !
end;
```

**Problème :** Si `ProcesserLigne` lève une exception, le fichier reste ouvert. C'est une **fuite de ressource**.

### Visualisation du problème

```
Début de la procédure
    │
    ├─► Ouvrir le fichier (ressource acquise)
    │
    ├─► Lire les données
    │
    ├─► Traiter les données ──► ⚠️ EXCEPTION !
    │                              │
    │                              └─► Sort immédiatement
    │
    └─► Fermer le fichier ◄─────────── ✗ Jamais atteint !
```

## La solution : Try-Finally

Le bloc `try-finally` **garantit** que le code de libération sera exécuté, qu'une erreur survienne ou non.

### Pattern fondamental

```pascal
Ressource := AcquerirRessource;
try
  // Utilisation de la ressource
finally
  LibererRessource(Ressource);
end;
```

### Application au fichier

```pascal
procedure LireFichierCorrect(const nom: String);
var
  f: TextFile;
  ligne: String;
begin
  AssignFile(f, nom);
  Reset(f);

  try
    ReadLn(f, ligne);
    ProcesserLigne(ligne);  // Même si exception ici...
  finally
    CloseFile(f);  // ...le fichier sera TOUJOURS fermé !
  end;
end;
```

### Visualisation de la solution

```
Début de la procédure
    │
    ├─► Ouvrir le fichier (ressource acquise)
    │
    ├─► try
    │     │
    │     ├─► Lire les données
    │     │
    │     ├─► Traiter les données ──► ⚠️ EXCEPTION !
    │     │                              │
    │     └────────────────────┐         │
    │                          ▼         │
    └─► finally ◄──────────────┴─────────┘
          │
          └─► Fermer le fichier ✓ TOUJOURS exécuté
```

## Exemples pratiques

### Exemple 1 : Gestion d'objet

```pascal
procedure TraiterDonnees;
var
  liste: TStringList;
begin
  liste := TStringList.Create;
  try
    liste.LoadFromFile('donnees.txt');
    liste.Sort;
    // Traitement...
    AfficherListe(liste);  // Peut lever une exception
  finally
    liste.Free;  // Libère TOUJOURS la mémoire
  end;
end;
```

**Important :** `Free` vérifie si l'objet est `nil` avant de libérer, donc c'est sûr même si la création a échoué.

### Exemple 2 : Fichier avec exception

```pascal
procedure CopierFichier(const source, destination: String);
var
  fSource, fDest: TextFile;
  ligne: String;
begin
  AssignFile(fSource, source);
  Reset(fSource);

  try
    AssignFile(fDest, destination);
    Rewrite(fDest);

    try
      while not EOF(fSource) do
      begin
        ReadLn(fSource, ligne);
        WriteLn(fDest, ligne);  // Peut échouer (disque plein)
      end;
    finally
      CloseFile(fDest);  // Ferme destination
    end;
  finally
    CloseFile(fSource);  // Ferme source
  end;
end;
```

**Note :** Nous utilisons deux `try-finally` imbriqués car nous avons deux ressources indépendantes.

### Exemple 3 : Connexion base de données

```pascal
procedure ExecuterRequete(const SQL: String);
var
  connexion: TSQLConnection;
  requete: TSQLQuery;
begin
  connexion := TSQLConnection.Create(nil);
  try
    connexion.DatabaseName := 'mabase';
    connexion.Open;

    requete := TSQLQuery.Create(nil);
    try
      requete.Database := connexion;
      requete.SQL.Text := SQL;
      requete.ExecSQL;  // Peut lever une exception
    finally
      requete.Free;
    end;
  finally
    if connexion.Connected then
      connexion.Close;
    connexion.Free;
  end;
end;
```

### Exemple 4 : Téléchargement HTTP

```pascal
procedure TelechargerFichier(const URL, fichierLocal: String);
var
  client: TFPHttpClient;
  flux: TFileStream;
begin
  client := TFPHttpClient.Create(nil);
  try
    flux := TFileStream.Create(fichierLocal, fmCreate);
    try
      client.Get(URL, flux);  // Peut échouer (réseau, serveur...)
      WriteLn('Téléchargement réussi');
    finally
      flux.Free;  // Ferme et libère le fichier
    end;
  finally
    client.Free;  // Libère le client HTTP
  end;
end;
```

## Combiner Try-Except et Try-Finally

Souvent, vous voulez à la fois gérer les erreurs ET libérer les ressources.

### Méthode 1 : Try-Finally puis Try-Except

```pascal
procedure TraiterAvecGestion;
var
  liste: TStringList;
begin
  try
    liste := TStringList.Create;
    try
      liste.LoadFromFile('donnees.txt');
      ProcesserListe(liste);
    finally
      liste.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;
end;
```

**Avantage :** Séparation claire entre libération et gestion d'erreur.

### Méthode 2 : Try-Except englobant Try-Finally

On peut aussi placer le `try-except` à l'extérieur :

```pascal
procedure TraiterAvecGestionCombinee;
var
  liste: TStringList;
begin
  try
    liste := TStringList.Create;
    try
      liste.LoadFromFile('donnees.txt');
      ProcesserListe(liste);
    finally
      liste.Free;  // Libération TOUJOURS exécutée
    end;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;
end;
```

**Important :** En FreePascal, on ne peut **pas** combiner `except` et `finally` dans un seul bloc `try`. La forme `try...except...finally...end` n'existe pas. Il faut toujours imbriquer deux blocs séparés.

### Ordre d'exécution avec imbrication

```
1. Code dans try interne s'exécute
   │
   ├─► Pas d'erreur ─────┐
   │                     │
   └─► Erreur            │
                         │
                         ▼
   2. Bloc finally (TOUJOURS)
         │
         ├─► Si erreur
         │     │
         │     ▼
         │   3. Bloc except (externe)
         │     │
         └─────┤
               ▼
   4. Suite du programme
```

## Le piège de l'initialisation

Un piège courant est d'initialiser la variable **après** le `try` :

### ✗ INCORRECT

```pascal
procedure MauvaisExemple;
var
  liste: TStringList;
begin
  try
    liste := TStringList.Create;  // ⚠️ Si erreur ici, finally sera quand même appelé !
    // ...
  finally
    liste.Free;  // ⚠️ Peut crasher si liste n'a pas été créée !
  end;
end;
```

**Problème :** Si `Create` échoue, `liste` n'est pas initialisée mais `finally` essaiera quand même de la libérer.

### ✓ CORRECT : Initialiser avant Try

```pascal
procedure BonExemple;
var
  liste: TStringList;
begin
  liste := nil;  // ✓ Initialisation à nil
  try
    liste := TStringList.Create;
    // ...
  finally
    liste.Free;  // ✓ Free vérifie si nil avant de libérer
  end;
end;
```

Ou encore mieux :

```pascal
procedure MeilleurExemple;
var
  liste: TStringList;
begin
  liste := TStringList.Create;  // ✓ Création AVANT try
  try
    // ... utilisation
  finally
    liste.Free;  // ✓ Toujours valide
  end;
end;
```

## Gestion de multiples ressources

Quand vous avez plusieurs ressources à gérer, il y a plusieurs approches.

### Approche 1 : Try-Finally imbriqués

```pascal
procedure GererMultiplesRessources;
var
  ressource1, ressource2, ressource3: TObject;
begin
  ressource1 := TObject.Create;
  try
    ressource2 := TObject.Create;
    try
      ressource3 := TObject.Create;
      try
        // Utilisation des trois ressources
      finally
        ressource3.Free;
      end;
    finally
      ressource2.Free;
    end;
  finally
    ressource1.Free;
  end;
end;
```

**Avantage :** Chaque ressource est protégée individuellement.
**Inconvénient :** Code profondément imbriqué, moins lisible.

### Approche 2 : Initialisation puis Try unique

```pascal
procedure GererMultiplesRessourcesMieux;
var
  ressource1, ressource2, ressource3: TObject;
begin
  ressource1 := nil;
  ressource2 := nil;
  ressource3 := nil;

  try
    ressource1 := TObject.Create;
    ressource2 := TObject.Create;
    ressource3 := TObject.Create;

    // Utilisation des ressources
  finally
    ressource3.Free;
    ressource2.Free;
    ressource1.Free;  // Libération dans l'ordre inverse
  end;
end;
```

**Avantage :** Plus lisible, un seul niveau d'imbrication.
**Note :** Si `Create` de ressource2 échoue, ressource1 sera quand même libérée dans `finally`.

### Approche 3 : FreeAndNil

`FreeAndNil` libère un objet et met la variable à `nil` :

```pascal
procedure AvecFreeAndNil;
var
  ressource1, ressource2: TObject;
begin
  ressource1 := nil;
  ressource2 := nil;

  try
    ressource1 := TObject.Create;
    ressource2 := TObject.Create;
    // Utilisation
  finally
    FreeAndNil(ressource2);
    FreeAndNil(ressource1);
  end;
end;
```

**Avantage :** Protection supplémentaire contre les doubles libérations.

## Pattern avec vérification d'existence

Pour les ressources qui peuvent ne pas exister :

```pascal
procedure TraiterFichierOptional(const nom: String);
var
  f: TextFile;
  ligne: String;
  fichierOuvert: Boolean;
begin
  fichierOuvert := False;

  if FileExists(nom) then
  begin
    AssignFile(f, nom);
    Reset(f);
    fichierOuvert := True;
  end;

  try
    if fichierOuvert then
    begin
      // Traiter le fichier
      while not EOF(f) do
      begin
        ReadLn(f, ligne);
        ProcesserLigne(ligne);
      end;
    end
    else
      WriteLn('Fichier non trouvé, traitement par défaut');
  finally
    if fichierOuvert then
      CloseFile(f);
  end;
end;
```

## Les ressources automatiques : interfaces

FreePascal supporte les interfaces avec comptage de références, qui libèrent automatiquement les ressources :

```pascal
type
  IMonInterface = interface
    procedure FaireTravail;
  end;

  TMonObjet = class(TInterfacedObject, IMonInterface)
    procedure FaireTravail;
  end;

procedure UtiliserInterface;
var
  obj: IMonInterface;
begin
  obj := TMonObjet.Create;
  obj.FaireTravail;
  // Pas besoin de Free ! L'interface se libère automatiquement
end;
```

**Avantage :** Pas de `try-finally` nécessaire.
**Note :** Sujet avancé, nous verrons les interfaces en détail dans un autre chapitre.

## Erreurs courantes

### Erreur 1 : Oublier le Finally

```pascal
// ✗ MAUVAIS
var
  liste: TStringList;
begin
  liste := TStringList.Create;
  try
    liste.LoadFromFile('donnees.txt');
  except
    on E: Exception do
      WriteLn('Erreur');
  end;
  liste.Free;  // ⚠️ Ne sera pas appelé si exception non capturée !
end;
```

### Erreur 2 : Free dans le Try

```pascal
// ✗ MAUVAIS
var
  liste: TStringList;
begin
  try
    liste := TStringList.Create;
    liste.LoadFromFile('donnees.txt');
    liste.Free;  // ⚠️ Si erreur avant, Free n'est pas appelé !
  except
    WriteLn('Erreur');
  end;
end;
```

### Erreur 3 : Double libération

```pascal
// ✗ MAUVAIS
var
  liste: TStringList;
begin
  liste := TStringList.Create;
  try
    liste.LoadFromFile('donnees.txt');
    liste.Free;  // ⚠️ Première libération
  finally
    liste.Free;  // ⚠️ Deuxième libération = CRASH !
  end;
end;
```

**Solution :** Libérez seulement dans le `finally`.

## Bonnes pratiques

### 1. Toujours utiliser Try-Finally pour les ressources

```pascal
// ✓ BONNE PRATIQUE
Ressource := Creer;
try
  Utiliser(Ressource);
finally
  Liberer(Ressource);
end;
```

### 2. Initialiser les variables à nil

```pascal
// ✓ BONNE PRATIQUE
var
  obj: TObject;
begin
  obj := nil;
  try
    obj := TObject.Create;
    // ...
  finally
    obj.Free;  // Free vérifie nil automatiquement
  end;
end;
```

### 3. Libérer dans l'ordre inverse de création

```pascal
// ✓ BONNE PRATIQUE
obj1 := nil;
obj2 := nil;
try
  obj1 := TObject1.Create;  // Créé en premier
  obj2 := TObject2.Create;  // Créé en second
  // Utilisation
finally
  obj2.Free;  // Libéré en premier
  obj1.Free;  // Libéré en second
end;
```

**Note :** En initialisant à `nil` avant le `try`, si la création de `obj2` échoue, `obj1` sera quand même libéré dans le `finally`.

### 4. Documenter la propriété des ressources

```pascal
/// Crée et retourne un nouvel objet
/// L'appelant est responsable de libérer l'objet avec Free
function CreerObjet: TMonObjet;
begin
  Result := TMonObjet.Create;
end;
```

### 5. Utiliser FreeAndNil pour plus de sécurité

```pascal
// ✓ BONNE PRATIQUE
var
  obj: TObject;
begin
  obj := TObject.Create;
  try
    // Utilisation
  finally
    FreeAndNil(obj);  // Libère ET met à nil
  end;
  // obj est maintenant nil, pas de risque d'utilisation ultérieure
end;
```

## Checklist de sécurité

Avant de valider votre code, vérifiez :

- [ ] Chaque `Create` a un `Free` correspondant dans un `finally`
- [ ] Chaque fichier ouvert est fermé dans un `finally`
- [ ] Chaque connexion ouverte est fermée dans un `finally`
- [ ] Les variables sont initialisées avant le `try`
- [ ] Les ressources sont libérées dans l'ordre inverse de création
- [ ] Aucune double libération n'est possible
- [ ] Le code fonctionne même si une exception survient à n'importe quel moment

## Exemple complet : application robuste

Voici un exemple complet montrant toutes les bonnes pratiques :

```pascal
procedure TraiterFichierComplet(const nomFichier: String);
var
  fichier: TextFile;
  ligne: String;
  liste: TStringList;
  connexion: TSQLConnection;
  fichierOuvert: Boolean;
begin
  // Initialisation
  liste := nil;
  connexion := nil;
  fichierOuvert := False;

  try
    // Création des ressources
    liste := TStringList.Create;
    connexion := TSQLConnection.Create(nil);

    // Ouverture du fichier
    if FileExists(nomFichier) then
    begin
      AssignFile(fichier, nomFichier);
      Reset(fichier);
      fichierOuvert := True;

      // Lecture
      while not EOF(fichier) do
      begin
        ReadLn(fichier, ligne);
        liste.Add(ligne);
      end;
    end;

    // Traitement avec base de données
    if liste.Count > 0 then
    begin
      connexion.DatabaseName := 'mabase';
      connexion.Open;

      // ... traitement

      WriteLn('Traitement terminé : ', liste.Count, ' lignes');
    end;

  except
    on E: EInOutError do
      WriteLn('Erreur fichier : ', E.Message);
    on E: EDatabaseError do
      WriteLn('Erreur base de données : ', E.Message);
    on E: Exception do
      WriteLn('Erreur inattendue : ', E.Message);
  end;

  // Libération dans l'ordre inverse (dans un bloc finally séparé)
  if fichierOuvert then
    CloseFile(fichier);

  if Assigned(connexion) and connexion.Connected then
    connexion.Close;

  FreeAndNil(connexion);
  FreeAndNil(liste);
end;
```

**Note :** En FreePascal, `except` et `finally` ne peuvent pas être dans le même bloc `try`. Pour combiner les deux, il faut imbriquer un `try-finally` à l'intérieur du `try-except`, ou gérer la libération après le bloc `except` (comme ici, puisque les exceptions sont capturées et ne se propagent pas).

## Conclusion

La gestion correcte des ressources en présence d'exceptions est **cruciale** pour créer des applications stables et fiables. Les principes clés sont :

1. **Toujours** utiliser `try-finally` pour les ressources
2. Créer les ressources **avant** le `try` ou initialiser à `nil`
3. Libérer les ressources dans l'ordre **inverse** de création
4. Ne **jamais** oublier de libérer une ressource
5. Utiliser `FreeAndNil` pour plus de sécurité

En suivant ces règles, vous éviterez les fuites mémoire, les fichiers bloqués et les connexions orphelines qui empoisonnent tant d'applications.

---

**Points clés à retenir :**

- Une ressource doit toujours être libérée après utilisation
- `try-finally` garantit la libération même en cas d'exception
- Le bloc `finally` s'exécute TOUJOURS (erreur ou pas)
- Initialisez les variables à `nil` avant le `try` pour plus de sécurité
- `Free` vérifie automatiquement si l'objet est `nil`
- `FreeAndNil` libère et met à `nil` en une seule opération
- Libérez les ressources dans l'ordre inverse de leur création
- Chaque ouverture de fichier, création d'objet ou connexion nécessite un `try-finally`

⏭️ [Bonnes pratiques](/13-gestion-exceptions/07-bonnes-pratiques.md)
