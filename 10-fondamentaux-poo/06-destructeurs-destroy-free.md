🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.6 Destructeurs (Destroy, Free)

## Qu'est-ce qu'un destructeur ?

Un **destructeur** est une méthode spéciale qui est appelée lors de la **destruction** d'un objet. Son rôle principal est de **libérer les ressources** utilisées par l'objet avant qu'il ne soit supprimé de la mémoire.

**Analogie :** Quand vous quittez un hôtel, vous rendez la clé de chambre, vous réglez la note, et vous libérez la chambre. Le destructeur fait la même chose pour un objet : il "range" et "libère" tout ce qui doit l'être avant que l'objet disparaisse.

## Pourquoi a-t-on besoin d'un destructeur ?

### Le problème : les fuites mémoire

Quand vous créez un objet avec `Create`, de la mémoire est allouée. Si vous ne libérez pas cette mémoire, elle reste occupée même si vous n'utilisez plus l'objet. C'est ce qu'on appelle une **fuite mémoire** (memory leak).

```pascal
var
  P: TPersonne;
begin
  P := TPersonne.Create;
  P.DefinirNom('Alice');
  // ✗ PROBLÈME : P n'est pas libéré
  // La mémoire reste occupée jusqu'à la fin du programme
end;
```

### La solution : libérer la mémoire

Il faut appeler la méthode de destruction pour libérer la mémoire :

```pascal
var
  P: TPersonne;
begin
  P := TPersonne.Create;
  P.DefinirNom('Alice');
  P.Free;  // ✓ La mémoire est libérée
end;
```

## Destroy vs Free : quelle différence ?

### Destroy

`Destroy` est le **destructeur réel**. C'est une méthode que vous pouvez redéfinir dans votre classe pour effectuer un nettoyage personnalisé.

```pascal
type
  TPersonne = class
  public
    destructor Destroy; override;
  end;

destructor TPersonne.Destroy;  
begin  
  WriteLn('Destruction de la personne');
  inherited Destroy;  // Appelle le destructeur parent
end;
```

**Important :** On appelle rarement `Destroy` directement.

### Free

`Free` est une **méthode sécurisée** héritée de `TObject` qui vérifie si l'objet existe avant d'appeler `Destroy` :

```pascal
procedure TObject.Free;  
begin  
  if Self <> nil then
    Destroy;
end;
```

**Avantage :** `Free` peut être appelé sur un objet `nil` sans provoquer d'erreur.

```pascal
var
  P: TPersonne;
begin
  P := nil;
  P.Free;  // ✓ OK : Free vérifie si P est nil
  P.Destroy;  // ✗ ERREUR : plantage car P est nil
end;
```

**Règle d'or :** Utilisez toujours `Free`, jamais `Destroy` directement.

## Déclarer un destructeur

### Syntaxe de base

```pascal
type
  TClasse = class
  public
    destructor Destroy; override;
  end;

destructor TClasse.Destroy;  
begin  
  // Code de nettoyage
  inherited Destroy;  // IMPORTANT : appel du destructeur parent
end;
```

**Points importants :**
- Le destructeur s'appelle toujours `Destroy`
- Le mot-clé `override` est obligatoire (on redéfinit le destructeur de TObject)
- On doit appeler `inherited Destroy` **à la fin** du destructeur

## Destructeur simple

### Exemple sans ressources spéciales

Si votre classe ne contient que des types simples, vous n'avez généralement pas besoin de définir un destructeur :

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
  public
    constructor Create(const Nom: string; Age: Integer);
    // Pas besoin de destructeur personnalisé
  end;

var
  P: TPersonne;
begin
  P := TPersonne.Create('Alice', 30);
  P.Free;  // Le destructeur par défaut suffit
end;
```

### Exemple avec affichage

Pour des raisons pédagogiques ou de débogage, on peut ajouter un message :

```pascal
type
  TCompteur = class
  private
    FValeur: Integer;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TCompteur.Create;  
begin  
  inherited Create;
  FValeur := 0;
  WriteLn('Compteur créé');
end;

destructor TCompteur.Destroy;  
begin  
  WriteLn('Compteur détruit (valeur finale : ', FValeur, ')');
  inherited Destroy;
end;

// Utilisation
var
  C: TCompteur;
begin
  C := TCompteur.Create;  // Affiche : "Compteur créé"
  C.Free;                 // Affiche : "Compteur détruit (valeur finale : 0)"
end;
```

## Libération d'objets contenus

### Le problème

Si votre classe contient d'autres objets, vous devez les libérer dans le destructeur :

```pascal
type
  TAdresse = class
  private
    FRue: string;
    FVille: string;
  end;

  TPersonne = class
  private
    FNom: string;
    FAdresse: TAdresse;  // Objet contenu
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TPersonne.Create;  
begin  
  inherited Create;
  FNom := 'Anonyme';
  FAdresse := TAdresse.Create;  // Création de l'objet contenu
end;

destructor TPersonne.Destroy;  
begin  
  FAdresse.Free;  // ✓ IMPORTANT : libération de l'objet contenu
  inherited Destroy;
end;
```

**Important :** Si vous créez un objet dans le constructeur, vous devez le libérer dans le destructeur.

### Ordre de libération

Libérez les objets dans l'**ordre inverse** de leur création :

```pascal
type
  TEntreprise = class
  private
    FAdresse: TAdresse;
    FDirecteur: TPersonne;
    FComptable: TPersonne;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TEntreprise.Create;  
begin  
  inherited Create;
  FAdresse := TAdresse.Create;      // 1. Créé en premier
  FDirecteur := TPersonne.Create;   // 2. Créé en deuxième
  FComptable := TPersonne.Create;   // 3. Créé en troisième
end;

destructor TEntreprise.Destroy;  
begin  
  FComptable.Free;   // 3. Libéré en premier
  FDirecteur.Free;   // 2. Libéré en deuxième
  FAdresse.Free;     // 1. Libéré en dernier
  inherited Destroy; // Toujours en dernier
end;
```

## Libération de tableaux dynamiques d'objets

### Tableau dynamique

```pascal
type
  TEquipe = class
  private
    FMembres: array of TPersonne;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AjouterMembre(const Nom: string);
  end;

constructor TEquipe.Create;  
begin  
  inherited Create;
  SetLength(FMembres, 0);
end;

destructor TEquipe.Destroy;  
var  
  I: Integer;
begin
  // Libérer chaque objet du tableau
  for I := 0 to High(FMembres) do
    FMembres[I].Free;

  // Libérer le tableau lui-même
  SetLength(FMembres, 0);

  inherited Destroy;
end;

procedure TEquipe.AjouterMembre(const Nom: string);  
var  
  Index: Integer;
begin
  Index := Length(FMembres);
  SetLength(FMembres, Index + 1);
  FMembres[Index] := TPersonne.Create(Nom);
end;
```

## Gestion sécurisée avec try-finally

Pour garantir qu'un objet sera toujours libéré, même en cas d'erreur, utilisez un bloc `try-finally` :

### Structure de base

```pascal
var
  P: TPersonne;
begin
  P := TPersonne.Create;
  try
    // Utilisation de l'objet
    P.DefinirNom('Alice');
    P.Afficher;
    // Même si une erreur se produit ici...
  finally
    P.Free;  // ...l'objet sera toujours libéré
  end;
end;
```

### Avec plusieurs objets

```pascal
var
  P1, P2: TPersonne;
begin
  P1 := TPersonne.Create;
  try
    P2 := TPersonne.Create;
    try
      // Utilisation des objets
      P1.DefinirNom('Alice');
      P2.DefinirNom('Bob');
    finally
      P2.Free;
    end;
  finally
    P1.Free;
  end;
end;
```

### Méthode alternative (recommandée)

```pascal
var
  P1, P2: TPersonne;
begin
  P1 := nil;
  P2 := nil;
  try
    P1 := TPersonne.Create;
    P2 := TPersonne.Create;

    // Utilisation des objets
    P1.DefinirNom('Alice');
    P2.DefinirNom('Bob');
  finally
    P2.Free;
    P1.Free;
  end;
end;
```

## Exemple complet : Classe TFichierLog

```pascal
program ExempleDestructeur;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  TFichierLog = class
  private
    FNomFichier: string;
    FFichier: TextFile;
    FEstOuvert: Boolean;

  public
    constructor Create(const NomFichier: string);
    destructor Destroy; override;

    procedure Ecrire(const Message: string);
    procedure EcrireAvecDate(const Message: string);
  end;

// === IMPLÉMENTATION ===

constructor TFichierLog.Create(const NomFichier: string);  
begin  
  inherited Create;
  FNomFichier := NomFichier;
  FEstOuvert := False;

  try
    AssignFile(FFichier, FNomFichier);

    // Créer ou ouvrir le fichier en ajout
    if FileExists(FNomFichier) then
      Append(FFichier)
    else
      Rewrite(FFichier);

    FEstOuvert := True;
    WriteLn('Fichier log ouvert : ', FNomFichier);
  except
    on E: Exception do
      WriteLn('Erreur lors de l''ouverture du fichier : ', E.Message);
  end;
end;

destructor TFichierLog.Destroy;  
begin  
  // Fermer le fichier s'il est ouvert
  if FEstOuvert then
  begin
    try
      CloseFile(FFichier);
      WriteLn('Fichier log fermé : ', FNomFichier);
    except
      on E: Exception do
        WriteLn('Erreur lors de la fermeture : ', E.Message);
    end;
  end;

  inherited Destroy;  // Toujours en dernier
end;

procedure TFichierLog.Ecrire(const Message: string);  
begin  
  if FEstOuvert then
  begin
    try
      WriteLn(FFichier, Message);
      Flush(FFichier);  // Forcer l'écriture
    except
      on E: Exception do
        WriteLn('Erreur d''écriture : ', E.Message);
    end;
  end;
end;

procedure TFichierLog.EcrireAvecDate(const Message: string);  
var  
  Ligne: string;
begin
  Ligne := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ' + Message;
  Ecrire(Ligne);
end;

// === PROGRAMME PRINCIPAL ===

var
  Log: TFichierLog;
begin
  Log := TFichierLog.Create('application.log');
  try
    Log.EcrireAvecDate('Démarrage de l''application');
    Log.EcrireAvecDate('Opération 1 effectuée');
    Log.EcrireAvecDate('Opération 2 effectuée');
    Log.EcrireAvecDate('Arrêt de l''application');
  finally
    Log.Free;  // Le fichier sera fermé proprement
  end;

  WriteLn('Programme terminé');
end.
```

## Gestion des références nil

### Vérifier avant de libérer

Bien que `Free` vérifie déjà si l'objet est `nil`, c'est une bonne pratique de mettre l'objet à `nil` après l'avoir libéré :

```pascal
var
  P: TPersonne;
begin
  P := TPersonne.Create;
  try
    P.Afficher;
  finally
    FreeAndNil(P);  // Libère et met à nil
  end;

  // P est maintenant nil
  if P = nil then
    WriteLn('Objet correctement libéré');
end;
```

### FreeAndNil

`FreeAndNil` est une procédure utilitaire qui fait deux choses :
1. Libère l'objet (appelle `Free`)
2. Met la variable à `nil`

```pascal
procedure FreeAndNil(var Obj);  
begin  
  if Assigned(Obj) then
  begin
    TObject(Obj).Free;
    Pointer(Obj) := nil;
  end;
end;
```

## Objets avec propriétaires

Parfois, un objet peut être créé par un autre objet mais utilisé ailleurs. Il faut alors savoir **qui est responsable** de la destruction.

### Propriétaire responsable de la destruction

```pascal
type
  TDocument = class
  private
    FTitre: string;
  public
    constructor Create(const Titre: string);
  end;

  TGestionnaireDocuments = class
  private
    FDocuments: array of TDocument;
  public
    destructor Destroy; override;
    function CreerDocument(const Titre: string): TDocument;
  end;

destructor TGestionnaireDocuments.Destroy;  
var  
  I: Integer;
begin
  // Le gestionnaire est responsable de libérer les documents
  for I := 0 to High(FDocuments) do
    FDocuments[I].Free;

  SetLength(FDocuments, 0);
  inherited Destroy;
end;

function TGestionnaireDocuments.CreerDocument(const Titre: string): TDocument;  
var  
  Index: Integer;
begin
  Index := Length(FDocuments);
  SetLength(FDocuments, Index + 1);
  Result := TDocument.Create(Titre);
  FDocuments[Index] := Result;
  // Le document appartient au gestionnaire
end;
```

## Bonnes pratiques

### 1. Toujours utiliser try-finally

```pascal
// ✓ BON
var
  P: TPersonne;
begin
  P := TPersonne.Create;
  try
    P.Afficher;
  finally
    P.Free;
  end;
end;

// ✗ RISQUÉ
var
  P: TPersonne;
begin
  P := TPersonne.Create;
  P.Afficher;  // Si erreur ici, P ne sera pas libéré
  P.Free;
end;
```

### 2. Libérer dans l'ordre inverse de création

```pascal
destructor TClasse.Destroy;  
begin  
  FObjet3.Free;  // Créé en 3ème, libéré en 1er
  FObjet2.Free;  // Créé en 2ème, libéré en 2ème
  FObjet1.Free;  // Créé en 1er, libéré en 3ème
  inherited Destroy;  // Toujours en dernier
end;
```

### 3. Toujours appeler inherited Destroy

```pascal
destructor TClasse.Destroy;  
begin  
  // Nettoyage de vos ressources
  FObjet.Free;

  inherited Destroy;  // ✓ OBLIGATOIRE
end;
```

### 4. Initialiser les objets à nil

```pascal
constructor TClasse.Create;  
begin  
  inherited Create;
  FObjet1 := nil;  // ✓ BON
  FObjet2 := nil;  // Même si c'est automatique, c'est plus clair
end;
```

### 5. Utiliser FreeAndNil quand approprié

```pascal
procedure TForm.DetruireObjet;  
begin  
  FreeAndNil(FMonObjet);  // Libère et met à nil
  // Maintenant FMonObjet = nil, évite les accès à un objet détruit
end;
```

## Erreurs courantes à éviter

### Erreur n°1 : Oublier de libérer un objet

```pascal
var
  P: TPersonne;
begin
  P := TPersonne.Create;
  P.Afficher;
  // ✗ ERREUR : P.Free manquant → fuite mémoire
end;
```

### Erreur n°2 : Libérer deux fois

```pascal
var
  P: TPersonne;
begin
  P := TPersonne.Create;
  try
    P.Afficher;
  finally
    P.Free;
  end;
  P.Free;  // ✗ ERREUR : déjà libéré → plantage
end;
```

### Erreur n°3 : Utiliser un objet après Free

```pascal
var
  P: TPersonne;
begin
  P := TPersonne.Create;
  P.Free;
  P.Afficher;  // ✗ ERREUR : objet déjà détruit → plantage
end;
```

### Erreur n°4 : Ne pas libérer les objets contenus

```pascal
destructor TPersonne.Destroy;  
begin  
  // ✗ ERREUR : FAdresse n'est pas libéré → fuite mémoire
  inherited Destroy;
end;

// ✓ CORRECT
destructor TPersonne.Destroy;  
begin  
  FAdresse.Free;
  inherited Destroy;
end;
```

### Erreur n°5 : Oublier inherited Destroy

```pascal
destructor TClasse.Destroy;  
begin  
  FObjet.Free;
  // ✗ ERREUR : manque inherited Destroy
end;

// ✓ CORRECT
destructor TClasse.Destroy;  
begin  
  FObjet.Free;
  inherited Destroy;
end;
```

### Erreur n°6 : Appeler Destroy au lieu de Free

```pascal
var
  P: TPersonne;
begin
  P := TPersonne.Create;
  P.Destroy;  // ✗ DÉCONSEILLÉ : utiliser Free à la place
end;

// ✓ CORRECT
var
  P: TPersonne;
begin
  P := TPersonne.Create;
  P.Free;  // ✓ Free vérifie si P n'est pas nil
end;
```

## Débogage des fuites mémoire

### Utiliser HeapTrc

FreePascal inclut HeapTrc pour détecter les fuites mémoire :

```pascal
program TestFuites;

{$mode objfpc}{$H+}

// Active le traçage de la heap
{$IFDEF DEBUG}
uses
  HeapTrc;
{$ENDIF}

type
  TPersonne = class
  private
    FNom: string;
  public
    constructor Create(const Nom: string);
  end;

constructor TPersonne.Create(const Nom: string);  
begin  
  inherited Create;
  FNom := Nom;
end;

var
  P: TPersonne;
begin
  P := TPersonne.Create('Test');
  // Oubli volontaire de P.Free pour tester

  WriteLn('Programme terminé');
  // HeapTrc affichera un rapport de fuite mémoire
end.
```

Compilez avec `-gh` pour activer HeapTrc :
```bash
fpc -gh programme.pas
```

## Points clés à retenir

- Le **destructeur** libère les ressources utilisées par un objet
- Utilisez toujours **Free**, jamais `Destroy` directement
- `Free` vérifie si l'objet est `nil` avant de le détruire
- Le destructeur s'appelle toujours **Destroy** avec le mot-clé **override**
- Appelez `inherited Destroy` **à la fin** du destructeur
- Utilisez **try-finally** pour garantir la libération
- Libérez les objets contenus dans le destructeur
- Ordre de libération : **inverse de la création**
- **FreeAndNil** libère et met à `nil` en une seule opération
- Les fuites mémoire sont causées par des objets non libérés

## Vers la suite

Dans la section suivante, nous explorerons le mot-clé **Self** et la référence à l'objet courant, ce qui vous permettra de mieux comprendre comment un objet peut se référencer lui-même dans ses propres méthodes.

⏭️ [Self et référence à l'objet courant](/10-fondamentaux-poo/07-self-reference-objet-courant.md)
