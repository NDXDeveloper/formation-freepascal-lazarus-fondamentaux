🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.5 Constructeurs (Create)

## Qu'est-ce qu'un constructeur ?

Un **constructeur** est une méthode spéciale qui est appelée lors de la **création** d'un objet. Son rôle principal est d'**initialiser** l'objet en lui donnant des valeurs de départ cohérentes.

**Analogie :** Quand vous achetez une voiture neuve, elle arrive déjà avec un réservoir partiellement rempli, les pneus gonflés, et tous les réglages de base effectués. Le constructeur fait la même chose pour un objet : il le prépare à être utilisé.

## Pourquoi a-t-on besoin d'un constructeur ?

### Sans constructeur personnalisé

Les attributs sont initialisés avec des valeurs par défaut qui ne sont pas toujours appropriées :

```pascal
type
  TCompteBancaire = class
  private
    FNumeroCompte: string;
    FSolde: Real;
    FTitulaire: string;
  end;

var
  Compte: TCompteBancaire;
begin
  Compte := TCompteBancaire.Create;
  // À ce stade :
  // FNumeroCompte = ''
  // FSolde = 0
  // FTitulaire = ''
  // L'objet existe mais n'a pas de données cohérentes
end;
```

### Avec un constructeur personnalisé

On peut initialiser l'objet avec des valeurs appropriées dès sa création :

```pascal
type
  TCompteBancaire = class
  private
    FNumeroCompte: string;
    FSolde: Real;
    FTitulaire: string;
  public
    constructor Create(const NumeroCompte, Titulaire: string; SoldeInitial: Real);
  end;

constructor TCompteBancaire.Create(const NumeroCompte, Titulaire: string; SoldeInitial: Real);
begin
  FNumeroCompte := NumeroCompte;
  FTitulaire := Titulaire;
  FSolde := SoldeInitial;
end;

var
  Compte: TCompteBancaire;
begin
  Compte := TCompteBancaire.Create('FR123456', 'Jean Dupont', 1000);
  // L'objet est immédiatement utilisable avec des données cohérentes
end;
```

## Le constructeur par défaut

### Syntaxe

En Pascal, un constructeur se déclare avec le mot-clé `constructor` et porte généralement le nom `Create` :

```pascal
type
  TPersonne = class
  public
    constructor Create;
  end;

constructor TPersonne.Create;
begin
  // Code d'initialisation
end;
```

### Héritage du constructeur de TObject

Toutes les classes héritent de `TObject`, qui possède déjà un constructeur `Create`. Ce constructeur s'occupe de :
- Allouer la mémoire nécessaire pour l'objet
- Initialiser les attributs avec leurs valeurs par défaut

Quand vous définissez votre propre constructeur, vous devez d'abord appeler le constructeur parent avec `inherited` :

```pascal
constructor TPersonne.Create;
begin
  inherited Create;  // Appelle le constructeur de TObject
  // Votre code d'initialisation personnalisé
  FNom := 'Anonyme';
  FAge := 0;
end;
```

**Note :** Dans la pratique, si vous n'avez rien de spécial à faire dans le constructeur parent, vous pouvez omettre `inherited Create`.

## Constructeurs sans paramètres

### Exemple simple

```pascal
type
  TCompteur = class
  private
    FValeur: Integer;
  public
    constructor Create;
    procedure Incrementer;
    function ObtenirValeur: Integer;
  end;

constructor TCompteur.Create;
begin
  inherited Create;
  FValeur := 0;  // Initialisation explicite
  WriteLn('Compteur créé avec valeur = 0');
end;

// Utilisation
var
  C: TCompteur;
begin
  C := TCompteur.Create;  // Affiche : "Compteur créé avec valeur = 0"
  C.Incrementer;
  WriteLn(C.ObtenirValeur);  // Affiche : 1
  C.Free;
end;
```

### Initialisation de plusieurs attributs

```pascal
type
  TJoueur = class
  private
    FNom: string;
    FScore: Integer;
    FVies: Integer;
    FNiveau: Integer;
  public
    constructor Create;
  end;

constructor TJoueur.Create;
begin
  inherited Create;
  FNom := 'Joueur 1';
  FScore := 0;
  FVies := 3;
  FNiveau := 1;
  WriteLn('Nouveau joueur créé');
end;
```

## Constructeurs avec paramètres

### Constructeur avec un paramètre

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
  public
    constructor Create(const Nom: string);
  end;

constructor TPersonne.Create(const Nom: string);
begin
  inherited Create;
  FNom := Nom;
  FAge := 0;  // Valeur par défaut
end;

// Utilisation
var
  P: TPersonne;
begin
  P := TPersonne.Create('Marie');
  P.Free;
end;
```

### Constructeur avec plusieurs paramètres

```pascal
type
  TRectangle = class
  private
    FLargeur: Real;
    FHauteur: Real;
  public
    constructor Create(Largeur, Hauteur: Real);
    function CalculerSurface: Real;
  end;

constructor TRectangle.Create(Largeur, Hauteur: Real);
begin
  inherited Create;

  // Validation des paramètres
  if (Largeur > 0) and (Hauteur > 0) then
  begin
    FLargeur := Largeur;
    FHauteur := Hauteur;
  end
  else
  begin
    WriteLn('Erreur : dimensions invalides, valeurs par défaut utilisées');
    FLargeur := 1;
    FHauteur := 1;
  end;
end;

function TRectangle.CalculerSurface: Real;
begin
  Result := FLargeur * FHauteur;
end;

// Utilisation
var
  R: TRectangle;
begin
  R := TRectangle.Create(10, 5);
  WriteLn('Surface : ', R.CalculerSurface:0:2);
  R.Free;
end;
```

## Surcharge de constructeurs

Vous pouvez définir **plusieurs constructeurs** avec des paramètres différents. C'est ce qu'on appelle la **surcharge** :

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FPrenom: string;
    FAge: Integer;
  public
    constructor Create; overload;
    constructor Create(const Nom: string); overload;
    constructor Create(const Nom, Prenom: string; Age: Integer); overload;
  end;

// Constructeur sans paramètres
constructor TPersonne.Create;
begin
  inherited Create;
  FNom := 'Inconnu';
  FPrenom := 'Inconnu';
  FAge := 0;
end;

// Constructeur avec nom seulement
constructor TPersonne.Create(const Nom: string);
begin
  inherited Create;
  FNom := Nom;
  FPrenom := 'Inconnu';
  FAge := 0;
end;

// Constructeur complet
constructor TPersonne.Create(const Nom, Prenom: string; Age: Integer);
begin
  inherited Create;
  FNom := Nom;
  FPrenom := Prenom;
  if (Age >= 0) and (Age <= 150) then
    FAge := Age
  else
    FAge := 0;
end;

// Utilisation : vous pouvez choisir le constructeur approprié
var
  P1, P2, P3: TPersonne;
begin
  P1 := TPersonne.Create;                          // Utilise le 1er constructeur
  P2 := TPersonne.Create('Dupont');                // Utilise le 2ème constructeur
  P3 := TPersonne.Create('Curie', 'Marie', 66);    // Utilise le 3ème constructeur

  P1.Free;
  P2.Free;
  P3.Free;
end;
```

**Important :** Remarquez le mot-clé `overload` après chaque déclaration de constructeur. Il est obligatoire pour indiquer au compilateur que vous surchargez la méthode.

## Appel entre constructeurs

Pour éviter la duplication de code, un constructeur peut appeler un autre constructeur :

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FPrenom: string;
    FAge: Integer;
  public
    constructor Create; overload;
    constructor Create(const Nom, Prenom: string; Age: Integer); overload;
  end;

// Constructeur complet
constructor TPersonne.Create(const Nom, Prenom: string; Age: Integer);
begin
  inherited Create;
  FNom := Nom;
  FPrenom := Prenom;

  if (Age >= 0) and (Age <= 150) then
    FAge := Age
  else
    FAge := 0;
end;

// Constructeur simple qui appelle le constructeur complet
constructor TPersonne.Create;
begin
  Create('Inconnu', 'Inconnu', 0);  // Appelle l'autre constructeur
end;
```

## Initialisation d'objets composites

Quand un objet contient d'autres objets, le constructeur doit les créer :

```pascal
type
  TAdresse = class
  private
    FRue: string;
    FVille: string;
  public
    constructor Create(const Rue, Ville: string);
  end;

  TPersonne = class
  private
    FNom: string;
    FAdresse: TAdresse;  // Objet contenu
  public
    constructor Create(const Nom, Rue, Ville: string);
    destructor Destroy; override;
  end;

constructor TAdresse.Create(const Rue, Ville: string);
begin
  inherited Create;
  FRue := Rue;
  FVille := Ville;
end;

constructor TPersonne.Create(const Nom, Rue, Ville: string);
begin
  inherited Create;
  FNom := Nom;
  // IMPORTANT : créer l'objet contenu
  FAdresse := TAdresse.Create(Rue, Ville);
end;

destructor TPersonne.Destroy;
begin
  // IMPORTANT : libérer l'objet contenu
  FAdresse.Free;
  inherited Destroy;
end;
```

## Exemple complet : Classe TArticle

```pascal
program ExempleConstructeur;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TArticle = class
  private
    FCode: string;
    FLibelle: string;
    FPrixHT: Real;
    FQuantiteStock: Integer;
    FTauxTVA: Real;

    // Méthode privée de validation
    function PrixValide(Prix: Real): Boolean;

  public
    // Plusieurs constructeurs
    constructor Create; overload;
    constructor Create(const Code, Libelle: string); overload;
    constructor Create(const Code, Libelle: string; PrixHT: Real; Quantite: Integer); overload;

    // Autres méthodes
    function CalculerPrixTTC: Real;
    procedure Afficher;
  end;

// === IMPLÉMENTATION ===

function TArticle.PrixValide(Prix: Real): Boolean;
begin
  Result := Prix >= 0;
end;

// Constructeur par défaut
constructor TArticle.Create;
begin
  inherited Create;
  FCode := 'ART000';
  FLibelle := 'Article sans nom';
  FPrixHT := 0;
  FQuantiteStock := 0;
  FTauxTVA := 0.20;  // 20% par défaut
  WriteLn('Article créé avec valeurs par défaut');
end;

// Constructeur avec code et libellé
constructor TArticle.Create(const Code, Libelle: string);
begin
  inherited Create;
  FCode := Code;
  FLibelle := Libelle;
  FPrixHT := 0;
  FQuantiteStock := 0;
  FTauxTVA := 0.20;
  WriteLn('Article créé : ', Code);
end;

// Constructeur complet
constructor TArticle.Create(const Code, Libelle: string; PrixHT: Real; Quantite: Integer);
begin
  inherited Create;
  FCode := Code;
  FLibelle := Libelle;

  // Validation du prix
  if PrixValide(PrixHT) then
    FPrixHT := PrixHT
  else
  begin
    WriteLn('Attention : prix invalide, initialisé à 0');
    FPrixHT := 0;
  end;

  // Validation de la quantité
  if Quantite >= 0 then
    FQuantiteStock := Quantite
  else
  begin
    WriteLn('Attention : quantité invalide, initialisée à 0');
    FQuantiteStock := 0;
  end;

  FTauxTVA := 0.20;
  WriteLn('Article créé : ', Code, ' - ', Libelle);
end;

function TArticle.CalculerPrixTTC: Real;
begin
  Result := FPrixHT * (1 + FTauxTVA);
end;

procedure TArticle.Afficher;
begin
  WriteLn('=== Article ===');
  WriteLn('Code        : ', FCode);
  WriteLn('Libellé     : ', FLibelle);
  WriteLn('Prix HT     : ', FPrixHT:0:2, ' €');
  WriteLn('Prix TTC    : ', CalculerPrixTTC:0:2, ' €');
  WriteLn('Stock       : ', FQuantiteStock);
  WriteLn('===============');
end;

// === PROGRAMME PRINCIPAL ===

var
  Article1, Article2, Article3: TArticle;
begin
  WriteLn('--- Création avec constructeur par défaut ---');
  Article1 := TArticle.Create;
  Article1.Afficher;
  WriteLn;

  WriteLn('--- Création avec code et libellé ---');
  Article2 := TArticle.Create('ART001', 'Clavier mécanique');
  Article2.Afficher;
  WriteLn;

  WriteLn('--- Création complète ---');
  Article3 := TArticle.Create('ART002', 'Souris sans fil', 29.99, 50);
  Article3.Afficher;
  WriteLn;

  // Libération de la mémoire
  Article1.Free;
  Article2.Free;
  Article3.Free;

  WriteLn('Programme terminé');
end.
```

## Gestion des erreurs dans les constructeurs

### Lever une exception

Si l'initialisation échoue, vous pouvez lever une exception :

```pascal
constructor TCompteBancaire.Create(const NumeroCompte: string; SoldeInitial: Real);
begin
  inherited Create;

  if Length(NumeroCompte) < 5 then
    raise Exception.Create('Numéro de compte invalide');

  if SoldeInitial < 0 then
    raise Exception.Create('Le solde initial ne peut pas être négatif');

  FNumeroCompte := NumeroCompte;
  FSolde := SoldeInitial;
end;

// Utilisation avec gestion d'erreur
var
  Compte: TCompteBancaire;
begin
  try
    Compte := TCompteBancaire.Create('123', -100);  // Lèvera une exception
  except
    on E: Exception do
      WriteLn('Erreur lors de la création : ', E.Message);
  end;
end;
```

### Utiliser des valeurs par défaut sûres

Alternative : accepter les paramètres mais corriger les valeurs invalides :

```pascal
constructor TPersonne.Create(const Nom: string; Age: Integer);
begin
  inherited Create;

  // Correction silencieuse
  if Length(Nom) > 0 then
    FNom := Nom
  else
    FNom := 'Anonyme';

  if (Age >= 0) and (Age <= 150) then
    FAge := Age
  else
    FAge := 0;
end;
```

## Constructeurs et tableaux d'objets

Pour créer un tableau d'objets, il faut créer chaque objet individuellement :

```pascal
var
  Personnes: array[1..3] of TPersonne;
  I: Integer;
begin
  // Créer chaque objet
  for I := 1 to 3 do
    Personnes[I] := TPersonne.Create;

  // Utiliser les objets
  Personnes[1].DefinirNom('Alice');
  Personnes[2].DefinirNom('Bob');
  Personnes[3].DefinirNom('Charlie');

  // Libérer chaque objet
  for I := 1 to 3 do
    Personnes[I].Free;
end;
```

## Bonnes pratiques

### 1. Toujours appeler inherited

```pascal
constructor TPersonne.Create;
begin
  inherited Create;  // ✓ Important !
  // Votre code...
end;
```

### 2. Initialiser tous les attributs

```pascal
constructor TPersonne.Create;
begin
  inherited Create;
  FNom := '';        // ✓ Initialisation explicite
  FPrenom := '';     // ✓ même si c'est la valeur par défaut
  FAge := 0;         // ✓ clarté du code
  FEstActif := True; // ✓ Valeur cohérente
end;
```

### 3. Valider les paramètres

```pascal
constructor TRectangle.Create(Largeur, Hauteur: Real);
begin
  inherited Create;

  // ✓ Toujours valider
  if (Largeur > 0) and (Hauteur > 0) then
  begin
    FLargeur := Largeur;
    FHauteur := Hauteur;
  end
  else
    raise Exception.Create('Dimensions invalides');
end;
```

### 4. Créer les objets contenus

```pascal
constructor TPersonne.Create;
begin
  inherited Create;
  FNom := 'Anonyme';
  FAdresse := TAdresse.Create;  // ✓ Créer l'objet contenu
end;
```

### 5. Documenter les constructeurs

```pascal
type
  TPersonne = class
  public
    // Crée une personne avec des valeurs par défaut
    constructor Create; overload;

    // Crée une personne avec un nom
    // @param Nom Le nom de la personne
    constructor Create(const Nom: string); overload;
  end;
```

## Erreurs courantes à éviter

### Erreur n°1 : Oublier d'appeler inherited

```pascal
constructor TPersonne.Create;
begin
  // ✗ ERREUR : oubli de inherited Create
  FNom := 'Test';
end;
```

### Erreur n°2 : Ne pas valider les paramètres

```pascal
constructor TRectangle.Create(Largeur, Hauteur: Real);
begin
  inherited Create;
  FLargeur := Largeur;   // ✗ Et si Largeur est négative ?
  FHauteur := Hauteur;   // ✗ Pas de validation
end;
```

### Erreur n°3 : Oublier le mot-clé overload

```pascal
type
  TPersonne = class
    constructor Create;              // ✗ Manque overload
    constructor Create(Nom: string); // ✗ Manque overload
  end;
```

### Erreur n°4 : Ne pas créer les objets contenus

```pascal
constructor TPersonne.Create;
begin
  inherited Create;
  FNom := 'Test';
  // ✗ ERREUR : FAdresse n'est pas créé (reste nil)
  // Plus tard : FAdresse.Ville := 'Paris'; // Plantage !
end;
```

## Points clés à retenir

- Un **constructeur** initialise un objet lors de sa création
- Le constructeur porte généralement le nom **Create**
- Toujours appeler `inherited Create` au début du constructeur
- On peut avoir plusieurs constructeurs avec des paramètres différents (**surcharge**)
- Le mot-clé `overload` est obligatoire pour la surcharge
- Toujours **valider** les paramètres dans le constructeur
- Initialiser **tous les attributs** pour un état cohérent
- Les objets contenus doivent être **créés** dans le constructeur
- En cas d'erreur grave, lever une **exception**

## Vers la suite

Dans la section suivante, nous verrons les **destructeurs** (Destroy, Free), qui sont le pendant des constructeurs et permettent de libérer correctement les ressources utilisées par un objet.

⏭️ [Destructeurs (Destroy, Free)](/10-fondamentaux-poo/06-destructeurs-destroy-free.md)
