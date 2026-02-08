🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.7 Self et référence à l'objet courant

## Qu'est-ce que Self ?

`Self` est un **mot-clé spécial** qui représente une référence à l'**objet courant**, c'est-à-dire l'instance de la classe sur laquelle une méthode est appelée.

**Analogie :** Imaginez que vous êtes dans une pièce remplie de personnes. Quand vous dites "je", vous faites référence à vous-même. Dans une classe, `Self` est comme dire "je" : c'est la façon pour l'objet de se référencer lui-même.

## Comprendre le concept

### Sans Self (implicite)

Dans la plupart des cas, vous n'avez pas besoin d'utiliser `Self` car il est **implicite** :

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
  public
    procedure Afficher;
  end;

procedure TPersonne.Afficher;
begin
  // Ces deux lignes sont équivalentes :
  WriteLn('Nom : ', FNom);        // Accès implicite
  WriteLn('Nom : ', Self.FNom);   // Accès explicite avec Self
end;
```

### Avec Self (explicite)

Parfois, il est nécessaire ou utile d'utiliser `Self` explicitement :

```pascal
procedure TPersonne.SePresenter;
begin
  WriteLn('Je suis ', Self.FNom);  // Self clarifie qu'on parle de cet objet
end;
```

## Quand est-ce que Self est utile ?

### 1. Lever l'ambiguïté avec les paramètres

Quand un paramètre a le même nom qu'un attribut, `Self` permet de différencier :

```pascal
type
  TPersonne = class
  private
    FNom: string;
  public
    procedure DefinirNom(Nom: string);  // Paramètre nommé "Nom"
  end;

// ✗ PROBLÈME sans Self
procedure TPersonne.DefinirNom(Nom: string);
begin
  Nom := Nom;  // Confusion ! Assigne le paramètre à lui-même
end;

// ✓ SOLUTION avec Self
procedure TPersonne.DefinirNom(Nom: string);
begin
  Self.FNom := Nom;  // Clair : attribut de l'objet = paramètre
  // Ou mieux, utiliser la convention F :
  FNom := Nom;  // Pas d'ambiguïté si on suit la convention
end;
```

**Note :** C'est pourquoi on préfixe les attributs par `F` : pour éviter ce genre de confusion !

### 2. Passer l'objet courant comme paramètre

Vous pouvez passer l'objet actuel à une autre méthode ou fonction :

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
  public
    procedure AjouterAListe(Liste: TList);
  end;

procedure TPersonne.AjouterAListe(Liste: TList);
begin
  Liste.Add(Self);  // Ajoute cet objet (Self) à la liste
end;

// Utilisation
var
  P: TPersonne;
  MesPersonnes: TList;
begin
  MesPersonnes := TList.Create;
  P := TPersonne.Create;

  P.AjouterAListe(MesPersonnes);  // P s'ajoute lui-même à la liste

  MesPersonnes.Free;
  P.Free;
end;
```

### 3. Retourner l'objet courant (chaînage de méthodes)

`Self` permet de retourner l'objet lui-même pour créer des **méthodes chaînables** :

```pascal
type
  TConstructeurChaine = class
  private
    FTexte: string;
  public
    function Ajouter(const S: string): TConstructeurChaine;
    function AjouterLigne(const S: string): TConstructeurChaine;
    function ObtenirTexte: string;
  end;

function TConstructeurChaine.Ajouter(const S: string): TConstructeurChaine;
begin
  FTexte := FTexte + S;
  Result := Self;  // Retourne l'objet lui-même
end;

function TConstructeurChaine.AjouterLigne(const S: string): TConstructeurChaine;
begin
  FTexte := FTexte + S + sLineBreak;
  Result := Self;  // Retourne l'objet lui-même
end;

function TConstructeurChaine.ObtenirTexte: string;
begin
  Result := FTexte;
end;

// Utilisation avec chaînage
var
  C: TConstructeurChaine;
  Texte: string;
begin
  C := TConstructeurChaine.Create;
  try
    Texte := C.Ajouter('Bonjour ')
              .Ajouter('le ')
              .Ajouter('monde!')
              .AjouterLigne('')
              .Ajouter('Comment allez-vous?')
              .ObtenirTexte;

    WriteLn(Texte);
  finally
    C.Free;
  end;
end;
```

### 4. Comparaison avec d'autres objets

`Self` permet de comparer l'objet courant avec un autre :

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
  public
    function EstPlusAgeeQue(Autre: TPersonne): Boolean;
    function EstIdentique(Autre: TPersonne): Boolean;
  end;

function TPersonne.EstPlusAgeeQue(Autre: TPersonne): Boolean;
begin
  Result := Self.FAge > Autre.FAge;
  // Ou simplement : Result := FAge > Autre.FAge;
end;

function TPersonne.EstIdentique(Autre: TPersonne): Boolean;
begin
  Result := Self = Autre;  // Compare les références
end;

// Utilisation
var
  P1, P2: TPersonne;
begin
  P1 := TPersonne.Create;
  P2 := TPersonne.Create;

  P1.FAge := 30;
  P2.FAge := 25;

  if P1.EstPlusAgeeQue(P2) then
    WriteLn('P1 est plus âgée que P2');

  if P1.EstIdentique(P1) then
    WriteLn('Un objet est identique à lui-même');

  P1.Free;
  P2.Free;
end;
```

### 5. Événements et callbacks

`Self` est souvent utilisé pour passer l'objet aux gestionnaires d'événements :

```pascal
type
  TMonFormulaire = class
  private
    FBouton: TButton;
    procedure GererClicBouton(Sender: TObject);
  public
    constructor Create;
  end;

constructor TMonFormulaire.Create;
begin
  inherited Create;
  FBouton := TButton.Create(Self);  // Self est le propriétaire
  FBouton.Parent := Self;
  FBouton.OnClick := @GererClicBouton;
end;

procedure TMonFormulaire.GererClicBouton(Sender: TObject);
begin
  if Sender = FBouton then
    WriteLn('Le bouton a été cliqué');
  // Sender peut être comparé avec Self.FBouton
end;
```

## Self dans différents contextes

### Dans les méthodes normales

```pascal
procedure TPersonne.Afficher;
begin
  // Self fait référence à l'objet TPersonne sur lequel Afficher est appelée
  WriteLn('Nom : ', Self.FNom);
  WriteLn('Adresse de l''objet : ', IntToHex(PtrUInt(Self), 16));
end;
```

### Dans les constructeurs

```pascal
constructor TPersonne.Create(const Nom: string);
begin
  inherited Create;
  // Self fait référence au nouvel objet en cours de création
  Self.FNom := Nom;
  WriteLn('Création de ', Self.FNom);
end;
```

### Dans les destructeurs

```pascal
destructor TPersonne.Destroy;
begin
  // Self fait référence à l'objet en cours de destruction
  WriteLn('Destruction de ', Self.FNom);
  inherited Destroy;
end;
```

## Exemple complet : Classe TPoint

```pascal
program ExempleSelf;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TPoint = class
  private
    FX: Real;
    FY: Real;
  public
    constructor Create(X, Y: Real);

    // Méthodes de modification qui retournent Self (chaînage)
    function Deplacer(DeltaX, DeltaY: Real): TPoint;
    function DefinirX(X: Real): TPoint;
    function DefinirY(Y: Real): TPoint;

    // Méthodes de comparaison utilisant Self
    function DistanceVers(Autre: TPoint): Real;
    function EstPlusProcheQue(Point1, Point2: TPoint): Boolean;
    function EstALOrigine: Boolean;

    // Méthodes utilitaires
    function Cloner: TPoint;
    procedure Afficher;
    procedure CopierVers(Destination: TPoint);
  end;

// === IMPLÉMENTATION ===

constructor TPoint.Create(X, Y: Real);
begin
  inherited Create;
  Self.FX := X;  // Self explicite pour la clarté
  Self.FY := Y;
  WriteLn('Point créé : (', X:0:2, ', ', Y:0:2, ')');
end;

function TPoint.Deplacer(DeltaX, DeltaY: Real): TPoint;
begin
  FX := FX + DeltaX;
  FY := FY + DeltaY;
  Result := Self;  // Retourne l'objet pour permettre le chaînage
end;

function TPoint.DefinirX(X: Real): TPoint;
begin
  Self.FX := X;
  Result := Self;
end;

function TPoint.DefinirY(Y: Real): TPoint;
begin
  Self.FY := Y;
  Result := Self;
end;

function TPoint.DistanceVers(Autre: TPoint): Real;
var
  DX, DY: Real;
begin
  // Self représente le point actuel, Autre est le point de comparaison
  DX := Self.FX - Autre.FX;
  DY := Self.FY - Autre.FY;
  Result := Sqrt(DX * DX + DY * DY);
end;

function TPoint.EstPlusProcheQue(Point1, Point2: TPoint): Boolean;
var
  Distance1, Distance2: Real;
begin
  // Self est le point de référence
  Distance1 := Self.DistanceVers(Point1);
  Distance2 := Self.DistanceVers(Point2);
  Result := Distance1 < Distance2;
end;

function TPoint.EstALOrigine: Boolean;
begin
  Result := (Self.FX = 0) and (Self.FY = 0);
end;

function TPoint.Cloner: TPoint;
begin
  // Crée une copie de Self
  Result := TPoint.Create(Self.FX, Self.FY);
end;

procedure TPoint.Afficher;
begin
  WriteLn('Point : (', FX:0:2, ', ', FY:0:2, ')');
  WriteLn('Adresse mémoire : ', IntToHex(PtrUInt(Self), 16));
end;

procedure TPoint.CopierVers(Destination: TPoint);
begin
  // Copie les coordonnées de Self vers Destination
  Destination.FX := Self.FX;
  Destination.FY := Self.FY;
end;

// === PROGRAMME PRINCIPAL ===

var
  P1, P2, P3: TPoint;
begin
  WriteLn('=== Création des points ===');
  P1 := TPoint.Create(0, 0);
  P2 := TPoint.Create(3, 4);
  P3 := TPoint.Create(10, 10);
  WriteLn;

  WriteLn('=== Affichage initial ===');
  P1.Afficher;
  P2.Afficher;
  P3.Afficher;
  WriteLn;

  WriteLn('=== Chaînage de méthodes ===');
  P1.DefinirX(5).DefinirY(5).Deplacer(2, 2);
  WriteLn('P1 après chaînage :');
  P1.Afficher;
  WriteLn;

  WriteLn('=== Calcul de distances ===');
  WriteLn('Distance P1 -> P2 : ', P1.DistanceVers(P2):0:2);
  WriteLn('Distance P1 -> P3 : ', P1.DistanceVers(P3):0:2);
  WriteLn;

  WriteLn('=== Comparaisons ===');
  if P1.EstPlusProcheQue(P2, P3) then
    WriteLn('P2 est plus proche de P1 que P3')
  else
    WriteLn('P3 est plus proche de P1 que P2');
  WriteLn;

  WriteLn('=== Clonage ===');
  P3.Free;
  P3 := P2.Cloner;  // P3 est maintenant une copie de P2
  WriteLn('P3 après clonage de P2 :');
  P3.Afficher;
  WriteLn;

  WriteLn('=== Copie de coordonnées ===');
  P1.CopierVers(P2);  // Copie P1 dans P2
  WriteLn('P2 après copie depuis P1 :');
  P2.Afficher;
  WriteLn;

  // Libération
  P1.Free;
  P2.Free;
  P3.Free;

  WriteLn('Programme terminé');
end.
```

## Self et les propriétés

Dans les getters et setters de propriétés, `Self` est souvent implicite mais peut être utilisé :

```pascal
type
  TPersonne = class
  private
    FNom: string;
    function GetNomMajuscules: string;
    procedure SetNom(const Valeur: string);
  public
    property Nom: string read FNom write SetNom;
    property NomMajuscules: string read GetNomMajuscules;
  end;

function TPersonne.GetNomMajuscules: string;
begin
  Result := UpperCase(Self.FNom);  // Self explicite
  // Ou simplement : Result := UpperCase(FNom);
end;

procedure TPersonne.SetNom(const Valeur: string);
begin
  if Length(Valeur) > 0 then
    Self.FNom := Valeur
  else
    Self.FNom := 'Anonyme';
end;
```

## Self n'est pas toujours nécessaire

### Cas où Self est implicite

Dans la plupart des situations, vous n'avez pas besoin d'écrire `Self` :

```pascal
procedure TPersonne.Afficher;
begin
  // Ces deux versions sont identiques :

  // Version 1 : sans Self (recommandée)
  WriteLn('Nom : ', FNom);
  WriteLn('Age : ', FAge);

  // Version 2 : avec Self (plus verbeux)
  WriteLn('Nom : ', Self.FNom);
  WriteLn('Age : ', Self.FAge);
end;
```

**Recommandation :** N'utilisez `Self` que lorsque c'est nécessaire ou que cela améliore la clarté.

## Cas d'utilisation avancés

### 1. Enregistrement d'observateurs

```pascal
type
  TObservateur = class;

  TSubject = class
  private
    FObservateurs: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AjouterObservateur(Obs: TObservateur);
    procedure Notifier;
  end;

  TObservateur = class
  private
    FNom: string;
  public
    constructor Create(const Nom: string; Subject: TSubject);
    procedure Actualiser(Subject: TSubject);
  end;

constructor TObservateur.Create(const Nom: string; Subject: TSubject);
begin
  inherited Create;
  FNom := Nom;
  Subject.AjouterObservateur(Self);  // S'enregistre lui-même
end;

procedure TObservateur.Actualiser(Subject: TSubject);
begin
  WriteLn(FNom, ' a été notifié');
end;
```

### 2. Comparaison d'identité vs égalité

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
  public
    function EstMemeReference(Autre: TPersonne): Boolean;
    function EstEquivalent(Autre: TPersonne): Boolean;
  end;

function TPersonne.EstMemeReference(Autre: TPersonne): Boolean;
begin
  // Compare les adresses mémoire
  Result := Self = Autre;
end;

function TPersonne.EstEquivalent(Autre: TPersonne): Boolean;
begin
  // Compare les valeurs
  Result := (Self.FNom = Autre.FNom) and (Self.FAge = Autre.FAge);
end;

// Utilisation
var
  P1, P2, P3: TPersonne;
begin
  P1 := TPersonne.Create;
  P2 := TPersonne.Create;
  P3 := P1;  // P3 pointe vers le même objet que P1

  WriteLn(P1.EstMemeReference(P2));  // False (objets différents)
  WriteLn(P1.EstMemeReference(P3));  // True (même objet)
end;
```

## Self avec les interfaces

Quand vous implémentez des interfaces, `Self` peut être utilisé pour obtenir l'interface :

```pascal
type
  IComparable = interface
    function ComparerAvec(Autre: IComparable): Integer;
  end;

  TPersonne = class(TInterfacedObject, IComparable)
  private
    FAge: Integer;
  public
    function ComparerAvec(Autre: IComparable): Integer;
    function ObtenirInterface: IComparable;
  end;

function TPersonne.ComparerAvec(Autre: IComparable): Integer;
begin
  // Implémentation
  Result := 0;
end;

function TPersonne.ObtenirInterface: IComparable;
begin
  Result := Self;  // Self peut être converti en interface
end;
```

## Bonnes pratiques

### 1. Utilisez Self quand nécessaire

```pascal
// ✓ BON : Self nécessaire pour lever l'ambiguïté
procedure TPersonne.DefinirNom(Nom: string);
begin
  Self.FNom := Nom;
end;

// ✓ BON : Self nécessaire pour passer l'objet
procedure TPersonne.AjouterAListe(Liste: TList);
begin
  Liste.Add(Self);
end;
```

### 2. Omettez Self quand il est implicite

```pascal
// ✓ BON : simple et clair
procedure TPersonne.Afficher;
begin
  WriteLn(FNom);
  WriteLn(FAge);
end;

// ✗ VERBEUX : Self inutile ici
procedure TPersonne.Afficher;
begin
  WriteLn(Self.FNom);
  WriteLn(Self.FAge);
end;
```

### 3. Utilisez Self pour le chaînage

```pascal
// ✓ BON : permet le chaînage fluent
function TConstructeur.Ajouter(const S: string): TConstructeur;
begin
  FTexte := FTexte + S;
  Result := Self;
end;
```

### 4. Documentez l'utilisation de Self

```pascal
// ✓ BON : commentaire expliquant pourquoi Self est utilisé
function TPersonne.Cloner: TPersonne;
begin
  // Crée une copie indépendante de Self
  Result := TPersonne.Create;
  Result.FNom := Self.FNom;
  Result.FAge := Self.FAge;
end;
```

## Erreurs courantes à éviter

### Erreur n°1 : Utiliser Self avant inherited Create

```pascal
// ✗ ERREUR : Self n'est pas encore complètement initialisé
constructor TPersonne.Create;
begin
  Self.FNom := 'Test';  // Peut causer des problèmes
  inherited Create;
end;

// ✓ CORRECT
constructor TPersonne.Create;
begin
  inherited Create;
  Self.FNom := 'Test';
end;
```

### Erreur n°2 : Confusion entre Self et l'objet paramètre

```pascal
// ✗ ERREUR : confusion dans la logique
function TPersonne.EstPlusAgeeQue(Autre: TPersonne): Boolean;
begin
  Result := Autre.FAge > Self.FAge;  // Inversé !
end;

// ✓ CORRECT
function TPersonne.EstPlusAgeeQue(Autre: TPersonne): Boolean;
begin
  Result := Self.FAge > Autre.FAge;
end;
```

### Erreur n°3 : Oublier de retourner Self dans le chaînage

```pascal
// ✗ ERREUR : ne retourne pas Self
function TConstructeur.Ajouter(const S: string): TConstructeur;
begin
  FTexte := FTexte + S;
  // Oubli de : Result := Self;
end;

// ✓ CORRECT
function TConstructeur.Ajouter(const S: string): TConstructeur;
begin
  FTexte := FTexte + S;
  Result := Self;
end;
```

### Erreur n°4 : Utiliser Self après Free

```pascal
// ✗ ERREUR : Self n'est plus valide après Free
procedure TPersonne.SeDetruire;
begin
  Self.Free;
  WriteLn(Self.FNom);  // PLANTAGE ! Self est détruit
end;
```

## Points clés à retenir

- **Self** représente l'objet courant (l'instance sur laquelle la méthode est appelée)
- `Self` est **implicite** dans la plupart des cas, vous n'avez pas besoin de l'écrire
- Utilisez `Self` pour **lever l'ambiguïté** avec les paramètres
- Utilisez `Self` pour **passer l'objet** à d'autres méthodes ou fonctions
- Retournez `Self` pour permettre le **chaînage de méthodes**
- `Self` permet de **comparer** l'objet courant avec d'autres objets
- Dans les constructeurs, `Self` représente le nouvel objet en création
- Dans les destructeurs, `Self` représente l'objet en destruction
- N'abusez pas de `Self` : utilisez-le uniquement quand c'est utile ou nécessaire

## Vers la suite

Dans la section suivante, nous explorerons la **visibilité** en détail (private, protected, public, published) et comment organiser correctement les membres d'une classe pour une meilleure encapsulation.

⏭️ [Visibilité : private, protected, public, published](/10-fondamentaux-poo/08-visibilite-private-protected-public-published.md)
