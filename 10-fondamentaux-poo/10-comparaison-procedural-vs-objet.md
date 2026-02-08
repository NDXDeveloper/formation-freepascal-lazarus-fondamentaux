🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.10 Comparaison procédural vs objet

## Introduction

Vous avez maintenant découvert les bases de la Programmation Orientée Objet (POO). Mais quand faut-il utiliser la POO plutôt que la programmation procédurale que vous connaissez déjà ? Cette section compare les deux approches pour vous aider à faire le bon choix.

## Les deux paradigmes

### Programmation procédurale

La programmation **procédurale** organise le code autour de **procédures et fonctions** qui manipulent des données. Les données et les traitements sont séparés.

**Analogie :** Une usine où les pièces (données) circulent entre différents postes de travail (procédures).

### Programmation orientée objet

La programmation **orientée objet** organise le code autour d'**objets** qui regroupent données et traitements. Les données et les méthodes qui les manipulent sont dans le même endroit.

**Analogie :** Des robots autonomes (objets) qui possèdent leurs propres données et savent comment se gérer eux-mêmes.

## Exemple 1 : Gestion d'un rectangle

### Approche procédurale

```pascal
program RectangleProcedural;

{$mode objfpc}{$H+}

type
  TRectangle = record
    Largeur: Real;
    Hauteur: Real;
  end;

// Procédures et fonctions séparées
procedure InitialiserRectangle(var R: TRectangle; L, H: Real);
begin
  R.Largeur := L;
  R.Hauteur := H;
end;

function CalculerSurface(R: TRectangle): Real;
begin
  Result := R.Largeur * R.Hauteur;
end;

function CalculerPerimetre(R: TRectangle): Real;
begin
  Result := 2 * (R.Largeur + R.Hauteur);
end;

procedure Redimensionner(var R: TRectangle; Facteur: Real);
begin
  R.Largeur := R.Largeur * Facteur;
  R.Hauteur := R.Hauteur * Facteur;
end;

procedure AfficherRectangle(R: TRectangle);
begin
  WriteLn('Rectangle:');
  WriteLn('  Largeur: ', R.Largeur:0:2);
  WriteLn('  Hauteur: ', R.Hauteur:0:2);
  WriteLn('  Surface: ', CalculerSurface(R):0:2);
  WriteLn('  Périmètre: ', CalculerPerimetre(R):0:2);
end;

var
  MonRectangle: TRectangle;
begin
  InitialiserRectangle(MonRectangle, 10, 5);
  AfficherRectangle(MonRectangle);

  Redimensionner(MonRectangle, 2);
  WriteLn;
  WriteLn('Après redimensionnement:');
  AfficherRectangle(MonRectangle);
end.
```

**Caractéristiques :**
- Données dans une structure `record`
- Fonctions/procédures séparées
- Les données sont passées en paramètre
- Pas d'encapsulation

### Approche orientée objet

```pascal
program RectangleObjet;

{$mode objfpc}{$H+}

type
  TRectangle = class
  private
    FLargeur: Real;
    FHauteur: Real;
  public
    constructor Create(L, H: Real);
    function CalculerSurface: Real;
    function CalculerPerimetre: Real;
    procedure Redimensionner(Facteur: Real);
    procedure Afficher;
    property Largeur: Real read FLargeur write FLargeur;
    property Hauteur: Real read FHauteur write FHauteur;
  end;

constructor TRectangle.Create(L, H: Real);
begin
  inherited Create;
  FLargeur := L;
  FHauteur := H;
end;

function TRectangle.CalculerSurface: Real;
begin
  Result := FLargeur * FHauteur;
end;

function TRectangle.CalculerPerimetre: Real;
begin
  Result := 2 * (FLargeur + FHauteur);
end;

procedure TRectangle.Redimensionner(Facteur: Real);
begin
  FLargeur := FLargeur * Facteur;
  FHauteur := FHauteur * Facteur;
end;

procedure TRectangle.Afficher;
begin
  WriteLn('Rectangle:');
  WriteLn('  Largeur: ', FLargeur:0:2);
  WriteLn('  Hauteur: ', FHauteur:0:2);
  WriteLn('  Surface: ', CalculerSurface:0:2);
  WriteLn('  Périmètre: ', CalculerPerimetre:0:2);
end;

var
  MonRectangle: TRectangle;
begin
  MonRectangle := TRectangle.Create(10, 5);
  try
    MonRectangle.Afficher;

    MonRectangle.Redimensionner(2);
    WriteLn;
    WriteLn('Après redimensionnement:');
    MonRectangle.Afficher;
  finally
    MonRectangle.Free;
  end;
end.
```

**Caractéristiques :**
- Données et méthodes regroupées dans une classe
- L'objet "se gère lui-même"
- Encapsulation (attributs privés)
- Syntaxe plus intuitive

## Exemple 2 : Système de gestion de comptes bancaires

### Approche procédurale

```pascal
program CompteProcedural;

{$mode objfpc}{$H+}

type
  TCompte = record
    NumeroCompte: string;
    Titulaire: string;
    Solde: Real;
  end;

  TArrayComptes = array of TCompte;

var
  Comptes: TArrayComptes;

procedure AjouterCompte(var Comptes: TArrayComptes; const Numero, Titulaire: string; SoldeInitial: Real);
var
  Index: Integer;
begin
  Index := Length(Comptes);
  SetLength(Comptes, Index + 1);
  Comptes[Index].NumeroCompte := Numero;
  Comptes[Index].Titulaire := Titulaire;
  Comptes[Index].Solde := SoldeInitial;
end;

function TrouverCompte(const Comptes: TArrayComptes; const Numero: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(Comptes) do
    if Comptes[I].NumeroCompte = Numero then
    begin
      Result := I;
      Break;
    end;
end;

procedure Crediter(var Compte: TCompte; Montant: Real);
begin
  if Montant > 0 then
  begin
    Compte.Solde := Compte.Solde + Montant;
    WriteLn('Crédit de ', Montant:0:2, ' € effectué');
  end
  else
    WriteLn('Erreur: montant invalide');
end;

procedure Debiter(var Compte: TCompte; Montant: Real);
begin
  if Montant > 0 then
  begin
    if Compte.Solde >= Montant then
    begin
      Compte.Solde := Compte.Solde - Montant;
      WriteLn('Débit de ', Montant:0:2, ' € effectué');
    end
    else
      WriteLn('Erreur: solde insuffisant');
  end
  else
    WriteLn('Erreur: montant invalide');
end;

procedure AfficherCompte(const Compte: TCompte);
begin
  WriteLn('Compte: ', Compte.NumeroCompte);
  WriteLn('Titulaire: ', Compte.Titulaire);
  WriteLn('Solde: ', Compte.Solde:0:2, ' €');
end;

var
  Index: Integer;
begin
  // Création de comptes
  AjouterCompte(Comptes, 'FR001', 'Alice Martin', 1000);
  AjouterCompte(Comptes, 'FR002', 'Bob Durand', 500);

  // Opérations sur le premier compte
  Index := TrouverCompte(Comptes, 'FR001');
  if Index >= 0 then
  begin
    Crediter(Comptes[Index], 500);
    Debiter(Comptes[Index], 200);
    AfficherCompte(Comptes[Index]);
  end;

  SetLength(Comptes, 0);
end.
```

**Problèmes de cette approche :**
- Code dispersé (fonctions séparées)
- Pas de protection des données (on peut modifier `Solde` directement)
- Difficile à maintenir et à faire évoluer
- Risque d'oublier des validations

### Approche orientée objet

```pascal
program CompteObjet;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type
  TCompte = class
  private
    FNumeroCompte: string;
    FTitulaire: string;
    FSolde: Real;
  public
    constructor Create(const Numero, Titulaire: string; SoldeInitial: Real);
    procedure Crediter(Montant: Real);
    procedure Debiter(Montant: Real);
    procedure Afficher;
    property NumeroCompte: string read FNumeroCompte;
    property Titulaire: string read FTitulaire;
    property Solde: Real read FSolde;
  end;

  TBanque = class
  private
    FComptes: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AjouterCompte(Compte: TCompte);
    function TrouverCompte(const Numero: string): TCompte;
  end;

// Implémentation TCompte

constructor TCompte.Create(const Numero, Titulaire: string; SoldeInitial: Real);
begin
  inherited Create;
  FNumeroCompte := Numero;
  FTitulaire := Titulaire;
  FSolde := SoldeInitial;
end;

procedure TCompte.Crediter(Montant: Real);
begin
  if Montant > 0 then
  begin
    FSolde := FSolde + Montant;
    WriteLn('Crédit de ', Montant:0:2, ' € effectué');
  end
  else
    WriteLn('Erreur: montant invalide');
end;

procedure TCompte.Debiter(Montant: Real);
begin
  if Montant > 0 then
  begin
    if FSolde >= Montant then
    begin
      FSolde := FSolde - Montant;
      WriteLn('Débit de ', Montant:0:2, ' € effectué');
    end
    else
      WriteLn('Erreur: solde insuffisant');
  end
  else
    WriteLn('Erreur: montant invalide');
end;

procedure TCompte.Afficher;
begin
  WriteLn('Compte: ', FNumeroCompte);
  WriteLn('Titulaire: ', FTitulaire);
  WriteLn('Solde: ', FSolde:0:2, ' €');
end;

// Implémentation TBanque

constructor TBanque.Create;
begin
  inherited Create;
  FComptes := TList.Create;
end;

destructor TBanque.Destroy;
var
  I: Integer;
begin
  for I := 0 to FComptes.Count - 1 do
    TCompte(FComptes[I]).Free;  // TList stocke des Pointer : cast nécessaire pour accéder aux méthodes
  FComptes.Free;
  inherited Destroy;
end;

procedure TBanque.AjouterCompte(Compte: TCompte);
begin
  FComptes.Add(Compte);
end;

function TBanque.TrouverCompte(const Numero: string): TCompte;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FComptes.Count - 1 do
    if TCompte(FComptes[I]).NumeroCompte = Numero then
    begin
      Result := TCompte(FComptes[I]);
      Break;
    end;
end;

// Programme principal

var
  Banque: TBanque;
  Compte: TCompte;
begin
  Banque := TBanque.Create;
  try
    // Création de comptes
    Banque.AjouterCompte(TCompte.Create('FR001', 'Alice Martin', 1000));
    Banque.AjouterCompte(TCompte.Create('FR002', 'Bob Durand', 500));

    // Opérations
    Compte := Banque.TrouverCompte('FR001');
    if Compte <> nil then
    begin
      Compte.Crediter(500);
      Compte.Debiter(200);
      Compte.Afficher;
    end;
  finally
    Banque.Free;
  end;
end.
```

**Avantages de cette approche :**
- Code organisé et structuré
- Encapsulation : le solde est protégé
- Chaque objet se gère lui-même
- Plus facile à maintenir et à étendre
- Validations garanties

## Comparaison détaillée

### 1. Organisation du code

**Procédural :**
```pascal
// Données séparées
type
  TPersonne = record
    Nom: string;
    Age: Integer;
  end;

// Fonctions séparées
procedure Vieillir(var P: TPersonne);
function EstMajeur(P: TPersonne): Boolean;
```

**Objet :**
```pascal
// Données et méthodes ensemble
type
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
  public
    procedure Vieillir;
    function EstMajeur: Boolean;
  end;
```

### 2. Encapsulation et protection

**Procédural :**
```pascal
var
  P: TPersonne;
begin
  P.Age := -5;  // ✗ Aucune protection, valeur invalide acceptée
end;
```

**Objet :**
```pascal
var
  P: TPersonne;
begin
  P.Age := -5;  // ✓ Le setter valide et rejette la valeur
end;
```

### 3. Réutilisabilité

**Procédural :**
```pascal
// Pour chaque nouveau type, recréer toutes les fonctions
type
  TVoiture = record ... end;
  TMoto = record ... end;

procedure AccelererVoiture(var V: TVoiture);
procedure AccelererMoto(var M: TMoto);
// Duplication de code
```

**Objet :**
```pascal
// Héritage et réutilisation
type
  TVehicule = class
    procedure Accelerer; virtual;
  end;

  TVoiture = class(TVehicule)
    procedure Accelerer; override;  // Réutilise et spécialise
  end;
```

### 4. Gestion de la complexité

**Procédural :**
```pascal
// À mesure que le programme grandit, on a des centaines de fonctions
procedure Fonction1(var Data: TData);
procedure Fonction2(var Data: TData);
// ... 100 fonctions plus tard ...
procedure Fonction100(var Data: TData);
// Difficile de savoir quelle fonction utiliser
```

**Objet :**
```pascal
// L'objet regroupe les opérations liées
type
  TData = class
    procedure Operation1;
    procedure Operation2;
    // Toutes les opérations pertinentes sont dans la classe
  end;

var
  Data: TData;
begin
  Data.Operation1;  // Clair : c'est une opération sur Data
end;
```

### 5. Maintenance du code

**Procédural :**
```pascal
// Si on change la structure TPersonne
type
  TPersonne = record
    Nom: string;
    Age: Integer;
    Email: string;  // Nouveau champ
  end;

// Il faut trouver et modifier TOUTES les fonctions qui l'utilisent
procedure Afficher(P: TPersonne);
procedure Sauvegarder(P: TPersonne);
// ... beaucoup de fonctions à modifier
```

**Objet :**
```pascal
// Si on ajoute un attribut
type
  TPersonne = class
  private
    FEmail: string;  // Nouveau
  public
    property Email: string read FEmail write FEmail;
    // Seule la classe TPersonne est modifiée
  end;
```

## Quand utiliser quelle approche ?

### Utilisez la programmation procédurale quand :

✓ **Le programme est simple et petit**
```pascal
// Script simple de calcul
program CalculMoyenne;
var
  Notes: array[1..5] of Real;
  Moyenne: Real;
begin
  // Pas besoin d'objets ici
end;
```

✓ **Vous manipulez des données simples sans comportement**
```pascal
// Traitement de fichiers simples
procedure LireFichier(const NomFichier: string);
```

✓ **Pas besoin de réutilisation complexe**
```pascal
// Utilitaire unique
function ConvertirTemperature(Celsius: Real): Real;
```

✓ **Performance critique avec structures légères**
```pascal
// Calculs mathématiques intensifs
type
  TPoint2D = record
    X, Y: Real;
  end;
```

### Utilisez la programmation orientée objet quand :

✓ **Le programme est complexe et évolutif**
```pascal
// Application de gestion
type
  TApplication = class
    // Beaucoup de fonctionnalités interconnectées
  end;
```

✓ **Vous avez besoin d'encapsulation**
```pascal
// Protection des données sensibles
type
  TCompteBancaire = class
  private
    FSolde: Real;  // Protégé
  end;
```

✓ **Vous voulez réutiliser du code (héritage)**
```pascal
// Hiérarchie de classes
type
  TAnimal = class ... end;
  TChien = class(TAnimal) ... end;
  TChat = class(TAnimal) ... end;
```

✓ **Vous modélisez des entités du monde réel**
```pascal
// Application de gestion d'entreprise
type
  TEmploye = class ... end;
  TDepartement = class ... end;
  TEntreprise = class ... end;
```

✓ **Vous créez des interfaces graphiques**
```pascal
// Composants visuels
type
  TMonFormulaire = class(TForm)
    // Les GUI sont naturellement orientées objet
  end;
```

## Exemple de transition : d'une approche à l'autre

### Programme simple : calcul de moyenne (procédural)

```pascal
program MoyenneProcedural;

var
  Notes: array[1..5] of Real;
  I: Integer;
  Somme, Moyenne: Real;
begin
  // Saisie
  for I := 1 to 5 do
  begin
    Write('Note ', I, ' : ');
    ReadLn(Notes[I]);
  end;

  // Calcul
  Somme := 0;
  for I := 1 to 5 do
    Somme := Somme + Notes[I];
  Moyenne := Somme / 5;

  // Affichage
  WriteLn('Moyenne : ', Moyenne:0:2);
end.
```

**Verdict :** Procédural parfait ici, simple et direct.

### Programme complexe : gestion d'étudiants (objet)

```pascal
program GestionEtudiants;

type
  TEtudiant = class
  private
    FNom: string;
    FNotes: array of Real;
    function GetMoyenne: Real;
  public
    constructor Create(const Nom: string);
    procedure AjouterNote(Note: Real);
    procedure Afficher;
    property Moyenne: Real read GetMoyenne;
  end;

  TClasse = class
  private
    FEtudiants: array of TEtudiant;
  public
    procedure AjouterEtudiant(Etudiant: TEtudiant);
    procedure AfficherMoyenneGenerale;
    procedure AfficherMeilleursEtudiants;
  end;

// ... implémentation ...

var
  MaClasse: TClasse;
begin
  MaClasse := TClasse.Create;
  try
    // Code structuré et extensible
  finally
    MaClasse.Free;
  end;
end.
```

**Verdict :** Objet recommandé pour gérer la complexité.

## Peut-on mélanger les deux ?

Oui ! Dans FreePascal, vous pouvez combiner les deux approches :

```pascal
// Fonctions utilitaires (procédural)
function Max(A, B: Integer): Integer;
begin
  if A > B then Result := A else Result := B;
end;

// Classes pour la logique métier (objet)
type
  TCalculateur = class
  public
    function CalculerMax(A, B: Integer): Integer;
  end;

function TCalculateur.CalculerMax(A, B: Integer): Integer;
begin
  Result := Max(A, B);  // Utilise la fonction procédurale
end;
```

**Bonne pratique :** Utilisez des fonctions utilitaires procédurales pour les opérations simples, et des classes pour la logique complexe.

## Tableau comparatif récapitulatif

| Critère | Procédural | Orienté Objet |
|---------|-----------|---------------|
| **Organisation** | Fonctions séparées | Données + méthodes groupées |
| **Encapsulation** | Non | Oui (private/public) |
| **Réutilisation** | Copier-coller | Héritage |
| **Maintenance** | Difficile (code dispersé) | Facile (code groupé) |
| **Complexité** | Simple pour petits projets | Gérable pour gros projets |
| **Courbe d'apprentissage** | Rapide | Plus longue |
| **Performance** | Légèrement plus rapide | Légèrement plus lent |
| **Modélisation** | Procédures et données | Objets du monde réel |

## Évolution d'un projet

### Phase 1 : Script simple (procédural)
```pascal
program Script;
var
  X, Y: Integer;
begin
  X := 10;
  Y := 20;
  WriteLn(X + Y);
end.
```

### Phase 2 : Ajout de fonctions (procédural)
```pascal
program Programme;

function Additionner(A, B: Integer): Integer;
begin
  Result := A + B;
end;

begin
  WriteLn(Additionner(10, 20));
end.
```

### Phase 3 : Programme qui grandit (mixte)
```pascal
program ApplicationMoyenne;

// Fonctions utilitaires
function EstValide(Note: Real): Boolean;

// Classes pour la logique
type
  TEtudiant = class
    // ...
  end;
```

### Phase 4 : Application complète (objet)
```pascal
program ApplicationComplete;

type
  TApplication = class
  private
    FEtudiants: TList;
    FClasses: TList;
  public
    procedure Initialiser;
    procedure Executer;
  end;

begin
  with TApplication.Create do
  try
    Initialiser;
    Executer;
  finally
    Free;
  end;
end.
```

## Points clés à retenir

- La programmation **procédurale** est simple et directe pour les petits programmes
- La programmation **orientée objet** excelle pour les programmes complexes
- L'**encapsulation** protège les données en POO
- L'**héritage** permet la réutilisation du code en POO
- La POO facilite la **maintenance** et l'**évolution** du code
- On peut **mélanger** les deux approches selon les besoins
- Commencez procédural, passez à l'objet quand la complexité augmente
- La POO a une courbe d'apprentissage plus longue mais offre plus d'avantages à long terme
- Choisissez l'approche en fonction de la **taille** et de la **complexité** du projet

## Vers la suite

Dans la section suivante, nous aborderons l'**introduction aux diagrammes de classes UML**, un outil visuel pour concevoir et documenter vos classes avant même d'écrire du code. Cela vous aidera à mieux planifier vos applications orientées objet.

⏭️ [UML et diagrammes de classes basics](/10-fondamentaux-poo/11-uml-diagrammes-classes-basics.md)
