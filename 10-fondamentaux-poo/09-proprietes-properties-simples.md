🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.9 Propriétés (properties) simples

## Qu'est-ce qu'une propriété ?

Une **propriété** (property) est une fonctionnalité de Pascal qui permet d'accéder à des attributs privés avec une **syntaxe simple**, tout en gardant le **contrôle** offert par les méthodes accesseurs (getters/setters).

**Analogie :** Imaginez un thermostat. Vous tournez simplement un bouton (interface simple), mais en interne, il y a des circuits complexes qui vérifient et ajustent la température (contrôle et validation). Les propriétés fonctionnent de la même manière.

## Le problème que les propriétés résolvent

### Sans propriétés : méthodes accesseurs

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
  public
    function ObtenirNom: string;
    procedure DefinirNom(const Valeur: string);
    function ObtenirAge: Integer;
    procedure DefinirAge(Valeur: Integer);
  end;

// Utilisation : syntaxe lourde
var
  P: TPersonne;
begin
  P := TPersonne.Create;
  P.DefinirNom('Alice');           // ✗ Verbeux
  P.DefinirAge(30);
  WriteLn(P.ObtenirNom);           // ✗ Verbeux
  WriteLn(P.ObtenirAge);
  P.Free;
end;
```

### Avec propriétés : syntaxe naturelle

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
  public
    property Nom: string read FNom write FNom;
    property Age: Integer read FAge write FAge;
  end;

// Utilisation : syntaxe simple et naturelle
var
  P: TPersonne;
begin
  P := TPersonne.Create;
  P.Nom := 'Alice';      // ✓ Simple et clair
  P.Age := 30;
  WriteLn(P.Nom);        // ✓ Comme un attribut
  WriteLn(P.Age);
  P.Free;
end;
```

**Avantage :** La syntaxe est simple comme pour un attribut public, mais on garde le contrôle de l'encapsulation.

## Syntaxe de base

### Structure générale

```pascal
property NomPropriete: Type read MéthodeOuAttributLecture write MéthodeOuAttributEcriture;
```

**Éléments :**
- `property` : mot-clé
- `NomPropriete` : nom de la propriété (convention : PascalCase, pas de préfixe F)
- `Type` : type de données de la propriété
- `read` : comment lire la valeur
- `write` : comment écrire la valeur (facultatif)

### Exemple minimal

```pascal
type
  TCompteur = class
  private
    FValeur: Integer;
  public
    property Valeur: Integer read FValeur write FValeur;
  end;

var
  C: TCompteur;
begin
  C := TCompteur.Create;
  C.Valeur := 10;        // Équivaut à : C.FValeur := 10
  WriteLn(C.Valeur);     // Équivaut à : WriteLn(C.FValeur)
  C.Free;
end;
```

## Types de propriétés

### 1. Propriété en lecture/écriture (Read/Write)

La plus courante : on peut lire et écrire la valeur.

```pascal
type
  TRectangle = class
  private
    FLargeur: Real;
    FHauteur: Real;
  public
    property Largeur: Real read FLargeur write FLargeur;
    property Hauteur: Real read FHauteur write FHauteur;
  end;

var
  R: TRectangle;
begin
  R := TRectangle.Create;
  R.Largeur := 10;     // Écriture
  R.Hauteur := 5;      // Écriture
  WriteLn(R.Largeur);  // Lecture
  R.Free;
end;
```

### 2. Propriété en lecture seule (Read Only)

On peut lire la valeur mais pas la modifier depuis l'extérieur.

```pascal
type
  TCompteur = class
  private
    FValeur: Integer;
  public
    procedure Incrementer;
    property Valeur: Integer read FValeur;  // Pas de write
  end;

procedure TCompteur.Incrementer;
begin
  FValeur := FValeur + 1;
end;

var
  C: TCompteur;
begin
  C := TCompteur.Create;
  C.Incrementer;
  WriteLn(C.Valeur);    // ✓ OK : lecture
  // C.Valeur := 10;    // ✗ ERREUR : propriété en lecture seule
  C.Free;
end;
```

### 3. Propriété en écriture seule (Write Only)

Rarement utilisée : on peut écrire mais pas lire.

```pascal
type
  TConfiguration = class
  private
    FMotDePasse: string;
  public
    property MotDePasse: string write FMotDePasse;  // Pas de read
  end;

var
  Config: TConfiguration;
begin
  Config := TConfiguration.Create;
  Config.MotDePasse := 'secret123';  // ✓ OK : écriture
  // WriteLn(Config.MotDePasse);     // ✗ ERREUR : propriété en écriture seule
  Config.Free;
end;
```

## Propriétés avec méthodes accesseurs

### Getter (méthode de lecture)

Au lieu d'accéder directement à l'attribut, on peut utiliser une méthode :

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FPrenom: string;
    function GetNomComplet: string;  // Getter
  public
    property NomComplet: string read GetNomComplet;
  end;

function TPersonne.GetNomComplet: string;
begin
  Result := FPrenom + ' ' + FNom;
end;

var
  P: TPersonne;
begin
  P := TPersonne.Create;
  P.FPrenom := 'Marie';
  P.FNom := 'Curie';
  WriteLn(P.NomComplet);  // Appelle GetNomComplet, affiche : "Marie Curie"
  P.Free;
end;
```

### Setter (méthode d'écriture)

Pour valider ou traiter les données avant de les stocker :

```pascal
type
  TPersonne = class
  private
    FAge: Integer;
    procedure SetAge(Valeur: Integer);  // Setter
  public
    property Age: Integer read FAge write SetAge;
  end;

procedure TPersonne.SetAge(Valeur: Integer);
begin
  if (Valeur >= 0) and (Valeur <= 150) then
    FAge := Valeur
  else
  begin
    WriteLn('Erreur : âge invalide (', Valeur, ')');
    FAge := 0;
  end;
end;

var
  P: TPersonne;
begin
  P := TPersonne.Create;
  P.Age := 30;      // Appelle SetAge(30), valide et accepte
  P.Age := -5;      // Appelle SetAge(-5), rejette et met à 0
  P.Age := 200;     // Appelle SetAge(200), rejette et met à 0
  WriteLn(P.Age);   // Affiche : 0
  P.Free;
end;
```

### Getter et Setter combinés

```pascal
type
  TRectangle = class
  private
    FLargeur: Real;
    function GetSurface: Real;
    procedure SetLargeur(Valeur: Real);
  public
    property Largeur: Real read FLargeur write SetLargeur;
    property Surface: Real read GetSurface;  // Lecture seule calculée
  end;

procedure TRectangle.SetLargeur(Valeur: Real);
begin
  if Valeur > 0 then
    FLargeur := Valeur
  else
    raise Exception.Create('Largeur doit être positive');
end;

function TRectangle.GetSurface: Real;
begin
  Result := FLargeur * FHauteur;
end;
```

## Propriétés calculées

Une propriété peut être calculée à la volée sans avoir d'attribut correspondant :

```pascal
type
  TCercle = class
  private
    FRayon: Real;
    function GetDiametre: Real;
    procedure SetDiametre(Valeur: Real);
    function GetCirconference: Real;
    function GetSurface: Real;
  public
    property Rayon: Real read FRayon write FRayon;
    property Diametre: Real read GetDiametre write SetDiametre;
    property Circonference: Real read GetCirconference;  // Calculée, lecture seule
    property Surface: Real read GetSurface;              // Calculée, lecture seule
  end;

function TCercle.GetDiametre: Real;
begin
  Result := FRayon * 2;
end;

procedure TCercle.SetDiametre(Valeur: Real);
begin
  FRayon := Valeur / 2;
end;

function TCercle.GetCirconference: Real;
const
  PI = 3.14159265359;
begin
  Result := 2 * PI * FRayon;
end;

function TCercle.GetSurface: Real;
const
  PI = 3.14159265359;
begin
  Result := PI * FRayon * FRayon;
end;

// Utilisation
var
  C: TCercle;
begin
  C := TCercle.Create;

  C.Rayon := 5;
  WriteLn('Rayon : ', C.Rayon:0:2);
  WriteLn('Diamètre : ', C.Diametre:0:2);       // Calculé automatiquement
  WriteLn('Circonférence : ', C.Circonference:0:2);  // Calculé automatiquement
  WriteLn('Surface : ', C.Surface:0:2);         // Calculé automatiquement

  WriteLn;

  // On peut aussi définir le diamètre
  C.Diametre := 20;
  WriteLn('Nouveau rayon : ', C.Rayon:0:2);     // Affiche : 10.00

  C.Free;
end;
```

## Conventions de nommage

### Pour les attributs

```pascal
private
  FNom: string;        // ✓ Préfixe F (Field)
  FAge: Integer;
  FEstActif: Boolean;
```

### Pour les propriétés

```pascal
public
  property Nom: string ...       // ✓ Pas de préfixe, PascalCase
  property Age: Integer ...
  property EstActif: Boolean ...
```

### Pour les getters/setters

```pascal
private
  function GetNom: string;       // ✓ Get + nom de la propriété
  procedure SetNom(const Valeur: string);  // ✓ Set + nom de la propriété

  function GetAge: Integer;
  procedure SetAge(Valeur: Integer);
```

## Exemple complet : Classe TCompteBancaire

```pascal
program ExempleProperties;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TCompteBancaire = class
  private
    FNumeroCompte: string;
    FSolde: Real;
    FTitulaire: string;
    FTauxInteret: Real;
    FHistorique: array of string;

    // Getters
    function GetSoldeFormate: string;
    function GetInteretsAnnuels: Real;
    function GetNombreOperations: Integer;

    // Setters
    procedure SetTitulaire(const Valeur: string);
    procedure SetTauxInteret(Valeur: Real);

    // Méthode privée
    procedure AjouterHistorique(const Operation: string);

  public
    constructor Create(const NumeroCompte, Titulaire: string; SoldeInitial: Real);
    destructor Destroy; override;

    procedure Crediter(Montant: Real);
    procedure Debiter(Montant: Real);
    procedure AfficherHistorique;

    // Propriétés
    property NumeroCompte: string read FNumeroCompte;  // Lecture seule
    property Solde: Real read FSolde;                  // Lecture seule
    property SoldeFormate: string read GetSoldeFormate; // Calculée
    property Titulaire: string read FTitulaire write SetTitulaire;
    property TauxInteret: Real read FTauxInteret write SetTauxInteret;
    property InteretsAnnuels: Real read GetInteretsAnnuels;  // Calculée
    property NombreOperations: Integer read GetNombreOperations;  // Calculée
  end;

// === IMPLÉMENTATION ===

constructor TCompteBancaire.Create(const NumeroCompte, Titulaire: string; SoldeInitial: Real);
begin
  inherited Create;
  FNumeroCompte := NumeroCompte;
  FTitulaire := Titulaire;
  FSolde := SoldeInitial;
  FTauxInteret := 0.02;  // 2% par défaut
  SetLength(FHistorique, 0);
  AjouterHistorique('Ouverture du compte avec solde : ' + FloatToStr(SoldeInitial) + ' €');
end;

destructor TCompteBancaire.Destroy;
begin
  SetLength(FHistorique, 0);
  inherited Destroy;
end;

procedure TCompteBancaire.AjouterHistorique(const Operation: string);
var
  Index: Integer;
begin
  Index := Length(FHistorique);
  SetLength(FHistorique, Index + 1);
  FHistorique[Index] := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ' + Operation;
end;

function TCompteBancaire.GetSoldeFormate: string;
begin
  Result := FormatFloat('#,##0.00', FSolde) + ' €';
end;

function TCompteBancaire.GetInteretsAnnuels: Real;
begin
  Result := FSolde * FTauxInteret;
end;

function TCompteBancaire.GetNombreOperations: Integer;
begin
  Result := Length(FHistorique);
end;

procedure TCompteBancaire.SetTitulaire(const Valeur: string);
begin
  if Length(Valeur) > 0 then
  begin
    FTitulaire := Valeur;
    AjouterHistorique('Changement de titulaire : ' + Valeur);
  end
  else
    WriteLn('Erreur : nom de titulaire invalide');
end;

procedure TCompteBancaire.SetTauxInteret(Valeur: Real);
begin
  if (Valeur >= 0) and (Valeur <= 0.10) then  // Max 10%
  begin
    FTauxInteret := Valeur;
    AjouterHistorique('Nouveau taux d''intérêt : ' + FloatToStr(Valeur * 100) + '%');
  end
  else
    WriteLn('Erreur : taux d''intérêt invalide (doit être entre 0 et 10%)');
end;

procedure TCompteBancaire.Crediter(Montant: Real);
begin
  if Montant > 0 then
  begin
    FSolde := FSolde + Montant;
    AjouterHistorique('Crédit de ' + FloatToStr(Montant) + ' €');
    WriteLn('Crédit effectué : ', Montant:0:2, ' €');
  end
  else
    WriteLn('Erreur : montant invalide');
end;

procedure TCompteBancaire.Debiter(Montant: Real);
begin
  if Montant > 0 then
  begin
    if FSolde >= Montant then
    begin
      FSolde := FSolde - Montant;
      AjouterHistorique('Débit de ' + FloatToStr(Montant) + ' €');
      WriteLn('Débit effectué : ', Montant:0:2, ' €');
    end
    else
      WriteLn('Erreur : solde insuffisant');
  end
  else
    WriteLn('Erreur : montant invalide');
end;

procedure TCompteBancaire.AfficherHistorique;
var
  I: Integer;
begin
  WriteLn('=== Historique du compte ', FNumeroCompte, ' ===');
  for I := 0 to High(FHistorique) do
    WriteLn(FHistorique[I]);
  WriteLn('==========================================');
end;

// === PROGRAMME PRINCIPAL ===

var
  Compte: TCompteBancaire;
begin
  WriteLn('=== Création du compte ===');
  Compte := TCompteBancaire.Create('FR123456789', 'Jean Dupont', 1000);
  WriteLn;

  WriteLn('=== Affichage des propriétés ===');
  WriteLn('Numéro : ', Compte.NumeroCompte);           // Lecture seule
  WriteLn('Titulaire : ', Compte.Titulaire);
  WriteLn('Solde : ', Compte.Solde:0:2, ' €');
  WriteLn('Solde formaté : ', Compte.SoldeFormate);    // Propriété calculée
  WriteLn('Taux d''intérêt : ', (Compte.TauxInteret * 100):0:2, '%');
  WriteLn('Intérêts annuels : ', Compte.InteretsAnnuels:0:2, ' €');  // Calculée
  WriteLn('Nombre d''opérations : ', Compte.NombreOperations);
  WriteLn;

  WriteLn('=== Opérations ===');
  Compte.Crediter(500);
  Compte.Debiter(200);
  WriteLn('Nouveau solde : ', Compte.SoldeFormate);
  WriteLn;

  WriteLn('=== Modification des propriétés ===');
  Compte.Titulaire := 'Marie Dupont';  // Utilise SetTitulaire avec validation
  Compte.TauxInteret := 0.03;          // 3%, utilise SetTauxInteret
  WriteLn('Nouveau titulaire : ', Compte.Titulaire);
  WriteLn('Nouveau taux : ', (Compte.TauxInteret * 100):0:2, '%');
  WriteLn('Nouveaux intérêts annuels : ', Compte.InteretsAnnuels:0:2, ' €');
  WriteLn;

  WriteLn('=== Tentative de modification invalide ===');
  Compte.TauxInteret := 0.15;  // Trop élevé, sera rejeté
  WriteLn;

  // Tentative d'écriture sur propriété en lecture seule (décommentez pour voir l'erreur)
  // Compte.Solde := 5000;  // ✗ ERREUR de compilation
  // Compte.NumeroCompte := 'AUTRE';  // ✗ ERREUR de compilation

  Compte.AfficherHistorique;
  WriteLn;

  Compte.Free;
  WriteLn('Programme terminé');
end.
```

## Propriétés et tableaux

Vous pouvez créer des propriétés pour accéder aux éléments d'un tableau :

```pascal
type
  TListeNombres = class
  private
    FNombres: array of Integer;
    function GetNombre(Index: Integer): Integer;
    procedure SetNombre(Index: Integer; Valeur: Integer);
    function GetTaille: Integer;
  public
    constructor Create(Taille: Integer);
    property Nombres[Index: Integer]: Integer read GetNombre write SetNombre; default;
    property Taille: Integer read GetTaille;
  end;

constructor TListeNombres.Create(Taille: Integer);
begin
  inherited Create;
  SetLength(FNombres, Taille);
end;

function TListeNombres.GetNombre(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < Length(FNombres)) then
    Result := FNombres[Index]
  else
    raise Exception.Create('Index hors limites');
end;

procedure TListeNombres.SetNombre(Index: Integer; Valeur: Integer);
begin
  if (Index >= 0) and (Index < Length(FNombres)) then
    FNombres[Index] := Valeur
  else
    raise Exception.Create('Index hors limites');
end;

function TListeNombres.GetTaille: Integer;
begin
  Result := Length(FNombres);
end;

// Utilisation
var
  Liste: TListeNombres;
  I: Integer;
begin
  Liste := TListeNombres.Create(5);

  // Remplissage
  for I := 0 to Liste.Taille - 1 do
    Liste.Nombres[I] := I * 10;  // Utilise SetNombre

  // Lecture
  for I := 0 to Liste.Taille - 1 do
    WriteLn('Nombres[', I, '] = ', Liste.Nombres[I]);  // Utilise GetNombre

  Liste.Free;
end;
```

## Avantages des propriétés

### 1. Syntaxe simple

```pascal
// Avec propriétés
P.Age := 30;
WriteLn(P.Age);

// Sans propriétés (méthodes)
P.DefinirAge(30);
WriteLn(P.ObtenirAge);
```

### 2. Validation transparente

```pascal
P.Age := -5;  // Le setter valide et rejette automatiquement
```

### 3. Propriétés calculées

```pascal
WriteLn(Cercle.Surface);  // Calculé à la volée, pas d'attribut nécessaire
```

### 4. Évolution du code

Vous pouvez commencer avec un accès direct, puis ajouter de la validation plus tard :

```pascal
// Version 1 : accès direct
property Age: Integer read FAge write FAge;

// Version 2 : avec validation (le code utilisant la propriété ne change pas)
property Age: Integer read FAge write SetAge;
```

### 5. Lecture seule

```pascal
property NumeroCompte: string read FNumeroCompte;  // Protection simple
```

## Bonnes pratiques

### 1. Nommer les propriétés sans préfixe

```pascal
// ✓ BON
property Nom: string read FNom write FNom;

// ✗ MAUVAIS
property FNom: string read FNom write FNom;
```

### 2. Attributs privés + propriétés publiques

```pascal
type
  TClasse = class
  private
    FValeur: Integer;  // ✓ Attribut privé
  public
    property Valeur: Integer read FValeur write FValeur;  // ✓ Propriété publique
  end;
```

### 3. Utiliser des setters pour la validation

```pascal
private
  procedure SetAge(Valeur: Integer);
public
  property Age: Integer read FAge write SetAge;  // ✓ Validation via setter
```

### 4. Propriétés calculées en lecture seule

```pascal
// ✓ BON : propriété calculée sans setter
property Surface: Real read GetSurface;

// ✗ Pas de sens : on ne peut pas "définir" une surface calculée
// property Surface: Real read GetSurface write SetSurface;
```

### 5. Documenter les propriétés

```pascal
type
  TPersonne = class
  public
    // Obtient ou définit le nom de la personne
    // Le nom ne peut pas être vide
    property Nom: string read FNom write SetNom;

    // Obtient l'âge en années
    // Calculé à partir de la date de naissance
    property Age: Integer read GetAge;
  end;
```

## Erreurs courantes à éviter

### Erreur n°1 : Accès direct à l'attribut au lieu de la propriété

```pascal
// ✗ MAUVAIS : contourne la validation
MonObjet.FAge := -5;

// ✓ BON : utilise la propriété avec validation
MonObjet.Age := -5;
```

### Erreur n°2 : Setter qui ne fait rien

```pascal
// ✗ MAUVAIS : setter inutile
procedure TPersonne.SetNom(const Valeur: string);
begin
  FNom := Valeur;  // Pas de validation, autant utiliser l'accès direct
end;

// ✓ MIEUX : accès direct si pas de validation
property Nom: string read FNom write FNom;
```

### Erreur n°3 : Oublier le const pour les strings

```pascal
// ✗ Moins efficace
procedure SetNom(Valeur: string);

// ✓ BON : évite la copie
procedure SetNom(const Valeur: string);
```

### Erreur n°4 : Propriété en écriture avec calcul complexe

```pascal
// ✗ DÉCONSEILLÉ : setter avec calcul lourd
function TClasse.SetValeurComplexe(Valeur: Integer);
begin
  // Calculs très lourds...
  FValeur := /* résultat */;
end;

// ✓ MIEUX : méthode explicite
procedure TClasse.CalculerEtDefinirValeur(Valeur: Integer);
```

### Erreur n°5 : Mélanger accès direct et propriétés

```pascal
type
  TClasse = class
  public
    Attribut1: Integer;  // ✗ Accès direct
    property Attribut2: Integer read FAttribut2 write FAttribut2;  // Propriété
  end;

// ✓ MIEUX : cohérence
type
  TClasse = class
  private
    FAttribut1: Integer;
    FAttribut2: Integer;
  public
    property Attribut1: Integer read FAttribut1 write FAttribut1;
    property Attribut2: Integer read FAttribut2 write FAttribut2;
  end;
```

## Propriétés vs Méthodes : quand utiliser quoi ?

### Utilisez une propriété quand :

- Vous accédez à une **valeur** (comme un attribut)
- L'opération est **rapide** et sans effet de bord majeur
- La syntaxe `Objet.Propriete` est naturelle

```pascal
property Nom: string read FNom write SetNom;
property Age: Integer read GetAge;
property Surface: Real read GetSurface;
```

### Utilisez une méthode quand :

- L'opération est **complexe** ou **coûteuse**
- L'opération a des **effets de bord** importants
- Vous avez besoin de **plusieurs paramètres**

```pascal
procedure CalculerStatistiquesCompletes;  // Opération lourde
procedure EnvoyerEmail(const Destinataire, Sujet: string);  // Plusieurs paramètres
function TrouverUtilisateur(ID: Integer): TUtilisateur;  // Action explicite
```

## Points clés à retenir

- Les **propriétés** combinent simplicité d'accès et contrôle
- Syntaxe : `property Nom: Type read ... write ...;`
- **read** : comment lire (attribut ou méthode getter)
- **write** : comment écrire (attribut ou méthode setter)
- Les propriétés peuvent être **en lecture seule**, **en écriture seule**, ou **lecture/écriture**
- Les **propriétés calculées** n'ont pas d'attribut correspondant
- Toujours mettre les attributs en **private** et utiliser des propriétés **public**
- Utiliser des **setters** pour valider les données
- Nommer les propriétés sans préfixe (pas de F)
- Les propriétés offrent une **évolution** facile du code

## Vers la suite

Dans la section suivante, nous aborderons la **comparaison entre programmation procédurale et programmation orientée objet**, ce qui vous permettra de mieux comprendre quand et pourquoi utiliser l'approche objet plutôt que l'approche procédurale.

⏭️ [Comparaison procédural vs objet](/10-fondamentaux-poo/10-comparaison-procedural-vs-objet.md)
