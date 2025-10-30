🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.4 Attributs et méthodes

## Introduction

Une classe est composée de deux éléments fondamentaux :
- Les **attributs** (ou champs) : les **données** que contient l'objet
- Les **méthodes** : les **actions** que l'objet peut effectuer

**Analogie :** Une voiture possède des caractéristiques (couleur, marque, vitesse actuelle) = **attributs**, et des actions (accélérer, freiner, tourner) = **méthodes**.

## Les Attributs

### Définition

Un **attribut** est une variable appartenant à un objet. Chaque instance de la classe possède sa propre copie des attributs.

```pascal
type
  TVoiture = class
  private
    FMarque: string;      // Attribut
    FCouleur: string;     // Attribut
    FVitesse: Integer;    // Attribut
  end;

var
  Voiture1, Voiture2: TVoiture;
begin
  Voiture1 := TVoiture.Create;
  Voiture2 := TVoiture.Create;

  // Chaque voiture a ses propres attributs
  // Voiture1.FMarque est différent de Voiture2.FMarque
end;
```

### Types d'attributs

#### Attributs de types simples

```pascal
type
  TPersonne = class
  private
    // Types numériques
    FAge: Integer;
    FTaille: Real;
    FPoids: Real;

    // Types texte
    FNom: string;
    FPrenom: string;
    FEmail: string;

    // Types booléens
    FEstMajeur: Boolean;
    FEstActif: Boolean;

    // Types caractère
    FInitiale: Char;
  end;
```

#### Attributs de types structurés

```pascal
type
  TEmploye = class
  private
    // Types date/heure
    FDateNaissance: TDateTime;
    FDateEmbauche: TDateTime;

    // Tableaux
    FNotes: array[1..5] of Integer;
    FCompetences: array of string;  // Tableau dynamique

    // Enregistrements
    FAdresse: record
      Rue: string;
      Ville: string;
      CodePostal: string;
    end;
  end;
```

#### Attributs objets (composition)

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
    FAdresse: TAdresse;  // Attribut objet
  end;
```

### Initialisation des attributs

Les attributs sont automatiquement initialisés avec des valeurs par défaut :
- **Integer, Real** : `0`
- **String** : `''` (chaîne vide)
- **Boolean** : `False`
- **Pointeurs, objets** : `nil`

```pascal
type
  TCompteur = class
  private
    FValeur: Integer;  // Sera initialisé à 0
  end;

var
  C: TCompteur;
begin
  C := TCompteur.Create;
  WriteLn(C.FValeur);  // Affiche : 0
end;
```

**Important :** Les objets contenus dans les attributs doivent être créés explicitement (nous verrons cela avec les constructeurs).

### Accès aux attributs

#### Depuis l'intérieur de la classe

Les méthodes de la classe peuvent accéder directement aux attributs :

```pascal
type
  TCompteur = class
  private
    FValeur: Integer;
  public
    procedure Incrementer;
    procedure Afficher;
  end;

procedure TCompteur.Incrementer;
begin
  FValeur := FValeur + 1;  // Accès direct
end;

procedure TCompteur.Afficher;
begin
  WriteLn('Valeur : ', FValeur);  // Accès direct
end;
```

#### Depuis l'extérieur de la classe

Les attributs privés ne sont pas accessibles depuis l'extérieur. On doit passer par des méthodes publiques :

```pascal
var
  C: TCompteur;
begin
  C := TCompteur.Create;
  // C.FValeur := 10;  // ✗ ERREUR : FValeur est private
  C.Incrementer;       // ✓ OK : méthode publique
end;
```

## Les Méthodes

### Définition

Une **méthode** est une procédure ou une fonction qui appartient à une classe. Elle peut manipuler les attributs de l'objet et effectuer des actions.

### Types de méthodes

#### Procédures (ne retournent rien)

```pascal
type
  TVoiture = class
  private
    FVitesse: Integer;
  public
    procedure Accelerer(Increment: Integer);
    procedure Freiner(Decrement: Integer);
    procedure Afficher;
  end;

procedure TVoiture.Accelerer(Increment: Integer);
begin
  FVitesse := FVitesse + Increment;
end;

procedure TVoiture.Afficher;
begin
  WriteLn('Vitesse actuelle : ', FVitesse, ' km/h');
end;
```

#### Fonctions (retournent une valeur)

```pascal
type
  TRectangle = class
  private
    FLargeur: Real;
    FHauteur: Real;
  public
    function CalculerSurface: Real;
    function CalculerPerimetre: Real;
    function EstCarre: Boolean;
  end;

function TRectangle.CalculerSurface: Real;
begin
  Result := FLargeur * FHauteur;
end;

function TRectangle.CalculerPerimetre: Real;
begin
  Result := 2 * (FLargeur + FHauteur);
end;

function TRectangle.EstCarre: Boolean;
begin
  Result := FLargeur = FHauteur;
end;
```

### Paramètres des méthodes

#### Paramètres par valeur

La méthode reçoit une **copie** de la valeur :

```pascal
procedure TVoiture.DefinirVitesse(Vitesse: Integer);
begin
  if (Vitesse >= 0) and (Vitesse <= 200) then
    FVitesse := Vitesse;
  // Modifier 'Vitesse' ici n'affecte pas la variable originale
end;
```

#### Paramètres par référence (var)

La méthode reçoit une **référence** à la variable originale :

```pascal
type
  TCalculatrice = class
  public
    procedure Diviser(A, B: Real; var Quotient, Reste: Real);
  end;

procedure TCalculatrice.Diviser(A, B: Real; var Quotient, Reste: Real);
begin
  if B <> 0 then
  begin
    Quotient := A / B;
    Reste := A - (Trunc(A / B) * B);
  end;
end;

// Utilisation
var
  Q, R: Real;
  Calc: TCalculatrice;
begin
  Calc := TCalculatrice.Create;
  Calc.Diviser(10, 3, Q, R);
  WriteLn('Quotient : ', Q:0:2);  // 3.33
  WriteLn('Reste : ', R:0:2);     // 1.00
end;
```

#### Paramètres constants (const)

Pour les paramètres de type complexe (string, objets), utilisez `const` pour éviter des copies inutiles :

```pascal
type
  TPersonne = class
  public
    procedure DefinirNom(const Nom: string);  // Pas de copie
    procedure AfficherMessage(const Msg: string);
  end;

procedure TPersonne.DefinirNom(const Nom: string);
begin
  FNom := Nom;  // OK, on peut lire
  // Nom := 'autre';  // ✗ ERREUR : on ne peut pas modifier
end;
```

### Méthodes avec plusieurs responsabilités

#### Méthodes accesseurs (Getters)

Méthodes qui **lisent** un attribut :

```pascal
type
  TCompteBancaire = class
  private
    FSolde: Real;
    FNumeroCompte: string;
  public
    function ObtenirSolde: Real;
    function ObtenirNumeroCompte: string;
  end;

function TCompteBancaire.ObtenirSolde: Real;
begin
  Result := FSolde;
end;

function TCompteBancaire.ObtenirNumeroCompte: string;
begin
  Result := FNumeroCompte;
end;
```

#### Méthodes modificateurs (Setters)

Méthodes qui **modifient** un attribut avec validation :

```pascal
type
  TCompteBancaire = class
  private
    FSolde: Real;
  public
    procedure DefinirSolde(Montant: Real);
    procedure Crediter(Montant: Real);
    procedure Debiter(Montant: Real);
  end;

procedure TCompteBancaire.DefinirSolde(Montant: Real);
begin
  if Montant >= 0 then
    FSolde := Montant
  else
    WriteLn('Erreur : solde ne peut pas être négatif');
end;

procedure TCompteBancaire.Crediter(Montant: Real);
begin
  if Montant > 0 then
    FSolde := FSolde + Montant;
end;

procedure TCompteBancaire.Debiter(Montant: Real);
begin
  if (Montant > 0) and (FSolde >= Montant) then
    FSolde := FSolde - Montant
  else
    WriteLn('Opération impossible');
end;
```

#### Méthodes de calcul

Méthodes qui effectuent des calculs basés sur les attributs :

```pascal
type
  TCercle = class
  private
    FRayon: Real;
  public
    procedure DefinirRayon(Valeur: Real);
    function CalculerSurface: Real;
    function CalculerCirconference: Real;
    function CalculerDiametre: Real;
  end;

function TCercle.CalculerSurface: Real;
const
  PI = 3.14159265359;
begin
  Result := PI * FRayon * FRayon;
end;

function TCercle.CalculerCirconference: Real;
const
  PI = 3.14159265359;
begin
  Result := 2 * PI * FRayon;
end;

function TCercle.CalculerDiametre: Real;
begin
  Result := 2 * FRayon;
end;
```

#### Méthodes utilitaires

Méthodes qui effectuent des actions diverses :

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FPrenom: string;
    FAge: Integer;
  public
    procedure Afficher;
    procedure Vieillir;
    function EstMajeur: Boolean;
    function ObtenirNomComplet: string;
  end;

procedure TPersonne.Afficher;
begin
  WriteLn('Nom complet : ', ObtenirNomComplet);
  WriteLn('Age : ', FAge, ' ans');
  if EstMajeur then
    WriteLn('Statut : Majeur')
  else
    WriteLn('Statut : Mineur');
end;

procedure TPersonne.Vieillir;
begin
  FAge := FAge + 1;
  WriteLn(FNom, ' a maintenant ', FAge, ' ans');
end;

function TPersonne.EstMajeur: Boolean;
begin
  Result := FAge >= 18;
end;

function TPersonne.ObtenirNomComplet: string;
begin
  Result := FPrenom + ' ' + FNom;
end;
```

## Interaction entre attributs et méthodes

### Méthodes qui modifient plusieurs attributs

```pascal
type
  TPoint = class
  private
    FX: Real;
    FY: Real;
  public
    procedure Deplacer(DeltaX, DeltaY: Real);
    procedure DeplacerVers(NouveauX, NouveauY: Real);
    procedure Centrer;
    function Distance(Autre: TPoint): Real;
  end;

procedure TPoint.Deplacer(DeltaX, DeltaY: Real);
begin
  FX := FX + DeltaX;
  FY := FY + DeltaY;
end;

procedure TPoint.DeplacerVers(NouveauX, NouveauY: Real);
begin
  FX := NouveauX;
  FY := NouveauY;
end;

procedure TPoint.Centrer;
begin
  FX := 0;
  FY := 0;
end;

function TPoint.Distance(Autre: TPoint): Real;
var
  DX, DY: Real;
begin
  DX := FX - Autre.FX;
  DY := FY - Autre.FY;
  Result := Sqrt(DX * DX + DY * DY);
end;
```

### Méthodes qui appellent d'autres méthodes

```pascal
type
  TCompteBancaire = class
  private
    FSolde: Real;
    FDecouvertAutorise: Real;
    function PeutDebiter(Montant: Real): Boolean;
  public
    procedure Debiter(Montant: Real);
    procedure Transferer(Destination: TCompteBancaire; Montant: Real);
  end;

function TCompteBancaire.PeutDebiter(Montant: Real): Boolean;
begin
  Result := (FSolde - Montant) >= -FDecouvertAutorise;
end;

procedure TCompteBancaire.Debiter(Montant: Real);
begin
  if PeutDebiter(Montant) then  // Appel d'une autre méthode
    FSolde := FSolde - Montant
  else
    WriteLn('Débit refusé : dépassement du découvert autorisé');
end;

procedure TCompteBancaire.Transferer(Destination: TCompteBancaire; Montant: Real);
begin
  if PeutDebiter(Montant) then
  begin
    Debiter(Montant);              // Appel d'une méthode de l'objet
    Destination.Crediter(Montant); // Appel d'une méthode d'un autre objet
    WriteLn('Transfert effectué : ', Montant:0:2, ' €');
  end
  else
    WriteLn('Transfert impossible');
end;
```

## Exemple complet : Classe TTemperature

```pascal
program ExempleTemperature;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TTemperature = class
  private
    // Attribut : température en Celsius
    FCelsius: Real;

    // Méthode privée de validation
    function TemperatureValide(Temp: Real): Boolean;

  public
    // Modificateurs
    procedure DefinirCelsius(Valeur: Real);
    procedure DefinirFahrenheit(Valeur: Real);
    procedure DefinirKelvin(Valeur: Real);

    // Accesseurs
    function ObtenirCelsius: Real;
    function ObtenirFahrenheit: Real;
    function ObtenirKelvin: Real;

    // Méthodes utilitaires
    procedure Augmenter(Delta: Real);
    procedure Diminuer(Delta: Real);
    function EstGelant: Boolean;
    function EstBouillant: Boolean;
    procedure Afficher;
  end;

// === IMPLÉMENTATION ===

function TTemperature.TemperatureValide(Temp: Real): Boolean;
begin
  // Le zéro absolu est -273.15°C
  Result := Temp >= -273.15;
end;

procedure TTemperature.DefinirCelsius(Valeur: Real);
begin
  if TemperatureValide(Valeur) then
    FCelsius := Valeur
  else
    WriteLn('Erreur : température invalide (< -273.15°C)');
end;

procedure TTemperature.DefinirFahrenheit(Valeur: Real);
var
  TempCelsius: Real;
begin
  TempCelsius := (Valeur - 32) * 5 / 9;
  DefinirCelsius(TempCelsius);
end;

procedure TTemperature.DefinirKelvin(Valeur: Real);
var
  TempCelsius: Real;
begin
  TempCelsius := Valeur - 273.15;
  DefinirCelsius(TempCelsius);
end;

function TTemperature.ObtenirCelsius: Real;
begin
  Result := FCelsius;
end;

function TTemperature.ObtenirFahrenheit: Real;
begin
  Result := (FCelsius * 9 / 5) + 32;
end;

function TTemperature.ObtenirKelvin: Real;
begin
  Result := FCelsius + 273.15;
end;

procedure TTemperature.Augmenter(Delta: Real);
begin
  DefinirCelsius(FCelsius + Delta);
end;

procedure TTemperature.Diminuer(Delta: Real);
begin
  DefinirCelsius(FCelsius - Delta);
end;

function TTemperature.EstGelant: Boolean;
begin
  Result := FCelsius <= 0;
end;

function TTemperature.EstBouillant: Boolean;
begin
  Result := FCelsius >= 100;
end;

procedure TTemperature.Afficher;
begin
  WriteLn('=== Température ===');
  WriteLn('Celsius    : ', ObtenirCelsius:0:2, ' °C');
  WriteLn('Fahrenheit : ', ObtenirFahrenheit:0:2, ' °F');
  WriteLn('Kelvin     : ', ObtenirKelvin:0:2, ' K');

  if EstGelant then
    WriteLn('État : L''eau gèle')
  else if EstBouillant then
    WriteLn('État : L''eau bout')
  else
    WriteLn('État : L''eau est liquide');

  WriteLn('==================');
end;

// === PROGRAMME PRINCIPAL ===

var
  Temp: TTemperature;
begin
  Temp := TTemperature.Create;
  try
    // Test avec Celsius
    WriteLn('--- Définition en Celsius ---');
    Temp.DefinirCelsius(25);
    Temp.Afficher;

    WriteLn;
    WriteLn('--- Définition en Fahrenheit ---');
    Temp.DefinirFahrenheit(68);  // ~20°C
    Temp.Afficher;

    WriteLn;
    WriteLn('--- Augmentation de 10°C ---');
    Temp.Augmenter(10);
    Temp.Afficher;

    WriteLn;
    WriteLn('--- Test température négative ---');
    Temp.DefinirCelsius(-10);
    Temp.Afficher;

  finally
    Temp.Free;
  end;
end.
```

## Organisation des attributs et méthodes

### Ordre recommandé dans la déclaration

```pascal
type
  TClasse = class
  private
    // 1. Attributs privés
    FAttribut1: Type1;
    FAttribut2: Type2;

    // 2. Méthodes privées (helpers internes)
    procedure MethodePrivee;

  protected
    // 3. Attributs et méthodes protégés (pour héritage)

  public
    // 4. Constructeur et destructeur (nous verrons cela plus tard)

    // 5. Modificateurs (setters)
    procedure DefinirAttribut1(Valeur: Type1);

    // 6. Accesseurs (getters)
    function ObtenirAttribut1: Type1;

    // 7. Méthodes de calcul
    function CalculerQuelqueChose: Real;

    // 8. Méthodes utilitaires
    procedure Afficher;
  end;
```

## Bonnes pratiques

### 1. Nommer clairement les méthodes

```pascal
// ✓ BON : noms explicites
procedure DefinirNom(const Valeur: string);
function ObtenirAge: Integer;
function CalculerSalaireAnnuel: Real;
procedure AfficherDetails;

// ✗ MAUVAIS : noms vagues
procedure Set1(V: string);
function Get2: Integer;
function Calc: Real;
procedure Show;
```

### 2. Une méthode = une responsabilité

```pascal
// ✗ MAUVAIS : fait trop de choses
procedure TPersonne.ToutFaire;
begin
  FAge := FAge + 1;
  FSalaire := FSalaire * 1.05;
  WriteLn('Fait');
end;

// ✓ BON : méthodes séparées
procedure TPersonne.Vieillir;
begin
  FAge := FAge + 1;
end;

procedure TPersonne.AugmenterSalaire(Pourcentage: Real);
begin
  FSalaire := FSalaire * (1 + Pourcentage / 100);
end;
```

### 3. Valider les données dans les setters

```pascal
procedure TPersonne.DefinirAge(Valeur: Integer);
begin
  if (Valeur >= 0) and (Valeur <= 150) then
    FAge := Valeur
  else
    raise Exception.Create('Age invalide');
end;
```

### 4. Utiliser des méthodes privées pour la logique complexe

```pascal
type
  TCalculatrice = class
  private
    // Méthode helper privée
    function EstNombrePremier(N: Integer): Boolean;
  public
    // Méthode publique qui utilise le helper
    procedure AfficherNombresPremiers(Max: Integer);
  end;
```

## Points clés à retenir

- Les **attributs** stockent l'état de l'objet (les données)
- Les **méthodes** définissent le comportement de l'objet (les actions)
- Attributs privés (préfixe `F`) + méthodes publiques = **encapsulation**
- Les getters lisent, les setters modifient avec validation
- Une méthode peut être une procédure (pas de retour) ou une fonction (avec retour)
- Les méthodes peuvent appeler d'autres méthodes de la même classe
- Toujours valider les données dans les setters
- Nommer les méthodes de façon descriptive (verbes d'action)

## Vers la suite

Dans la section suivante, nous approfondirons les **constructeurs et destructeurs**, qui sont des méthodes spéciales permettant d'initialiser et de libérer correctement les objets.

⏭️ [Constructeurs (Create)](/10-fondamentaux-poo/05-constructeurs-create.md)
