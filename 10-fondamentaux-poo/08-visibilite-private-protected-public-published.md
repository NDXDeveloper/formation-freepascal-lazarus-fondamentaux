🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.8 Visibilité : private, protected, public, published

## Introduction

La **visibilité** (ou **portée**) détermine quelles parties du code peuvent accéder aux membres (attributs et méthodes) d'une classe. C'est un mécanisme fondamental pour **contrôler l'accès** et garantir l'**encapsulation**.

**Analogie :** Imaginez votre maison. Certaines pièces sont privées (votre chambre), d'autres sont accessibles aux invités (le salon), et certaines peuvent être partagées avec la famille élargie (le jardin). Les niveaux de visibilité en POO fonctionnent de la même manière.

## Les quatre niveaux de visibilité

Pascal propose quatre niveaux de visibilité, du plus restrictif au plus ouvert :

| Niveau | Accessible depuis la classe | Accessible depuis les descendants | Accessible depuis l'extérieur |
|--------|----------------------------|----------------------------------|------------------------------|
| `private` | ✓ Oui | ✗ Non | ✗ Non |
| `protected` | ✓ Oui | ✓ Oui | ✗ Non |
| `public` | ✓ Oui | ✓ Oui | ✓ Oui |
| `published` | ✓ Oui | ✓ Oui | ✓ Oui (+ RTTI) |

## 1. Private : Le plus restrictif

### Définition

Les membres `private` sont accessibles **uniquement depuis l'intérieur de la classe** où ils sont déclarés.

### Quand utiliser private ?

- Pour les **détails d'implémentation** qui ne concernent que la classe
- Pour les **attributs** (la plupart du temps)
- Pour les **méthodes auxiliaires** internes

### Exemple détaillé

```pascal
type
  TCompteBancaire = class
  private
    // Attributs privés : détails internes
    FSolde: Real;
    FNumeroCompte: string;
    FCodeSecret: Integer;
    FHistorique: array of string;

    // Méthodes privées : helpers internes
    procedure AjouterHistorique(const Operation: string);
    function VerifierCodeSecret(Code: Integer): Boolean;
    function CalculerInterets: Real;

  public
    constructor Create(const NumeroCompte: string; CodeSecret: Integer);
    procedure Crediter(Montant: Real; Code: Integer);
    procedure Debiter(Montant: Real; Code: Integer);
    function ObtenirSolde(Code: Integer): Real;
  end;

// Méthode privée : utilisée uniquement en interne
procedure TCompteBancaire.AjouterHistorique(const Operation: string);
var
  Index: Integer;
begin
  Index := Length(FHistorique);
  SetLength(FHistorique, Index + 1);
  FHistorique[Index] := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ' + Operation;
end;

function TCompteBancaire.VerifierCodeSecret(Code: Integer): Boolean;
begin
  Result := Code = FCodeSecret;
end;

// Méthode publique qui utilise les méthodes privées
procedure TCompteBancaire.Crediter(Montant: Real; Code: Integer);
begin
  if not VerifierCodeSecret(Code) then
  begin
    WriteLn('Code secret incorrect');
    Exit;
  end;

  if Montant > 0 then
  begin
    FSolde := FSolde + Montant;
    AjouterHistorique('Crédit de ' + FloatToStr(Montant) + ' €');
    WriteLn('Crédit effectué : ', Montant:0:2, ' €');
  end;
end;

// Programme principal
var
  Compte: TCompteBancaire;
begin
  Compte := TCompteBancaire.Create('FR123456', 1234);

  // ✓ OK : méthode publique
  Compte.Crediter(100, 1234);

  // ✗ ERREUR de compilation : FSolde est private
  // WriteLn(Compte.FSolde);

  // ✗ ERREUR de compilation : AjouterHistorique est private
  // Compte.AjouterHistorique('Test');

  Compte.Free;
end;
```

### Avantages de private

1. **Protection des données** : empêche les modifications accidentelles
2. **Flexibilité** : on peut changer l'implémentation interne sans affecter le code externe
3. **Clarté** : sépare clairement l'interface publique de l'implémentation

## 2. Protected : Pour l'héritage

### Définition

Les membres `protected` sont accessibles depuis la classe **et depuis ses classes dérivées** (héritage).

### Quand utiliser protected ?

- Pour les attributs et méthodes qui seront utiles aux **classes filles**
- Pour les **méthodes auxiliaires** qu'on veut rendre disponibles aux descendants
- Quand on prévoit que la classe sera **étendue** par héritage

### Exemple simple (aperçu de l'héritage)

```pascal
type
  TAnimal = class
  protected
    FNom: string;              // Accessible aux classes dérivées
    FAge: Integer;
    procedure FaireUnBruit;     // Accessible aux classes dérivées
  public
    constructor Create(const Nom: string; Age: Integer);
    procedure Afficher;
  end;

  TChien = class(TAnimal)  // TChien hérite de TAnimal
  public
    procedure Aboyer;
    procedure Presenter;
  end;

constructor TAnimal.Create(const Nom: string; Age: Integer);
begin
  inherited Create;
  FNom := Nom;    // OK : accessible dans TAnimal
  FAge := Age;
end;

procedure TAnimal.FaireUnBruit;
begin
  WriteLn('L''animal fait un bruit');
end;

procedure TAnimal.Afficher;
begin
  WriteLn('Animal : ', FNom, ', Age : ', FAge);
end;

procedure TChien.Aboyer;
begin
  // ✓ OK : FNom est protected, donc accessible ici
  WriteLn(FNom, ' aboie : Ouaf ! Ouaf !');
end;

procedure TChien.Presenter;
begin
  // ✓ OK : FNom et FAge sont protected
  WriteLn('Je m''appelle ', FNom, ' et j''ai ', FAge, ' ans');

  // ✓ OK : FaireUnBruit est protected
  FaireUnBruit;
end;

// Programme principal
var
  MonChien: TChien;
begin
  MonChien := TChien.Create('Rex', 5);

  // ✓ OK : méthodes publiques
  MonChien.Afficher;
  MonChien.Aboyer;

  // ✗ ERREUR : FNom est protected, pas accessible depuis l'extérieur
  // WriteLn(MonChien.FNom);

  MonChien.Free;
end;
```

### Protected vs Private

```pascal
type
  TPersonne = class
  private
    FCodeSecuriteSociale: string;  // Vraiment privé
  protected
    FNom: string;                   // Partagé avec les descendants
    FAge: Integer;
  public
    procedure Afficher;
  end;

  TEmploye = class(TPersonne)
  public
    procedure AfficherDetails;
  end;

procedure TEmploye.AfficherDetails;
begin
  // ✓ OK : FNom et FAge sont protected
  WriteLn('Employé : ', FNom, ', ', FAge, ' ans');

  // ✗ ERREUR : FCodeSecuriteSociale est private dans TPersonne
  // WriteLn('SS : ', FCodeSecuriteSociale);
end;
```

## 3. Public : L'interface de la classe

### Définition

Les membres `public` sont accessibles **partout** : dans la classe, dans les descendants, et depuis l'extérieur.

### Quand utiliser public ?

- Pour les **méthodes** qui constituent l'interface de la classe
- Pour les **constructeurs** et **destructeurs**
- Pour les **propriétés** (que nous verrons plus tard)
- Rarement pour les attributs (préférez private + accesseurs)

### Exemple d'interface publique bien conçue

```pascal
type
  TRectangle = class
  private
    // Attributs privés
    FLargeur: Real;
    FHauteur: Real;

    // Validation privée
    function DimensionValide(Valeur: Real): Boolean;

  public
    // Constructeurs
    constructor Create; overload;
    constructor Create(Largeur, Hauteur: Real); overload;

    // Modificateurs (Setters)
    procedure DefinirLargeur(Valeur: Real);
    procedure DefinirHauteur(Valeur: Real);
    procedure DefinirDimensions(Largeur, Hauteur: Real);

    // Accesseurs (Getters)
    function ObtenirLargeur: Real;
    function ObtenirHauteur: Real;

    // Méthodes de calcul
    function CalculerSurface: Real;
    function CalculerPerimetre: Real;
    function CalculerDiagonale: Real;

    // Méthodes de transformation
    procedure Redimensionner(Facteur: Real);
    procedure Pivoter;  // Échange largeur et hauteur

    // Méthodes de comparaison
    function EstCarre: Boolean;
    function EstPlusGrandQue(Autre: TRectangle): Boolean;

    // Utilitaires
    procedure Afficher;
    function Cloner: TRectangle;
  end;
```

### Danger : attributs publics

```pascal
type
  // ✗ MAUVAISE PRATIQUE : attributs publics
  TRectangleMauvais = class
  public
    Largeur: Real;  // Accessible partout, pas de contrôle
    Hauteur: Real;  // Pas de validation
  end;

var
  R: TRectangleMauvais;
begin
  R := TRectangleMauvais.Create;
  R.Largeur := -10;   // ✗ Valeur invalide acceptée !
  R.Hauteur := 0;     // ✗ Pas de validation !
  R.Free;
end;
```

```pascal
type
  // ✓ BONNE PRATIQUE : attributs privés + méthodes publiques
  TRectangleBon = class
  private
    FLargeur: Real;
    FHauteur: Real;
  public
    procedure DefinirLargeur(Valeur: Real);
    procedure DefinirHauteur(Valeur: Real);
  end;

procedure TRectangleBon.DefinirLargeur(Valeur: Real);
begin
  if Valeur > 0 then
    FLargeur := Valeur
  else
    raise Exception.Create('Largeur doit être positive');
end;
```

## 4. Published : Pour les composants visuels

### Définition

Les membres `published` sont comme `public`, mais avec des **informations de type à l'exécution** (RTTI - Run-Time Type Information). Principalement utilisé pour les composants visuels Lazarus.

### Quand utiliser published ?

- Pour les **propriétés** qui doivent apparaître dans l'**inspecteur d'objets** de Lazarus
- Pour les **composants visuels** (boutons, labels, etc.)
- Pour permettre la **sérialisation** automatique

### Exemple avec composants Lazarus

```pascal
type
  TMonComposant = class(TCustomControl)
  private
    FTitre: string;
    FCouleurFond: TColor;
    FVisible: Boolean;

    procedure SetTitre(const Valeur: string);
    procedure SetCouleurFond(Valeur: TColor);

  published
    // Ces propriétés apparaîtront dans l'inspecteur d'objets
    property Titre: string read FTitre write SetTitre;
    property CouleurFond: TColor read FCouleurFond write SetCouleurFond;
    property Visible: Boolean read FVisible write FVisible;
  end;

procedure TMonComposant.SetTitre(const Valeur: string);
begin
  if FTitre <> Valeur then
  begin
    FTitre := Valeur;
    Invalidate;  // Redessine le composant
  end;
end;
```

### Published vs Public

```pascal
type
  TConfiguration = class
  private
    FNom: string;
    FValeur: Integer;
  public
    // Simple accès public
    property Nom: string read FNom write FNom;
  published
    // Accessible + informations RTTI (peut être sauvegardé/chargé automatiquement)
    property Valeur: Integer read FValeur write FValeur;
  end;
```

## Choix du niveau de visibilité : Guide pratique

### Arbre de décision

```
Pour un attribut :
└─ Est-ce un détail d'implémentation ?
   ├─ OUI → private
   └─ NON → Les classes dérivées en auront-elles besoin ?
      ├─ OUI → protected
      └─ NON → Utilisez private + propriété published (composants)

Pour une méthode :
└─ Fait-elle partie de l'interface publique ?
   ├─ OUI → public
   └─ NON → Les classes dérivées l'utiliseront-elles ?
      ├─ OUI → protected
      └─ NON → private
```

### Exemples de décisions

```pascal
type
  TVehicule = class
  private
    // Détails internes, personne d'autre n'en a besoin
    FNumeroSerie: string;
    FDerniereMaintenance: TDateTime;

    // Méthode helper interne
    function CalculerUsure: Real;

  protected
    // Les classes dérivées (TVoiture, TMoto) en auront besoin
    FVitesseActuelle: Real;
    FVitesseMax: Real;

    // Méthode que les descendants voudront peut-être redéfinir
    procedure InitialiserParametres; virtual;

  public
    // Interface publique : ce que tout le monde peut utiliser
    procedure Accelerer(Delta: Real);
    procedure Freiner(Delta: Real);
    function ObtenirVitesse: Real;
    procedure Afficher;

  published
    // Pour un composant visuel (si on crée un composant)
    // property VitesseMax: Real read FVitesseMax write FVitesseMax;
  end;
```

## Interactions entre les niveaux

### Accès depuis différents contextes

```pascal
type
  TClasse = class
  private
    FPrivate: Integer;
  protected
    FProtected: Integer;
  public
    FPublic: Integer;
  published
    FPublished: Integer;

    procedure TesterAcces;
  end;

procedure TClasse.TesterAcces;
begin
  // ✓ Depuis la classe elle-même : TOUS accessibles
  FPrivate := 1;
  FProtected := 2;
  FPublic := 3;
  FPublished := 4;
end;

type
  TClasseDerivee = class(TClasse)
  public
    procedure TesterAccesDeriv;
  end;

procedure TClasseDerivee.TesterAccesDeriv;
begin
  // ✗ Private n'est PAS accessible
  // FPrivate := 1;

  // ✓ Protected, Public, Published accessibles
  FProtected := 2;
  FPublic := 3;
  FPublished := 4;
end;

// Code externe
var
  C: TClasse;
begin
  C := TClasse.Create;

  // ✗ Private et Protected NON accessibles
  // C.FPrivate := 1;
  // C.FProtected := 2;

  // ✓ Public et Published accessibles
  C.FPublic := 3;
  C.FPublished := 4;

  C.Free;
end;
```

## Exemple complet : Système de notes d'étudiants

```pascal
program SystemeNotes;

{$mode objfpc}{$H+}

uses
  SysUtils, Math;

type
  // Classe de base
  TPersonne = class
  protected
    // Protected : les classes dérivées en auront besoin
    FNom: string;
    FPrenom: string;
    FDateNaissance: TDateTime;
  public
    constructor Create(const Nom, Prenom: string; DateNaissance: TDateTime);
    function ObtenirNomComplet: string;
    function CalculerAge: Integer;
    procedure Afficher; virtual;
  end;

  // Classe dérivée
  TEtudiant = class(TPersonne)
  private
    // Private : détails internes de TEtudiant
    FNumeroEtudiant: string;
    FNotes: array of Real;

    // Méthodes privées
    function NotesValidees: Boolean;
    procedure TrierNotes;

  protected
    // Protected : au cas où on dérive encore (TDoctorant, TMaster, etc.)
    FFormation: string;

  public
    constructor Create(const Nom, Prenom: string; DateNaissance: TDateTime; const NumeroEtudiant: string);
    destructor Destroy; override;

    // Interface publique
    procedure AjouterNote(Note: Real);
    function CalculerMoyenne: Real;
    function ObtenirMeilleureNote: Real;
    function ObtenirPireNote: Real;
    function ObtenirNombreNotes: Integer;
    procedure AfficherNotes;
    procedure Afficher; override;
  end;

// === IMPLÉMENTATION TPersonne ===

constructor TPersonne.Create(const Nom, Prenom: string; DateNaissance: TDateTime);
begin
  inherited Create;
  FNom := Nom;
  FPrenom := Prenom;
  FDateNaissance := DateNaissance;
end;

function TPersonne.ObtenirNomComplet: string;
begin
  Result := FPrenom + ' ' + FNom;
end;

function TPersonne.CalculerAge: Integer;
begin
  Result := YearsBetween(Now, FDateNaissance);
end;

procedure TPersonne.Afficher;
begin
  WriteLn('Nom : ', ObtenirNomComplet);
  WriteLn('Age : ', CalculerAge, ' ans');
end;

// === IMPLÉMENTATION TEtudiant ===

constructor TEtudiant.Create(const Nom, Prenom: string; DateNaissance: TDateTime; const NumeroEtudiant: string);
begin
  inherited Create(Nom, Prenom, DateNaissance);
  FNumeroEtudiant := NumeroEtudiant;
  SetLength(FNotes, 0);
  FFormation := 'Non définie';
end;

destructor TEtudiant.Destroy;
begin
  SetLength(FNotes, 0);
  inherited Destroy;
end;

function TEtudiant.NotesValidees: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to High(FNotes) do
    if (FNotes[I] < 0) or (FNotes[I] > 20) then
    begin
      Result := False;
      Break;
    end;
end;

procedure TEtudiant.TrierNotes;
var
  I, J: Integer;
  Temp: Real;
begin
  for I := 0 to High(FNotes) - 1 do
    for J := I + 1 to High(FNotes) do
      if FNotes[I] > FNotes[J] then
      begin
        Temp := FNotes[I];
        FNotes[I] := FNotes[J];
        FNotes[J] := Temp;
      end;
end;

procedure TEtudiant.AjouterNote(Note: Real);
var
  Index: Integer;
begin
  if (Note >= 0) and (Note <= 20) then
  begin
    Index := Length(FNotes);
    SetLength(FNotes, Index + 1);
    FNotes[Index] := Note;
    WriteLn('Note ajoutée : ', Note:0:2);
  end
  else
    WriteLn('Erreur : note invalide (doit être entre 0 et 20)');
end;

function TEtudiant.CalculerMoyenne: Real;
var
  I: Integer;
  Somme: Real;
begin
  if Length(FNotes) = 0 then
  begin
    Result := 0;
    Exit;
  end;

  Somme := 0;
  for I := 0 to High(FNotes) do
    Somme := Somme + FNotes[I];

  Result := Somme / Length(FNotes);
end;

function TEtudiant.ObtenirMeilleureNote: Real;
var
  I: Integer;
begin
  if Length(FNotes) = 0 then
  begin
    Result := 0;
    Exit;
  end;

  Result := FNotes[0];
  for I := 1 to High(FNotes) do
    if FNotes[I] > Result then
      Result := FNotes[I];
end;

function TEtudiant.ObtenirPireNote: Real;
var
  I: Integer;
begin
  if Length(FNotes) = 0 then
  begin
    Result := 0;
    Exit;
  end;

  Result := FNotes[0];
  for I := 1 to High(FNotes) do
    if FNotes[I] < Result then
      Result := FNotes[I];
end;

function TEtudiant.ObtenirNombreNotes: Integer;
begin
  Result := Length(FNotes);
end;

procedure TEtudiant.AfficherNotes;
var
  I: Integer;
begin
  WriteLn('=== Notes de ', ObtenirNomComplet, ' ===');

  if Length(FNotes) = 0 then
  begin
    WriteLn('Aucune note enregistrée');
    Exit;
  end;

  for I := 0 to High(FNotes) do
    WriteLn('Note ', I + 1, ' : ', FNotes[I]:0:2);

  WriteLn('---');
  WriteLn('Nombre de notes : ', ObtenirNombreNotes);
  WriteLn('Moyenne : ', CalculerMoyenne:0:2);
  WriteLn('Meilleure note : ', ObtenirMeilleureNote:0:2);
  WriteLn('Pire note : ', ObtenirPireNote:0:2);
  WriteLn('==================');
end;

procedure TEtudiant.Afficher;
begin
  WriteLn('=== Étudiant ===');
  inherited Afficher;  // Affiche nom et âge
  WriteLn('Numéro étudiant : ', FNumeroEtudiant);
  WriteLn('Formation : ', FFormation);
  WriteLn('Nombre de notes : ', ObtenirNombreNotes);
  if ObtenirNombreNotes > 0 then
    WriteLn('Moyenne générale : ', CalculerMoyenne:0:2);
  WriteLn('================');
end;

// === PROGRAMME PRINCIPAL ===

var
  Etudiant: TEtudiant;
begin
  Etudiant := TEtudiant.Create('Dupont', 'Marie', EncodeDate(2003, 5, 15), 'E2024001');
  try
    Etudiant.FFormation := 'Informatique';  // Protected, accessible ici car même unité

    Etudiant.Afficher;
    WriteLn;

    WriteLn('--- Ajout de notes ---');
    Etudiant.AjouterNote(15.5);
    Etudiant.AjouterNote(18);
    Etudiant.AjouterNote(12.5);
    Etudiant.AjouterNote(16);
    WriteLn;

    Etudiant.AfficherNotes;
    WriteLn;

    Etudiant.Afficher;

    // Tentatives d'accès incorrect (décommentez pour voir les erreurs)
    // WriteLn(Etudiant.FNumeroEtudiant);  // ✗ ERREUR : private
    // WriteLn(Etudiant.FNotes[0]);         // ✗ ERREUR : private

  finally
    Etudiant.Free;
  end;
end.
```

## Bonnes pratiques récapitulatives

### 1. Attributs toujours private ou protected

```pascal
// ✓ BON
type
  TClasse = class
  private
    FAttribut: Integer;
  public
    function ObtenirAttribut: Integer;
  end;

// ✗ MAUVAIS
type
  TClasse = class
  public
    Attribut: Integer;  // Pas de contrôle
  end;
```

### 2. Interface publique minimale

```pascal
// ✓ BON : seul ce qui est nécessaire est public
type
  TClasse = class
  private
    FDonnee: Integer;
    procedure MethodeInterne;
  public
    procedure MethodePublique;
  end;
```

### 3. Protected pour l'extensibilité

```pascal
// ✓ BON : protected pour permettre la dérivation
type
  TBase = class
  protected
    FValeur: Integer;  // Les classes dérivées pourront y accéder
    procedure Initialiser; virtual;
  end;
```

### 4. Published uniquement pour les composants

```pascal
// ✓ BON : published pour les composants Lazarus
type
  TMonBouton = class(TButton)
  published
    property CouleurPerso: TColor;
  end;
```

## Points clés à retenir

- **Private** : accessible uniquement dans la classe → détails d'implémentation
- **Protected** : accessible dans la classe et ses descendants → pour l'héritage
- **Public** : accessible partout → interface publique
- **Published** : comme public + RTTI → composants visuels
- Les **attributs** doivent être private ou protected
- Les **méthodes publiques** constituent l'interface de la classe
- Utilisez le niveau de visibilité le **plus restrictif** possible
- La visibilité peut être **changée** dans les classes dérivées (mais rarement conseillé)
- Protected est un **compromis** entre private et public pour l'héritage

## Vers la suite

Dans la section suivante, nous explorerons les **propriétés** (properties) qui permettent d'accéder aux attributs de manière contrôlée et élégante, en combinant la simplicité d'accès des attributs publics avec la sécurité des méthodes accesseurs.

⏭️ [Propriétés (properties) simples](/10-fondamentaux-poo/09-proprietes-properties-simples.md)
