🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.9 Hiérarchies de classes

## Introduction

Une **hiérarchie de classes** est un ensemble de classes organisées selon leurs relations d'héritage, formant un arbre généalogique où chaque classe hérite des caractéristiques de sa classe parent. C'est l'une des structures fondamentales de la programmation orientée objet.

### Analogie du monde réel

Pensez à la classification du vivant en biologie :

```
Être vivant
├── Animal
│   ├── Mammifère
│   │   ├── Carnivore
│   │   │   ├── Chien
│   │   │   └── Chat
│   │   └── Herbivore
│   │       ├── Vache
│   │       └── Cheval
│   └── Oiseau
│       ├── Rapace
│       └── Passereau
└── Végétal
    ├── Arbre
    └── Fleur
```

Chaque niveau hérite des caractéristiques du niveau supérieur et ajoute ses spécificités. C'est exactement ce qu'est une hiérarchie de classes !

## Qu'est-ce qu'une hiérarchie de classes ?

Une hiérarchie de classes est une structure arborescente où :
- **La racine** : la classe la plus générale (souvent `TObject` en Pascal)
- **Les branches** : les classes intermédiaires
- **Les feuilles** : les classes les plus spécialisées

```pascal
type
  // Racine
  TVehicule = class
  end;

  // Branches
  TVehiculeTerrestre = class(TVehicule)
  end;

  TVehiculeAerien = class(TVehicule)
  end;

  // Feuilles
  TVoiture = class(TVehiculeTerrestre)
  end;

  TMoto = class(TVehiculeTerrestre)
  end;

  TAvion = class(TVehiculeAerien)
  end;
```

## Types de hiérarchies

### 1. Hiérarchie en profondeur (Deep Hierarchy)

Beaucoup de niveaux, peu de classes par niveau :

```
TObject
  ↓
TStream
  ↓
THandleStream
  ↓
TFileStream
  ↓
TBufferedFileStream
```

**Avantages :**
- Réutilisation maximale du code
- Chaque niveau ajoute une fonctionnalité précise

**Inconvénients :**
- Complexité accrue
- Difficile à comprendre et maintenir si trop profond

**Règle générale** : Maximum 4-5 niveaux pour rester lisible

### 2. Hiérarchie en largeur (Wide Hierarchy)

Peu de niveaux, beaucoup de classes par niveau :

```
                TForme
                   ↓
    ┌──────┬──────┼──────┬──────┬──────┐
    │      │      │      │      │      │
Rectangle Cercle Triangle Carré Ovale Polygone
```

**Avantages :**
- Simple à comprendre
- Chaque classe est indépendante

**Inconvénients :**
- Beaucoup de duplication de code
- Difficile d'ajouter des fonctionnalités communes

### 3. Hiérarchie équilibrée (recommandée)

Un compromis entre profondeur et largeur :

```
              TDocument
                 ↓
        ┌────────┴────────┐
    TDocumentTexte    TDocumentGraphique
        ↓                  ↓
    ┌───┴───┐         ┌────┴────┐
  TTexte  TMarkdown  TImage  TDessin
```

**Avantages :**
- Balance entre réutilisation et simplicité
- Facile à étendre
- Maintenable

## Exemple complet : Système de gestion de personnel

```pascal
program HierarchiePersonnel;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, StrUtils;

type
  {
    Hiérarchie :

    TPersonne (racine)
      ├── TEmploye
      │   ├── TEmployePermanent
      │   │   ├── TManager
      │   │   └── TDirecteur
      │   └── TEmployeTemporaire
      │       ├── TStagiaire
      │       └── TInterimaire
      └── TClient
          ├── TClientParticulier
          └── TClientEntreprise
  }

  { === NIVEAU 1 : Racine === }

  TPersonne = class
  protected
    FNom: string;
    FPrenom: string;
    FDateNaissance: TDateTime;
    FEmail: string;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime);
    procedure Afficher; virtual;
    function GetAge: Integer;
    function GetNomComplet: string;
    property Email: string read FEmail write FEmail;
  end;

  { === NIVEAU 2 : Branches principales === }

  TEmploye = class(TPersonne)
  protected
    FNumeroEmploye: Integer;
    FSalaire: Real;
    FDateEmbauche: TDateTime;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime);
    procedure Afficher; override;
    function GetAnciennete: Integer;
    procedure AugmenterSalaire(Pourcentage: Real); virtual;
    property NumeroEmploye: Integer read FNumeroEmploye;
    property Salaire: Real read FSalaire;
  end;

  TClient = class(TPersonne)
  protected
    FNumeroClient: string;
    FMontantAchats: Real;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumeroClient: string);
    procedure Afficher; override;
    procedure AjouterAchat(Montant: Real);
    function GetRemise: Real; virtual;
    property MontantAchats: Real read FMontantAchats;
  end;

  { === NIVEAU 3 : Spécialisations d'employés === }

  TEmployePermanent = class(TEmploye)
  protected
    FAvantages: string;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime;
                       AAvantages: string);
    procedure Afficher; override;
  end;

  TEmployeTemporaire = class(TEmploye)
  protected
    FDateFin: TDateTime;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumero: Integer; ASalaire: Real; ADateEmbauche, ADateFin: TDateTime);
    procedure Afficher; override;
    function EstEnCours: Boolean;
  end;

  { === NIVEAU 3 : Spécialisations de clients === }

  TClientParticulier = class(TClient)
  private
    FProgrammeFidelite: Boolean;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumeroClient: string; AFidelite: Boolean);
    procedure Afficher; override;
    function GetRemise: Real; override;
  end;

  TClientEntreprise = class(TClient)
  private
    FRaisonSociale: string;
    FSiret: string;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumeroClient, ARaisonSociale, ASiret: string);
    procedure Afficher; override;
    function GetRemise: Real; override;
  end;

  { === NIVEAU 4 : Spécialisations avancées === }

  TManager = class(TEmployePermanent)
  private
    FTailleEquipe: Integer;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime;
                       AAvantages: string; ATailleEquipe: Integer);
    procedure Afficher; override;
    procedure AugmenterSalaire(Pourcentage: Real); override;
  end;

  TDirecteur = class(TEmployePermanent)
  private
    FDepartement: string;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime;
                       AAvantages, ADepartement: string);
    procedure Afficher; override;
  end;

  TStagiaire = class(TEmployeTemporaire)
  private
    FEcole: string;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumero: Integer; ASalaire: Real; ADateEmbauche, ADateFin: TDateTime;
                       AEcole: string);
    procedure Afficher; override;
  end;

  TInterimaire = class(TEmployeTemporaire)
  private
    FAgence: string;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumero: Integer; ASalaire: Real; ADateEmbauche, ADateFin: TDateTime;
                       AAgence: string);
    procedure Afficher; override;
  end;

{ === Implémentation TPersonne === }

constructor TPersonne.Create(ANom, APrenom: string; ADateNaissance: TDateTime);
begin
  inherited Create;
  FNom := ANom;
  FPrenom := APrenom;
  FDateNaissance := ADateNaissance;
end;

procedure TPersonne.Afficher;
begin
  WriteLn('╔════════════════════════════════════════════════');
  WriteLn('║ PERSONNE');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ Nom complet : ', GetNomComplet);
  WriteLn('║ Age : ', GetAge, ' ans');
  WriteLn('║ Date naissance : ', DateToStr(FDateNaissance));
  if FEmail <> '' then
    WriteLn('║ Email : ', FEmail);
end;

function TPersonne.GetAge: Integer;
begin
  Result := YearsBetween(Now, FDateNaissance);
end;

function TPersonne.GetNomComplet: string;
begin
  Result := FPrenom + ' ' + FNom;
end;

{ === Implémentation TEmploye === }

constructor TEmploye.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                            ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime);
begin
  inherited Create(ANom, APrenom, ADateNaissance);
  FNumeroEmploye := ANumero;
  FSalaire := ASalaire;
  FDateEmbauche := ADateEmbauche;
end;

procedure TEmploye.Afficher;
begin
  inherited Afficher;
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ EMPLOYE');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ Numéro : ', FNumeroEmploye);
  WriteLn('║ Salaire : ', FSalaire:0:2, ' €');
  WriteLn('║ Date embauche : ', DateToStr(FDateEmbauche));
  WriteLn('║ Ancienneté : ', GetAnciennete, ' ans');
end;

function TEmploye.GetAnciennete: Integer;
begin
  Result := YearsBetween(Now, FDateEmbauche);
end;

procedure TEmploye.AugmenterSalaire(Pourcentage: Real);
begin
  FSalaire := FSalaire * (1 + Pourcentage / 100);
end;

{ === Implémentation TClient === }

constructor TClient.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                           ANumeroClient: string);
begin
  inherited Create(ANom, APrenom, ADateNaissance);
  FNumeroClient := ANumeroClient;
  FMontantAchats := 0;
end;

procedure TClient.Afficher;
begin
  inherited Afficher;
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ CLIENT');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ Numéro : ', FNumeroClient);
  WriteLn('║ Achats totaux : ', FMontantAchats:0:2, ' €');
  WriteLn('║ Remise : ', GetRemise:0:1, '%');
end;

procedure TClient.AjouterAchat(Montant: Real);
begin
  FMontantAchats := FMontantAchats + Montant;
end;

function TClient.GetRemise: Real;
begin
  Result := 0;  // Pas de remise par défaut
end;

{ === Implémentation TEmployePermanent === }

constructor TEmployePermanent.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                                     ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime;
                                     AAvantages: string);
begin
  inherited Create(ANom, APrenom, ADateNaissance, ANumero, ASalaire, ADateEmbauche);
  FAvantages := AAvantages;
end;

procedure TEmployePermanent.Afficher;
begin
  inherited Afficher;
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ EMPLOYE PERMANENT');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ Avantages : ', FAvantages);
end;

{ === Implémentation TEmployeTemporaire === }

constructor TEmployeTemporaire.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                                      ANumero: Integer; ASalaire: Real;
                                      ADateEmbauche, ADateFin: TDateTime);
begin
  inherited Create(ANom, APrenom, ADateNaissance, ANumero, ASalaire, ADateEmbauche);
  FDateFin := ADateFin;
end;

procedure TEmployeTemporaire.Afficher;
begin
  inherited Afficher;
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ EMPLOYE TEMPORAIRE');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ Date fin : ', DateToStr(FDateFin));
  WriteLn('║ Statut : ', IfThen(EstEnCours, 'EN COURS', 'TERMINE'));
end;

function TEmployeTemporaire.EstEnCours: Boolean;
begin
  Result := Now < FDateFin;
end;

{ === Implémentation TClientParticulier === }

constructor TClientParticulier.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                                      ANumeroClient: string; AFidelite: Boolean);
begin
  inherited Create(ANom, APrenom, ADateNaissance, ANumeroClient);
  FProgrammeFidelite := AFidelite;
end;

procedure TClientParticulier.Afficher;
begin
  inherited Afficher;
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ CLIENT PARTICULIER');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ Programme fidélité : ', IfThen(FProgrammeFidelite, 'OUI', 'NON'));
end;

function TClientParticulier.GetRemise: Real;
begin
  if FProgrammeFidelite then
    Result := 5.0  // 5% de remise
  else
    Result := 0;
end;

{ === Implémentation TClientEntreprise === }

constructor TClientEntreprise.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                                     ANumeroClient, ARaisonSociale, ASiret: string);
begin
  inherited Create(ANom, APrenom, ADateNaissance, ANumeroClient);
  FRaisonSociale := ARaisonSociale;
  FSiret := ASiret;
end;

procedure TClientEntreprise.Afficher;
begin
  inherited Afficher;
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ CLIENT ENTREPRISE');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ Raison sociale : ', FRaisonSociale);
  WriteLn('║ SIRET : ', FSiret);
end;

function TClientEntreprise.GetRemise: Real;
begin
  if FMontantAchats > 10000 then
    Result := 15.0  // 15% si > 10k€
  else
    Result := 10.0;  // 10% par défaut
end;

{ === Implémentation TManager === }

constructor TManager.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                            ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime;
                            AAvantages: string; ATailleEquipe: Integer);
begin
  inherited Create(ANom, APrenom, ADateNaissance, ANumero, ASalaire, ADateEmbauche, AAvantages);
  FTailleEquipe := ATailleEquipe;
end;

procedure TManager.Afficher;
begin
  inherited Afficher;
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ MANAGER');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ Taille équipe : ', FTailleEquipe, ' personnes');
  WriteLn('╚════════════════════════════════════════════════');
end;

procedure TManager.AugmenterSalaire(Pourcentage: Real);
begin
  // Bonus supplémentaire pour les managers
  inherited AugmenterSalaire(Pourcentage + 2);
end;

{ === Implémentation TDirecteur === }

constructor TDirecteur.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                              ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime;
                              AAvantages, ADepartement: string);
begin
  inherited Create(ANom, APrenom, ADateNaissance, ANumero, ASalaire, ADateEmbauche, AAvantages);
  FDepartement := ADepartement;
end;

procedure TDirecteur.Afficher;
begin
  inherited Afficher;
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ DIRECTEUR');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ Département : ', FDepartement);
  WriteLn('╚════════════════════════════════════════════════');
end;

{ === Implémentation TStagiaire === }

constructor TStagiaire.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                              ANumero: Integer; ASalaire: Real;
                              ADateEmbauche, ADateFin: TDateTime; AEcole: string);
begin
  inherited Create(ANom, APrenom, ADateNaissance, ANumero, ASalaire, ADateEmbauche, ADateFin);
  FEcole := AEcole;
end;

procedure TStagiaire.Afficher;
begin
  inherited Afficher;
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ STAGIAIRE');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ École : ', FEcole);
  WriteLn('╚════════════════════════════════════════════════');
end;

{ === Implémentation TInterimaire === }

constructor TInterimaire.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                                ANumero: Integer; ASalaire: Real;
                                ADateEmbauche, ADateFin: TDateTime; AAgence: string);
begin
  inherited Create(ANom, APrenom, ADateNaissance, ANumero, ASalaire, ADateEmbauche, ADateFin);
  FAgence := AAgence;
end;

procedure TInterimaire.Afficher;
begin
  inherited Afficher;
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ INTERIMAIRE');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ Agence : ', FAgence);
  WriteLn('╚════════════════════════════════════════════════');
end;

{ === Fonctions utilitaires === }

procedure AfficherHierarchie;
begin
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('               HIERARCHIE DES CLASSES');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;
  WriteLn('TPersonne (racine)');
  WriteLn('  ├── TEmploye');
  WriteLn('  │   ├── TEmployePermanent');
  WriteLn('  │   │   ├── TManager');
  WriteLn('  │   │   └── TDirecteur');
  WriteLn('  │   └── TEmployeTemporaire');
  WriteLn('  │       ├── TStagiaire');
  WriteLn('  │       └── TInterimaire');
  WriteLn('  └── TClient');
  WriteLn('      ├── TClientParticulier');
  WriteLn('      └── TClientEntreprise');
  WriteLn;
end;

procedure AnalyserPersonnes(Personnes: array of TPersonne);
var
  i: Integer;
  NbEmployes, NbClients, NbManagers: Integer;
begin
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('               ANALYSE DE LA COLLECTION');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  NbEmployes := 0;
  NbClients := 0;
  NbManagers := 0;

  for i := 0 to High(Personnes) do
  begin
    if Personnes[i] is TManager then
      Inc(NbManagers);

    if Personnes[i] is TEmploye then
      Inc(NbEmployes);

    if Personnes[i] is TClient then
      Inc(NbClients);
  end;

  WriteLn('Total personnes : ', Length(Personnes));
  WriteLn('  - Employés : ', NbEmployes);
  WriteLn('    dont Managers : ', NbManagers);
  WriteLn('  - Clients : ', NbClients);
  WriteLn;
end;

{ === Programme principal === }
var
  Manager: TManager;
  Directeur: TDirecteur;
  Stagiaire: TStagiaire;
  Interimaire: TInterimaire;
  ClientPart: TClientParticulier;
  ClientEntr: TClientEntreprise;

  Personnes: array[0..5] of TPersonne;
begin
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('        DEMONSTRATION DES HIERARCHIES DE CLASSES');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  AfficherHierarchie;

  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('               CREATION DES OBJETS');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  Manager := TManager.Create('Dupont', 'Marie', EncodeDate(1985, 3, 15),
                             1001, 4500, EncodeDate(2015, 1, 10),
                             'Voiture, Tickets resto', 8);

  Directeur := TDirecteur.Create('Martin', 'Pierre', EncodeDate(1975, 7, 22),
                                 2001, 7000, EncodeDate(2010, 5, 1),
                                 'Voiture, Tickets resto, Stock-options',
                                 'Informatique');

  Stagiaire := TStagiaire.Create('Durand', 'Sophie', EncodeDate(2002, 11, 8),
                                 3001, 600, EncodeDate(2024, 9, 1),
                                 EncodeDate(2025, 2, 28), 'ENSIMAG');

  Interimaire := TInterimaire.Create('Lefebvre', 'Thomas', EncodeDate(1990, 4, 12),
                                     4001, 1800, EncodeDate(2024, 10, 1),
                                     EncodeDate(2024, 12, 31), 'Manpower');

  ClientPart := TClientParticulier.Create('Bernard', 'Julie', EncodeDate(1988, 6, 5),
                                          'CP-001', True);
  ClientPart.AjouterAchat(1500);

  ClientEntr := TClientEntreprise.Create('Rousseau', 'Marc', EncodeDate(1970, 2, 18),
                                         'CE-001', 'TechCorp SAS', '12345678901234');
  ClientEntr.AjouterAchat(15000);

  WriteLn('✓ Tous les objets créés');
  WriteLn;

  // Stockage polymorphe
  Personnes[0] := Manager;
  Personnes[1] := Directeur;
  Personnes[2] := Stagiaire;
  Personnes[3] := Interimaire;
  Personnes[4] := ClientPart;
  Personnes[5] := ClientEntr;

  // Analyse
  AnalyserPersonnes(Personnes);

  // Affichage détaillé de chaque personne
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('               DETAILS DE CHAQUE PERSONNE');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  Manager.Afficher;
  WriteLn;

  Directeur.Afficher;
  WriteLn;

  Stagiaire.Afficher;
  WriteLn;

  Interimaire.Afficher;
  WriteLn;

  ClientPart.Afficher;
  WriteLn;

  ClientEntr.Afficher;
  WriteLn;

  // Test d'augmentation de salaire
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('               TEST AUGMENTATION SALAIRE');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  WriteLn('Manager (avant) : ', Manager.Salaire:0:2, ' €');
  Manager.AugmenterSalaire(10);  // +2% bonus = 12% au total
  WriteLn('Manager (après) : ', Manager.Salaire:0:2, ' €');
  WriteLn;

  // Libération
  Manager.Free;
  Directeur.Free;
  Stagiaire.Free;
  Interimaire.Free;
  ClientPart.Free;
  ClientEntr.Free;

  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Conception d'une hiérarchie de classes

### Étape 1 : Identifier les concepts

Listez tous les concepts de votre domaine :
- Quels sont les objets ?
- Quelles sont leurs caractéristiques ?
- Quelles sont leurs actions ?

**Exemple - Bibliothèque :**
- Livre, Magazine, DVD, CD
- Caractéristiques : titre, date, disponibilité
- Actions : emprunter, retourner

### Étape 2 : Trouver les points communs

Regroupez les concepts par similarités :

```
Tous ont : titre, date, disponibilité → TDocument

Documents papier : pages → TDocumentPapier
  - Livre : auteur, ISBN
  - Magazine : numéro, périodicité

Documents média : durée → TDocumentMedia
  - DVD : réalisateur, format
  - CD : artiste, genre
```

### Étape 3 : Définir la hiérarchie

```
            TDocument
               ↓
      ┌────────┴────────┐
TDocumentPapier   TDocumentMedia
      ↓                 ↓
  ┌───┴───┐         ┌───┴───┐
Livre  Magazine    DVD     CD
```

### Étape 4 : Implémenter du général au spécifique

```pascal
type
  // 1. Racine
  TDocument = class
  protected
    FTitre: string;
    FDisponible: Boolean;
  public
    procedure Emprunter; virtual;
    procedure Retourner; virtual;
  end;

  // 2. Branches
  TDocumentPapier = class(TDocument)
  protected
    FNombrePages: Integer;
  end;

  // 3. Feuilles
  TLivre = class(TDocumentPapier)
  private
    FAuteur: string;
    FISBN: string;
  end;
```

## Principes de conception

### 1. Principe de substitution de Liskov

**Un objet dérivé doit pouvoir remplacer son parent** sans casser le programme.

```pascal
procedure TraiterDocument(Doc: TDocument);
begin
  Doc.Emprunter;  // Doit fonctionner pour TOUS les types
end;

var
  Livre: TLivre;
  Magazine: TMagazine;
begin
  TraiterDocument(Livre);     // ✅ OK
  TraiterDocument(Magazine);  // ✅ OK
end;
```

### 2. Principe ouvert/fermé

**Ouvert à l'extension, fermé à la modification.**

Vous devez pouvoir **ajouter** de nouvelles classes sans **modifier** les existantes.

```pascal
// Hiérarchie existante
type
  TForme = class
    function CalculerAire: Real; virtual; abstract;
  end;

  TRectangle = class(TForme)
    // ...
  end;

// ✅ Ajout d'un nouveau type SANS modifier l'existant
type
  THexagone = class(TForme)
    function CalculerAire: Real; override;
  end;
```

### 3. Principe DRY (Don't Repeat Yourself)

**Ne dupliquez jamais de code.** Si deux classes ont du code identique, créez un parent commun.

```pascal
// ❌ Mauvais : duplication
type
  TChien = class
    procedure Manger;  // Code identique
    procedure Dormir;  // Code identique
  end;

  TChat = class
    procedure Manger;  // Code identique
    procedure Dormir;  // Code identique
  end;

// ✅ Bon : factorisation
type
  TAnimal = class
    procedure Manger;   // Code commun ici
    procedure Dormir;   // Code commun ici
  end;

  TChien = class(TAnimal)
    // Hérite de Manger et Dormir
  end;
```

### 4. Principe de responsabilité unique

**Chaque classe doit avoir une seule raison de changer.**

```pascal
// ❌ Mauvais : trop de responsabilités
type
  TUtilisateur = class
    procedure Authentifier;
    procedure EnvoyerEmail;
    procedure SauvegarderDansDB;
    procedure GenererPDF;
  end;

// ✅ Bon : responsabilités séparées
type
  TUtilisateur = class
    // Données uniquement
  end;

  TAuthentification = class
    procedure Authentifier(User: TUtilisateur);
  end;

  TEmailService = class
    procedure Envoyer(User: TUtilisateur);
  end;
```

## Erreurs courantes

### Erreur 1 : Hiérarchie trop profonde

```pascal
// ❌ Trop de niveaux
TObject
  → TVehicule
    → TVehiculeMoteur
      → TVehiculeMoteurEssence
        → TVehiculeMoteurEssenceDiesel
          → TVehiculeMoteurEssenceDieselHybride
```

**Solution** : Simplifier avec des attributs au lieu de classes.

### Erreur 2 : Hériter pour réutiliser du code

```pascal
// ❌ Mauvais : hérite juste pour la méthode Log
type
  TLogger = class
    procedure Log(Message: string);
  end;

  TCalculatrice = class(TLogger)  // Une calculatrice n'EST PAS un logger !
    function Additionner(A, B: Integer): Integer;
  end;
```

**Solution** : Utiliser la **composition** au lieu de l'héritage.

```pascal
// ✅ Bon : composition
type
  TCalculatrice = class
  private
    FLogger: TLogger;
  public
    function Additionner(A, B: Integer): Integer;
  end;
```

### Erreur 3 : Classe abstraite trop générale

```pascal
// ❌ Mauvais : trop abstrait, pas utile
type
  TChose = class
    procedure Faire; virtual; abstract;
  end;
```

**Solution** : Donner des noms significatifs.

### Erreur 4 : Trop de classes pour rien

```pascal
// ❌ Mauvais : surconception
type
  TPersonne = class
  end;

  TPersonneAvecNom = class(TPersonne)
  end;

  TPersonneAvecNomEtPrenom = class(TPersonneAvecNom)
  end;
```

**Solution** : Utiliser des attributs.

## Avantages des hiérarchies bien conçues

### ✅ Réutilisation du code

```pascal
// Code une fois dans TAnimal
procedure TAnimal.Manger;

// Réutilisé par Chien, Chat, Lion, etc.
```

### ✅ Polymorphisme

```pascal
var
  Animaux: array of TAnimal;
begin
  // Traitement uniforme de tous les types
  for Animal in Animaux do
    Animal.FaireDuBruit;
end;
```

### ✅ Extensibilité

```pascal
// Ajouter un nouveau type sans toucher à l'existant
type
  TElephant = class(TAnimal)
    // Nouveau type
  end;
```

### ✅ Maintenabilité

```pascal
// Modification centralisée
procedure TAnimal.Manger;
begin
  // Changement ici affecte tous les types
end;
```

## Quand créer une hiérarchie ?

### ✅ Créez une hiérarchie quand :

1. **Relation "EST UN"**
   - Un chien EST UN animal
   - Une voiture EST UN véhicule

2. **Comportements communs**
   - Plusieurs classes partagent des méthodes

3. **Variations d'un même concept**
   - Différents types de documents
   - Différents types de comptes

4. **Extensibilité nécessaire**
   - Vous prévoyez d'ajouter des types

### ❌ N'utilisez PAS une hiérarchie quand :

1. **Relation "A UN"**
   - Une voiture A UN moteur → composition

2. **Pas de lien logique**
   - Les classes sont indépendantes

3. **Réutilisation ponctuelle**
   - Créez des fonctions utilitaires

4. **Attribut suffit**
   - Utilisez un enum ou un booléen

## Documentation d'une hiérarchie

```pascal
{
  Hiérarchie des documents de bibliothèque

  TDocument (racine abstraite)
    Définit : Titre, Disponibilité, Emprunter/Retourner

    ├── TDocumentPapier (branche abstraite)
    │   Ajoute : NombrePages
    │
    │   ├── TLivre (feuille)
    │   │   Ajoute : Auteur, ISBN
    │   │
    │   └── TMagazine (feuille)
    │       Ajoute : Numéro, Périodicité
    │
    └── TDocumentMedia (branche abstraite)
        Ajoute : Durée

        ├── TDVD (feuille)
        │   Ajoute : Réalisateur, Format
        │
        └── TCD (feuille)
            Ajoute : Artiste, Genre
}
```

## Résumé

Les hiérarchies de classes permettent de :
- ✅ Organiser le code de manière **logique** et **structurée**
- ✅ **Réutiliser** efficacement le code
- ✅ Créer des systèmes **extensibles** et **maintenables**
- ✅ Modéliser les relations du **monde réel**

**Principes clés :**
- **Du général au spécifique** : racine → branches → feuilles
- **Équilibre** : ni trop profond, ni trop large
- **Cohérence** : relation "EST UN" obligatoire
- **Simplicité** : ne pas surconcevoir

**Règles d'or :**
1. Maximum 4-5 niveaux de profondeur
2. Héritage pour "EST UN", composition pour "A UN"
3. Classes abstraites pour les concepts généraux
4. Classes concrètes pour les implémentations spécifiques

**Conception :**
1. Identifier les concepts
2. Trouver les points communs
3. Définir la structure
4. Implémenter et tester

Une bonne hiérarchie de classes est comme un arbre : des racines solides (classes de base), un tronc robuste (classes intermédiaires), et des branches diversifiées (classes spécialisées) !

⏭️ [Classe TObject et hiérarchie Pascal](/11-poo-avancee-heritage/10-classe-tobject-hierarchie-pascal.md)
