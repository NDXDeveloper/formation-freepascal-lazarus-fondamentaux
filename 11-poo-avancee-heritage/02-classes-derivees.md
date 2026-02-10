🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.2 Classes dérivées

## Introduction

Maintenant que vous comprenez le concept d'héritage, nous allons approfondir la création et l'utilisation des **classes dérivées** (aussi appelées classes enfants ou sous-classes). Vous allez apprendre à créer des hiérarchies de classes complètes et à exploiter pleinement la puissance de l'héritage.

## Anatomie d'une classe dérivée

Une classe dérivée se compose de deux parties :

1. **La partie héritée** : tout ce qui vient de la classe parent
2. **La partie spécifique** : ce que vous ajoutez dans la classe dérivée

```pascal
type
  // Classe PARENT
  TPersonne = class
  private
    FNom: string;
    FPrenom: string;
  public
    constructor Create(ANom, APrenom: string);
    procedure Afficher;
  end;

  // Classe DERIVEE
  TEmploye = class(TPersonne)  // Hérite de TPersonne
  private
    FNumeroEmploye: Integer;   // ← Partie spécifique
    FSalaire: Real;            // ← Partie spécifique
  public
    constructor Create(ANom, APrenom: string; ANumero: Integer; ASalaire: Real);
    procedure AfficherFichePaie;  // ← Partie spécifique
    // Hérite aussi de : FNom, FPrenom, Afficher()
  end;
```

## Création de classes dérivées - Étape par étape

### Étape 1 : Définir la hiérarchie

Avant de coder, réfléchissez à votre hiérarchie. Posez-vous la question : "Est-ce que X **est un** Y ?"

**Exemple** : Système de gestion d'employés

```
                    TPersonne
                        |
            +-----------+-----------+
            |                       |
        TEmploye                TClient
            |
    +-------+-------+
    |               |
TEmployePermanent  TEmployeTemporaire
```

### Étape 2 : Commencer par la classe de base

Identifiez ce qui est **commun** à toutes les classes :

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FPrenom: string;
    FDateNaissance: TDateTime;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime);
    destructor Destroy; override;
    procedure Afficher; virtual;
    function GetAge: Integer;
  end;
```

**Points importants :**
- Les membres `private` ne sont accessibles que dans `TPersonne`
- Les membres `public` sont accessibles partout
- `virtual` permet aux classes dérivées de modifier le comportement (on verra ça en détail plus tard)

### Étape 3 : Créer les classes dérivées

Ajoutez les spécificités de chaque type :

```pascal
type
  TEmploye = class(TPersonne)
  private
    FNumeroEmploye: Integer;
    FSalaire: Real;
    FDateEmbauche: TDateTime;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime);
    procedure Afficher; override;
    function GetAnciennete: Integer;
    procedure AugmenterSalaire(Pourcentage: Real);
  end;

  TClient = class(TPersonne)
  private
    FNumeroClient: string;
    FMontantAchats: Real;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumeroClient: string);
    procedure Afficher; override;
    procedure AjouterAchat(Montant: Real);
    function EstClientFidele: Boolean;
  end;
```

## Exemple complet et détaillé

Voici un exemple complet que vous pouvez compiler et exécuter :

```pascal
program GestionPersonnes;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils;

type
  { Classe de base : TPersonne }
  TPersonne = class
  private
    FNom: string;
    FPrenom: string;
    FDateNaissance: TDateTime;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime);
    destructor Destroy; override;
    procedure Afficher; virtual;
    function GetAge: Integer;
    function GetNomComplet: string;
  end;

  { Classe dérivée : TEmploye }
  TEmploye = class(TPersonne)
  private
    FNumeroEmploye: Integer;
    FSalaire: Real;
    FDateEmbauche: TDateTime;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime);
    procedure Afficher; override;
    function GetAnciennete: Integer;
    procedure AugmenterSalaire(Pourcentage: Real);
  end;

  { Classe dérivée : TClient }
  TClient = class(TPersonne)
  private
    FNumeroClient: string;
    FMontantAchats: Real;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumeroClient: string);
    procedure Afficher; override;
    procedure AjouterAchat(Montant: Real);
    function EstClientFidele: Boolean;
  end;

{ === Implémentation de TPersonne === }

constructor TPersonne.Create(ANom, APrenom: string; ADateNaissance: TDateTime);  
begin  
  inherited Create;  // Appelle le constructeur de TObject
  FNom := ANom;
  FPrenom := APrenom;
  FDateNaissance := ADateNaissance;
  WriteLn('[TPersonne] Création de ', GetNomComplet);
end;

destructor TPersonne.Destroy;  
begin  
  WriteLn('[TPersonne] Destruction de ', GetNomComplet);
  inherited Destroy;  // Appelle le destructeur de TObject
end;

procedure TPersonne.Afficher;  
begin  
  WriteLn('Nom complet : ', GetNomComplet);
  WriteLn('Age : ', GetAge, ' ans');
  WriteLn('Né(e) le : ', DateToStr(FDateNaissance));
end;

function TPersonne.GetAge: Integer;  
begin  
  Result := YearsBetween(Now, FDateNaissance);
end;

function TPersonne.GetNomComplet: string;  
begin  
  Result := FPrenom + ' ' + FNom;
end;

{ === Implémentation de TEmploye === }

constructor TEmploye.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                            ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime);
begin
  // D'abord, on initialise la partie TPersonne
  inherited Create(ANom, APrenom, ADateNaissance);

  // Ensuite, on initialise la partie spécifique à TEmploye
  FNumeroEmploye := ANumero;
  FSalaire := ASalaire;
  FDateEmbauche := ADateEmbauche;
  WriteLn('[TEmploye] Numéro employé : ', ANumero);
end;

procedure TEmploye.Afficher;  
begin  
  WriteLn('=== FICHE EMPLOYE ===');
  inherited Afficher;  // Affiche les infos de base (nom, âge, etc.)
  WriteLn('Numéro employé : ', FNumeroEmploye);
  WriteLn('Salaire : ', FSalaire:0:2, ' €');
  WriteLn('Date embauche : ', DateToStr(FDateEmbauche));
  WriteLn('Ancienneté : ', GetAnciennete, ' ans');
  WriteLn('=====================');
end;

function TEmploye.GetAnciennete: Integer;  
begin  
  Result := YearsBetween(Now, FDateEmbauche);
end;

procedure TEmploye.AugmenterSalaire(Pourcentage: Real);  
begin  
  WriteLn('Augmentation de ', Pourcentage:0:1, '% pour ', GetNomComplet);
  FSalaire := FSalaire * (1 + Pourcentage / 100);
  WriteLn('Nouveau salaire : ', FSalaire:0:2, ' €');
end;

{ === Implémentation de TClient === }

constructor TClient.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                           ANumeroClient: string);
begin
  inherited Create(ANom, APrenom, ADateNaissance);
  FNumeroClient := ANumeroClient;
  FMontantAchats := 0;
  WriteLn('[TClient] Numéro client : ', ANumeroClient);
end;

procedure TClient.Afficher;  
begin  
  WriteLn('=== FICHE CLIENT ===');
  inherited Afficher;
  WriteLn('Numéro client : ', FNumeroClient);
  WriteLn('Montant total achats : ', FMontantAchats:0:2, ' €');
  if EstClientFidele then
    WriteLn('Statut : CLIENT FIDELE ⭐')
  else
    WriteLn('Statut : Client standard');
  WriteLn('====================');
end;

procedure TClient.AjouterAchat(Montant: Real);  
begin  
  FMontantAchats := FMontantAchats + Montant;
  WriteLn('Achat de ', Montant:0:2, ' € ajouté pour ', GetNomComplet);
end;

function TClient.EstClientFidele: Boolean;  
begin  
  Result := FMontantAchats >= 1000;  // Fidèle si > 1000€ d'achats
end;

{ === Programme principal === }
var
  Employe1: TEmploye;
  Client1: TClient;
begin
  WriteLn('=== DEMONSTRATION DES CLASSES DERIVEES ===');
  WriteLn;

  // Création d'un employé
  WriteLn('--- Création d''un employé ---');
  Employe1 := TEmploye.Create('Dupont', 'Jean', EncodeDate(1985, 3, 15),
                               1001, 2500.00, EncodeDate(2015, 1, 10));
  WriteLn;

  // Affichage de l'employé
  Employe1.Afficher;
  WriteLn;

  // Augmentation de salaire
  Employe1.AugmenterSalaire(10);
  WriteLn;

  // Création d'un client
  WriteLn('--- Création d''un client ---');
  Client1 := TClient.Create('Martin', 'Sophie', EncodeDate(1990, 7, 22), 'CL-2024-001');
  WriteLn;

  // Ajout d'achats
  Client1.AjouterAchat(450.00);
  Client1.AjouterAchat(325.50);
  Client1.AjouterAchat(280.00);
  WriteLn;

  // Affichage du client
  Client1.Afficher;
  WriteLn;

  // Libération de la mémoire
  WriteLn('--- Libération des objets ---');
  Employe1.Free;
  Client1.Free;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Résultat de l'exécution

```
=== DEMONSTRATION DES CLASSES DERIVEES ===

--- Création d'un employé ---
[TPersonne] Création de Jean Dupont
[TEmploye] Numéro employé : 1001

=== FICHE EMPLOYE ===
Nom complet : Jean Dupont  
Age : 39 ans  
Né(e) le : 15/03/1985  
Numéro employé : 1001  
Salaire : 2500.00 €  
Date embauche : 10/01/2015  
Ancienneté : 9 ans  
=====================

Augmentation de 10.0% pour Jean Dupont  
Nouveau salaire : 2750.00 €  

--- Création d'un client ---
[TPersonne] Création de Sophie Martin
[TClient] Numéro client : CL-2024-001

Achat de 450.00 € ajouté pour Sophie Martin  
Achat de 325.50 € ajouté pour Sophie Martin  
Achat de 280.00 € ajouté pour Sophie Martin  

=== FICHE CLIENT ===
Nom complet : Sophie Martin  
Age : 34 ans  
Né(e) le : 22/07/1990  
Numéro client : CL-2024-001  
Montant total achats : 1055.50 €  
Statut : CLIENT FIDELE ⭐  
====================

--- Libération des objets ---
[TPersonne] Destruction de Jean Dupont
[TPersonne] Destruction de Sophie Martin
```

## Points importants sur les classes dérivées

### 1. Accès aux membres de la classe parent

Les classes dérivées ont accès aux membres selon leur visibilité :

```pascal
type
  TParent = class
  private
    FPrive: Integer;        // ❌ NON accessible dans la classe dérivée
  protected
    FProtege: Integer;      // ✅ Accessible dans la classe dérivée
  public
    FPublic: Integer;       // ✅ Accessible partout
  end;

  TEnfant = class(TParent)
    procedure Test;
  end;

procedure TEnfant.Test;  
begin  
  // FPrive := 10;    // ❌ ERREUR : membre privé
  FProtege := 20;     // ✅ OK : membre protégé
  FPublic := 30;      // ✅ OK : membre public
end;
```

### 2. L'ordre d'exécution des constructeurs

Lors de la création d'un objet dérivé, l'ordre est **toujours** :

1. Constructeur du parent (via `inherited Create`)
2. Code spécifique de la classe dérivée

```pascal
constructor TEmploye.Create(...);  
begin  
  inherited Create(...);  // 1. D'abord le parent
  FNumeroEmploye := ...;  // 2. Ensuite la partie enfant
end;
```

**Important** : Appelez toujours `inherited Create` en **premier** dans votre constructeur !

### 3. L'ordre d'exécution des destructeurs

Pour les destructeurs, c'est **l'inverse** :

1. Code spécifique de la classe dérivée
2. Destructeur du parent (via `inherited Destroy`)

```pascal
destructor TEmploye.Destroy;  
begin  
  // 1. D'abord nettoyage spécifique
  WriteLn('Nettoyage de TEmploye');

  inherited Destroy;  // 2. Ensuite le parent
end;
```

### 4. Compatibilité de types

Une classe dérivée est **compatible** avec sa classe parent :

```pascal
var
  UnePersonne: TPersonne;
  UnEmploye: TEmploye;
begin
  UnEmploye := TEmploye.Create(...);

  // ✅ OK : un employé EST une personne
  UnePersonne := UnEmploye;

  // UnePersonne pointe maintenant vers l'objet TEmploye
  UnePersonne.Afficher;  // Appelle TEmploye.Afficher (polymorphisme)
end;
```

Mais l'inverse n'est **pas** possible :

```pascal
var
  UnePersonne: TPersonne;
  UnEmploye: TEmploye;
begin
  UnePersonne := TPersonne.Create(...);

  // ❌ ERREUR : une personne n'est pas forcément un employé
  UnEmploye := UnePersonne;
end;
```

### 5. Plusieurs niveaux d'héritage

Vous pouvez créer des hiérarchies à plusieurs niveaux :

```pascal
type
  TPersonne = class
    // ...
  end;

  TEmploye = class(TPersonne)
    // TEmploye hérite de TPersonne
  end;

  TEmployePermanent = class(TEmploye)
    // TEmployePermanent hérite de TEmploye
    // ET AUSSI de TPersonne (héritage transitif)
  private
    FAvantages: string;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime;
                       AAvantages: string);
  end;

constructor TEmployePermanent.Create(ANom, APrenom: string;
                                     ADateNaissance: TDateTime;
                                     ANumero: Integer; ASalaire: Real;
                                     ADateEmbauche: TDateTime;
                                     AAvantages: string);
begin
  // Appelle le constructeur de TEmploye
  // qui lui-même appelle le constructeur de TPersonne
  inherited Create(ANom, APrenom, ADateNaissance, ANumero, ASalaire, ADateEmbauche);
  FAvantages := AAvantages;
end;
```

## Bonnes pratiques

### ✅ À FAIRE

1. **Toujours appeler `inherited` dans les constructeurs**
   ```pascal
   constructor TEnfant.Create;
   begin
     inherited Create;  // Toujours en premier !
     // Votre code ici
   end;
   ```

2. **Utiliser `protected` pour les membres que les dérivées doivent voir**
   ```pascal
   type
     TParent = class
     protected  // Accessible dans les classes dérivées
       FDonneePartagee: Integer;
     end;
   ```

3. **Créer une hiérarchie logique**
   - Placez le code commun dans les classes parentes
   - Spécialisez progressivement dans les classes dérivées

4. **Documenter la hiérarchie**
   ```pascal
   {
     Hiérarchie :
       TPersonne
         ├── TEmploye
         │     ├── TEmployePermanent
         │     └── TEmployeTemporaire
         └── TClient
   }
   ```

### ❌ À ÉVITER

1. **Créer trop de niveaux d'héritage**
   - Maximum 3-4 niveaux généralement
   - Si plus, votre conception est probablement trop complexe

2. **Hériter juste pour réutiliser du code**
   - L'héritage représente une relation "EST UN"
   - Pour "A UN", utilisez la composition

3. **Oublier d'appeler le destructeur parent**
   ```pascal
   destructor TEnfant.Destroy;
   begin
     // Nettoyage
     inherited Destroy;  // Ne pas oublier !
   end;
   ```

## Cas d'usage concrets

### Exemple 1 : Formes géométriques

```pascal
type
  TForme = class
  protected
    FCouleur: string;
  public
    constructor Create(ACouleur: string);
    function CalculerAire: Real; virtual; abstract;
    procedure Dessiner; virtual; abstract;
  end;

  TRectangle = class(TForme)
  private
    FLargeur, FHauteur: Real;
  public
    constructor Create(ACouleur: string; ALargeur, AHauteur: Real);
    function CalculerAire: Real; override;
    procedure Dessiner; override;
  end;

  TCercle = class(TForme)
  private
    FRayon: Real;
  public
    constructor Create(ACouleur: string; ARayon: Real);
    function CalculerAire: Real; override;
    procedure Dessiner; override;
  end;
```

### Exemple 2 : Comptes bancaires

```pascal
type
  TCompteBancaire = class
  protected
    FNumeroCompte: string;
    FSolde: Real;
  public
    procedure Deposer(Montant: Real); virtual;
    function Retirer(Montant: Real): Boolean; virtual;
    function GetSolde: Real;
  end;

  TCompteEpargne = class(TCompteBancaire)
  private
    FTauxInteret: Real;
  public
    procedure CalculerInterets;
    function Retirer(Montant: Real): Boolean; override;  // Restrictions
  end;

  TCompteCourant = class(TCompteBancaire)
  private
    FDecouvertAutorise: Real;
  public
    function Retirer(Montant: Real): Boolean; override;  // Avec découvert
  end;
```

## Résumé

Les classes dérivées permettent de :
- ✅ Réutiliser et étendre le code existant
- ✅ Créer des hiérarchies logiques et organisées
- ✅ Spécialiser progressivement les comportements
- ✅ Maintenir un code DRY (Don't Repeat Yourself)

**Points clés :**
- Une classe dérivée hérite de tout ce qui est `public` ou `protected`
- Utilisez `inherited` pour appeler les méthodes parentes
- L'ordre constructeur : parent → enfant
- L'ordre destructeur : enfant → parent
- Une classe dérivée peut être utilisée partout où le parent est attendu

Dans la prochaine section, nous verrons comment **redéfinir** le comportement des méthodes héritées pour créer des classes vraiment personnalisées !

⏭️ [Redéfinition de méthodes](/11-poo-avancee-heritage/03-redefinition-methodes.md)
