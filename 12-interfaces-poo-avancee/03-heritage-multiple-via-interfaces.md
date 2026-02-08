🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.3 Héritage multiple via interfaces

## Introduction : Le problème de l'héritage simple

En Pascal, comme dans beaucoup de langages orientés objet, une classe ne peut hériter que d'**une seule** classe parente. C'est ce qu'on appelle l'**héritage simple**.

```pascal
type
  TAnimal = class
    // ...
  end;

  TChien = class(TAnimal)  // ✅ OK : hérite de TAnimal
    // ...
  end;

  TRobot = class
    // ...
  end;

  // ❌ IMPOSSIBLE en Pascal !
  TChienRobot = class(TChien, TRobot)  // Erreur de compilation
    // ...
  end;
```

**Mais dans la vraie vie, certains objets ont plusieurs "natures" :**
- Un canard peut **nager**, **voler** et **marcher**
- Un smartphone peut **téléphoner**, **prendre des photos** et **naviguer sur internet**
- Une voiture amphibie peut **rouler** et **flotter**

Comment modéliser ces objets qui ont plusieurs comportements différents ?

**La solution : l'héritage multiple via interfaces !**

---

## Qu'est-ce que l'héritage multiple ?

### Définition simple

L'**héritage multiple** consiste à hériter de plusieurs classes parentes en même temps pour combiner leurs fonctionnalités.

### Analogie : Les talents multiples

Imaginez une personne qui est à la fois :
- **Musicien** (sait jouer d'un instrument)
- **Sportif** (sait courir, sauter)
- **Cuisinier** (sait préparer des plats)

Cette personne a hérité de compétences de trois "familles" différentes. En programmation, ce serait de l'héritage multiple.

### Pourquoi Pascal interdit l'héritage multiple de classes

L'héritage multiple de classes pose des problèmes complexes :

**Problème du diamant :**
```
      TAnimal
      /     \
  TChien   TChat
      \     /
    TChienChat  ← Lequel des deux parents choisir ?
```

Si `TChien` et `TChat` ont tous les deux une méthode `Manger`, laquelle devrait hériter `TChienChat` ?

Pour éviter ces complications, Pascal (comme Java, C#) a choisi :
- ✅ **Héritage simple** de classes (une seule classe parente)
- ✅ **Héritage multiple** d'interfaces (plusieurs interfaces)

---

## Solution : Implémenter plusieurs interfaces

### Principe

Une classe ne peut hériter que d'**une seule classe**, mais peut implémenter **autant d'interfaces qu'elle veut**.

```pascal
type
  TCanard = class(TAnimal, INageur, IVolant, IMarcheur)
    //              ↑         ↑        ↑        ↑
    //           Classe    Interface Interface Interface
  end;
```

**Règle syntaxique :**
1. La classe parente vient en premier (si elle existe)
2. Les interfaces suivent, séparées par des virgules
3. Si pas de classe parente, commencer par `TObject` ou `TInterfacedObject`

---

## Exemple 1 : Le canard multifonction

Créons un canard qui peut nager, voler et marcher.

### Étape 1 : Déclarer les interfaces

```pascal
type
  // Interface pour tout ce qui nage
  INageur = interface
    ['{A1B2C3D4-E5F6-1111-2222-333344445555}']
    procedure Nager;
    function ObtenirVitesseNage: Integer;
  end;

  // Interface pour tout ce qui vole
  IVolant = interface
    ['{B2C3D4E5-F6A7-2222-3333-444455556666}']
    procedure Voler;
    function ObtenirAltitude: Integer;
  end;

  // Interface pour tout ce qui marche
  IMarcheur = interface
    ['{C3D4E5F6-A7B8-3333-4444-555566667777}']
    procedure Marcher;
    function ObtenirVitesseMarche: Integer;
  end;
```

### Étape 2 : Créer la classe qui implémente les trois

```pascal
type
  TCanard = class(TInterfacedObject, INageur, IVolant, IMarcheur)
  private
    FVitesseNage: Integer;
    FAltitude: Integer;
    FVitesseMarche: Integer;
  public
    constructor Create;

    // Implémentation de INageur
    procedure Nager;
    function ObtenirVitesseNage: Integer;

    // Implémentation de IVolant
    procedure Voler;
    function ObtenirAltitude: Integer;

    // Implémentation de IMarcheur
    procedure Marcher;
    function ObtenirVitesseMarche: Integer;
  end;
```

### Étape 3 : Implémenter toutes les méthodes

```pascal
implementation

constructor TCanard.Create;
begin
  inherited Create;
  FVitesseNage := 0;
  FAltitude := 0;
  FVitesseMarche := 0;
end;

// ─── Implémentation de INageur ───

procedure TCanard.Nager;
begin
  FVitesseNage := 5;
  WriteLn('🦆 Le canard nage avec ses pattes palmées');
end;

function TCanard.ObtenirVitesseNage: Integer;
begin
  Result := FVitesseNage;
end;

// ─── Implémentation de IVolant ───

procedure TCanard.Voler;
begin
  FAltitude := 100;
  WriteLn('🦆 Le canard s''envole en battant des ailes');
end;

function TCanard.ObtenirAltitude: Integer;
begin
  Result := FAltitude;
end;

// ─── Implémentation de IMarcheur ───

procedure TCanard.Marcher;
begin
  FVitesseMarche := 2;
  WriteLn('🦆 Le canard se dandine sur la terre');
end;

function TCanard.ObtenirVitesseMarche: Integer;
begin
  Result := FVitesseMarche;
end;
```

### Utilisation

```pascal
var
  MonCanard: TCanard;
  Nageur: INageur;
  Volant: IVolant;
  Marcheur: IMarcheur;
begin
  MonCanard := TCanard.Create;

  // Utilisation directe
  MonCanard.Nager;
  MonCanard.Voler;
  MonCanard.Marcher;

  // Utilisation via les interfaces
  Nageur := MonCanard;
  Nageur.Nager;
  WriteLn('Vitesse de nage: ', Nageur.ObtenirVitesseNage, ' km/h');

  Volant := MonCanard;
  Volant.Voler;
  WriteLn('Altitude: ', Volant.ObtenirAltitude, ' m');

  Marcheur := MonCanard;
  Marcheur.Marcher;
  WriteLn('Vitesse de marche: ', Marcheur.ObtenirVitesseMarche, ' km/h');

  // Pas de Free : les variables d'interface (Nageur, Volant, Marcheur)
  // gèrent automatiquement la mémoire de l'objet
end.
```

**Résultat :**
```
🦆 Le canard nage avec ses pattes palmées
🦆 Le canard s'envole en battant des ailes
🦆 Le canard se dandine sur la terre
🦆 Le canard nage avec ses pattes palmées
Vitesse de nage: 5 km/h
🦆 Le canard s'envole en battant des ailes
Altitude: 100 m
🦆 Le canard se dandine sur la terre
Vitesse de marche: 2 km/h
```

---

## Exemple 2 : Appareil multimédia

Un appareil moderne peut faire beaucoup de choses différentes. Créons un smartphone.

### Déclaration des interfaces

```pascal
type
  // Peut passer des appels
  ITelephone = interface
    ['{D4E5F6A7-B8C9-4444-5555-666677778888}']
    procedure AppelerNumero(const Numero: string);
    procedure RaccrocherAppel;
  end;

  // Peut prendre des photos
  IAppareilPhoto = interface
    ['{E5F6A7B8-C9D0-5555-6666-777788889999}']
    procedure PrendrePhoto;
    function ObtenirNombrePhotos: Integer;
  end;

  // Peut naviguer sur internet
  INavigateurWeb = interface
    ['{F6A7B8C9-D0E1-6666-7777-8888999900AA}']
    procedure OuvrirSiteWeb(const URL: string);
    procedure FermerNavigateur;
  end;
```

### Implémentation : le smartphone

```pascal
type
  TSmartphone = class(TInterfacedObject, ITelephone, IAppareilPhoto, INavigateurWeb)
  private
    FEnAppel: Boolean;
    FNombrePhotos: Integer;
    FNavigateurOuvert: Boolean;
  public
    constructor Create;

    // ITelephone
    procedure AppelerNumero(const Numero: string);
    procedure RaccrocherAppel;

    // IAppareilPhoto
    procedure PrendrePhoto;
    function ObtenirNombrePhotos: Integer;

    // INavigateurWeb
    procedure OuvrirSiteWeb(const URL: string);
    procedure FermerNavigateur;
  end;

implementation

constructor TSmartphone.Create;
begin
  inherited Create;
  FEnAppel := False;
  FNombrePhotos := 0;
  FNavigateurOuvert := False;
end;

// ─── ITelephone ───

procedure TSmartphone.AppelerNumero(const Numero: string);
begin
  WriteLn('📱 Appel en cours vers ', Numero);
  FEnAppel := True;
end;

procedure TSmartphone.RaccrocherAppel;
begin
  if FEnAppel then
  begin
    WriteLn('📱 Appel terminé');
    FEnAppel := False;
  end;
end;

// ─── IAppareilPhoto ───

procedure TSmartphone.PrendrePhoto;
begin
  Inc(FNombrePhotos);
  WriteLn('📸 Photo prise ! Total: ', FNombrePhotos);
end;

function TSmartphone.ObtenirNombrePhotos: Integer;
begin
  Result := FNombrePhotos;
end;

// ─── INavigateurWeb ───

procedure TSmartphone.OuvrirSiteWeb(const URL: string);
begin
  WriteLn('🌐 Ouverture de ', URL);
  FNavigateurOuvert := True;
end;

procedure TSmartphone.FermerNavigateur;
begin
  if FNavigateurOuvert then
  begin
    WriteLn('🌐 Navigateur fermé');
    FNavigateurOuvert := False;
  end;
end;
```

### Utilisation polyvalente

```pascal
procedure UtiliserTelephone(Tel: ITelephone);
begin
  WriteLn('=== Mode Téléphone ===');
  Tel.AppelerNumero('+33 6 12 34 56 78');
  Tel.RaccrocherAppel;
  WriteLn('');
end;

procedure UtiliserAppareilPhoto(Appareil: IAppareilPhoto);
begin
  WriteLn('=== Mode Photo ===');
  Appareil.PrendrePhoto;
  Appareil.PrendrePhoto;
  WriteLn('Photos en mémoire: ', Appareil.ObtenirNombrePhotos);
  WriteLn('');
end;

procedure UtiliserNavigateur(Nav: INavigateurWeb);
begin
  WriteLn('=== Mode Navigation ===');
  Nav.OuvrirSiteWeb('https://www.exemple.com');
  Nav.FermerNavigateur;
  WriteLn('');
end;

var
  MonTelephone: TSmartphone;
  Tel: ITelephone;
  Photo: IAppareilPhoto;
  Nav: INavigateurWeb;
begin
  MonTelephone := TSmartphone.Create;

  // Obtenir les différentes interfaces du même objet
  Tel := MonTelephone;
  Photo := MonTelephone;
  Nav := MonTelephone;

  // Le même objet peut être utilisé via différentes interfaces
  UtiliserTelephone(Tel);
  UtiliserAppareilPhoto(Photo);
  UtiliserNavigateur(Nav);

  // Pas de Free : les variables d'interface gèrent la mémoire automatiquement
end.
```

---

## Avantages de l'héritage multiple via interfaces

### 1. Flexibilité maximale

Vous pouvez combiner autant de comportements que nécessaire :

```pascal
type
  TSuperheros = class(TInterfacedObject, IVolant, IFort, IRapide, IIntelligent)
    // Combine 4 capacités différentes !
  end;
```

### 2. Pas de conflit de noms

Contrairement à l'héritage multiple de classes, il n'y a pas de "problème du diamant" car les interfaces n'ont pas d'implémentation.

### 3. Séparation des responsabilités

Chaque interface représente une responsabilité claire et distincte.

```pascal
// Responsabilités bien séparées
ILecteur      // Responsable de la lecture
IEcrivain     // Responsable de l'écriture
ICompresseur  // Responsable de la compression
IChiffreur    // Responsable du chiffrement
```

### 4. Réutilisabilité du code

Les mêmes interfaces peuvent être implémentées par des classes totalement différentes :

```pascal
// Même interface, implémentations différentes
IVolant → TOiseau, TAvion, THelicoptere, TDrone, TInsecte
```

---

## Patterns courants d'utilisation

### Pattern 1 : Capacités optionnelles

```pascal
type
  TVehicule = class(TInterfacedObject)
    // Fonctionnalités de base
  end;

  TVoiture = class(TVehicule, IRoulant)
    // Peut rouler
  end;

  TBateau = class(TVehicule, IFlottant)
    // Peut flotter
  end;

  TVoitureAmphibie = class(TVehicule, IRoulant, IFlottant)
    // Peut rouler ET flotter !
  end;
```

### Pattern 2 : Mixins (mélanges de fonctionnalités)

```pascal
type
  // Fonctionnalités de base
  ISerializable = interface
    function VersJSON: string;
  end;

  IComparable = interface
    function EstEgalA(Autre: TObject): Boolean;
  end;

  ILoggable = interface
    procedure EcrireLog(const Message: string);
  end;

  // Classe avec toutes ces fonctionnalités
  TUtilisateur = class(TInterfacedObject, ISerializable, IComparable, ILoggable)
    // ...
  end;
```

### Pattern 3 : Adaptateurs multiples

```pascal
type
  // Peut se connecter à différentes sources
  TConnecteurUniversel = class(TInterfacedObject,
                                IConnecteurSQL,
                                IConnecteurNoSQL,
                                IConnecteurAPI,
                                IConnecteurFichier)
    // Implémente tous les types de connexions
  end;
```

---

## Règles importantes à respecter

### ✅ Ce qu'on peut faire

```pascal
// Hériter d'une classe + plusieurs interfaces
TClasse = class(TParent, IInterface1, IInterface2, IInterface3)

// Hériter seulement d'interfaces (commence par TInterfacedObject)
TClasse = class(TInterfacedObject, IInterface1, IInterface2)

// Une interface peut aussi hériter d'autres interfaces (voir section 12.4)
IInterface3 = interface(IInterface1)
```

### ❌ Ce qu'on ne peut PAS faire

```pascal
// Hériter de plusieurs classes
TClasse = class(TParent1, TParent2)  // ❌ ERREUR !

// Mettre les interfaces avant la classe parente
TClasse = class(IInterface1, TParent)  // ❌ ERREUR ! Ordre incorrect

// Oublier d'implémenter toutes les méthodes
TClasse = class(TInterfacedObject, IInterface1, IInterface2)
  // Seulement les méthodes de IInterface1 implémentées
  // ❌ ERREUR : IInterface2 pas complètement implémentée
end;
```

---

## Ordre des paramètres : Important !

**Syntaxe correcte :**

```pascal
TClasse = class(ClasseParente, Interface1, Interface2, ...)
             //   ↑ D'ABORD      ↑ ENSUITE les interfaces
             //   la classe
```

**Exemples corrects :**

```pascal
// Avec classe parente
TCanard = class(TAnimal, INageur, IVolant)

// Sans classe parente spécifique
TRobot = class(TInterfacedObject, IMarcheur, IParleur)

// Ou simplement (TObject implicite)
TRobot = class(TInterfacedObject, IMarcheur, IParleur)
```

---

## Exemple complet : Système de paiement

Voici un exemple réaliste d'un système qui gère différents moyens de paiement :

```pascal
unit UPaiements;

{$mode objfpc}{$H+}

interface

type
  // Interfaces pour différentes capacités
  IPaiementCarte = interface
    ['{11111111-2222-3333-4444-555555555555}']
    procedure PayerParCarte(Montant: Currency; const NumeroCarte: string);
  end;

  IPaiementMobile = interface
    ['{22222222-3333-4444-5555-666666666666}']
    procedure PayerParMobile(Montant: Currency; const NumeroTel: string);
  end;

  IPaiementCrypto = interface
    ['{33333333-4444-5555-6666-777777777777}']
    procedure PayerParCrypto(Montant: Currency; const AdresseWallet: string);
  end;

  // Terminal qui accepte TOUS les types de paiement
  TTerminalUniversel = class(TInterfacedObject,
                              IPaiementCarte,
                              IPaiementMobile,
                              IPaiementCrypto)
  private
    FTotalEncaisse: Currency;
  public
    constructor Create;

    procedure PayerParCarte(Montant: Currency; const NumeroCarte: string);
    procedure PayerParMobile(Montant: Currency; const NumeroTel: string);
    procedure PayerParCrypto(Montant: Currency; const AdresseWallet: string);

    function ObtenirTotal: Currency;
  end;

implementation

constructor TTerminalUniversel.Create;
begin
  inherited Create;
  FTotalEncaisse := 0;
end;

procedure TTerminalUniversel.PayerParCarte(Montant: Currency; const NumeroCarte: string);
begin
  WriteLn('💳 Paiement carte: ', Montant:0:2, ' € sur carte ', NumeroCarte);
  FTotalEncaisse := FTotalEncaisse + Montant;
end;

procedure TTerminalUniversel.PayerParMobile(Montant: Currency; const NumeroTel: string);
begin
  WriteLn('📱 Paiement mobile: ', Montant:0:2, ' € via ', NumeroTel);
  FTotalEncaisse := FTotalEncaisse + Montant;
end;

procedure TTerminalUniversel.PayerParCrypto(Montant: Currency; const AdresseWallet: string);
begin
  WriteLn('₿ Paiement crypto: ', Montant:0:2, ' € vers ', AdresseWallet);
  FTotalEncaisse := FTotalEncaisse + Montant;
end;

function TTerminalUniversel.ObtenirTotal: Currency;
begin
  Result := FTotalEncaisse;
end;

end.
```

**Utilisation :**

```pascal
var
  Terminal: TTerminalUniversel;
begin
  Terminal := TTerminalUniversel.Create;

  // Accepte différents moyens de paiement
  Terminal.PayerParCarte(45.90, '**** **** **** 1234');
  Terminal.PayerParMobile(23.50, '+33 6 12 34 56 78');
  Terminal.PayerParCrypto(150.00, '1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa');

  WriteLn('');
  WriteLn('Total encaissé: ', Terminal.ObtenirTotal:0:2, ' €');

  Terminal.Free;
end.
```

---

## Résumé

### L'héritage simple en Pascal
- Une classe ne peut hériter que d'**une seule** classe parente
- Limite : impossible de combiner plusieurs classes

### L'héritage multiple via interfaces
- Une classe peut implémenter **plusieurs interfaces**
- Syntaxe : `TClasse = class(TParent, IInterface1, IInterface2, ...)`
- **Ordre important** : classe parente d'abord, puis interfaces

### Avantages
✅ Combine plusieurs comportements dans une seule classe
✅ Pas de conflit de noms (problème du diamant évité)
✅ Flexibilité maximale
✅ Séparation claire des responsabilités
✅ Code réutilisable

### Règles à retenir
- La classe parente (si elle existe) vient **toujours en premier**
- Les interfaces suivent, **séparées par des virgules**
- Il faut implémenter **toutes** les méthodes de **toutes** les interfaces
- Utiliser `TInterfacedObject` comme classe de base si pas d'autre parent

---

## Prochaine étape

Dans la section suivante (12.4), vous découvrirez que les interfaces elles-mêmes peuvent hériter d'autres interfaces, créant ainsi des hiérarchies d'interfaces pour organiser encore mieux votre code.

⏭️ [IInterface et IUnknown](/12-interfaces-poo-avancee/04-iinterface-iunknown.md)
