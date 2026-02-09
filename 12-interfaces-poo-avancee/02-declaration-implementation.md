🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.2 Déclaration et implémentation

## Introduction

Maintenant que vous comprenez le concept d'interface, il est temps d'apprendre à les déclarer et à les implémenter correctement. Cette section vous guidera pas à pas à travers la syntaxe exacte et les règles à respecter.

---

## Déclaration d'une interface

### Structure de base

Une interface se déclare dans la section `type` de votre unité, comme une classe :

```pascal
type
  INomInterface = interface
    ['{GUID}']
    // Déclarations des méthodes
  end;
```

**Éléments constitutifs :**
1. Le mot-clé `interface`
2. Un GUID (identifiant unique) optionnel mais recommandé
3. Les déclarations de méthodes (pas d'implémentation)
4. Le mot-clé `end`

### Convention de nommage

Par convention, les interfaces commencent toujours par la lettre **I** majuscule :

✅ **Bon :**
- `IConnectable`
- `ISerializable`
- `IComparable`
- `IObservateur`

❌ **Mauvais :**
- `Connectable` (manque le I)
- `InterfaceConnectable` (redondant)
- `iConnectable` (I en minuscule)

---

## Le GUID : Identifiant unique

### Qu'est-ce qu'un GUID ?

Un **GUID** (Globally Unique IDentifier) est un code unique qui identifie votre interface de manière universelle. Il ressemble à ceci :

```pascal
['{8F7A2B3C-4D5E-6F7A-8B9C-0D1E2F3A4B5C}']
```

### Pourquoi utiliser un GUID ?

Le GUID permet :
- D'identifier l'interface de façon unique dans tout le système
- D'utiliser certaines fonctionnalités avancées (COM, CORBA)
- D'éviter les conflits si deux interfaces ont le même nom

### Comment générer un GUID ?

Dans **Lazarus IDE** :
1. Placez votre curseur à l'endroit où vous voulez le GUID
2. Menu : **Outils** → **Générer un GUID** (ou `Ctrl+Shift+G`)
3. Choisissez le format avec crochets : `['{...}']`
4. Collez-le dans votre code

**Exemple complet :**

```pascal
type
  ICalculateur = interface
    ['{B7F8E4A2-1C9D-4E6F-A3B2-7D8E9F0A1B2C}']
    function Additionner(A, B: Integer): Integer;
    function Soustraire(A, B: Integer): Integer;
  end;
```

---

## Déclaration des méthodes dans une interface

### Types de méthodes autorisées

Une interface peut contenir :
- ✅ Des **procédures** (sans valeur de retour)
- ✅ Des **fonctions** (avec valeur de retour)

Une interface **NE peut PAS** contenir :
- ❌ Des variables ou attributs
- ❌ Des constructeurs ou destructeurs
- ❌ Des propriétés avec implémentation
- ❌ Du code exécutable

### Syntaxe des méthodes

```pascal
type
  IGestionnaireFichier = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']

    // Procédures (pas de retour)
    procedure Ouvrir(const NomFichier: string);
    procedure Fermer;
    procedure Ecrire(const Donnees: string);

    // Fonctions (avec retour)
    function Lire: string;
    function EstOuvert: Boolean;
    function ObtenirTaille: Int64;
  end;
```

**Points importants :**
- Terminez chaque déclaration par un point-virgule
- Vous pouvez mettre des paramètres comme dans une procédure/fonction normale
- Utilisez `const` pour les paramètres en lecture seule
- Pas de mot-clé `public` ou `private` : tout est public

---

## Implémentation d'une interface

### Étape 1 : Déclarer la classe

Pour implémenter une interface, votre classe doit :
1. Hériter de `TInterfacedObject` (ou `TObject` avec gestion manuelle)
2. Spécifier l'interface après une virgule

```pascal
type
  TGestionnaireFichierTexte = class(TInterfacedObject, IGestionnaireFichier)
  private
    FFichier: TextFile;
    FOuvert: Boolean;
  public
    // Déclaration des méthodes de l'interface
    procedure Ouvrir(const NomFichier: string);
    procedure Fermer;
    procedure Ecrire(const Donnees: string);
    function Lire: string;
    function EstOuvert: Boolean;
    function ObtenirTaille: Int64;
  end;
```

### Étape 2 : Implémenter toutes les méthodes

**Règle absolue :** Vous devez implémenter **TOUTES** les méthodes déclarées dans l'interface, sinon le code ne compilera pas.

```pascal
implementation

procedure TGestionnaireFichierTexte.Ouvrir(const NomFichier: string);
begin
  AssignFile(FFichier, NomFichier);
  Reset(FFichier);
  FOuvert := True;
end;

procedure TGestionnaireFichierTexte.Fermer;
begin
  if FOuvert then
  begin
    CloseFile(FFichier);
    FOuvert := False;
  end;
end;

procedure TGestionnaireFichierTexte.Ecrire(const Donnees: string);
begin
  if FOuvert then
    WriteLn(FFichier, Donnees);
end;

function TGestionnaireFichierTexte.Lire: string;
begin
  if FOuvert then
    ReadLn(FFichier, Result)
  else
    Result := '';
end;

function TGestionnaireFichierTexte.EstOuvert: Boolean;
begin
  Result := FOuvert;
end;

function TGestionnaireFichierTexte.ObtenirTaille: Int64;
begin
  // Implémentation simplifiée
  Result := 0;
end;
```

---

## TInterfacedObject : La classe de base

### Pourquoi hériter de TInterfacedObject ?

`TInterfacedObject` est une classe fournie par FreePascal qui :
- Implémente automatiquement la gestion des références (comptage)
- Fournit les méthodes nécessaires pour les interfaces (`QueryInterface`, `_AddRef`, `_Release`)
- Simplifie grandement le travail du programmeur

### Syntaxe

```pascal
type
  MaClasse = class(TInterfacedObject, IMonInterface)
    // ...
  end;
```

**Attention à l'ordre :**
1. D'abord la classe parente (`TInterfacedObject`)
2. Puis l'interface (ou les interfaces) séparées par des virgules

---

## Implémenter plusieurs interfaces

Une classe peut implémenter plusieurs interfaces simultanément. Il suffit de les lister séparées par des virgules :

```pascal
type
  // Première interface
  ILisible = interface
    ['{F1E2D3C4-B5A6-7890-1234-567890ABCDEF}']
    function Lire: string;
  end;

  // Deuxième interface
  IEcrivable = interface
    ['{A9B8C7D6-E5F4-3210-FEDC-BA9876543210}']
    procedure Ecrire(const Texte: string);
  end;

  // Classe qui implémente les deux
  TFichierComplet = class(TInterfacedObject, ILisible, IEcrivable)
  private
    FContenu: string;
  public
    // Méthodes de ILisible
    function Lire: string;

    // Méthodes de IEcrivable
    procedure Ecrire(const Texte: string);
  end;

implementation

function TFichierComplet.Lire: string;
begin
  Result := FContenu;
end;

procedure TFichierComplet.Ecrire(const Texte: string);
begin
  FContenu := Texte;
end;
```

**Règle :** Vous devez implémenter **toutes** les méthodes de **toutes** les interfaces listées.

---

## Exemple complet : Système de notifications

Voici un exemple réaliste montrant toute la chaîne :

```pascal
unit UNotifications;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // 1. DÉCLARATION DE L'INTERFACE
  INotificateur = interface
    ['{D4C3B2A1-9E8F-7D6C-5B4A-3E2F1D0C9B8A}']  // GUID : identifiant unique de l'interface
    procedure EnvoyerMessage(const Message: string);
    function ObtenirNomService: string;
  end;

  // 2. IMPLÉMENTATION PAR EMAIL
  TNotificateurEmail = class(TInterfacedObject, INotificateur)
  private
    FAdresseEmail: string;
  public
    constructor Create(const AdresseEmail: string);
    procedure EnvoyerMessage(const Message: string);
    function ObtenirNomService: string;
  end;

  // 3. IMPLÉMENTATION PAR SMS
  TNotificateurSMS = class(TInterfacedObject, INotificateur)
  private
    FNumeroTelephone: string;
  public
    constructor Create(const NumeroTel: string);
    procedure EnvoyerMessage(const Message: string);
    function ObtenirNomService: string;
  end;

implementation

{ TNotificateurEmail }

constructor TNotificateurEmail.Create(const AdresseEmail: string);
begin
  inherited Create;
  FAdresseEmail := AdresseEmail;
end;

procedure TNotificateurEmail.EnvoyerMessage(const Message: string);
begin
  WriteLn('📧 Envoi email à ', FAdresseEmail);
  WriteLn('   Message: ', Message);
end;

function TNotificateurEmail.ObtenirNomService: string;
begin
  Result := 'Service Email';
end;

{ TNotificateurSMS }

constructor TNotificateurSMS.Create(const NumeroTel: string);
begin
  inherited Create;
  FNumeroTelephone := NumeroTel;
end;

procedure TNotificateurSMS.EnvoyerMessage(const Message: string);
begin
  WriteLn('📱 Envoi SMS au ', FNumeroTelephone);
  WriteLn('   Message: ', Message);
end;

function TNotificateurSMS.ObtenirNomService: string;
begin
  Result := 'Service SMS';
end;

end.
```

**Utilisation :**

```pascal
program TestNotifications;

{$mode objfpc}{$H+}

uses
  UNotifications;

// Accepte tout objet implémentant INotificateur (polymorphisme via interface)
procedure EnvoyerAlerte(Notif: INotificateur; const Alerte: string);
begin
  WriteLn('=== ', Notif.ObtenirNomService, ' ===');
  Notif.EnvoyerMessage(Alerte);
  WriteLn('');
end;

var
  Email: INotificateur;
  SMS: INotificateur;
begin
  // Création des notificateurs
  Email := TNotificateurEmail.Create('utilisateur@exemple.com');
  SMS := TNotificateurSMS.Create('+33 6 12 34 56 78');

  // Utilisation via l'interface
  EnvoyerAlerte(Email, 'Nouveau message dans votre boîte');
  EnvoyerAlerte(SMS, 'Code de vérification: 123456');

  // Pas besoin de Free : gestion automatique !
end.
```

**Résultat :**
```
=== Service Email ===
📧 Envoi email à utilisateur@exemple.com
   Message: Nouveau message dans votre boîte

=== Service SMS ===
📱 Envoi SMS au +33 6 12 34 56 78
   Message: Code de vérification: 123456
```

---

## Bonnes pratiques de déclaration

### 1. Une interface = Un rôle clair

✅ **Bon :** Interfaces spécialisées
```pascal
ILecteur = interface      // Responsabilité: lire
IEcrivain = interface     // Responsabilité: écrire
IVerifiable = interface   // Responsabilité: vérifier
```

❌ **Mauvais :** Interface fourre-tout
```pascal
IFichier = interface
  procedure Lire;
  procedure Ecrire;
  procedure Verifier;
  procedure Comprimer;
  procedure Chiffrer;
  // ... 20 méthodes ...
end;
```

### 2. Noms expressifs

Utilisez des noms qui décrivent clairement le comportement :

✅ **Bon :**
- `ISerializable` : peut être sérialisé
- `IComparable` : peut être comparé
- `IDisposable` : peut être libéré

❌ **Mauvais :**
- `IHelper` (trop vague)
- `IUtility` (ne dit rien)
- `IManager` (trop générique)

### 3. Gardez les interfaces petites

**Principe ISP** (Interface Segregation Principle) :
> Il vaut mieux plusieurs petites interfaces qu'une grosse interface.

```pascal
// ✅ Bon : interfaces séparées
ILisible = interface
  function Lire: string;
end;

IEcrivable = interface
  procedure Ecrire(const Texte: string);
end;

// Une classe peut implémenter les deux si nécessaire
TFichier = class(TInterfacedObject, ILisible, IEcrivable)
```

---

## Erreurs courantes et solutions

### Erreur 1 : Oublier d'implémenter une méthode

**Code :**
```pascal
type
  ITest = interface
    procedure Methode1;
    procedure Methode2;
  end;

  TTest = class(TInterfacedObject, ITest)
  public
    procedure Methode1;
    // Oubli de Methode2 !
  end;
```

**Erreur du compilateur :**
```
Error: No matching implementation for interface method "Methode2" found
```

**Solution :** Implémenter toutes les méthodes déclarées dans l'interface.

### Erreur 2 : Signature incorrecte

**Code :**
```pascal
type
  ICalcul = interface
    function Additionner(A, B: Integer): Integer;
  end;

  TCalcul = class(TInterfacedObject, ICalcul)
  public
    // Signature différente !
    function Additionner(A, B: Double): Double;
  end;
```

**Problème :** Les types de paramètres ne correspondent pas.

**Solution :** Respecter exactement la signature (noms, types, ordre des paramètres).

### Erreur 3 : Hériter de TObject au lieu de TInterfacedObject

**Code :**
```pascal
type
  TTest = class(TObject, ITest)  // ❌ TObject au lieu de TInterfacedObject
```

**Problème :** Vous devrez gérer manuellement le comptage de références (complexe pour un débutant).

**Solution :** Utiliser `TInterfacedObject` comme classe de base.

---

## Récapitulatif de la syntaxe

```pascal
// ═══════════════════════════════════════════════════════════
// DÉCLARATION (dans la section interface de l'unité)
// ═══════════════════════════════════════════════════════════

type
  // Interface
  INomInterface = interface
    ['{GUID-GENERE-PAR-LAZARUS}']
    procedure MethodeSansRetour(Param: Type);
    function MethodeAvecRetour(Param: Type): TypeRetour;
  end;

  // Classe d'implémentation
  TNomClasse = class(TInterfacedObject, INomInterface)
  private
    // Attributs privés
  public
    // Déclaration des méthodes de l'interface
    procedure MethodeSansRetour(Param: Type);
    function MethodeAvecRetour(Param: Type): TypeRetour;
  end;

// ═══════════════════════════════════════════════════════════
// IMPLÉMENTATION (dans la section implementation de l'unité)
// ═══════════════════════════════════════════════════════════

implementation

procedure TNomClasse.MethodeSansRetour(Param: Type);
begin
  // Code ici
end;

function TNomClasse.MethodeAvecRetour(Param: Type): TypeRetour;
begin
  // Code ici
  Result := ...;
end;
```

---

## Résumé

### Déclaration d'une interface
- Se fait dans la section `type`
- Commence par `I` par convention
- Contient un GUID (généré par Lazarus)
- Liste uniquement des déclarations de méthodes
- Pas d'attributs, pas de code

### Implémentation d'une interface
- La classe hérite de `TInterfacedObject`
- L'interface est listée après une virgule
- **Toutes** les méthodes doivent être implémentées
- Les signatures doivent correspondre exactement
- Plusieurs interfaces peuvent être implémentées

### Points clés à retenir
✅ Une interface = un contrat obligatoire  
✅ `TInterfacedObject` simplifie le travail  
✅ Toujours générer un GUID pour vos interfaces  
✅ Respecter les conventions de nommage (I...)  
✅ Garder les interfaces simples et ciblées  

---

## Prochaine étape

Dans la section suivante (12.3), vous découvrirez comment utiliser les interfaces pour réaliser de l'**héritage multiple**, une fonctionnalité puissante que les classes seules ne peuvent pas offrir.

⏭️ [Héritage multiple via interfaces](/12-interfaces-poo-avancee/03-heritage-multiple-via-interfaces.md)
