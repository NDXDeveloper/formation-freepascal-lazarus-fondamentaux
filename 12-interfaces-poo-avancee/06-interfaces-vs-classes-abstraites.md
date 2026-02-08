🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.6 Interfaces vs classes abstraites

## Introduction : Deux outils, deux usages

Vous avez maintenant appris deux concepts puissants de la POO :
- Les **classes abstraites** (chapitre 11)
- Les **interfaces** (chapitre 12)

Les deux semblent similaires au premier abord : ils définissent des "contrats" que d'autres classes doivent respecter. Mais ils ont des différences importantes qui influencent quand les utiliser.

**Analogie :**
- Une **classe abstraite** est comme un **modèle de maison** avec certaines pièces déjà construites (cuisine, salle de bain) et d'autres à terminer (chambres, salon).
- Une **interface** est comme un **cahier des charges** qui dit "la maison DOIT avoir une porte, des fenêtres et un toit", sans rien construire du tout.

---

## Rappel : Qu'est-ce qu'une classe abstraite ?

### Définition

Une **classe abstraite** est une classe qui :
- Peut contenir des méthodes **avec implémentation** (code)
- Peut contenir des méthodes **abstraites** (sans code)
- Peut avoir des **attributs** (variables)
- **Ne peut pas être instanciée** directement
- Sert de **base** pour d'autres classes

### Exemple de classe abstraite

```pascal
type
  TAnimal = class abstract
  private
    FNom: string;
    FAge: Integer;
  public
    constructor Create(const Nom: string; Age: Integer);

    // Méthode concrète (avec code)
    procedure SePresenter;

    // Méthodes abstraites (sans code)
    procedure Manger; virtual; abstract;
    procedure Dormir; virtual; abstract;

    property Nom: string read FNom;
    property Age: Integer read FAge;
  end;

constructor TAnimal.Create(const Nom: string; Age: Integer);
begin
  FNom := Nom;
  FAge := Age;
end;

procedure TAnimal.SePresenter;
begin
  WriteLn('Je m''appelle ', FNom, ' et j''ai ', FAge, ' ans');
end;
```

**Points clés :**
- ✅ A des attributs (`FNom`, `FAge`)
- ✅ A du code réutilisable (`SePresenter`)
- ✅ Définit des méthodes à implémenter (`Manger`, `Dormir`)

---

## Rappel : Qu'est-ce qu'une interface ?

### Définition

Une **interface** est un contrat qui :
- Ne contient **aucune implémentation** (pas de code)
- Ne peut **pas avoir d'attributs**
- Définit uniquement des **déclarations de méthodes**
- Peut être implémentée par **plusieurs classes différentes**
- Permet l'**héritage multiple**

### Exemple d'interface

```pascal
type
  IMangeur = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    procedure Manger;
    function AimeLaNourriture(const Nourriture: string): Boolean;
  end;

  IDormeur = interface
    ['{B2C3D4E5-F6A7-8901-BCDE-F12345678901}']
    procedure Dormir;
    function ObtenirHeuresSommeil: Integer;
  end;
```

**Points clés :**
- ❌ Pas d'attributs
- ❌ Pas de code
- ✅ Seulement des déclarations
- ✅ Héritage multiple possible

---

## Comparaison détaillée

### Tableau comparatif

| Aspect | Classe Abstraite | Interface |
|--------|------------------|-----------|
| **Code réutilisable** | ✅ Oui (méthodes concrètes) | ❌ Non (seulement déclarations) |
| **Attributs** | ✅ Oui | ❌ Non |
| **Constructeur** | ✅ Oui | ❌ Non |
| **Héritage multiple** | ❌ Non (un seul parent) | ✅ Oui (plusieurs interfaces) |
| **Instanciation** | ❌ Non (abstraite) | ❌ Non |
| **Relation** | "EST-UN" (is-a) | "PEUT-FAIRE" (can-do) |
| **Comptage références** | ❌ Gestion manuelle | ✅ Automatique |
| **Visibilité** | ✅ public, private, protected | ❌ Tout public |
| **Modification** | ⚠️ Impacte les descendants | ✅ Plus flexible |
| **Usage principal** | Hiérarchie d'objets similaires | Comportements indépendants |

---

## Différences fondamentales

### 1. Code réutilisable

**Classe abstraite :**
```pascal
type
  TVehicule = class abstract
  private
    FVitesse: Integer;
  public
    constructor Create;

    // ✅ Code réutilisable pour tous les véhicules
    procedure Accelerer;
    procedure Freiner;

    // Méthodes à implémenter
    procedure Demarrer; virtual; abstract;
  end;

constructor TVehicule.Create;
begin
  FVitesse := 0;
end;

procedure TVehicule.Accelerer;
begin
  Inc(FVitesse, 10);
  WriteLn('Vitesse: ', FVitesse, ' km/h');
end;

procedure TVehicule.Freiner;
begin
  if FVitesse > 0 then
    Dec(FVitesse, 10);
  WriteLn('Vitesse: ', FVitesse, ' km/h');
end;
```

**Interface :**
```pascal
type
  IVehicule = interface
    ['{...}']
    // ❌ Pas de code, seulement des déclarations
    procedure Demarrer;
    procedure Accelerer;
    procedure Freiner;
  end;

  // Chaque classe doit implémenter TOUT le code
  TVoiture = class(TInterfacedObject, IVehicule)
  private
    FVitesse: Integer;
  public
    procedure Demarrer;
    procedure Accelerer;
    procedure Freiner;
  end;
```

**Conclusion :** Si vous avez du code **commun** à partager, utilisez une classe abstraite.

---

### 2. Héritage multiple

**Classe abstraite (héritage simple) :**
```pascal
type
  TAnimal = class abstract
    // ...
  end;

  TMachine = class abstract
    // ...
  end;

  // ❌ IMPOSSIBLE : on ne peut pas hériter des deux
  TRobotAnimal = class(TAnimal, TMachine)  // ERREUR !
  end;
```

**Interface (héritage multiple) :**
```pascal
type
  IAnimal = interface
    ['{...}']
    procedure Manger;
  end;

  IMachine = interface
    ['{...}']
    procedure Fonctionner;
  end;

  // ✅ POSSIBLE : on peut implémenter les deux
  TRobotAnimal = class(TInterfacedObject, IAnimal, IMachine)
  public
    procedure Manger;
    procedure Fonctionner;
  end;
```

**Conclusion :** Si vous avez besoin de combiner plusieurs comportements différents, utilisez des interfaces.

---

### 3. Relation sémantique

**Classe abstraite : relation "EST-UN" (is-a)**

```pascal
type
  TAnimal = class abstract
    // Chien EST-UN Animal
    // Chat EST-UN Animal
  end;

  TChien = class(TAnimal)
    // Un chien EST vraiment un animal
  end;
```

**Interface : relation "PEUT-FAIRE" (can-do)**

```pascal
type
  IVolant = interface
    // Oiseau PEUT Voler
    // Avion PEUT Voler
    // Mais un oiseau N'EST PAS un avion !
  end;

  TOiseau = class(TInterfacedObject, IVolant)
    // Un oiseau PEUT voler (capacité)
  end;

  TAvion = class(TInterfacedObject, IVolant)
    // Un avion PEUT voler (capacité)
  end;
```

**Conclusion :**
- Utilisez une classe abstraite pour une **hiérarchie d'objets similaires**
- Utilisez une interface pour des **capacités indépendantes**

---

## Quand utiliser une classe abstraite ?

### Cas d'usage idéaux

✅ **Utilisez une classe abstraite quand :**

1. Vous avez une **hiérarchie claire** d'objets similaires
2. Vous voulez **partager du code** entre les descendants
3. Vous avez besoin d'**attributs communs**
4. Les classes dérivées sont **étroitement liées**
5. Vous voulez un **constructeur commun**

### Exemple : Système de documents

```pascal
type
  // Classe abstraite : représente tous les types de documents
  TDocument = class abstract
  private
    FNomFichier: string;
    FDateCreation: TDateTime;
    FModifie: Boolean;
  public
    constructor Create(const NomFichier: string);

    // Code commun à tous les documents
    procedure Sauvegarder;
    procedure Fermer;
    function EstModifie: Boolean;

    // Méthodes spécifiques à implémenter
    procedure Ouvrir; virtual; abstract;
    procedure Afficher; virtual; abstract;

    property NomFichier: string read FNomFichier;
  end;

constructor TDocument.Create(const NomFichier: string);
begin
  FNomFichier := NomFichier;
  FDateCreation := Now;
  FModifie := False;
end;

procedure TDocument.Sauvegarder;
begin
  WriteLn('💾 Sauvegarde de ', FNomFichier);
  FModifie := False;
end;

procedure TDocument.Fermer;
begin
  if FModifie then
    WriteLn('⚠️  Document non sauvegardé !');
  WriteLn('❌ Fermeture de ', FNomFichier);
end;

function TDocument.EstModifie: Boolean;
begin
  Result := FModifie;
end;

// Documents spécifiques
type
  TDocumentTexte = class(TDocument)
  public
    procedure Ouvrir; override;
    procedure Afficher; override;
  end;

  TDocumentImage = class(TDocument)
  public
    procedure Ouvrir; override;
    procedure Afficher; override;
  end;

procedure TDocumentTexte.Ouvrir;
begin
  WriteLn('📄 Ouverture du document texte: ', NomFichier);
end;

procedure TDocumentTexte.Afficher;
begin
  WriteLn('📖 Affichage du texte...');
end;

procedure TDocumentImage.Ouvrir;
begin
  WriteLn('🖼️  Ouverture de l''image: ', NomFichier);
end;

procedure TDocumentImage.Afficher;
begin
  WriteLn('🎨 Affichage de l''image...');
end;
```

**Avantages ici :**
- Code commun partagé (`Sauvegarder`, `Fermer`, `EstModifie`)
- Attributs communs (`FNomFichier`, `FDateCreation`)
- Hiérarchie claire : tous sont des Documents

---

## Quand utiliser une interface ?

### Cas d'usage idéaux

✅ **Utilisez une interface quand :**

1. Vous voulez définir un **comportement** sans relation de parenté
2. Vous avez besoin d'**héritage multiple**
3. Les classes qui l'implémentent sont **très différentes**
4. Vous voulez un **couplage faible**
5. Vous visez la **testabilité** (mocks/stubs)

### Exemple : Système de notifications multiples

```pascal
type
  // Interfaces : définissent des capacités
  IEnvoyable = interface
    ['{11111111-2222-3333-4444-555555555555}']
    procedure Envoyer(const Destinataire, Message: string);
  end;

  IArchivable = interface
    ['{22222222-3333-4444-5555-666666666666}']
    procedure Archiver;
    function ObtenirDateArchivage: TDateTime;
  end;

  IChiffrable = interface
    ['{33333333-4444-5555-6666-777777777777}']
    procedure Chiffrer(const Cle: string);
    procedure Dechiffrer(const Cle: string);
  end;

// Email : envoyable + archivable
type
  TEmail = class(TInterfacedObject, IEnvoyable, IArchivable)
  private
    FDateArchivage: TDateTime;
  public
    procedure Envoyer(const Destinataire, Message: string);
    procedure Archiver;
    function ObtenirDateArchivage: TDateTime;
  end;

// SMS : envoyable seulement
type
  TSMS = class(TInterfacedObject, IEnvoyable)
  public
    procedure Envoyer(const Destinataire, Message: string);
  end;

// Message sécurisé : envoyable + chiffrable
type
  TMessageSecurise = class(TInterfacedObject, IEnvoyable, IChiffrable)
  public
    procedure Envoyer(const Destinataire, Message: string);
    procedure Chiffrer(const Cle: string);
    procedure Dechiffrer(const Cle: string);
  end;

// Implémentations
procedure TEmail.Envoyer(const Destinataire, Message: string);
begin
  WriteLn('📧 Email envoyé à ', Destinataire, ': ', Message);
end;

procedure TEmail.Archiver;
begin
  FDateArchivage := Now;
  WriteLn('📦 Email archivé');
end;

function TEmail.ObtenirDateArchivage: TDateTime;
begin
  Result := FDateArchivage;
end;

procedure TSMS.Envoyer(const Destinataire, Message: string);
begin
  WriteLn('📱 SMS envoyé à ', Destinataire, ': ', Message);
end;

procedure TMessageSecurise.Envoyer(const Destinataire, Message: string);
begin
  WriteLn('🔐 Message sécurisé envoyé à ', Destinataire);
end;

procedure TMessageSecurise.Chiffrer(const Cle: string);
begin
  WriteLn('🔒 Message chiffré avec la clé');
end;

procedure TMessageSecurise.Dechiffrer(const Cle: string);
begin
  WriteLn('🔓 Message déchiffré');
end;
```

**Utilisation polymorphe :**

```pascal
procedure TraiterEnvoyable(Msg: IEnvoyable);
begin
  Msg.Envoyer('utilisateur@exemple.com', 'Bonjour !');
end;

procedure ArchiverSiPossible(Obj: IInterface);
var
  Archivable: IArchivable;
begin
  if Supports(Obj, IArchivable, Archivable) then
  begin
    Archivable.Archiver;
    WriteLn('Date d''archivage: ', DateTimeToStr(Archivable.ObtenirDateArchivage));
  end
  else
    WriteLn('Cet objet n''est pas archivable');
end;

var
  Email: IEnvoyable;
  SMS: IEnvoyable;
  Secure: IEnvoyable;
begin
  Email := TEmail.Create;
  SMS := TSMS.Create;
  Secure := TMessageSecurise.Create;

  // Tous peuvent être traités comme IEnvoyable
  TraiterEnvoyable(Email);
  TraiterEnvoyable(SMS);
  TraiterEnvoyable(Secure);

  WriteLn('');

  // Seul l'email peut être archivé
  ArchiverSiPossible(Email as IInterface);   // ✅ Fonctionne
  ArchiverSiPossible(SMS as IInterface);      // ❌ N'est pas archivable

  // Pas de Free : gestion automatique via les interfaces
end.
```

**Avantages ici :**
- Flexibilité totale : chaque classe choisit ses capacités
- Héritage multiple : Email est envoyable ET archivable
- Pas de code dupliqué inutile
- Facile de tester avec des mock

---

## Peut-on combiner les deux ?

### Oui ! Et c'est souvent la meilleure solution

Une classe peut **hériter d'une classe abstraite** ET **implémenter des interfaces** :

```pascal
type
  // Classe abstraite : base commune
  TAnimal = class abstract
  private
    FNom: string;
  public
    constructor Create(const Nom: string);
    procedure SePresenter;
    procedure Manger; virtual; abstract;
  end;

  // Interfaces : capacités optionnelles
  IVolant = interface
    ['{...}']
    procedure Voler;
  end;

  INageur = interface
    ['{...}']
    procedure Nager;
  end;

  // Combinaison : hérite de TAnimal + implémente des interfaces
  TCanard = class(TAnimal, IVolant, INageur)
  public
    procedure Manger; override;
    procedure Voler;
    procedure Nager;
  end;

  TChien = class(TAnimal)
  public
    procedure Manger; override;
    // Pas de vol ni de nage
  end;

constructor TAnimal.Create(const Nom: string);
begin
  FNom := Nom;
end;

procedure TAnimal.SePresenter;
begin
  WriteLn('Je suis ', FNom);
end;

procedure TCanard.Manger;
begin
  WriteLn('🦆 Le canard mange du pain');
end;

procedure TCanard.Voler;
begin
  WriteLn('🦆 Le canard vole');
end;

procedure TCanard.Nager;
begin
  WriteLn('🦆 Le canard nage');
end;

procedure TChien.Manger;
begin
  WriteLn('🐕 Le chien mange des croquettes');
end;
```

**C'est le meilleur des deux mondes :**
- Code commun dans `TAnimal` (nom, présentation)
- Capacités optionnelles via interfaces (voler, nager)

---

## Guide de décision

### Arbre de décision

```
Avez-vous du code à partager entre les classes ?
├─ OUI → Utilisez une CLASSE ABSTRAITE
│         (ou combinez classe abstraite + interfaces)
│
└─ NON → Les classes ont-elles une relation "est-un" ?
          ├─ OUI → CLASSE ABSTRAITE
          │        (relation hiérarchique forte)
          │
          └─ NON → Avez-vous besoin d'héritage multiple ?
                   ├─ OUI → INTERFACE
                   │        (plusieurs comportements)
                   │
                   └─ NON → Les classes sont-elles très différentes ?
                            ├─ OUI → INTERFACE
                            │        (couplage faible)
                            │
                            └─ NON → CLASSE ABSTRAITE
                                     (hiérarchie logique)
```

### Questions à se poser

| Question | Classe abstraite | Interface |
|----------|------------------|-----------|
| Y a-t-il du code à partager ? | ✅ Oui | ❌ Non |
| Relation "EST-UN" forte ? | ✅ Oui | ❌ Non |
| Besoin d'attributs communs ? | ✅ Oui | ❌ Non |
| Héritage multiple nécessaire ? | ❌ Non | ✅ Oui |
| Classes très différentes ? | ❌ Non | ✅ Oui |
| Besoin d'un constructeur commun ? | ✅ Oui | ❌ Non |
| Priorité à la flexibilité ? | ❌ Non | ✅ Oui |

---

## Exemples du monde réel

### Exemple 1 : Application de dessin

**Classe abstraite pour les formes :**

```pascal
type
  TForme = class abstract
  private
    FCouleur: TColor;
    FPosition: TPoint;
  public
    constructor Create(Couleur: TColor; Position: TPoint);

    // Code commun
    procedure Deplacer(NouvellePosition: TPoint);
    procedure ChangerCouleur(NouvelleCouleur: TColor);

    // À implémenter
    procedure Dessiner; virtual; abstract;
    function CalculerAire: Double; virtual; abstract;
  end;

  TCercle = class(TForme)
  private
    FRayon: Integer;
  public
    constructor Create(Couleur: TColor; Position: TPoint; Rayon: Integer);
    procedure Dessiner; override;
    function CalculerAire: Double; override;
  end;

  TRectangle = class(TForme)
  private
    FLargeur, FHauteur: Integer;
  public
    constructor Create(Couleur: TColor; Position: TPoint; L, H: Integer);
    procedure Dessiner; override;
    function CalculerAire: Double; override;
  end;
```

**Interfaces pour les capacités :**

```pascal
type
  ISelectionnable = interface
    ['{...}']
    procedure Selectionner;
    procedure Deselectionner;
    function EstSelectionne: Boolean;
  end;

  IRedimensionnable = interface
    ['{...}']
    procedure Redimensionner(Facteur: Double);
  end;

  // Cercle avec capacités
  TCercleAvance = class(TForme, ISelectionnable, IRedimensionnable)
  private
    FRayon: Integer;
    FSelectionne: Boolean;
  public
    procedure Dessiner; override;
    function CalculerAire: Double; override;

    // ISelectionnable
    procedure Selectionner;
    procedure Deselectionner;
    function EstSelectionne: Boolean;

    // IRedimensionnable
    procedure Redimensionner(Facteur: Double);
  end;
```

**Pourquoi cette combinaison ?**
- `TForme` : hiérarchie claire, code commun
- Interfaces : capacités optionnelles (toutes les formes ne sont pas redimensionnables)

---

### Exemple 2 : Système de base de données

**Interface pour l'abstraction :**

```pascal
type
  IConnexionBD = interface
    ['{...}']
    procedure Connecter;
    procedure Deconnecter;
    function ExecuterRequete(const SQL: string): Boolean;
    function EstConnecte: Boolean;
  end;

  // Différentes implémentations
  TConnexionMySQL = class(TInterfacedObject, IConnexionBD)
    // Implémentation spécifique MySQL
  end;

  TConnexionPostgreSQL = class(TInterfacedObject, IConnexionBD)
    // Implémentation spécifique PostgreSQL
  end;

  TConnexionSQLite = class(TInterfacedObject, IConnexionBD)
    // Implémentation spécifique SQLite
  end;
```

**Pourquoi une interface ?**
- Les trois bases de données sont **très différentes** en interne
- Pas de code commun à partager
- On veut pouvoir changer facilement de base de données
- Couplage faible = testabilité

---

## Erreurs courantes

### Erreur 1 : Utiliser une interface quand une classe abstraite serait mieux

**❌ Mauvais choix :**

```pascal
type
  IAnimal = interface
    ['{...}']
    procedure SePresenter;
    procedure Manger;
    procedure Dormir;
  end;

  TChien = class(TInterfacedObject, IAnimal)
  private
    FNom: string;  // ⚠️ Code dupliqué dans chaque animal
  public
    procedure SePresenter;  // ⚠️ Code identique partout
    procedure Manger;
    procedure Dormir;
  end;

  TChat = class(TInterfacedObject, IAnimal)
  private
    FNom: string;  // ⚠️ Duplication !
  public
    procedure SePresenter;  // ⚠️ Duplication !
    procedure Manger;
    procedure Dormir;
  end;
```

**✅ Meilleur choix :**

```pascal
type
  TAnimal = class abstract
  private
    FNom: string;  // ✅ Une seule fois
  public
    constructor Create(const Nom: string);
    procedure SePresenter;  // ✅ Code partagé
    procedure Manger; virtual; abstract;
    procedure Dormir; virtual; abstract;
  end;

  TChien = class(TAnimal)
  public
    procedure Manger; override;
    procedure Dormir; override;
  end;

  TChat = class(TAnimal)
  public
    procedure Manger; override;
    procedure Dormir; override;
  end;
```

---

### Erreur 2 : Créer des interfaces trop grosses

**❌ Interface "fourre-tout" :**

```pascal
type
  IDocument = interface
    ['{...}']
    procedure Ouvrir;
    procedure Fermer;
    procedure Sauvegarder;
    procedure Imprimer;
    procedure EnvoyerParEmail;
    procedure CompresserEnZip;
    procedure ChiffrerAES;
    procedure VerifierSignature;
    // ... 20 autres méthodes
  end;
```

**✅ Interfaces séparées :**

```pascal
type
  IDocument = interface
    procedure Ouvrir;
    procedure Fermer;
    procedure Sauvegarder;
  end;

  IImprimable = interface
    procedure Imprimer;
  end;

  IEnvoyable = interface
    procedure EnvoyerParEmail;
  end;

  ISecurise = interface
    procedure Chiffrer;
    procedure Dechiffrer;
  end;
```

---

## Résumé

### Classe abstraite
**Utilisez pour :**
- ✅ Hiérarchie d'objets similaires
- ✅ Partager du code et des attributs
- ✅ Relation "EST-UN" forte
- ✅ Constructeur commun nécessaire

**Caractéristiques :**
- Peut avoir du code
- Peut avoir des attributs
- Héritage simple uniquement
- Gestion mémoire manuelle

### Interface
**Utilisez pour :**
- ✅ Comportements indépendants
- ✅ Héritage multiple nécessaire
- ✅ Classes très différentes
- ✅ Couplage faible / testabilité

**Caractéristiques :**
- Pas de code
- Pas d'attributs
- Héritage multiple possible
- Comptage de références automatique

### Combinaison
**Le meilleur des deux mondes :**
```pascal
TClasse = class(TClasseAbstraite, IInterface1, IInterface2)
```
- Code partagé via la classe abstraite
- Comportements optionnels via les interfaces

### Règle d'or
> **Privilégiez la composition (interfaces) à l'héritage (classes) quand c'est possible**

Mais n'hésitez pas à utiliser des classes abstraites quand vous avez vraiment du code à partager dans une hiérarchie cohérente !

---

## Prochaine étape

Dans la section suivante (12.7), vous découvrirez la **délégation et la composition**, deux techniques puissantes qui complètent l'héritage et les interfaces pour créer des architectures logicielles flexibles.

⏭️ [Delegation et composition](/12-interfaces-poo-avancee/07-delegation-composition.md)
