🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.7 Délégation et composition

## Introduction : Une alternative à l'héritage

Vous avez appris l'héritage, les classes abstraites et les interfaces. Mais il existe une autre façon puissante de réutiliser du code : la **composition** et la **délégation**.

**Principe fondamental :**
> "Privilégiez la composition à l'héritage" (*Favor composition over inheritance*)

**Analogie simple :**
- **Héritage** : "Je suis comme mon parent" → Un chien **EST-UN** animal
- **Composition** : "J'ai un outil qui fait le travail" → Une voiture **A-UN** moteur

---

## Qu'est-ce que la composition ?

### Définition

La **composition** consiste à construire des objets complexes en **assemblant** d'autres objets plus simples, plutôt que d'hériter d'eux.

**Analogie : Construire une maison**
- **Héritage** : Une villa hérite de Maison, qui hérite de Bâtiment
- **Composition** : Une maison **contient** une cuisine, des chambres, une salle de bain

### Exemple visuel

```
┌─────────────────────────┐
│      Voiture            │
│                         │
│  ┌─────────┐            │
│  │ Moteur  │            │  ← La voiture CONTIENT un moteur
│  └─────────┘            │
│                         │
│  ┌─────────┐            │
│  │  Roues  │            │  ← La voiture CONTIENT des roues
│  └─────────┘            │
│                         │
└─────────────────────────┘
```

### Code de base

```pascal
type
  // Composant : Moteur
  TMoteur = class
  private
    FPuissance: Integer;
    FDemarre: Boolean;
  public
    constructor Create(Puissance: Integer);
    procedure Demarrer;
    procedure Arreter;
    function EstDemarre: Boolean;
  end;

  // Composant : Roues
  TRoues = class
  private
    FNombre: Integer;
  public
    constructor Create(Nombre: Integer);
    procedure Tourner;
  end;

  // Classe composite : Voiture
  TVoiture = class
  private
    FMoteur: TMoteur;  // ← COMPOSITION : la voiture A-UN moteur
    FRoues: TRoues;    // ← COMPOSITION : la voiture A-DES roues
  public
    constructor Create;
    destructor Destroy; override;

    procedure Demarrer;
    procedure Rouler;
    procedure Arreter;
  end;

// Implémentation du Moteur
constructor TMoteur.Create(Puissance: Integer);  
begin  
  FPuissance := Puissance;
  FDemarre := False;
end;

procedure TMoteur.Demarrer;  
begin  
  FDemarre := True;
  WriteLn('🔧 Moteur démarré (', FPuissance, ' ch)');
end;

procedure TMoteur.Arreter;  
begin  
  FDemarre := False;
  WriteLn('🔧 Moteur arrêté');
end;

function TMoteur.EstDemarre: Boolean;  
begin  
  Result := FDemarre;
end;

// Implémentation des Roues
constructor TRoues.Create(Nombre: Integer);  
begin  
  FNombre := Nombre;
end;

procedure TRoues.Tourner;  
begin  
  WriteLn('🛞 Les ', FNombre, ' roues tournent');
end;

// Implémentation de la Voiture
constructor TVoiture.Create;  
begin  
  // Création des composants
  FMoteur := TMoteur.Create(150);
  FRoues := TRoues.Create(4);
  WriteLn('🚗 Voiture assemblée');
end;

destructor TVoiture.Destroy;  
begin  
  // Important : libérer les composants
  FMoteur.Free;
  FRoues.Free;
  WriteLn('🚗 Voiture désassemblée');
  inherited;
end;

procedure TVoiture.Demarrer;  
begin  
  WriteLn('▶ Démarrage de la voiture...');
  FMoteur.Demarrer;
end;

procedure TVoiture.Rouler;  
begin  
  if FMoteur.EstDemarre then
  begin
    WriteLn('▶ La voiture roule...');
    FRoues.Tourner;
  end
  else
    WriteLn('⚠️  Impossible de rouler : moteur arrêté');
end;

procedure TVoiture.Arreter;  
begin  
  WriteLn('▶ Arrêt de la voiture...');
  FMoteur.Arreter;
end;
```

**Utilisation :**

```pascal
var
  MaVoiture: TVoiture;
begin
  MaVoiture := TVoiture.Create;

  MaVoiture.Demarrer;
  MaVoiture.Rouler;
  MaVoiture.Arreter;

  MaVoiture.Free;
end.
```

**Résultat :**
```
🚗 Voiture assemblée
▶ Démarrage de la voiture...
🔧 Moteur démarré (150 ch)
▶ La voiture roule...
🛞 Les 4 roues tournent
▶ Arrêt de la voiture...
🔧 Moteur arrêté
🚗 Voiture désassemblée
```

---

## Qu'est-ce que la délégation ?

### Définition

La **délégation** consiste à **confier** le travail à un autre objet plutôt que de le faire soi-même.

**Analogie : Le patron et son assistant**
- Le patron reçoit une demande
- Il ne fait pas le travail lui-même
- Il **délègue** à son assistant
- L'assistant fait le travail
- Le patron retourne le résultat

### Exemple simple

```pascal
type
  // L'assistant qui fait le travail
  TAssistant = class
  public
    function RedigerRapport(const Sujet: string): string;
  end;

  // Le patron qui délègue
  TPatron = class
  private
    FAssistant: TAssistant;
  public
    constructor Create;
    destructor Destroy; override;

    // Méthode qui délègue à l'assistant
    function DemanderRapport(const Sujet: string): string;
  end;

function TAssistant.RedigerRapport(const Sujet: string): string;  
begin  
  WriteLn('📝 Assistant : Je rédige le rapport sur "', Sujet, '"');
  Result := 'Rapport complet sur ' + Sujet;
end;

constructor TPatron.Create;  
begin  
  FAssistant := TAssistant.Create;
  WriteLn('👔 Patron créé avec son assistant');
end;

destructor TPatron.Destroy;  
begin  
  FAssistant.Free;
  inherited;
end;

function TPatron.DemanderRapport(const Sujet: string): string;  
begin  
  WriteLn('👔 Patron : Je demande un rapport à mon assistant');
  Result := FAssistant.RedigerRapport(Sujet);  // ← DÉLÉGATION
  WriteLn('👔 Patron : Merci, je transmets le rapport');
end;
```

**Utilisation :**

```pascal
var
  Patron: TPatron;
  Rapport: string;
begin
  Patron := TPatron.Create;

  Rapport := Patron.DemanderRapport('Ventes Q4');
  WriteLn('📄 Résultat : ', Rapport);

  Patron.Free;
end.
```

**Résultat :**
```
👔 Patron créé avec son assistant
👔 Patron : Je demande un rapport à mon assistant
📝 Assistant : Je rédige le rapport sur "Ventes Q4"
👔 Patron : Merci, je transmets le rapport
📄 Résultat : Rapport complet sur Ventes Q4
```

---

## Composition vs Héritage

### Comparaison avec un exemple

**Scénario :** Créer différents types de robots avec différentes capacités.

#### Approche 1 : Héritage (problématique)

```pascal
type
  TRobot = class
  public
    procedure Marcher; virtual;
  end;

  TRobotVolant = class(TRobot)
  public
    procedure Voler; virtual;
  end;

  TRobotAmphibie = class(TRobot)
  public
    procedure Nager; virtual;
  end;

  // ❌ PROBLÈME : Comment créer un robot qui vole ET nage ?
  // On ne peut pas hériter de TRobotVolant ET TRobotAmphibie !

  TRobotSuperHeros = class(TRobotVolant)  // Choisir un seul parent
  public
    // Comment ajouter la capacité de nager ? 🤔
    // Il faudrait dupliquer le code !
  end;
```

#### Approche 2 : Composition (flexible)

```pascal
type
  // Composants individuels
  TModuleMarcheuse = class
  public
    procedure Marcher;
  end;

  TModuleVol = class
  public
    procedure Voler;
  end;

  TModuleNage = class
  public
    procedure Nager;
  end;

  // Robot qui COMPOSE les modules qu'il veut
  TRobot = class
  private
    FMarcheuse: TModuleMarcheuse;
    FVol: TModuleVol;
    FNage: TModuleNage;
  public
    constructor Create(AvecMarche, AvecVol, AvecNage: Boolean);
    destructor Destroy; override;

    procedure Marcher;
    procedure Voler;
    procedure Nager;
  end;

procedure TModuleMarcheuse.Marcher;  
begin  
  WriteLn('🚶 Module de marche activé');
end;

procedure TModuleVol.Voler;  
begin  
  WriteLn('🚁 Module de vol activé');
end;

procedure TModuleNage.Nager;  
begin  
  WriteLn('🏊 Module de nage activé');
end;

constructor TRobot.Create(AvecMarche, AvecVol, AvecNage: Boolean);  
begin  
  // ✅ On crée seulement les modules nécessaires
  if AvecMarche then
    FMarcheuse := TModuleMarcheuse.Create;
  if AvecVol then
    FVol := TModuleVol.Create;
  if AvecNage then
    FNage := TModuleNage.Create;
end;

destructor TRobot.Destroy;  
begin  
  if Assigned(FMarcheuse) then FMarcheuse.Free;
  if Assigned(FVol) then FVol.Free;
  if Assigned(FNage) then FNage.Free;
  inherited;
end;

procedure TRobot.Marcher;  
begin  
  if Assigned(FMarcheuse) then
    FMarcheuse.Marcher  // ← DÉLÉGATION
  else
    WriteLn('❌ Ce robot ne peut pas marcher');
end;

procedure TRobot.Voler;  
begin  
  if Assigned(FVol) then
    FVol.Voler  // ← DÉLÉGATION
  else
    WriteLn('❌ Ce robot ne peut pas voler');
end;

procedure TRobot.Nager;  
begin  
  if Assigned(FNage) then
    FNage.Nager  // ← DÉLÉGATION
  else
    WriteLn('❌ Ce robot ne peut pas nager');
end;
```

**Utilisation flexible :**

```pascal
var
  RobotTerrestre, RobotVolant, RobotAmphibie, RobotSuperHeros: TRobot;
begin
  // Robot qui marche seulement
  WriteLn('=== Robot Terrestre ===');
  RobotTerrestre := TRobot.Create(True, False, False);
  RobotTerrestre.Marcher;
  RobotTerrestre.Voler;  // Ne peut pas
  WriteLn('');

  // Robot qui vole seulement
  WriteLn('=== Robot Volant ===');
  RobotVolant := TRobot.Create(False, True, False);
  RobotVolant.Voler;
  RobotVolant.Nager;  // Ne peut pas
  WriteLn('');

  // Robot amphibie (marche + nage)
  WriteLn('=== Robot Amphibie ===');
  RobotAmphibie := TRobot.Create(True, False, True);
  RobotAmphibie.Marcher;
  RobotAmphibie.Nager;
  WriteLn('');

  // ✅ Robot super-héros (toutes les capacités !)
  WriteLn('=== Robot Super-Héros ===');
  RobotSuperHeros := TRobot.Create(True, True, True);
  RobotSuperHeros.Marcher;
  RobotSuperHeros.Voler;
  RobotSuperHeros.Nager;
  WriteLn('');

  RobotTerrestre.Free;
  RobotVolant.Free;
  RobotAmphibie.Free;
  RobotSuperHeros.Free;
end.
```

**Résultat :**
```
=== Robot Terrestre ===
🚶 Module de marche activé
❌ Ce robot ne peut pas voler

=== Robot Volant ===
🚁 Module de vol activé
❌ Ce robot ne peut pas nager

=== Robot Amphibie ===
🚶 Module de marche activé
🏊 Module de nage activé

=== Robot Super-Héros ===
🚶 Module de marche activé
🚁 Module de vol activé
🏊 Module de nage activé
```

---

## Avantages de la composition

### 1. Flexibilité maximale

```pascal
// On peut changer les composants à l'exécution
procedure ChangerMoteur(Voiture: TVoiture; NouveauMoteur: TMoteur);  
begin  
  Voiture.Moteur.Free;
  Voiture.Moteur := NouveauMoteur;
end;
```

### 2. Réutilisation facile

```pascal
type
  TVoiture = class
  private
    FMoteur: TMoteur;  // ← Même moteur pour tous les véhicules
  end;

  TMoto = class
  private
    FMoteur: TMoteur;  // ← On réutilise TMoteur sans héritage
  end;

  TBateau = class
  private
    FMoteur: TMoteur;  // ← Pareil ici
  end;
```

### 3. Pas de couplage fort

Avec l'héritage, changer la classe parent peut casser tous les descendants. Avec la composition, les composants sont indépendants.

### 4. Plus facile à tester

```pascal
// On peut tester chaque composant séparément
procedure TesterMoteur;  
var  
  Moteur: TMoteur;
begin
  Moteur := TMoteur.Create(100);
  Moteur.Demarrer;
  Assert(Moteur.EstDemarre);
  Moteur.Free;
end;
```

### 5. Respect du principe de responsabilité unique

Chaque classe a une seule responsabilité claire :
- `TMoteur` : gérer le moteur
- `TRoues` : gérer les roues
- `TVoiture` : coordonner les composants

---

## Exemple pratique : Système de logging

### Avec composition et délégation

```pascal
type
  // Composant : écrivain de fichier
  IEcrivainFichier = interface
    ['{11111111-2222-3333-4444-555555555555}']
    procedure Ecrire(const Texte: string);
  end;

  TEcrivainFichier = class(TInterfacedObject, IEcrivainFichier)
  private
    FNomFichier: string;
  public
    constructor Create(const NomFichier: string);
    procedure Ecrire(const Texte: string);
  end;

  // Composant : formateur de date
  IFormateur = interface
    ['{22222222-3333-4444-5555-666666666666}']
    function FormaterMessage(const Message: string): string;
  end;

  TFormateur = class(TInterfacedObject, IFormateur)
  public
    function FormaterMessage(const Message: string): string;
  end;

  // Logger qui COMPOSE les autres classes
  TLogger = class
  private
    FEcrivain: IEcrivainFichier;
    FFormateur: IFormateur;
  public
    constructor Create(Ecrivain: IEcrivainFichier; Formateur: IFormateur);
    procedure Log(const Message: string);
  end;

// Implémentation de l'écrivain
constructor TEcrivainFichier.Create(const NomFichier: string);  
begin  
  inherited Create;
  FNomFichier := NomFichier;
end;

procedure TEcrivainFichier.Ecrire(const Texte: string);  
begin  
  WriteLn('💾 Écriture dans ', FNomFichier, ': ', Texte);
  // En vrai, on écrirait dans un fichier
end;

// Implémentation du formateur
function TFormateur.FormaterMessage(const Message: string): string;  
begin  
  Result := '[' + DateTimeToStr(Now) + '] ' + Message;
end;

// Implémentation du Logger
constructor TLogger.Create(Ecrivain: IEcrivainFichier; Formateur: IFormateur);  
begin  
  FEcrivain := Ecrivain;
  FFormateur := Formateur;
end;

procedure TLogger.Log(const Message: string);  
var  
  MessageFormate: string;
begin
  // Délégation au formateur
  MessageFormate := FFormateur.FormaterMessage(Message);

  // Délégation à l'écrivain
  FEcrivain.Ecrire(MessageFormate);
end;
```

**Utilisation :**

```pascal
var
  Logger: TLogger;
  Ecrivain: IEcrivainFichier;
  Formateur: IFormateur;
begin
  // Création des composants
  Ecrivain := TEcrivainFichier.Create('app.log');
  Formateur := TFormateur.Create;

  // Composition
  Logger := TLogger.Create(Ecrivain, Formateur);

  // Utilisation
  Logger.Log('Application démarrée');
  Logger.Log('Connexion utilisateur');
  Logger.Log('Traitement terminé');

  Logger.Free;
end.
```

**Avantages ici :**
- ✅ Facile de changer le format (autre `IFormateur`)
- ✅ Facile de changer la destination (console, base de données...)
- ✅ Chaque classe a une responsabilité unique
- ✅ Facile à tester chaque composant

---

## Pattern : Injection de dépendances

### Concept

Au lieu de créer les dépendances à l'intérieur, on les **injecte** de l'extérieur.

**❌ Mauvais : Création interne**

```pascal
type
  TService = class
  private
    FLogger: TLogger;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TService.Create;  
begin  
  // ❌ Dépendance forte : TService crée son logger
  FLogger := TLogger.Create('service.log');
end;
```

**✅ Bon : Injection de dépendances**

```pascal
type
  TService = class
  private
    FLogger: ILogger;
  public
    // ✅ On injecte le logger de l'extérieur
    constructor Create(Logger: ILogger);
  end;

constructor TService.Create(Logger: ILogger);  
begin  
  FLogger := Logger;  // Pas de création, juste stockage
end;
```

**Avantages :**
- Flexibilité : on peut changer de logger facilement
- Testabilité : on peut injecter un mock logger pour les tests
- Pas de dépendance forte

---

## Exemple complet : Application avec composition

### Système de notifications composable

```pascal
type
  // Interface pour les envoyeurs
  IEnvoyeur = interface
    ['{AAAAAAAA-BBBB-CCCC-DDDD-EEEEEEEEEEEE}']
    procedure Envoyer(const Destinataire, Message: string);
  end;

  // Envoyeur Email
  TEnvoyeurEmail = class(TInterfacedObject, IEnvoyeur)
  public
    procedure Envoyer(const Destinataire, Message: string);
  end;

  // Envoyeur SMS
  TEnvoyeurSMS = class(TInterfacedObject, IEnvoyeur)
  public
    procedure Envoyer(const Destinataire, Message: string);
  end;

  // Gestionnaire de notifications qui COMPOSE les envoyeurs
  TGestionnaireNotifications = class
  private
    FEnvoyeurs: array of IEnvoyeur;
  public
    constructor Create;
    procedure AjouterEnvoyeur(Envoyeur: IEnvoyeur);
    procedure EnvoyerNotification(const Destinataire, Message: string);
  end;

// Implémentations
procedure TEnvoyeurEmail.Envoyer(const Destinataire, Message: string);  
begin  
  WriteLn('📧 Email envoyé à ', Destinataire);
  WriteLn('   Contenu: ', Message);
end;

procedure TEnvoyeurSMS.Envoyer(const Destinataire, Message: string);  
begin  
  WriteLn('📱 SMS envoyé à ', Destinataire);
  WriteLn('   Contenu: ', Message);
end;

constructor TGestionnaireNotifications.Create;  
begin  
  SetLength(FEnvoyeurs, 0);
end;

procedure TGestionnaireNotifications.AjouterEnvoyeur(Envoyeur: IEnvoyeur);  
begin  
  SetLength(FEnvoyeurs, Length(FEnvoyeurs) + 1);
  FEnvoyeurs[High(FEnvoyeurs)] := Envoyeur;
end;

procedure TGestionnaireNotifications.EnvoyerNotification(const Destinataire, Message: string);  
var  
  Envoyeur: IEnvoyeur;
begin
  WriteLn('📬 Envoi de notification à ', Destinataire);
  WriteLn('');

  // Délégation à tous les envoyeurs
  for Envoyeur in FEnvoyeurs do
  begin
    Envoyeur.Envoyer(Destinataire, Message);
    WriteLn('');
  end;
end;
```

**Utilisation :**

```pascal
var
  Gestionnaire: TGestionnaireNotifications;
begin
  Gestionnaire := TGestionnaireNotifications.Create;

  // Composition : on ajoute les envoyeurs qu'on veut
  Gestionnaire.AjouterEnvoyeur(TEnvoyeurEmail.Create);
  Gestionnaire.AjouterEnvoyeur(TEnvoyeurSMS.Create);

  // On peut en ajouter d'autres facilement
  // Gestionnaire.AjouterEnvoyeur(TEnvoyeurPush.Create);

  // Envoi de notification (délégué à tous les envoyeurs)
  Gestionnaire.EnvoyerNotification('Jean Dupont', 'Réunion à 14h');

  Gestionnaire.Free;
end.
```

**Résultat :**
```
📬 Envoi de notification à Jean Dupont

📧 Email envoyé à Jean Dupont
   Contenu: Réunion à 14h

📱 SMS envoyé à Jean Dupont
   Contenu: Réunion à 14h
```

**Avantages :**
- Facile d'ajouter de nouveaux types d'envoyeurs
- Facile d'activer/désactiver certains envoyeurs
- Chaque envoyeur est indépendant
- Code modulaire et testable

---

## Quand utiliser la composition ?

### ✅ Utilisez la composition quand :

1. **Relation "A-UN"** plutôt que "EST-UN"
   - Une voiture **A-UN** moteur (pas EST-UN moteur)

2. **Besoin de flexibilité**
   - Changer les composants à l'exécution
   - Combiner différentes fonctionnalités

3. **Réutilisation sans héritage**
   - Utiliser le même composant dans des classes non liées

4. **Éviter les hiérarchies complexes**
   - L'arbre d'héritage devient trop profond
   - Trop de classes intermédiaires

5. **Respect du principe de responsabilité unique**
   - Chaque composant fait une seule chose

### ❌ N'utilisez PAS la composition quand :

1. **Relation "EST-UN" évidente**
   - Un chien EST vraiment un animal → héritage

2. **Pas de besoin de flexibilité**
   - La structure est fixe et ne changera pas

3. **Complexité inutile**
   - L'héritage est plus simple et suffit

---

## Combinaison : Héritage + Composition + Interfaces

### Le meilleur des trois mondes

```pascal
type
  // Interface pour le comportement
  IPersistable = interface
    ['{...}']
    procedure Sauvegarder;
    procedure Charger;
  end;

  // Classe de base abstraite (héritage)
  TEntite = class abstract
  private
    FID: Integer;
    FNom: string;
  public
    constructor Create(ID: Integer; const Nom: string);
    property ID: Integer read FID;
    property Nom: string read FNom;
  end;

  // Composant pour la persistence (composition)
  TGestionnairePersistence = class
  public
    procedure SauvegarderDansFichier(Entite: TEntite);
    procedure ChargerDepuisFichier(Entite: TEntite);
  end;

  // Classe finale (héritage + composition + interface)
  TUtilisateur = class(TEntite, IPersistable)
  private
    FGestionnairePersistence: TGestionnairePersistence;  // ← Composition
    FEmail: string;
  public
    constructor Create(ID: Integer; const Nom, Email: string);
    destructor Destroy; override;

    // Implémentation de IPersistable (délégation)
    procedure Sauvegarder;
    procedure Charger;

    property Email: string read FEmail write FEmail;
  end;

constructor TEntite.Create(ID: Integer; const Nom: string);  
begin  
  FID := ID;
  FNom := Nom;
end;

procedure TGestionnairePersistence.SauvegarderDansFichier(Entite: TEntite);  
begin  
  WriteLn('💾 Sauvegarde de ', Entite.Nom, ' dans la base');
end;

procedure TGestionnairePersistence.ChargerDepuisFichier(Entite: TEntite);  
begin  
  WriteLn('📂 Chargement de ', Entite.Nom, ' depuis la base');
end;

constructor TUtilisateur.Create(ID: Integer; const Nom, Email: string);  
begin  
  inherited Create(ID, Nom);
  FEmail := Email;
  FGestionnairePersistence := TGestionnairePersistence.Create;
end;

destructor TUtilisateur.Destroy;  
begin  
  FGestionnairePersistence.Free;
  inherited;
end;

procedure TUtilisateur.Sauvegarder;  
begin  
  // Délégation au gestionnaire de persistence
  FGestionnairePersistence.SauvegarderDansFichier(Self);
end;

procedure TUtilisateur.Charger;  
begin  
  // Délégation au gestionnaire de persistence
  FGestionnairePersistence.ChargerDepuisFichier(Self);
end;
```

**Cette approche combine :**
- ✅ **Héritage** : de `TEntite` pour les attributs communs
- ✅ **Composition** : contient un `TGestionnairePersistence`
- ✅ **Interface** : implémente `IPersistable`
- ✅ **Délégation** : confie le travail au gestionnaire

---

## Résumé

### Composition
**Définition :** Assembler des objets complexes à partir d'objets plus simples

**Relation :** "A-UN" (has-a)

**Quand l'utiliser :**
- ✅ Besoin de flexibilité
- ✅ Réutilisation de composants
- ✅ Éviter les hiérarchies complexes
- ✅ Responsabilité unique

### Délégation
**Définition :** Confier le travail à un autre objet

**Pattern :** Créer une méthode qui appelle une méthode d'un composant

**Avantages :**
- ✅ Découplage
- ✅ Code plus simple
- ✅ Facile à tester

### Règle d'or
> **"Privilégiez la composition à l'héritage"**

Mais utilisez l'héritage quand il y a une vraie relation "EST-UN" !

### Comparaison rapide

| Critère | Héritage | Composition |
|---------|----------|-------------|
| **Relation** | EST-UN | A-UN |
| **Flexibilité** | ⚠️ Moyenne | ✅ Élevée |
| **Réutilisation** | ⚠️ Verticale | ✅ Horizontale |
| **Couplage** | ⚠️ Fort | ✅ Faible |
| **Changement runtime** | ❌ Non | ✅ Oui |
| **Complexité** | ✅ Simple | ⚠️ Plus de code |

### Points clés
- La composition offre plus de flexibilité
- La délégation réduit le couplage
- On peut combiner héritage, composition et interfaces
- Choisissez selon le contexte et les besoins

---

## Prochaine étape

Dans la section suivante (12.8), vous découvrirez les **properties avec getters/setters**, une fonctionnalité puissante de Pascal qui permet de contrôler l'accès aux attributs tout en gardant une syntaxe élégante.

⏭️ [Properties avec getters/setters](/12-interfaces-poo-avancee/08-properties-getters-setters.md)
