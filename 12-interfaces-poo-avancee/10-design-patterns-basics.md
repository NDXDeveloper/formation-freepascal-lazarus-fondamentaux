🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.10 Design patterns basics (Singleton, Factory)

## Introduction : Qu'est-ce qu'un design pattern ?

### Définition simple

Un **design pattern** (patron de conception) est une **solution réutilisable** à un problème courant en programmation. C'est comme une recette de cuisine : une méthode éprouvée pour résoudre un type de problème spécifique.

**Analogie : Les plans d'architecte**
- Un architecte ne réinvente pas la roue pour chaque maison
- Il utilise des plans standards : cuisine, chambre, salle de bain
- Chaque plan résout un problème spécifique (où manger, où dormir)
- Les design patterns sont les "plans standards" de la programmation

### Pourquoi utiliser des design patterns ?

✅ **Solutions éprouvées** : Testées par des milliers de développeurs  
✅ **Vocabulaire commun** : Facilite la communication entre développeurs  
✅ **Code maintenable** : Structure claire et logique  
✅ **Évite les erreurs** : Prévient les problèmes connus  
✅ **Gagne du temps** : Pas besoin de réinventer la solution  

### Les patterns que vous allez apprendre

Dans cette section, nous couvrons deux patterns essentiels :

1. **Singleton** : Garantir qu'il n'existe qu'une seule instance d'une classe
2. **Factory** : Créer des objets sans exposer la logique de création

---

## Pattern 1 : Singleton

### Le problème

Certaines classes ne doivent avoir qu'**une seule instance** dans toute l'application :
- Configuration de l'application
- Gestionnaire de base de données
- Logger (système de journalisation)
- Gestionnaire de cache

**Exemple du problème :**

```pascal
// ❌ Sans Singleton : plusieurs instances peuvent être créées
var
  Config1, Config2: TConfiguration;
begin
  Config1 := TConfiguration.Create;
  Config2 := TConfiguration.Create;

  Config1.Langue := 'Français';
  Config2.Langue := 'Anglais';

  // ⚠️ PROBLÈME : Deux configurations différentes !
  // Laquelle est la "vraie" configuration ?
end;
```

### La solution : Le pattern Singleton

Le Singleton garantit :
- ✅ Une seule instance existe
- ✅ Point d'accès global à cette instance
- ✅ Instance créée seulement quand nécessaire (lazy initialization)

### Implémentation du Singleton

```pascal
type
  TConfiguration = class
  private
    // Instance unique (attribut de classe)
    class var FInstance: TConfiguration;

    // Attributs de configuration
    FLangue: string;
    FTheme: string;
    FDossierDonnees: string;

    // Constructeur privé : empêche la création directe
    constructor CreatePrivate;
  public
    // Point d'accès unique à l'instance
    class function Instance: TConfiguration;
    class procedure LibererInstance;

    // Properties de configuration
    property Langue: string read FLangue write FLangue;
    property Theme: string read FTheme write FTheme;
    property DossierDonnees: string read FDossierDonnees write FDossierDonnees;
  end;

// Note : FInstance est automatiquement initialisé à nil (valeur par défaut)

// Constructeur privé
constructor TConfiguration.CreatePrivate;  
begin  
  inherited Create;
  // Valeurs par défaut
  FLangue := 'Français';
  FTheme := 'Clair';
  FDossierDonnees := './data';
  WriteLn('⚙️  Configuration initialisée');
end;

// Méthode pour obtenir l'instance unique
class function TConfiguration.Instance: TConfiguration;  
begin  
  // Si l'instance n'existe pas, la créer
  if FInstance = nil then
  begin
    WriteLn('🔧 Création de l''instance Singleton');
    FInstance := TConfiguration.CreatePrivate;
  end
  else
    WriteLn('♻️  Réutilisation de l''instance existante');

  Result := FInstance;
end;

// Méthode pour libérer l'instance
class procedure TConfiguration.LibererInstance;  
begin  
  if FInstance <> nil then
  begin
    WriteLn('🗑️  Libération du Singleton');
    FInstance.Free;
    FInstance := nil;
  end;
end;
```

### Utilisation du Singleton

```pascal
var
  Config1, Config2, Config3: TConfiguration;
begin
  WriteLn('=== Test du Singleton ===');
  WriteLn('');

  // Premier accès : crée l'instance
  WriteLn('▶ Premier accès');
  Config1 := TConfiguration.Instance;
  Config1.Langue := 'Français';
  Config1.Theme := 'Sombre';
  WriteLn('Config1 - Langue : ', Config1.Langue, ', Thème : ', Config1.Theme);
  WriteLn('');

  // Deuxième accès : réutilise la même instance
  WriteLn('▶ Deuxième accès');
  Config2 := TConfiguration.Instance;
  WriteLn('Config2 - Langue : ', Config2.Langue, ', Thème : ', Config2.Theme);
  WriteLn('');

  // Troisième accès : toujours la même instance
  WriteLn('▶ Troisième accès');
  Config3 := TConfiguration.Instance;
  Config3.Langue := 'Anglais';  // Modifie l'instance unique
  WriteLn('Config3 - Langue : ', Config3.Langue);
  WriteLn('');

  // Vérification : tous pointent vers le même objet
  WriteLn('▶ Vérification');
  WriteLn('Config1 = Config2 ? ', Config1 = Config2);
  WriteLn('Config2 = Config3 ? ', Config2 = Config3);
  WriteLn('Config1.Langue (après modification par Config3) : ', Config1.Langue);
  WriteLn('');

  // Libération (une seule fois pour tout le monde)
  TConfiguration.LibererInstance;
end.
```

**Résultat :**
```
=== Test du Singleton ===

▶ Premier accès
🔧 Création de l'instance Singleton
⚙️  Configuration initialisée
Config1 - Langue : Français, Thème : Sombre

▶ Deuxième accès
♻️  Réutilisation de l'instance existante
Config2 - Langue : Français, Thème : Sombre

▶ Troisième accès
♻️  Réutilisation de l'instance existante
Config3 - Langue : Anglais

▶ Vérification
Config1 = Config2 ? TRUE  
Config2 = Config3 ? TRUE  
Config1.Langue (après modification par Config3) : Anglais  

🗑️  Libération du Singleton
```

### Quand utiliser le Singleton ?

#### ✅ Utilisez le Singleton pour :

1. **Configuration globale** : Paramètres de l'application
2. **Logger** : Système de journalisation unique
3. **Gestionnaire de connexion** : Pool de connexions BD
4. **Cache** : Mémoire cache partagée
5. **Gestionnaire de ressources** : Fichiers, images, sons

#### ❌ N'utilisez PAS le Singleton pour :

1. Classes qui peuvent avoir plusieurs instances légitimement
2. Données qui doivent être isolées (tests unitaires)
3. Tout ce qui pourrait évoluer vers plusieurs instances

### Exemple pratique : Logger Singleton

```pascal
type
  TNiveauLog = (nlDebug, nlInfo, nlWarning, nlError);

  TLogger = class
  private
    class var FInstance: TLogger;
    FFichier: TextFile;
    FFichierOuvert: Boolean;

    constructor CreatePrivate;
  public
    destructor Destroy; override;

    class function Instance: TLogger;
    class procedure LibererInstance;

    procedure Log(Niveau: TNiveauLog; const Message: string);
    procedure Debug(const Message: string);
    procedure Info(const Message: string);
    procedure Warning(const Message: string);
    procedure Error(const Message: string);
  end;

// Note : FInstance est automatiquement initialisé à nil

constructor TLogger.CreatePrivate;  
begin  
  inherited Create;
  AssignFile(FFichier, 'application.log');
  try
    Rewrite(FFichier);
    FFichierOuvert := True;
    WriteLn('📝 Logger initialisé');
  except
    FFichierOuvert := False;
    WriteLn('⚠️  Impossible d''ouvrir le fichier de log');
  end;
end;

destructor TLogger.Destroy;  
begin  
  if FFichierOuvert then
  begin
    CloseFile(FFichier);
    WriteLn('📝 Logger fermé');
  end;
  inherited;
end;

class function TLogger.Instance: TLogger;  
begin  
  if FInstance = nil then
    FInstance := TLogger.CreatePrivate;
  Result := FInstance;
end;

class procedure TLogger.LibererInstance;  
begin  
  if FInstance <> nil then
  begin
    FInstance.Free;
    FInstance := nil;
  end;
end;

procedure TLogger.Log(Niveau: TNiveauLog; const Message: string);  
const  
  NiveauTexte: array[TNiveauLog] of string = ('DEBUG', 'INFO', 'WARNING', 'ERROR');
var
  Ligne: string;
begin
  Ligne := Format('[%s] [%s] %s', [DateTimeToStr(Now), NiveauTexte[Niveau], Message]);
  WriteLn(Ligne);

  if FFichierOuvert then
    WriteLn(FFichier, Ligne);
end;

procedure TLogger.Debug(const Message: string);  
begin  
  Log(nlDebug, Message);
end;

procedure TLogger.Info(const Message: string);  
begin  
  Log(nlInfo, Message);
end;

procedure TLogger.Warning(const Message: string);  
begin  
  Log(nlWarning, Message);
end;

procedure TLogger.Error(const Message: string);  
begin  
  Log(nlError, Message);
end;
```

**Utilisation du Logger :**

```pascal
begin
  // Partout dans l'application, on peut logger facilement
  TLogger.Instance.Info('Application démarrée');
  TLogger.Instance.Debug('Chargement de la configuration');
  TLogger.Instance.Warning('Mémoire cache limitée');
  TLogger.Instance.Error('Impossible de se connecter à la base');

  // Libération à la fin de l'application
  TLogger.LibererInstance;
end.
```

---

## Pattern 2 : Factory (Fabrique)

### Le problème

Créer des objets peut devenir complexe :
- Logique de création compliquée
- Besoin de choisir quelle classe instancier
- Dépendance forte au constructeur

**Exemple du problème :**

```pascal
// ❌ Sans Factory : logique de création éparpillée
var
  Transport: TTransport;
begin
  if TypeTransport = 'voiture' then
    Transport := TVoiture.Create(4, 'Essence')
  else if TypeTransport = 'moto' then
    Transport := TMoto.Create(2, 'Essence')
  else if TypeTransport = 'velo' then
    Transport := TVelo.Create(2, 'Humaine');

  // ⚠️ Code de création répété partout dans l'application
end;
```

### La solution : Le pattern Factory

Le Factory :
- ✅ Centralise la logique de création
- ✅ Découple le code client de la classe concrète
- ✅ Facilite l'ajout de nouveaux types
- ✅ Code plus maintenable

### Implémentation du Factory

```pascal
type
  // Interface commune pour tous les transports
  ITransport = interface
    ['{11111111-2222-3333-4444-555555555555}']
    procedure Demarrer;
    procedure Avancer;
    procedure Arreter;
    function ObtenirType: string;
  end;

  // Implémentations concrètes
  TVoiture = class(TInterfacedObject, ITransport)
  public
    procedure Demarrer;
    procedure Avancer;
    procedure Arreter;
    function ObtenirType: string;
  end;

  TMoto = class(TInterfacedObject, ITransport)
  public
    procedure Demarrer;
    procedure Avancer;
    procedure Arreter;
    function ObtenirType: string;
  end;

  TVelo = class(TInterfacedObject, ITransport)
  public
    procedure Demarrer;
    procedure Avancer;
    procedure Arreter;
    function ObtenirType: string;
  end;

  // ✅ FACTORY : Classe qui crée les transports
  TTransportFactory = class
  public
    class function CreerTransport(const TypeTransport: string): ITransport;
  end;

// Implémentations
procedure TVoiture.Demarrer;  
begin  
  WriteLn('🚗 Voiture : Démarrage du moteur');
end;

procedure TVoiture.Avancer;  
begin  
  WriteLn('🚗 Voiture : Vroum ! La voiture roule');
end;

procedure TVoiture.Arreter;  
begin  
  WriteLn('🚗 Voiture : Arrêt en douceur');
end;

function TVoiture.ObtenirType: string;  
begin  
  Result := 'Voiture';
end;

procedure TMoto.Demarrer;  
begin  
  WriteLn('🏍️  Moto : Kick ! Le moteur démarre');
end;

procedure TMoto.Avancer;  
begin  
  WriteLn('🏍️  Moto : Vrouuum ! La moto file');
end;

procedure TMoto.Arreter;  
begin  
  WriteLn('🏍️  Moto : Freinage');
end;

function TMoto.ObtenirType: string;  
begin  
  Result := 'Moto';
end;

procedure TVelo.Demarrer;  
begin  
  WriteLn('🚴 Vélo : On enfourche le vélo');
end;

procedure TVelo.Avancer;  
begin  
  WriteLn('🚴 Vélo : Pédalage en cours');
end;

procedure TVelo.Arreter;  
begin  
  WriteLn('🚴 Vélo : On pose le pied');
end;

function TVelo.ObtenirType: string;  
begin  
  Result := 'Vélo';
end;

// ✅ Implémentation de la Factory
class function TTransportFactory.CreerTransport(const TypeTransport: string): ITransport;  
begin  
  WriteLn('🏭 Factory : Création d''un transport de type "', TypeTransport, '"');

  // Logique de création centralisée
  if TypeTransport = 'voiture' then
    Result := TVoiture.Create
  else if TypeTransport = 'moto' then
    Result := TMoto.Create
  else if TypeTransport = 'velo' then
    Result := TVelo.Create
  else
  begin
    WriteLn('❌ Type de transport inconnu : ', TypeTransport);
    raise Exception.Create('Type de transport non supporté');
  end;
end;
```

### Utilisation du Factory

```pascal
procedure UtiliserTransport(Transport: ITransport);  
begin  
  WriteLn('═══════════════════════════════');
  WriteLn('Utilisation du transport : ', Transport.ObtenirType);
  Transport.Demarrer;
  Transport.Avancer;
  Transport.Arreter;
  WriteLn('═══════════════════════════════');
  WriteLn('');
end;

var
  Transport: ITransport;
begin
  WriteLn('=== Démonstration du Pattern Factory ===');
  WriteLn('');

  // ✅ Utilisation simple : pas besoin de connaître les classes concrètes
  Transport := TTransportFactory.CreerTransport('voiture');
  UtiliserTransport(Transport);

  Transport := TTransportFactory.CreerTransport('moto');
  UtiliserTransport(Transport);

  Transport := TTransportFactory.CreerTransport('velo');
  UtiliserTransport(Transport);

  // Pas de Free nécessaire : gestion automatique via interface
end.
```

**Résultat :**
```
=== Démonstration du Pattern Factory ===

🏭 Factory : Création d'un transport de type "voiture"
═══════════════════════════════
Utilisation du transport : Voiture
🚗 Voiture : Démarrage du moteur
🚗 Voiture : Vroum ! La voiture roule
🚗 Voiture : Arrêt en douceur
═══════════════════════════════

🏭 Factory : Création d'un transport de type "moto"
═══════════════════════════════
Utilisation du transport : Moto
🏍️  Moto : Kick ! Le moteur démarre
🏍️  Moto : Vrouuum ! La moto file
🏍️  Moto : Freinage
═══════════════════════════════

🏭 Factory : Création d'un transport de type "velo"
═══════════════════════════════
Utilisation du transport : Vélo
🚴 Vélo : On enfourche le vélo
🚴 Vélo : Pédalage en cours
🚴 Vélo : On pose le pied
═══════════════════════════════
```

### Quand utiliser le Factory ?

#### ✅ Utilisez le Factory pour :

1. **Création conditionnelle** : Choisir quelle classe instancier selon un critère
2. **Logique complexe** : La création nécessite plusieurs étapes
3. **Découplage** : Le code client ne doit pas connaître les classes concrètes
4. **Plusieurs variantes** : De nombreuses classes implémentent la même interface
5. **Configuration** : Créer des objets depuis des fichiers de config

#### ❌ N'utilisez PAS le Factory pour :

1. Créations simples sans logique
2. Une seule classe à instancier
3. Quand la complexité ajoutée n'apporte rien

---

## Factory avancé : Factory Method Pattern

### Différence avec Simple Factory

- **Simple Factory** : Une classe avec une méthode statique
- **Factory Method** : Méthode virtuelle dans une hiérarchie de classes

### Implémentation du Factory Method

```pascal
type
  // Interface du produit
  IDocument = interface
    ['{22222222-3333-4444-5555-666666666666}']
    procedure Ouvrir;
    procedure Afficher;
    procedure Fermer;
  end;

  // Produits concrets
  TDocumentTexte = class(TInterfacedObject, IDocument)
  public
    procedure Ouvrir;
    procedure Afficher;
    procedure Fermer;
  end;

  TDocumentPDF = class(TInterfacedObject, IDocument)
  public
    procedure Ouvrir;
    procedure Afficher;
    procedure Fermer;
  end;

  // ✅ Créateur abstrait avec Factory Method
  TEditeur = class abstract
  public
    // Factory Method (virtuelle)
    function CreerDocument: IDocument; virtual; abstract;

    // Méthode qui utilise le Factory Method
    procedure OuvrirEtAfficher;
  end;

  // Créateurs concrets
  TEditeurTexte = class(TEditeur)
  public
    function CreerDocument: IDocument; override;
  end;

  TEditeurPDF = class(TEditeur)
  public
    function CreerDocument: IDocument; override;
  end;

// Implémentations des documents
procedure TDocumentTexte.Ouvrir;  
begin  
  WriteLn('📄 Ouverture d''un document texte');
end;

procedure TDocumentTexte.Afficher;  
begin  
  WriteLn('📖 Affichage du contenu texte');
end;

procedure TDocumentTexte.Fermer;  
begin  
  WriteLn('📄 Fermeture du document texte');
end;

procedure TDocumentPDF.Ouvrir;  
begin  
  WriteLn('📕 Ouverture d''un document PDF');
end;

procedure TDocumentPDF.Afficher;  
begin  
  WriteLn('📚 Affichage du PDF avec mise en page');
end;

procedure TDocumentPDF.Fermer;  
begin  
  WriteLn('📕 Fermeture du document PDF');
end;

// Implémentation du créateur abstrait
procedure TEditeur.OuvrirEtAfficher;  
var  
  Doc: IDocument;
begin
  WriteLn('═══════════════════════════════');
  Doc := CreerDocument;  // ← Appel du Factory Method
  Doc.Ouvrir;
  Doc.Afficher;
  Doc.Fermer;
  WriteLn('═══════════════════════════════');
  WriteLn('');
end;

// Créateurs concrets
function TEditeurTexte.CreerDocument: IDocument;  
begin  
  WriteLn('🏭 Création d''un document texte');
  Result := TDocumentTexte.Create;
end;

function TEditeurPDF.CreerDocument: IDocument;  
begin  
  WriteLn('🏭 Création d''un document PDF');
  Result := TDocumentPDF.Create;
end;
```

**Utilisation :**

```pascal
var
  Editeur: TEditeur;
begin
  WriteLn('=== Factory Method Pattern ===');
  WriteLn('');

  // Éditeur texte
  Editeur := TEditeurTexte.Create;
  Editeur.OuvrirEtAfficher;
  Editeur.Free;

  // Éditeur PDF
  Editeur := TEditeurPDF.Create;
  Editeur.OuvrirEtAfficher;
  Editeur.Free;
end.
```

**Résultat :**
```
=== Factory Method Pattern ===

═══════════════════════════════
🏭 Création d'un document texte
📄 Ouverture d'un document texte
📖 Affichage du contenu texte
📄 Fermeture du document texte
═══════════════════════════════

═══════════════════════════════
🏭 Création d'un document PDF
📕 Ouverture d'un document PDF
📚 Affichage du PDF avec mise en page
📕 Fermeture du document PDF
═══════════════════════════════
```

---

## Exemple complet : Application avec Singleton et Factory

### Système de gestion d'utilisateurs

```pascal
type
  // Niveaux d'utilisateur
  TNiveauUtilisateur = (nuInvite, nuUtilisateur, nuAdmin);

  // Interface utilisateur
  IUtilisateur = interface
    ['{33333333-4444-5555-6666-777777777777}']
    function ObtenirNom: string;
    function ObtenirNiveau: TNiveauUtilisateur;
    procedure SeConnecter;
    procedure SeDeconnecter;
  end;

  // Implémentations concrètes
  TUtilisateurInvite = class(TInterfacedObject, IUtilisateur)
  private
    FNom: string;
  public
    constructor Create(const Nom: string);
    function ObtenirNom: string;
    function ObtenirNiveau: TNiveauUtilisateur;
    procedure SeConnecter;
    procedure SeDeconnecter;
  end;

  TUtilisateurStandard = class(TInterfacedObject, IUtilisateur)
  private
    FNom: string;
  public
    constructor Create(const Nom: string);
    function ObtenirNom: string;
    function ObtenirNiveau: TNiveauUtilisateur;
    procedure SeConnecter;
    procedure SeDeconnecter;
  end;

  TUtilisateurAdmin = class(TInterfacedObject, IUtilisateur)
  private
    FNom: string;
  public
    constructor Create(const Nom: string);
    function ObtenirNom: string;
    function ObtenirNiveau: TNiveauUtilisateur;
    procedure SeConnecter;
    procedure SeDeconnecter;
  end;

  // ✅ SINGLETON : Gestionnaire de session
  TGestionnaireSession = class
  private
    class var FInstance: TGestionnaireSession;
    FUtilisateurCourant: IUtilisateur;

    constructor CreatePrivate;
  public
    class function Instance: TGestionnaireSession;
    class procedure LibererInstance;

    procedure ConnecterUtilisateur(Utilisateur: IUtilisateur);
    procedure DeconnecterUtilisateur;
    function ObtenirUtilisateurCourant: IUtilisateur;
    function EstConnecte: Boolean;
  end;

  // ✅ FACTORY : Fabrique d'utilisateurs
  TUtilisateurFactory = class
  public
    class function CreerUtilisateur(Niveau: TNiveauUtilisateur; const Nom: string): IUtilisateur;
  end;

// Implémentations des utilisateurs
constructor TUtilisateurInvite.Create(const Nom: string);  
begin  
  inherited Create;
  FNom := Nom;
end;

function TUtilisateurInvite.ObtenirNom: string;  
begin  
  Result := FNom;
end;

function TUtilisateurInvite.ObtenirNiveau: TNiveauUtilisateur;  
begin  
  Result := nuInvite;
end;

procedure TUtilisateurInvite.SeConnecter;  
begin  
  WriteLn('👤 Invité "', FNom, '" connecté (accès limité)');
end;

procedure TUtilisateurInvite.SeDeconnecter;  
begin  
  WriteLn('👋 Invité "', FNom, '" déconnecté');
end;

constructor TUtilisateurStandard.Create(const Nom: string);  
begin  
  inherited Create;
  FNom := Nom;
end;

function TUtilisateurStandard.ObtenirNom: string;  
begin  
  Result := FNom;
end;

function TUtilisateurStandard.ObtenirNiveau: TNiveauUtilisateur;  
begin  
  Result := nuUtilisateur;
end;

procedure TUtilisateurStandard.SeConnecter;  
begin  
  WriteLn('👤 Utilisateur "', FNom, '" connecté (accès standard)');
end;

procedure TUtilisateurStandard.SeDeconnecter;  
begin  
  WriteLn('👋 Utilisateur "', FNom, '" déconnecté');
end;

constructor TUtilisateurAdmin.Create(const Nom: string);  
begin  
  inherited Create;
  FNom := Nom;
end;

function TUtilisateurAdmin.ObtenirNom: string;  
begin  
  Result := FNom;
end;

function TUtilisateurAdmin.ObtenirNiveau: TNiveauUtilisateur;  
begin  
  Result := nuAdmin;
end;

procedure TUtilisateurAdmin.SeConnecter;  
begin  
  WriteLn('👑 Administrateur "', FNom, '" connecté (accès total)');
end;

procedure TUtilisateurAdmin.SeDeconnecter;  
begin  
  WriteLn('👋 Administrateur "', FNom, '" déconnecté');
end;

// Implémentation du Singleton
// Note : FInstance est automatiquement initialisé à nil

constructor TGestionnaireSession.CreatePrivate;  
begin  
  inherited Create;
  FUtilisateurCourant := nil;
  WriteLn('🔧 Gestionnaire de session initialisé');
end;

class function TGestionnaireSession.Instance: TGestionnaireSession;  
begin  
  if FInstance = nil then
    FInstance := TGestionnaireSession.CreatePrivate;
  Result := FInstance;
end;

class procedure TGestionnaireSession.LibererInstance;  
begin  
  if FInstance <> nil then
  begin
    FInstance.Free;
    FInstance := nil;
  end;
end;

procedure TGestionnaireSession.ConnecterUtilisateur(Utilisateur: IUtilisateur);  
begin  
  if FUtilisateurCourant <> nil then
    FUtilisateurCourant.SeDeconnecter;

  FUtilisateurCourant := Utilisateur;
  FUtilisateurCourant.SeConnecter;
end;

procedure TGestionnaireSession.DeconnecterUtilisateur;  
begin  
  if FUtilisateurCourant <> nil then
  begin
    FUtilisateurCourant.SeDeconnecter;
    FUtilisateurCourant := nil;
  end
  else
    WriteLn('⚠️  Aucun utilisateur connecté');
end;

function TGestionnaireSession.ObtenirUtilisateurCourant: IUtilisateur;  
begin  
  Result := FUtilisateurCourant;
end;

function TGestionnaireSession.EstConnecte: Boolean;  
begin  
  Result := FUtilisateurCourant <> nil;
end;

// Implémentation du Factory
class function TUtilisateurFactory.CreerUtilisateur(Niveau: TNiveauUtilisateur; const Nom: string): IUtilisateur;  
begin  
  WriteLn('🏭 Création d''un utilisateur : ', Nom);

  case Niveau of
    nuInvite: Result := TUtilisateurInvite.Create(Nom);
    nuUtilisateur: Result := TUtilisateurStandard.Create(Nom);
    nuAdmin: Result := TUtilisateurAdmin.Create(Nom);
  else
    raise Exception.Create('Niveau d''utilisateur inconnu');
  end;
end;
```

**Utilisation complète :**

```pascal
var
  Session: TGestionnaireSession;
  User: IUtilisateur;
begin
  WriteLn('=== Système de Gestion d''Utilisateurs ===');
  WriteLn('');

  // Singleton : une seule session pour toute l'application
  Session := TGestionnaireSession.Instance;
  WriteLn('');

  // Factory : création d'utilisateurs
  WriteLn('▶ Connexion invité');
  User := TUtilisateurFactory.CreerUtilisateur(nuInvite, 'Bob');
  Session.ConnecterUtilisateur(User);
  WriteLn('Connecté ? ', Session.EstConnecte);
  WriteLn('');

  // Changement d'utilisateur
  WriteLn('▶ Connexion utilisateur standard');
  User := TUtilisateurFactory.CreerUtilisateur(nuUtilisateur, 'Alice');
  Session.ConnecterUtilisateur(User);
  WriteLn('');

  // Affichage de l'utilisateur courant
  WriteLn('▶ Utilisateur courant');
  if Session.EstConnecte then
  begin
    User := Session.ObtenirUtilisateurCourant;
    WriteLn('Nom : ', User.ObtenirNom);
  end;
  WriteLn('');

  // Connexion admin
  WriteLn('▶ Connexion administrateur');
  User := TUtilisateurFactory.CreerUtilisateur(nuAdmin, 'SuperAdmin');
  Session.ConnecterUtilisateur(User);
  WriteLn('');

  // Déconnexion
  WriteLn('▶ Déconnexion');
  Session.DeconnecterUtilisateur;
  WriteLn('Connecté ? ', Session.EstConnecte);
  WriteLn('');

  // Libération du singleton
  TGestionnaireSession.LibererInstance;
end.
```

---

## Avantages et inconvénients

### Singleton

**Avantages :**  
✅ Instance unique garantie  
✅ Point d'accès global  
✅ Initialisation paresseuse (lazy)  
✅ Économie de mémoire  

**Inconvénients :**  
⚠️ État global (difficile à tester)  
⚠️ Couplage fort  
⚠️ Peut cacher des dépendances  
⚠️ Problèmes en multi-threading (sans précautions)  

### Factory

**Avantages :**  
✅ Découplage (code client vs classes concrètes)  
✅ Logique de création centralisée  
✅ Facilite l'ajout de nouveaux types  
✅ Code plus maintenable  

**Inconvénients :**  
⚠️ Complexité ajoutée  
⚠️ Plus de classes à gérer  
⚠️ Peut être "over-engineering" pour des cas simples  

---

## Résumé

### Pattern Singleton
**But :** Garantir qu'une classe n'a qu'une seule instance

**Structure :**
- Constructeur privé
- Attribut de classe statique
- Méthode `Instance` pour obtenir l'instance unique

**Quand l'utiliser :**
- Configuration
- Logger
- Gestionnaire de ressources partagées

### Pattern Factory
**But :** Créer des objets sans exposer la logique de création

**Structure :**
- Interface commune pour les produits
- Méthode de création centralisée
- Retourne l'interface, pas la classe concrète

**Quand l'utiliser :**
- Création conditionnelle
- Logique complexe
- Découplage nécessaire

### Points clés

**Singleton :**
```pascal
class function Instance: TClasse;  
begin  
  if FInstance = nil then
    FInstance := TClasse.CreatePrivate;
  Result := FInstance;
end;
```

**Factory :**
```pascal
class function Creer(Type: string): IInterface;  
begin  
  case Type of
    'A': Result := TClasseA.Create;
    'B': Result := TClasseB.Create;
  end;
end;
```

### Règles d'or
- ✅ Utilisez ces patterns quand ils simplifient vraiment le code
- ❌ N'en abusez pas : la simplicité prime
- ✅ Documentez pourquoi vous utilisez un pattern
- ✅ Testez votre code (les patterns doivent faciliter les tests)

---

## Conclusion du chapitre 12

Félicitations ! Vous avez terminé le chapitre sur les interfaces et la POO avancée. Vous maîtrisez maintenant :

✅ **Interfaces** : Contrats et polymorphisme  
✅ **Héritage multiple** : Via les interfaces  
✅ **IInterface** : Comptage de références automatique  
✅ **Properties** : Getters/setters élégants  
✅ **Méthodes de classe** : Fonctionnalités sans instance  
✅ **Design patterns** : Solutions éprouvées (Singleton, Factory)  

Ces concepts vous permettent de créer des architectures logicielles **solides, maintenables et évolutives**. Continuez à pratiquer et à explorer d'autres patterns pour devenir un développeur Pascal accompli !

⏭️ [Gestion des Exceptions](/13-gestion-exceptions/README.md)
