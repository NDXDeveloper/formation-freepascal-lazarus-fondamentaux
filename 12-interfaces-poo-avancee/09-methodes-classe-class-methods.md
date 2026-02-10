🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.9 Méthodes de classe (class methods)

## Introduction : Méthodes sans objet

Jusqu'à présent, vous avez toujours utilisé des méthodes sur des **instances** d'objets :

```pascal
var
  Personne: TPersonne;
begin
  Personne := TPersonne.Create;  // Créer une instance
  Personne.Afficher;              // Appeler une méthode sur l'instance
  Personne.Free;
end;
```

Mais parfois, vous avez besoin d'une méthode qui **ne nécessite pas d'instance**. C'est là qu'interviennent les **méthodes de classe** (class methods).

**Analogie : La bibliothèque**
- **Méthode d'instance** : Demander à un livre spécifique son nombre de pages
- **Méthode de classe** : Demander à la bibliothèque combien de livres elle contient au total (sans regarder un livre en particulier)

---

## Qu'est-ce qu'une méthode de classe ?

### Définition

Une **méthode de classe** (class method) est une méthode qui :
- Appartient à la **classe elle-même**, pas aux instances
- Peut être appelée **sans créer d'objet**
- N'a **pas accès** aux attributs d'instance (`FNom`, `FAge`, etc.)
- Utilise le mot-clé **`class`**

### Syntaxe de base

```pascal
type
  TMaClasse = class
  public
    // Méthode normale (d'instance)
    procedure MethodeNormale;

    // Méthode de classe
    class procedure MethodeDeClasse;
  end;

implementation

procedure TMaClasse.MethodeNormale;  
begin  
  WriteLn('Méthode d''instance : nécessite un objet');
end;

class procedure TMaClasse.MethodeDeClasse;  
begin  
  WriteLn('Méthode de classe : pas besoin d''objet');
end;
```

**Utilisation :**

```pascal
var
  Objet: TMaClasse;
begin
  // Méthode de classe : appel direct sur la classe
  TMaClasse.MethodeDeClasse;  // ✅ Pas besoin de créer d'objet

  // Méthode d'instance : nécessite un objet
  Objet := TMaClasse.Create;
  Objet.MethodeNormale;       // ✅ Appel sur une instance
  Objet.Free;
end.
```

**Résultat :**
```
Méthode de classe : pas besoin d'objet  
Méthode d'instance : nécessite un objet  
```

---

## Différence : Méthode d'instance vs Méthode de classe

### Comparaison visuelle

```pascal
type
  TCompteur = class
  private
    FValeur: Integer;                    // ← Attribut d'instance
    class var FTotal: Integer;           // ← Attribut de classe (partagé)
  public
    constructor Create;

    // Méthode d'instance : travaille sur UNE instance
    procedure Incrementer;
    function ObtenirValeur: Integer;

    // Méthode de classe : travaille au niveau de la classe
    class procedure ReinitialiserTotal;
    class function ObtenirTotal: Integer;
  end;

// Note : FTotal est automatiquement initialisé à 0 (valeur par défaut)

constructor TCompteur.Create;  
begin  
  FValeur := 0;
  Inc(FTotal);  // Chaque instance créée augmente le total
end;

procedure TCompteur.Incrementer;  
begin  
  Inc(FValeur);  // ✅ Peut accéder à FValeur (attribut d'instance)
  WriteLn('Valeur de cette instance : ', FValeur);
end;

function TCompteur.ObtenirValeur: Integer;  
begin  
  Result := FValeur;
end;

class procedure TCompteur.ReinitialiserTotal;  
begin  
  FTotal := 0;  // ✅ Peut accéder à FTotal (attribut de classe)
  WriteLn('Total réinitialisé');

  // Inc(FValeur);  ❌ ERREUR : pas d'accès aux attributs d'instance
end;

class function TCompteur.ObtenirTotal: Integer;  
begin  
  Result := FTotal;
end;
```

**Utilisation :**

```pascal
var
  C1, C2, C3: TCompteur;
begin
  WriteLn('=== Création d''instances ===');
  C1 := TCompteur.Create;
  C2 := TCompteur.Create;
  C3 := TCompteur.Create;

  WriteLn('Nombre total d''instances : ', TCompteur.ObtenirTotal);  // 3
  WriteLn('');

  WriteLn('=== Incrémentation individuelle ===');
  C1.Incrementer;  // Valeur = 1
  C1.Incrementer;  // Valeur = 2
  C2.Incrementer;  // Valeur = 1
  WriteLn('');

  WriteLn('C1 : ', C1.ObtenirValeur);  // 2
  WriteLn('C2 : ', C2.ObtenirValeur);  // 1
  WriteLn('C3 : ', C3.ObtenirValeur);  // 0
  WriteLn('');

  WriteLn('=== Réinitialisation du total ===');
  TCompteur.ReinitialiserTotal;
  WriteLn('Total : ', TCompteur.ObtenirTotal);  // 0

  C1.Free;
  C2.Free;
  C3.Free;
end.
```

---

## Cas d'usage 1 : Factory Methods (Méthodes de fabrication)

### Problème : Constructeurs avec logique complexe

Les constructeurs ont des limitations. Les méthodes de classe offrent plus de flexibilité.

```pascal
type
  TUtilisateur = class
  private
    FNom: string;
    FEmail: string;
    FRole: string;
  public
    constructor Create(const Nom, Email, Role: string);

    // ✅ Factory methods : créent des utilisateurs spécifiques
    class function CreerAdmin(const Nom, Email: string): TUtilisateur;
    class function CreerInvite(const Nom: string): TUtilisateur;
    class function CreerDepuisJSON(const JSON: string): TUtilisateur;

    procedure Afficher;
  end;

constructor TUtilisateur.Create(const Nom, Email, Role: string);  
begin  
  FNom := Nom;
  FEmail := Email;
  FRole := Role;
end;

class function TUtilisateur.CreerAdmin(const Nom, Email: string): TUtilisateur;  
begin  
  WriteLn('🔧 Création d''un administrateur');
  Result := TUtilisateur.Create(Nom, Email, 'Administrateur');
end;

class function TUtilisateur.CreerInvite(const Nom: string): TUtilisateur;  
begin  
  WriteLn('🔧 Création d''un invité');
  Result := TUtilisateur.Create(Nom, 'invite@exemple.com', 'Invité');
end;

class function TUtilisateur.CreerDepuisJSON(const JSON: string): TUtilisateur;  
begin  
  WriteLn('🔧 Création depuis JSON : ', JSON);
  // En réalité, on parserait le JSON ici
  Result := TUtilisateur.Create('Utilisateur', 'user@exemple.com', 'Utilisateur');
end;

procedure TUtilisateur.Afficher;  
begin  
  WriteLn('👤 ', FNom, ' (', FRole, ') - ', FEmail);
end;
```

**Utilisation :**

```pascal
var
  Admin, Invite, UserJSON: TUtilisateur;
begin
  // ✅ Factory methods : syntaxe expressive et claire
  Admin := TUtilisateur.CreerAdmin('Alice', 'alice@exemple.com');
  Invite := TUtilisateur.CreerInvite('Bob');
  UserJSON := TUtilisateur.CreerDepuisJSON('{"nom":"Charlie"}');

  WriteLn('');
  Admin.Afficher;
  Invite.Afficher;
  UserJSON.Afficher;

  Admin.Free;
  Invite.Free;
  UserJSON.Free;
end.
```

**Résultat :**
```
🔧 Création d'un administrateur
🔧 Création d'un invité
🔧 Création depuis JSON : {"nom":"Charlie"}

👤 Alice (Administrateur) - alice@exemple.com
👤 Bob (Invité) - invite@exemple.com
👤 Utilisateur (Utilisateur) - user@exemple.com
```

**Avantages des factory methods :**
- ✅ Noms explicites (`CreerAdmin` vs `Create`)  
- ✅ Logique de création centralisée  
- ✅ Peut retourner des sous-classes différentes  
- ✅ Facilite les tests (on peut mocker)  

---

## Cas d'usage 2 : Méthodes utilitaires

### Fonctions qui ne nécessitent pas d'état

```pascal
type
  TMathUtils = class
  public
    // Méthodes de classe : utilitaires mathématiques
    class function Max(A, B: Integer): Integer;
    class function Min(A, B: Integer): Integer;
    class function Clamp(Value, Min, Max: Integer): Integer;
    class function EstPair(N: Integer): Boolean;
  end;

class function TMathUtils.Max(A, B: Integer): Integer;  
begin  
  if A > B then
    Result := A
  else
    Result := B;
end;

class function TMathUtils.Min(A, B: Integer): Integer;  
begin  
  if A < B then
    Result := A
  else
    Result := B;
end;

class function TMathUtils.Clamp(Value, Min, Max: Integer): Integer;  
begin  
  Result := Value;
  if Result < Min then
    Result := Min
  else if Result > Max then
    Result := Max;
end;

class function TMathUtils.EstPair(N: Integer): Boolean;  
begin  
  Result := (N mod 2) = 0;
end;
```

**Utilisation :**

```pascal
begin
  // ✅ Pas besoin de créer d'objet TMathUtils
  WriteLn('Max(10, 20) = ', TMathUtils.Max(10, 20));
  WriteLn('Min(10, 20) = ', TMathUtils.Min(10, 20));
  WriteLn('Clamp(150, 0, 100) = ', TMathUtils.Clamp(150, 0, 100));
  WriteLn('EstPair(7) = ', TMathUtils.EstPair(7));
  WriteLn('EstPair(8) = ', TMathUtils.EstPair(8));
end.
```

**Résultat :**
```
Max(10, 20) = 20  
Min(10, 20) = 10  
Clamp(150, 0, 100) = 100  
EstPair(7) = FALSE  
EstPair(8) = TRUE  
```

---

## Cas d'usage 3 : Singleton Pattern

### Une seule instance pour toute l'application

Le pattern Singleton garantit qu'il n'existe qu'**une seule instance** d'une classe.

```pascal
type
  TConfiguration = class
  private
    class var FInstance: TConfiguration;
    FNomApplication: string;
    FVersion: string;

    // Constructeur privé : empêche la création directe
    constructor CreatePrivate;
  public
    // Méthode de classe pour obtenir l'unique instance
    class function Instance: TConfiguration;
    class procedure LibererInstance;

    property NomApplication: string read FNomApplication write FNomApplication;
    property Version: string read FVersion write FVersion;
  end;

// Note : FInstance est automatiquement initialisé à nil (valeur par défaut)

constructor TConfiguration.CreatePrivate;  
begin  
  inherited Create;
  FNomApplication := 'MonApp';
  FVersion := '1.0.0';
  WriteLn('⚙️  Configuration créée');
end;

class function TConfiguration.Instance: TConfiguration;  
begin  
  if FInstance = nil then
  begin
    WriteLn('🔧 Création de l''instance unique');
    FInstance := TConfiguration.CreatePrivate;
  end
  else
    WriteLn('♻️  Réutilisation de l''instance existante');

  Result := FInstance;
end;

class procedure TConfiguration.LibererInstance;  
begin  
  if FInstance <> nil then
  begin
    WriteLn('🗑️  Libération de l''instance');
    FInstance.Free;
    FInstance := nil;
  end;
end;
```

**Utilisation :**

```pascal
var
  Config1, Config2: TConfiguration;
begin
  WriteLn('=== Accès 1 ===');
  Config1 := TConfiguration.Instance;
  Config1.NomApplication := 'SuperApp';
  WriteLn('Application : ', Config1.NomApplication);
  WriteLn('');

  WriteLn('=== Accès 2 (même instance) ===');
  Config2 := TConfiguration.Instance;
  WriteLn('Application : ', Config2.NomApplication);  // SuperApp (modifié par Config1)
  WriteLn('');

  WriteLn('Config1 et Config2 pointent vers le même objet : ', Config1 = Config2);
  WriteLn('');

  // Libération (une seule fois)
  TConfiguration.LibererInstance;
end.
```

**Résultat :**
```
=== Accès 1 ===
🔧 Création de l'instance unique
⚙️  Configuration créée
Application : SuperApp

=== Accès 2 (même instance) ===
♻️  Réutilisation de l'instance existante
Application : SuperApp

Config1 et Config2 pointent vers le même objet : TRUE

🗑️  Libération de l'instance
```

---

## Cas d'usage 4 : Compteurs et statistiques

### Suivre des informations au niveau de la classe

```pascal
type
  TConnexion = class
  private
    FID: Integer;
    FUtilisateur: string;

    class var FNombreConnexions: Integer;
    class var FNombreActif: Integer;
  public
    constructor Create(const Utilisateur: string);
    destructor Destroy; override;

    // Méthodes de classe pour les statistiques
    class function ObtenirNombreTotal: Integer;
    class function ObtenirNombreActif: Integer;
    class procedure AfficherStatistiques;

    property ID: Integer read FID;
    property Utilisateur: string read FUtilisateur;
  end;

// Note : FNombreConnexions et FNombreActif sont automatiquement initialisés à 0

constructor TConnexion.Create(const Utilisateur: string);  
begin  
  inherited Create;
  Inc(FNombreConnexions);
  FID := FNombreConnexions;
  FUtilisateur := Utilisateur;
  Inc(FNombreActif);
  WriteLn('✅ Connexion #', FID, ' créée pour ', FUtilisateur);
end;

destructor TConnexion.Destroy;  
begin  
  Dec(FNombreActif);
  WriteLn('❌ Connexion #', FID, ' fermée (', FUtilisateur, ')');
  inherited;
end;

class function TConnexion.ObtenirNombreTotal: Integer;  
begin  
  Result := FNombreConnexions;
end;

class function TConnexion.ObtenirNombreActif: Integer;  
begin  
  Result := FNombreActif;
end;

class procedure TConnexion.AfficherStatistiques;  
begin  
  WriteLn('📊 Statistiques :');
  WriteLn('   Total de connexions créées : ', FNombreConnexions);
  WriteLn('   Connexions actives : ', FNombreActif);
end;
```

**Utilisation :**

```pascal
var
  C1, C2, C3: TConnexion;
begin
  WriteLn('=== Création de connexions ===');
  C1 := TConnexion.Create('Alice');
  C2 := TConnexion.Create('Bob');
  C3 := TConnexion.Create('Charlie');
  WriteLn('');

  TConnexion.AfficherStatistiques;
  WriteLn('');

  WriteLn('=== Fermeture d''une connexion ===');
  C2.Free;
  WriteLn('');

  TConnexion.AfficherStatistiques;
  WriteLn('');

  WriteLn('=== Fermeture des autres ===');
  C1.Free;
  C3.Free;
  WriteLn('');

  TConnexion.AfficherStatistiques;
end.
```

**Résultat :**
```
=== Création de connexions ===
✅ Connexion #1 créée pour Alice
✅ Connexion #2 créée pour Bob
✅ Connexion #3 créée pour Charlie

📊 Statistiques :
   Total de connexions créées : 3
   Connexions actives : 3

=== Fermeture d'une connexion ===
❌ Connexion #2 fermée (Bob)

📊 Statistiques :
   Total de connexions créées : 3
   Connexions actives : 2

=== Fermeture des autres ===
❌ Connexion #1 fermée (Alice)
❌ Connexion #3 fermée (Charlie)

📊 Statistiques :
   Total de connexions créées : 3
   Connexions actives : 0
```

---

## Méthodes de classe vs Méthodes de classe virtuelles

### Méthodes de classe normales

```pascal
type
  TAnimal = class
  public
    class function NomEspece: string;
  end;

  TChien = class(TAnimal)
  public
    class function NomEspece: string;  // ⚠️ Cache la méthode parente
  end;

class function TAnimal.NomEspece: string;  
begin  
  Result := 'Animal';
end;

class function TChien.NomEspece: string;  
begin  
  Result := 'Chien';
end;
```

### Méthodes de classe virtuelles (class virtual)

```pascal
type
  TAnimal = class
  public
    class function NomEspece: string; virtual;  // ✅ Virtuelle
  end;

  TChien = class(TAnimal)
  public
    class function NomEspece: string; override;  // ✅ Override
  end;

class function TAnimal.NomEspece: string;  
begin  
  Result := 'Animal';
end;

class function TChien.NomEspece: string;  
begin  
  Result := 'Chien';
end;
```

**Différence :**

```pascal
var
  ClasseAnimal: TClass;
begin
  ClasseAnimal := TChien;

  // Avec méthode normale : appelle TAnimal.NomEspece
  // Avec méthode virtuelle : appelle TChien.NomEspece (polymorphisme)

  WriteLn('Espèce : ', TAnimal(ClasseAnimal).NomEspece);
end;
```

---

## Restrictions des méthodes de classe

### ❌ Ce qu'on NE peut PAS faire

```pascal
type
  TExemple = class
  private
    FNom: string;               // Attribut d'instance
    class var FCompteur: Integer; // Attribut de classe
  public
    class procedure MethodeDeClasse;
  end;

class procedure TExemple.MethodeDeClasse;  
begin  
  // ❌ ERREUR : Pas d'accès aux attributs d'instance
  // WriteLn(FNom);

  // ❌ ERREUR : Pas d'accès à Self (pas d'instance)
  // WriteLn(Self.FNom);

  // ✅ OK : Accès aux attributs de classe
  Inc(FCompteur);
  WriteLn('Compteur : ', FCompteur);
end;
```

### ✅ Ce qu'on PEUT faire

```pascal
class procedure TExemple.MethodeDeClasse;  
var  
  Instance: TExemple;
  X: Integer;
begin
  // ✅ Accès aux attributs de classe (class var)
  WriteLn(FCompteur);

  // ✅ Créer des instances
  Instance := TExemple.Create;
  Instance.Free;

  // ✅ Appeler d'autres méthodes de classe
  AutreMethodeDeClasse;

  // ✅ Variables locales
  X := 10;
end;
```

---

## Quand utiliser les méthodes de classe ?

### ✅ Utilisez une méthode de classe quand :

1. **Factory methods** : Créer des objets avec logique spécifique
2. **Méthodes utilitaires** : Fonctions qui ne dépendent pas d'un état
3. **Singleton** : Gérer une instance unique
4. **Compteurs/Statistiques** : Suivre des infos au niveau de la classe
5. **Validation** : Vérifier des données avant création d'objet
6. **Conversion** : Créer des objets depuis différents formats

### ❌ N'utilisez PAS une méthode de classe quand :

1. Vous avez besoin d'accéder aux attributs d'instance
2. Le comportement dépend de l'état de l'objet
3. Une méthode d'instance normale suffit

---

## Exemple complet : Gestionnaire de base de données

```pascal
type
  TConnexionBD = class
  private
    FServeur: string;
    FUtilisateur: string;
    FConnecte: Boolean;

    class var FConnexionDefaut: TConnexionBD;
    class var FNombreConnexions: Integer;
  public
    constructor Create(const Serveur, Utilisateur: string);
    destructor Destroy; override;

    procedure Connecter;
    procedure Deconnecter;

    // Factory methods
    class function CreerConnexionLocale: TConnexionBD;
    class function CreerConnexionDistante(const Serveur: string): TConnexionBD;

    // Singleton
    class function ConnexionDefaut: TConnexionBD;
    class procedure LibererConnexionDefaut;

    // Statistiques
    class function NombreConnexions: Integer;

    property Serveur: string read FServeur;
    property Connecte: Boolean read FConnecte;
  end;

// Note : FConnexionDefaut (nil) et FNombreConnexions (0) sont initialisés par défaut

constructor TConnexionBD.Create(const Serveur, Utilisateur: string);  
begin  
  inherited Create;
  FServeur := Serveur;
  FUtilisateur := Utilisateur;
  FConnecte := False;
  Inc(FNombreConnexions);
  WriteLn('🔧 Connexion créée vers ', FServeur);
end;

destructor TConnexionBD.Destroy;  
begin  
  if FConnecte then
    Deconnecter;
  Dec(FNombreConnexions);
  WriteLn('🔧 Connexion détruite');
  inherited;
end;

procedure TConnexionBD.Connecter;  
begin  
  if not FConnecte then
  begin
    FConnecte := True;
    WriteLn('✅ Connecté à ', FServeur);
  end;
end;

procedure TConnexionBD.Deconnecter;  
begin  
  if FConnecte then
  begin
    FConnecte := False;
    WriteLn('❌ Déconnecté de ', FServeur);
  end;
end;

class function TConnexionBD.CreerConnexionLocale: TConnexionBD;  
begin  
  WriteLn('📍 Création d''une connexion locale');
  Result := TConnexionBD.Create('localhost', 'root');
end;

class function TConnexionBD.CreerConnexionDistante(const Serveur: string): TConnexionBD;  
begin  
  WriteLn('🌐 Création d''une connexion distante');
  Result := TConnexionBD.Create(Serveur, 'admin');
end;

class function TConnexionBD.ConnexionDefaut: TConnexionBD;  
begin  
  if FConnexionDefaut = nil then
  begin
    WriteLn('🏭 Création de la connexion par défaut');
    FConnexionDefaut := CreerConnexionLocale;
  end;
  Result := FConnexionDefaut;
end;

class procedure TConnexionBD.LibererConnexionDefaut;  
begin  
  if FConnexionDefaut <> nil then
  begin
    FConnexionDefaut.Free;
    FConnexionDefaut := nil;
  end;
end;

class function TConnexionBD.NombreConnexions: Integer;  
begin  
  Result := FNombreConnexions;
end;
```

**Utilisation :**

```pascal
var
  BD1, BD2, BDDefaut: TConnexionBD;
begin
  WriteLn('=== Factory Methods ===');
  BD1 := TConnexionBD.CreerConnexionLocale;
  BD2 := TConnexionBD.CreerConnexionDistante('db.exemple.com');
  WriteLn('Nombre de connexions : ', TConnexionBD.NombreConnexions);
  WriteLn('');

  WriteLn('=== Singleton (connexion par défaut) ===');
  BDDefaut := TConnexionBD.ConnexionDefaut;
  BDDefaut.Connecter;
  WriteLn('');

  WriteLn('=== Nettoyage ===');
  BD1.Free;
  BD2.Free;
  TConnexionBD.LibererConnexionDefaut;
  WriteLn('Connexions restantes : ', TConnexionBD.NombreConnexions);
end.
```

---

## Résumé

### Qu'est-ce qu'une méthode de classe ?
- Méthode qui appartient à la **classe**, pas aux instances
- Peut être appelée **sans créer d'objet**
- Utilise le mot-clé **`class`**

### Syntaxe
```pascal
class procedure NomMethode;  
class function NomMethode: Type;  
class procedure NomMethode; virtual;  // Peut être virtuelle  
```

### Restrictions
❌ Pas d'accès aux attributs d'instance (`FNom`, `FAge`)  
❌ Pas d'accès à `Self` (pas d'instance)  
✅ Accès aux attributs de classe (`class var`)  
✅ Peut créer des instances  
✅ Peut appeler d'autres méthodes de classe  

### Cas d'usage principaux
1. **Factory methods** : Créer des objets avec logique
2. **Méthodes utilitaires** : Fonctions sans état
3. **Singleton** : Instance unique
4. **Compteurs/Statistiques** : Données au niveau classe
5. **Validation** : Vérifier avant création

### Avantages
✅ Pas besoin de créer d'objet pour appeler  
✅ Code plus clair et expressif  
✅ Centralise la logique de création  
✅ Facilite les tests (mocking)  

### Différence clé
| Méthode d'instance | Méthode de classe |
|-------------------|-------------------|
| `Objet.Methode()` | `TClasse.Methode()` |
| Nécessite un objet | Pas besoin d'objet |
| Accès aux attributs | Pas d'accès aux attributs |
| `Self` = instance | Pas de `Self` valide |

---

## Prochaine étape

Dans la section suivante (12.10), vous découvrirez les **design patterns** utilisant les interfaces, des solutions éprouvées aux problèmes courants de conception logicielle, comme le Singleton, le Factory, l'Observer, et bien d'autres.

⏭️ [Design patterns basics (Singleton, Factory)](/12-interfaces-poo-avancee/10-design-patterns-basics.md)
