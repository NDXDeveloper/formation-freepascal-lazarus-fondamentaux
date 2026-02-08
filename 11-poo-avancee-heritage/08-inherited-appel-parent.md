🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.8 Inherited et appel au parent

## Introduction

Le mot-clé **`inherited`** est l'un des mécanismes les plus importants de l'héritage en programmation orientée objet. Il permet à une classe dérivée d'**appeler explicitement** les méthodes de sa classe parent, ce qui est essentiel pour réutiliser et étendre le comportement existant.

### Analogie du monde réel

Imaginez que vous apprenez une recette de cuisine de votre grand-mère. Vous suivez d'abord **exactement** sa recette (appel à la méthode parente), puis vous y ajoutez votre touche personnelle (votre code spécifique). Vous ne réinventez pas toute la recette, vous la **complétez**.

C'est exactement ce que fait `inherited` : il exécute d'abord le code du parent, puis vous ajoutez votre code spécifique.

## Qu'est-ce que `inherited` ?

**`inherited`** est un mot-clé qui permet d'appeler la version **parent** d'une méthode depuis une classe dérivée.

```pascal
type
  TParent = class
    procedure Afficher;
  end;

  TEnfant = class(TParent)
    procedure Afficher; override;
  end;

procedure TEnfant.Afficher;
begin
  inherited Afficher;  // Appelle TParent.Afficher
  WriteLn('Code spécifique de TEnfant');
end;
```

## Pourquoi utiliser `inherited` ?

### Problème sans `inherited`

```pascal
type
  TCompte = class
  private
    FSolde: Real;
  public
    constructor Create(ASoldeInitial: Real);
  end;

  TCompteEpargne = class(TCompte)
  private
    FTauxInteret: Real;
  public
    constructor Create(ASoldeInitial: Real; ATauxInteret: Real);
  end;

constructor TCompte.Create(ASoldeInitial: Real);
begin
  inherited Create;  // Initialise TObject
  FSolde := ASoldeInitial;
  WriteLn('Compte créé avec solde : ', ASoldeInitial:0:2);
end;

constructor TCompteEpargne.Create(ASoldeInitial: Real; ATauxInteret: Real);
begin
  // ❌ Si on n'appelle pas inherited, FSolde n'est pas initialisé !
  FTauxInteret := ATauxInteret;
end;
```

**Problème** : La partie `TCompte` n'est pas initialisée correctement.

### Solution avec `inherited`

```pascal
constructor TCompteEpargne.Create(ASoldeInitial: Real; ATauxInteret: Real);
begin
  inherited Create(ASoldeInitial);  // ✅ Initialise TCompte d'abord
  FTauxInteret := ATauxInteret;     // Puis la partie TCompteEpargne
  WriteLn('Compte épargne créé avec taux : ', ATauxInteret:0:2, '%');
end;
```

## Les deux formes de `inherited`

### 1. `inherited` seul (sans nom de méthode)

Appelle la méthode **de même nom** dans la classe parent :

```pascal
procedure TEnfant.MaMethode;
begin
  inherited;  // Appelle TParent.MaMethode
  // Code spécifique
end;
```

### 2. `inherited NomMethode` (avec nom explicite)

Appelle explicitement une méthode du parent :

```pascal
procedure TEnfant.MaMethode;
begin
  inherited MaMethode;  // Appelle TParent.MaMethode explicitement
  // Code spécifique
end;
```

**Note** : La deuxième forme est plus claire et recommandée.

## Utilisation dans les constructeurs

Les constructeurs sont l'endroit le plus important pour utiliser `inherited`.

### Règle d'or : TOUJOURS en premier

```pascal
constructor TEnfant.Create(Param1, Param2: Integer);
begin
  inherited Create(Param1);  // ← TOUJOURS en premier !

  // Ensuite, initialisation spécifique
  FMonAttribut := Param2;
end;
```

**Pourquoi ?** Parce que la partie parent doit être **entièrement initialisée** avant d'initialiser la partie enfant.

### Exemple complet

```pascal
program HeritageConstructeurs;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Classe de base : Personne }
  TPersonne = class
  private
    FNom: string;
    FPrenom: string;
    FAge: Integer;
  public
    constructor Create(ANom, APrenom: string; AAge: Integer);
    destructor Destroy; override;
    procedure Afficher; virtual;
  end;

  { Classe dérivée : Employé }
  TEmploye = class(TPersonne)
  private
    FNumeroEmploye: Integer;
    FSalaire: Real;
  public
    constructor Create(ANom, APrenom: string; AAge: Integer;
                       ANumero: Integer; ASalaire: Real);
    destructor Destroy; override;
    procedure Afficher; override;
    procedure AugmenterSalaire(Pourcentage: Real);
  end;

  { Classe dérivée de niveau 2 : Manager }
  TManager = class(TEmploye)
  private
    FEquipe: Integer;  // Nombre de personnes dans l'équipe
  public
    constructor Create(ANom, APrenom: string; AAge: Integer;
                       ANumero: Integer; ASalaire: Real; AEquipe: Integer);
    destructor Destroy; override;
    procedure Afficher; override;
  end;

{ === TPersonne === }

constructor TPersonne.Create(ANom, APrenom: string; AAge: Integer);
begin
  inherited Create;  // Appelle TObject.Create
  WriteLn('[TPersonne.Create] Début');

  FNom := ANom;
  FPrenom := APrenom;
  FAge := AAge;

  WriteLn('[TPersonne.Create] ', APrenom, ' ', ANom, ' créé(e)');
end;

destructor TPersonne.Destroy;
begin
  WriteLn('[TPersonne.Destroy] ', FPrenom, ' ', FNom, ' détruit(e)');
  inherited Destroy;  // Appelle TObject.Destroy
end;

procedure TPersonne.Afficher;
begin
  WriteLn('=== PERSONNE ===');
  WriteLn('Nom : ', FNom);
  WriteLn('Prénom : ', FPrenom);
  WriteLn('Age : ', FAge, ' ans');
end;

{ === TEmploye === }

constructor TEmploye.Create(ANom, APrenom: string; AAge: Integer;
                            ANumero: Integer; ASalaire: Real);
begin
  WriteLn('[TEmploye.Create] Début');

  // TOUJOURS appeler inherited en premier !
  inherited Create(ANom, APrenom, AAge);

  // Maintenant, initialiser la partie TEmploye
  FNumeroEmploye := ANumero;
  FSalaire := ASalaire;

  WriteLn('[TEmploye.Create] Employé #', ANumero, ' créé avec salaire ', ASalaire:0:2);
end;

destructor TEmploye.Destroy;
begin
  WriteLn('[TEmploye.Destroy] Employé #', FNumeroEmploye);

  // Dans le destructeur, le code spécifique EN PREMIER
  // Puis appel au parent
  inherited Destroy;
end;

procedure TEmploye.Afficher;
begin
  // Appelle la version parent pour afficher les infos de base
  inherited Afficher;

  // Ajoute les infos spécifiques
  WriteLn('Numéro employé : ', FNumeroEmploye);
  WriteLn('Salaire : ', FSalaire:0:2, ' €');
end;

procedure TEmploye.AugmenterSalaire(Pourcentage: Real);
begin
  FSalaire := FSalaire * (1 + Pourcentage / 100);
  WriteLn('Salaire augmenté de ', Pourcentage:0:1, '% → ', FSalaire:0:2, ' €');
end;

{ === TManager === }

constructor TManager.Create(ANom, APrenom: string; AAge: Integer;
                            ANumero: Integer; ASalaire: Real; AEquipe: Integer);
begin
  WriteLn('[TManager.Create] Début');

  // Appelle TEmploye.Create qui lui-même appelle TPersonne.Create
  inherited Create(ANom, APrenom, AAge, ANumero, ASalaire);

  FEquipe := AEquipe;

  WriteLn('[TManager.Create] Manager créé avec équipe de ', AEquipe, ' personnes');
end;

destructor TManager.Destroy;
begin
  WriteLn('[TManager.Destroy] Manager avec équipe de ', FEquipe);
  inherited Destroy;
end;

procedure TManager.Afficher;
begin
  // Appelle TEmploye.Afficher qui appelle TPersonne.Afficher
  inherited Afficher;

  WriteLn('Taille équipe : ', FEquipe, ' personnes');
  WriteLn('Rôle : MANAGER');
end;

{ === Programme principal === }
var
  Manager: TManager;
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('   DEMONSTRATION DE INHERITED');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  WriteLn('--- CREATION D''UN MANAGER ---');
  WriteLn;
  Manager := TManager.Create('Dupont', 'Marie', 35, 1001, 4500.00, 8);
  WriteLn;

  WriteLn('--- AFFICHAGE ---');
  WriteLn;
  Manager.Afficher;
  WriteLn;

  WriteLn('--- AUGMENTATION DE SALAIRE ---');
  WriteLn;
  Manager.AugmenterSalaire(10);
  WriteLn;

  WriteLn('--- DESTRUCTION ---');
  WriteLn;
  Manager.Free;
  WriteLn;

  WriteLn('═══════════════════════════════════════════════');
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Sortie du programme

```
═══════════════════════════════════════════════
   DEMONSTRATION DE INHERITED
═══════════════════════════════════════════════

--- CREATION D'UN MANAGER ---

[TManager.Create] Début
[TEmploye.Create] Début
[TPersonne.Create] Début
[TPersonne.Create] Marie Dupont créé(e)
[TEmploye.Create] Employé #1001 créé avec salaire 4500.00
[TManager.Create] Manager créé avec équipe de 8 personnes

--- AFFICHAGE ---

=== PERSONNE ===
Nom : Dupont
Prénom : Marie
Age : 35 ans
Numéro employé : 1001
Salaire : 4500.00 €
Taille équipe : 8 personnes
Rôle : MANAGER

--- AUGMENTATION DE SALAIRE ---

Salaire augmenté de 10.0% → 4950.00 €

--- DESTRUCTION ---

[TManager.Destroy] Manager avec équipe de 8
[TEmploye.Destroy] Employé #1001
[TPersonne.Destroy] Marie Dupont détruit(e)
```

## Ordre d'exécution : Constructeurs vs Destructeurs

### Constructeurs : Du parent vers l'enfant

```
TPersonne.Create
    ↓
TEmploye.Create
    ↓
TManager.Create
```

**Logique** : On construit d'abord les fondations (parent) avant le toit (enfant).

### Destructeurs : De l'enfant vers le parent

```
TManager.Destroy
    ↓
TEmploye.Destroy
    ↓
TPersonne.Destroy
```

**Logique** : On démonte d'abord le toit (enfant) avant les fondations (parent).

## Utilisation dans les méthodes normales

### Étendre le comportement

```pascal
type
  TDocument = class
  public
    procedure Sauvegarder; virtual;
  end;

  TDocumentChiffre = class(TDocument)
  public
    procedure Sauvegarder; override;
  end;

procedure TDocument.Sauvegarder;
begin
  WriteLn('Sauvegarde du document...');
  // Code de sauvegarde standard
end;

procedure TDocumentChiffre.Sauvegarder;
begin
  WriteLn('Chiffrement en cours...');
  // Code de chiffrement

  inherited Sauvegarder;  // Appelle la sauvegarde normale

  WriteLn('Document chiffré sauvegardé');
end;
```

### Réutiliser du code

```pascal
type
  TCompte = class
  public
    procedure AfficherSolde; virtual;
  end;

  TCompteVIP = class(TCompte)
  public
    procedure AfficherSolde; override;
  end;

procedure TCompte.AfficherSolde;
begin
  WriteLn('Solde : ', FSolde:0:2, ' €');
end;

procedure TCompteVIP.AfficherSolde;
begin
  WriteLn('*** COMPTE VIP ***');
  inherited AfficherSolde;  // Réutilise l'affichage du parent
  WriteLn('Avantages : Frais réduits, Conseiller dédié');
end;
```

## Exemple pratique : Système de logging hiérarchique

```pascal
program SystemeLogging;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Classe de base : Logger simple }
  TLogger = class
  protected
    FNomFichier: string;
  public
    constructor Create(ANomFichier: string);
    procedure Log(const Message: string); virtual;
    destructor Destroy; override;
  end;

  { Logger avec horodatage }
  TLoggerAvecDate = class(TLogger)
  public
    procedure Log(const Message: string); override;
  end;

  { Logger avec niveau de priorité }
  TLoggerAvecNiveau = class(TLoggerAvecDate)
  private
    FNiveau: string;
  public
    constructor Create(ANomFichier: string; ANiveau: string);
    procedure Log(const Message: string); override;
  end;

  { Logger complet avec contexte }
  TLoggerComplet = class(TLoggerAvecNiveau)
  private
    FContexte: string;
  public
    constructor Create(ANomFichier, ANiveau, AContexte: string);
    procedure Log(const Message: string); override;
  end;

{ === TLogger === }

constructor TLogger.Create(ANomFichier: string);
begin
  inherited Create;
  FNomFichier := ANomFichier;
  WriteLn('[TLogger] Fichier log : ', FNomFichier);
end;

procedure TLogger.Log(const Message: string);
begin
  WriteLn(Message);
end;

destructor TLogger.Destroy;
begin
  WriteLn('[TLogger] Fermeture du fichier log');
  inherited Destroy;
end;

{ === TLoggerAvecDate === }

procedure TLoggerAvecDate.Log(const Message: string);
var
  MessageAvecDate: string;
begin
  // Ajoute la date/heure
  MessageAvecDate := '[' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + '] ' + Message;

  // Appelle le logger parent
  inherited Log(MessageAvecDate);
end;

{ === TLoggerAvecNiveau === }

constructor TLoggerAvecNiveau.Create(ANomFichier: string; ANiveau: string);
begin
  inherited Create(ANomFichier);
  FNiveau := ANiveau;
  WriteLn('[TLoggerAvecNiveau] Niveau : ', FNiveau);
end;

procedure TLoggerAvecNiveau.Log(const Message: string);
var
  MessageAvecNiveau: string;
begin
  // Ajoute le niveau
  MessageAvecNiveau := '[' + FNiveau + '] ' + Message;

  // Appelle le logger parent (qui ajoutera la date)
  inherited Log(MessageAvecNiveau);
end;

{ === TLoggerComplet === }

constructor TLoggerComplet.Create(ANomFichier, ANiveau, AContexte: string);
begin
  inherited Create(ANomFichier, ANiveau);
  FContexte := AContexte;
  WriteLn('[TLoggerComplet] Contexte : ', FContexte);
end;

procedure TLoggerComplet.Log(const Message: string);
var
  MessageComplet: string;
begin
  // Ajoute le contexte
  MessageComplet := '[' + FContexte + '] ' + Message;

  // Appelle le logger parent (qui ajoutera niveau et date)
  inherited Log(MessageComplet);
end;

{ === Programme principal === }
var
  Logger1: TLogger;
  Logger2: TLoggerAvecDate;
  Logger3: TLoggerAvecNiveau;
  Logger4: TLoggerComplet;
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('   DEMONSTRATION DU LOGGING HIERARCHIQUE');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  WriteLn('--- LOGGER SIMPLE ---');
  Logger1 := TLogger.Create('app.log');
  Logger1.Log('Démarrage de l''application');
  WriteLn;

  WriteLn('--- LOGGER AVEC DATE ---');
  Logger2 := TLoggerAvecDate.Create('app.log');
  Logger2.Log('Connexion utilisateur');
  WriteLn;

  WriteLn('--- LOGGER AVEC NIVEAU ---');
  Logger3 := TLoggerAvecNiveau.Create('app.log', 'INFO');
  Logger3.Log('Opération réussie');
  WriteLn;

  WriteLn('--- LOGGER COMPLET ---');
  Logger4 := TLoggerComplet.Create('app.log', 'ERROR', 'Module-DB');
  Logger4.Log('Échec de connexion à la base de données');
  WriteLn;

  WriteLn('--- LIBERATION ---');
  Logger1.Free;
  Logger2.Free;
  Logger3.Free;
  Logger4.Free;

  WriteLn;
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Sortie du programme

```
═══════════════════════════════════════════════
   DEMONSTRATION DU LOGGING HIERARCHIQUE
═══════════════════════════════════════════════

--- LOGGER SIMPLE ---
[TLogger] Fichier log : app.log
Démarrage de l'application

--- LOGGER AVEC DATE ---
[TLogger] Fichier log : app.log
[2025-10-13 14:30:45] Connexion utilisateur

--- LOGGER AVEC NIVEAU ---
[TLogger] Fichier log : app.log
[TLoggerAvecNiveau] Niveau : INFO
[2025-10-13 14:30:45] [INFO] Opération réussie

--- LOGGER COMPLET ---
[TLogger] Fichier log : app.log
[TLoggerAvecNiveau] Niveau : ERROR
[TLoggerComplet] Contexte : Module-DB
[2025-10-13 14:30:45] [ERROR] [Module-DB] Échec de connexion à la base de données

--- LIBERATION ---
[TLogger] Fermeture du fichier log
[TLogger] Fermeture du fichier log
[TLogger] Fermeture du fichier log
[TLogger] Fermeture du fichier log
```

## Cas où `inherited` n'est PAS nécessaire

### 1. Remplacement complet du comportement

```pascal
type
  TAnimal = class
    procedure Dormir; virtual;
  end;

  TChauvesSouris = class(TAnimal)
    procedure Dormir; override;
  end;

procedure TAnimal.Dormir;
begin
  WriteLn('Dort la nuit');
end;

procedure TChauvesSouris.Dormir;
begin
  // PAS d'appel à inherited : comportement complètement différent
  WriteLn('Dort le jour, suspendue à l''envers');
end;
```

### 2. Méthode abstraite dans le parent

```pascal
type
  TForme = class
    function CalculerAire: Real; virtual; abstract;
  end;

  TRectangle = class(TForme)
    function CalculerAire: Real; override;
  end;

function TRectangle.CalculerAire: Real;
begin
  // PAS d'appel à inherited : la méthode parent est abstraite
  Result := FLargeur * FHauteur;
end;
```

## Erreurs courantes

### Erreur 1 : Oublier `inherited` dans le constructeur

```pascal
constructor TEnfant.Create;
begin
  // ❌ OUBLI : inherited Create manquant
  FMonAttribut := 10;
end;
```

**Conséquence** : La partie parent n'est pas initialisée.

**Solution** :
```pascal
constructor TEnfant.Create;
begin
  inherited Create;  // ✅ Toujours en premier
  FMonAttribut := 10;
end;
```

### Erreur 2 : Appeler `inherited` après le code

```pascal
constructor TEnfant.Create;
begin
  FMonAttribut := 10;    // ❌ Avant inherited
  inherited Create;      // Trop tard !
end;
```

**Solution** :
```pascal
constructor TEnfant.Create;
begin
  inherited Create;      // ✅ En premier
  FMonAttribut := 10;
end;
```

### Erreur 3 : Ordre inversé dans le destructeur

```pascal
destructor TEnfant.Destroy;
begin
  inherited Destroy;     // ❌ Trop tôt
  // Nettoyage de FMonAttribut  // Code jamais exécuté !
end;
```

**Solution** :
```pascal
destructor TEnfant.Destroy;
begin
  // Nettoyage de FMonAttribut  // ✅ D'abord le nettoyage
  inherited Destroy;             // Puis le parent
end;
```

### Erreur 4 : Appeler plusieurs fois `inherited`

```pascal
procedure TEnfant.MaMethode;
begin
  inherited MaMethode;  // Premier appel
  // ...
  inherited MaMethode;  // ❌ Deuxième appel : généralement inutile
end;
```

Sauf cas très spécifique, n'appelez `inherited` qu'**une seule fois**.

## Différence entre `inherited` et `inherited NomMethode`

### `inherited` seul

```pascal
procedure TEnfant.Test(X: Integer);
begin
  inherited;  // Appelle TParent.Test avec les mêmes paramètres
end;
```

**Équivalent à** : `inherited Test(X);`

### `inherited NomMethode`

```pascal
procedure TEnfant.Test(X: Integer);
begin
  inherited Test(X * 2);  // Appelle avec des paramètres différents
end;
```

Plus **flexible** : vous pouvez changer les paramètres.

## Appeler une méthode d'un ancêtre lointain

Vous ne pouvez appeler que le **parent direct** avec `inherited`, pas un ancêtre plus éloigné :

```pascal
type
  TGrandParent = class
    procedure Test; virtual;
  end;

  TParent = class(TGrandParent)
    procedure Test; override;
  end;

  TEnfant = class(TParent)
    procedure Test; override;
  end;

procedure TEnfant.Test;
begin
  inherited Test;  // ✅ Appelle TParent.Test

  // ❌ IMPOSSIBLE : appeler directement TGrandParent.Test
  // Il faut passer par TParent
end;
```

**Note** : Si vous avez vraiment besoin d'appeler un ancêtre lointain, c'est souvent le signe d'un problème de conception.

## Bonnes pratiques

### ✅ À FAIRE

1. **Toujours `inherited` dans les constructeurs (en premier)**
   ```pascal
   constructor TEnfant.Create;
   begin
     inherited Create;  // TOUJOURS
     // ...
   end;
   ```

2. **Toujours `inherited` dans les destructeurs (en dernier)**
   ```pascal
   destructor TEnfant.Destroy;
   begin
     // Nettoyage
     inherited Destroy;  // TOUJOURS
   end;
   ```

3. **Utiliser la forme explicite**
   ```pascal
   inherited NomMethode(Parametres);  // Plus clair
   ```

4. **Documenter quand on n'appelle PAS inherited**
   ```pascal
   procedure Test; override;
   // Note : pas d'appel à inherited car comportement complètement différent
   ```

5. **Appeler inherited quand on étend le comportement**
   ```pascal
   procedure TEnfant.Afficher;
   begin
     inherited Afficher;  // Garde le comportement parent
     // + Ajoute des infos
   end;
   ```

### ❌ À ÉVITER

1. **Oublier inherited dans les constructeurs**

2. **Appeler inherited après du code dans les constructeurs**

3. **Appeler inherited avant le nettoyage dans les destructeurs**

4. **Appeler inherited quand la méthode parent est abstraite**

5. **Appeler inherited plusieurs fois sans raison**

## Cas d'usage avancé : Chaîne de responsabilité

```pascal
type
  TValidateur = class
  public
    function Valider(const Valeur: string): Boolean; virtual;
  end;

  TValidateurLongueur = class(TValidateur)
  public
    function Valider(const Valeur: string): Boolean; override;
  end;

  TValidateurCaracteres = class(TValidateurLongueur)
  public
    function Valider(const Valeur: string): Boolean; override;
  end;

function TValidateur.Valider(const Valeur: string): Boolean;
begin
  Result := True;  // Validation de base : tout est OK
end;

function TValidateurLongueur.Valider(const Valeur: string): Boolean;
begin
  Result := inherited Valider(Valeur);  // D'abord validation parent

  if Result then
    Result := Length(Valeur) >= 3;  // Puis validation spécifique
end;

function TValidateurCaracteres.Valider(const Valeur: string): Boolean;
begin
  Result := inherited Valider(Valeur);  // Longueur OK ?

  if Result then
    Result := Pos('@', Valeur) > 0;  // Contient @ ?
end;
```

## Résumé

Le mot-clé **`inherited`** permet de :
- ✅ Appeler explicitement les méthodes de la classe parent
- ✅ **Réutiliser** le code existant au lieu de le dupliquer
- ✅ **Étendre** le comportement parent avec du code spécifique
- ✅ Garantir l'**initialisation correcte** de toute la hiérarchie

**Règles essentielles :**

| Situation | Règle | Position |
|-----------|-------|----------|
| **Constructeur** | TOUJOURS `inherited Create` | **En premier** |
| **Destructeur** | TOUJOURS `inherited Destroy` | **En dernier** |
| **Méthode normale** | Si on étend le comportement | Selon le besoin |
| **Méthode abstraite** | JAMAIS | - |

**Syntaxe :**
```pascal
inherited;                    // Appelle la méthode de même nom
inherited NomMethode(Params); // Appelle explicitement (recommandé)
```

**Ordre d'exécution :**
- **Constructeurs** : Parent → Enfant (fondations → toit)
- **Destructeurs** : Enfant → Parent (toit → fondations)

**Analogie finale :** `inherited` est comme suivre une recette de famille : vous commencez par ce que vos parents faisaient, puis vous y ajoutez votre touche personnelle !

Le mot-clé `inherited` est fondamental pour tirer pleinement parti de l'héritage en POO. Maîtrisez-le et vous créerez des hiérarchies de classes robustes et maintenables !

⏭️ [Hiérarchies de classes](/11-poo-avancee-heritage/09-hierarchies-classes.md)
