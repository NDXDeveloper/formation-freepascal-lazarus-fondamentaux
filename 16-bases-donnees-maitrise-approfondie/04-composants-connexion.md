🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.4 Composants de connexion

## Introduction : du SQL à Pascal

Jusqu'à présent, vous avez appris à manipuler des bases de données SQLite en ligne de commande. Maintenant, nous allons voir comment **intégrer** ces bases de données dans vos applications Lazarus.

### L'analogie du téléphone

Imaginez que votre base de données SQLite soit une personne que vous voulez appeler :

- **Le numéro de téléphone** = le chemin vers votre fichier `.db`
- **L'appareil téléphonique** = le composant de connexion (`TSQLite3Connection`)
- **La conversation** = les requêtes SQL que vous envoyez
- **Les réponses** = les données que vous recevez

Pour communiquer avec votre base de données depuis Pascal, vous avez besoin de **composants** qui font le lien entre votre code et le fichier `.db`.

## L'architecture de connexion dans Lazarus

Lazarus utilise une architecture en couches pour accéder aux bases de données. Voici les éléments principaux :

```
┌──────────────────────────────────────────┐
│  Votre Application Lazarus               │
│  ┌────────────────────────────────────┐  │
│  │  Composants visuels (Grilles,      │  │
│  │  TDBEdit, TDBGrid, etc.)           │  │
│  └──────────────┬─────────────────────┘  │
│                 │                        │
│  ┌──────────────▼─────────────────────┐  │
│  │  TDataSource                       │  │
│  │  (pont entre visuel et données)    │  │
│  └──────────────┬─────────────────────┘  │
│                 │                        │
│  ┌──────────────▼─────────────────────┐  │
│  │  TSQLQuery                         │  │
│  │  (exécute les requêtes SQL)        │  │
│  └──────────────┬─────────────────────┘  │
│                 │                        │
│  ┌──────────────▼─────────────────────┐  │
│  │  TSQLTransaction                   │  │
│  │  (gère les transactions)           │  │
│  └──────────────┬─────────────────────┘  │
│                 │                        │
│  ┌──────────────▼─────────────────────┐  │
│  │  TSQLite3Connection                │  │
│  │  (connexion à la base SQLite)      │  │
│  └──────────────┬─────────────────────┘  │
│                 │                        │
│                 ▼                        │
│  ┌────────────────────────────────────┐  │
│  │  Fichier ma_base.db                │  │
│  └────────────────────────────────────┘  │
└──────────────────────────────────────────┘
```

Ne vous inquiétez pas si cela semble complexe ! Nous allons voir chaque composant un par un.

## Les composants essentiels

Pour commencer à travailler avec SQLite dans Lazarus, vous aurez besoin de trois composants principaux :

### 1. TSQLite3Connection

**Rôle :** C'est le composant qui établit la connexion avec votre fichier de base de données SQLite.

**Analogie :** C'est comme la prise électrique qui connecte votre appareil au réseau.

**Package :** SQLDBLaz (généralement installé par défaut)

**Palette :** SQLdb dans l'IDE Lazarus

### 2. TSQLTransaction

**Rôle :** Gère les transactions pour garantir l'intégrité des données.

**Analogie :** C'est comme un gardien qui s'assure que toutes vos modifications sont validées ou annulées ensemble.

**Package :** SQLDBLaz

**Palette :** SQLdb

### 3. TSQLQuery

**Rôle :** Exécute vos requêtes SQL et récupère les résultats.

**Analogie :** C'est votre messager qui porte vos questions à la base de données et rapporte les réponses.

**Package :** SQLDBLaz

**Palette :** SQLdb

## Installation des packages nécessaires

### Vérifier l'installation

Lazarus inclut normalement le support SQLite par défaut. Pour vérifier :

1. Ouvrez Lazarus
2. Allez dans **Paquets** → **Ouvrir un paquet chargé (.lpk)**
3. Cherchez **SQLDBLaz** dans la liste

Si vous le trouvez, c'est bon ! Sinon, il faut l'installer.

### Installer SQLDBLaz (si nécessaire)

1. **Paquets** → **Ouvrir un fichier de paquet (.lpk)**
2. Naviguez vers le dossier d'installation de Lazarus
3. Allez dans `components/sqldb/`
4. Ouvrez `sqldblaz.lpk`
5. Cliquez sur **Compiler** puis **Utiliser** → **Installer**
6. Lazarus se recompilera

### Installer la bibliothèque SQLite

#### Sur Windows

La bibliothèque SQLite3 (fichier `sqlite3.dll`) doit être accessible à votre application.

**Option 1 - Copier dans le dossier projet (recommandé) :**

1. Téléchargez `sqlite3.dll` depuis https://sqlite.org/download.html
   - Cherchez "Precompiled Binaries for Windows"
   - Téléchargez `sqlite-dll-win64-x64-*.zip` (ou win32 si vous êtes en 32 bits)
2. Décompressez et copiez `sqlite3.dll` dans le dossier de votre projet Lazarus

**Option 2 - Copier dans Windows\System32 :**

Placez `sqlite3.dll` dans `C:\Windows\System32\` (moins recommandé car affecte tout le système)

#### Sur Ubuntu/Linux

Installez la bibliothèque de développement :

```bash
sudo apt update  
sudo apt install libsqlite3-dev  
```

C'est tout ! La bibliothèque sera automatiquement trouvée par votre application.

## Créer votre première connexion

Créons ensemble une application simple qui se connecte à une base de données SQLite.

### Étape 1 : Créer le projet

1. Ouvrez Lazarus
2. **Projet** → **Nouveau projet** → **Application**
3. Enregistrez le projet dans un dossier (ex: `ProjetSQLite1`)

### Étape 2 : Ajouter les composants non visuels

Les composants de connexion sont **non visuels** : ils n'apparaissent pas dans l'interface de l'application finale, uniquement dans le concepteur de formulaires.

1. Dans la palette de composants, onglet **SQLdb**
2. Ajoutez ces composants sur votre formulaire (Form1) :
   - `TSQLite3Connection` → renommez-le `SQLite3Connection1`
   - `TSQLTransaction` → renommez-le `SQLTransaction1`

Ils apparaîtront en bas du formulaire dans le concepteur, sous forme d'icônes.

### Étape 3 : Configurer TSQLite3Connection

Sélectionnez `SQLite3Connection1` et configurez ses propriétés dans l'Inspecteur d'objets :

#### Propriétés essentielles

**DatabaseName** (le plus important !)
- C'est le chemin vers votre fichier `.db`
- Exemples :
  ```
  Windows : C:\MesProjets\ma_base.db
  Linux   : /home/utilisateur/ma_base.db
  Relatif : ma_base.db  (dans le dossier de l'application)
  ```

**Transaction**
- Cliquez sur la liste déroulante
- Sélectionnez `SQLTransaction1`
- Cela lie la connexion à la transaction

**CharSet** (optionnel)
- Généralement : `UTF8`
- Assure le bon encodage des caractères accentués

**KeepConnection**
- `True` : garde la connexion ouverte (recommandé pour desktop)
- `False` : ouvre/ferme à chaque opération

**LoginPrompt**
- `False` pour SQLite (pas de login/mot de passe)

### Étape 4 : Configurer TSQLTransaction

Sélectionnez `SQLTransaction1` et configurez :

**Database**
- Sélectionnez `SQLite3Connection1`
- Lie la transaction à la connexion

**Action** (optionnel)
- `caCommit` : valide automatiquement
- `caRollback` : annule automatiquement
- `caNone` : gestion manuelle (recommandé pour débuter)

### Étape 5 : Exemple de code complet

Ajoutez un bouton `Button1` sur votre formulaire avec le caption "Se connecter".

Dans l'événement `OnClick` du bouton :

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  try
    // Définir le chemin de la base de données
    SQLite3Connection1.DatabaseName := 'ma_base.db';

    // Activer les clés étrangères (important pour SQLite !)
    SQLite3Connection1.Params.Add('foreign_keys=ON');

    // Ouvrir la connexion
    SQLite3Connection1.Open;

    // Si on arrive ici, la connexion a réussi
    ShowMessage('Connexion réussie à la base de données !');
  except
    on E: Exception do
    begin
      ShowMessage('Erreur de connexion : ' + E.Message);
    end;
  end;
end;
```

### Étape 6 : Fermer la connexion proprement

Dans l'événement `OnClose` du formulaire :

```pascal
procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);  
begin  
  // Fermer la connexion si elle est ouverte
  if SQLite3Connection1.Connected then
  begin
    SQLTransaction1.Commit;  // Valider les changements
    SQLite3Connection1.Close;
  end;
end;
```

## Propriétés détaillées de TSQLite3Connection

### DatabaseName (String)

Le chemin vers votre fichier de base de données.

**Chemins absolus :**
```pascal
// Windows
SQLite3Connection1.DatabaseName := 'C:\Data\ma_base.db';

// Linux
SQLite3Connection1.DatabaseName := '/home/user/data/ma_base.db';
```

**Chemins relatifs :**
```pascal
// Dans le même dossier que l'exécutable
SQLite3Connection1.DatabaseName := 'ma_base.db';

// Dans un sous-dossier
SQLite3Connection1.DatabaseName := 'data' + PathDelim + 'ma_base.db';
```

**Important pour le multi-plateforme :** Utilisez `PathDelim` ou `IncludeTrailingPathDelimiter` :

```pascal
uses
  SysUtils;

// Multi-plateforme
SQLite3Connection1.DatabaseName :=
  GetCurrentDir + PathDelim + 'data' + PathDelim + 'ma_base.db';
```

### Connected (Boolean)

Indique si la connexion est actuellement active.

```pascal
// Vérifier l'état
if SQLite3Connection1.Connected then
  ShowMessage('Connecté')
else
  ShowMessage('Non connecté');
```

### Params (TStringList)

Paramètres supplémentaires pour SQLite.

**Paramètres utiles :**

```pascal
// Activer les clés étrangères (TRÈS IMPORTANT pour SQLite)
SQLite3Connection1.Params.Add('foreign_keys=ON');

// Mode synchronisation (performance vs sécurité)
SQLite3Connection1.Params.Add('synchronous=NORMAL');
// Options : OFF (rapide, risqué), NORMAL (bon compromis), FULL (sûr, lent)

// Mode journal
SQLite3Connection1.Params.Add('journal_mode=WAL');
// WAL = Write-Ahead Logging, meilleure concurrence

// Timeout en millisecondes si la base est verrouillée
SQLite3Connection1.Params.Add('busy_timeout=5000');
```

### CharSet (String)

Encodage des caractères.

```pascal
SQLite3Connection1.CharSet := 'UTF8';
```

Essentiel pour les caractères accentués (français, espagnol, etc.).

### Transaction (TSQLTransaction)

La transaction associée à cette connexion.

```pascal
SQLite3Connection1.Transaction := SQLTransaction1;
```

Doit être défini avant d'ouvrir la connexion.

## Méthodes importantes de TSQLite3Connection

### Open

Ouvre la connexion à la base de données.

```pascal
SQLite3Connection1.Open;
```

**Si le fichier n'existe pas :** SQLite le crée automatiquement (fichier vide) !

### Close

Ferme la connexion.

```pascal
SQLite3Connection1.Close;
```

**Important :** Toujours fermer les connexions quand vous avez terminé.

### Connected (propriété lecture seule)

Vérifie si la connexion est active.

```pascal
if not SQLite3Connection1.Connected then
  SQLite3Connection1.Open;
```

### ExecuteDirect

Exécute une commande SQL directement sans utiliser TSQLQuery.

```pascal
SQLite3Connection1.ExecuteDirect('CREATE TABLE Test (id INTEGER, nom TEXT)');
```

Utile pour des commandes DDL (CREATE, DROP, ALTER).

## Propriétés et méthodes de TSQLTransaction

### Database

La connexion associée à cette transaction.

```pascal
SQLTransaction1.Database := SQLite3Connection1;
```

### Active (Boolean)

Indique si une transaction est en cours.

```pascal
if SQLTransaction1.Active then
  ShowMessage('Transaction en cours');
```

### Commit

Valide et enregistre toutes les modifications.

```pascal
try
  // Vos opérations SQL ici
  SQLQuery1.ExecSQL;

  // Valider
  SQLTransaction1.Commit;
  ShowMessage('Modifications enregistrées');
except
  on E: Exception do
  begin
    SQLTransaction1.Rollback;
    ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

### Rollback

Annule toutes les modifications non validées.

```pascal
SQLTransaction1.Rollback;
```

### StartTransaction

Démarre explicitement une transaction.

```pascal
SQLTransaction1.StartTransaction;  
try  
  // Vos modifications
  SQLTransaction1.Commit;
except
  SQLTransaction1.Rollback;
  raise;
end;
```

## Gestion des chemins multi-plateformes

Pour que votre application fonctionne sur Windows et Linux, utilisez les fonctions de `SysUtils` :

### PathDelim

Séparateur de chemin selon la plateforme.
- Windows : `\`
- Linux : `/`

```pascal
uses
  SysUtils;

var
  CheminBase: string;
begin
  // Construit automatiquement le bon chemin
  CheminBase := 'data' + PathDelim + 'ma_base.db';
  // Windows : data\ma_base.db
  // Linux   : data/ma_base.db
end;
```

### GetCurrentDir

Retourne le répertoire de travail actuel.

```pascal
CheminBase := GetCurrentDir + PathDelim + 'ma_base.db';
```

### ExtractFilePath

Extrait le chemin du répertoire d'un fichier.

```pascal
CheminBase := ExtractFilePath(Application.ExeName) + 'data' + PathDelim + 'ma_base.db';
```

### GetAppConfigDir

Retourne le dossier de configuration de l'application (recommandé).

```pascal
uses
  FileUtil;

var
  ConfigDir: string;
begin
  // Crée le dossier si nécessaire
  ConfigDir := GetAppConfigDir(False);
  ForceDirectories(ConfigDir);

  SQLite3Connection1.DatabaseName := ConfigDir + 'ma_base.db';
end;
```

**Emplacements par défaut :**
- Windows : `C:\Users\NomUtilisateur\AppData\Local\NomApplication\`
- Linux : `~/.config/NomApplication/` ou `~/.NomApplication/`

## Gestion des erreurs de connexion

### Erreurs courantes

#### "Unable to open database file"

**Cause :** Le fichier n'existe pas et ne peut pas être créé (permissions, chemin invalide).

**Solution :**
```pascal
var
  CheminBase: string;
begin
  CheminBase := 'ma_base.db';

  // Vérifier si le répertoire parent existe
  if not DirectoryExists(ExtractFilePath(CheminBase)) then
    ForceDirectories(ExtractFilePath(CheminBase));

  SQLite3Connection1.DatabaseName := CheminBase;
  SQLite3Connection1.Open;
end;
```

#### "Error loading library sqlite3.dll"

**Cause :** La bibliothèque SQLite n'est pas trouvée.

**Solution Windows :** Copiez `sqlite3.dll` dans le dossier de votre .exe

**Solution Linux :** Installez libsqlite3 :
```bash
sudo apt install libsqlite3-0
```

#### "Database is locked"

**Cause :** Un autre processus accède à la base.

**Solution :** Augmentez le timeout :
```pascal
SQLite3Connection1.Params.Add('busy_timeout=5000');
```

### Exemple de gestion d'erreurs robuste

```pascal
procedure TForm1.ConnecterBase;  
var  
  CheminBase: string;
begin
  try
    // Construire le chemin de façon portable
    CheminBase := GetAppConfigDir(False);
    ForceDirectories(CheminBase);
    CheminBase := CheminBase + 'ma_base.db';

    // Configurer la connexion
    SQLite3Connection1.DatabaseName := CheminBase;
    SQLite3Connection1.CharSet := 'UTF8';
    SQLite3Connection1.Params.Clear;
    SQLite3Connection1.Params.Add('foreign_keys=ON');
    SQLite3Connection1.Params.Add('busy_timeout=5000');

    // Définir la transaction
    SQLite3Connection1.Transaction := SQLTransaction1;
    SQLTransaction1.Database := SQLite3Connection1;

    // Ouvrir la connexion
    SQLite3Connection1.Open;

    ShowMessage('Connexion réussie !');
  except
    on E: EDatabaseError do
    begin
      ShowMessage('Erreur de base de données : ' + E.Message);
    end;
    on E: Exception do
    begin
      ShowMessage('Erreur : ' + E.Message);
    end;
  end;
end;
```

## Template de connexion complet

Voici un modèle complet que vous pouvez réutiliser :

```pascal
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls,
  sqldb, sqlite3conn;

type
  TForm1 = class(TForm)
    SQLite3Connection1: TSQLite3Connection;
    SQLTransaction1: TSQLTransaction;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    procedure InitialiserConnexion;
    procedure FermerConnexion;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Initialiser au démarrage
  InitialiserConnexion;
end;

procedure TForm1.InitialiserConnexion;  
var  
  CheminBase: string;
begin
  try
    // Construire le chemin (multi-plateforme)
    CheminBase := GetAppConfigDir(False);
    ForceDirectories(CheminBase);
    CheminBase := CheminBase + 'ma_base.db';

    // Configurer la connexion
    SQLite3Connection1.DatabaseName := CheminBase;
    SQLite3Connection1.CharSet := 'UTF8';
    SQLite3Connection1.Transaction := SQLTransaction1;

    // Paramètres SQLite
    SQLite3Connection1.Params.Clear;
    SQLite3Connection1.Params.Add('foreign_keys=ON');
    SQLite3Connection1.Params.Add('busy_timeout=5000');

    // Configurer la transaction
    SQLTransaction1.Database := SQLite3Connection1;

    // Ouvrir la connexion
    SQLite3Connection1.Open;

  except
    on E: Exception do
    begin
      ShowMessage('Erreur d''initialisation : ' + E.Message);
      Application.Terminate;
    end;
  end;
end;

procedure TForm1.FermerConnexion;  
begin  
  if SQLite3Connection1.Connected then
  begin
    try
      if SQLTransaction1.Active then
        SQLTransaction1.Commit;
    except
      SQLTransaction1.Rollback;
    end;
    SQLite3Connection1.Close;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);  
begin  
  if SQLite3Connection1.Connected then
    ShowMessage('Connexion active !')
  else
    ShowMessage('Connexion fermée');
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);  
begin  
  FermerConnexion;
end;

end.
```

## Bonnes pratiques

### 1. Toujours utiliser try-except

```pascal
try
  SQLite3Connection1.Open;
except
  on E: Exception do
    ShowMessage('Erreur : ' + E.Message);
end;
```

### 2. Fermer les connexions proprement

Dans `FormClose` ou `FormDestroy`, fermez toujours vos connexions.

### 3. Activer les clés étrangères

SQLite les désactive par défaut !

```pascal
SQLite3Connection1.Params.Add('foreign_keys=ON');
```

### 4. Utiliser des chemins relatifs ou configurables

Ne codez jamais en dur `C:\...`. Utilisez `GetAppConfigDir` ou permettez à l'utilisateur de choisir.

### 5. Vérifier l'existence du fichier

```pascal
if not FileExists(CheminBase) then
  ShowMessage('La base de données sera créée');
```

### 6. Logger les erreurs

Pour faciliter le débogage :

```pascal
except
  on E: Exception do
  begin
    WriteLn('Erreur : ' + E.Message);  // Console
    // Ou écrire dans un fichier log
  end;
end;
```

## Résumé

**Composants nécessaires :**
- `TSQLite3Connection` : connexion à la base
- `TSQLTransaction` : gestion des transactions
- `TSQLQuery` : exécution des requêtes (section suivante)

**Configuration minimale :**
1. Définir `DatabaseName`
2. Lier connexion et transaction
3. Activer `foreign_keys=ON`
4. Appeler `Open`

**Multi-plateforme :**
- Utiliser `PathDelim`
- Utiliser `GetAppConfigDir`
- Tester sur les deux plateformes

**Gestion d'erreurs :**
- Toujours utiliser try-except
- Gérer les cas courants (fichier absent, DLL manquante)
- Fermer proprement les connexions

Vous avez maintenant tous les outils pour connecter vos applications Lazarus à SQLite ! Dans la section suivante, nous verrons comment exécuter des requêtes SQL et manipuler les données.

---

*La connexion est établie. Vous êtes prêt à dialoguer avec votre base de données !*

⏭️ [TSQLQuery et requêtes](/16-bases-donnees-maitrise-approfondie/05-tsqlquery-requetes.md)
