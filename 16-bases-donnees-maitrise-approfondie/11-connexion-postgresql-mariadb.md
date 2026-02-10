🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.11 Connexion à PostgreSQL ou MariaDB

## Introduction : passer au client/serveur

Vous avez maîtrisé SQLite et compris les concepts des bases client/serveur. Il est temps de passer à la pratique ! Cette section vous guide pas à pas pour installer et utiliser PostgreSQL ou MariaDB avec vos applications Lazarus.

### Que vais-je apprendre ?

À la fin de cette section, vous saurez :
- ✅ Installer PostgreSQL ou MariaDB sur Windows et Ubuntu
- ✅ Configurer le serveur de base de données
- ✅ Créer des utilisateurs et des bases
- ✅ Connecter Lazarus au serveur
- ✅ Adapter votre code SQLite pour PostgreSQL/MariaDB
- ✅ Résoudre les problèmes courants

### Ne vous inquiétez pas !

L'installation peut sembler intimidante, mais :
- 📖 Je détaille chaque étape
- 🖼️ Les concepts restent les mêmes qu'avec SQLite
- 💻 Le code Lazarus change très peu
- 🔧 Une fois installé, c'est aussi simple que SQLite

## Choisir : PostgreSQL ou MariaDB ?

### PostgreSQL - Le choix recommandé pour ce tutoriel

**Pourquoi PostgreSQL ?**
- ✅ Plus riche en fonctionnalités
- ✅ Meilleure conformité aux standards SQL
- ✅ Excellent pour l'apprentissage
- ✅ Documentation exceptionnelle
- ✅ Très utilisé professionnellement

**Pour qui ?**
- Débutants voulant apprendre les bonnes pratiques
- Projets nécessitant l'intégrité des données
- Applications complexes

### MariaDB - L'alternative compatible MySQL

**Pourquoi MariaDB ?**
- ✅ 100% open source (contrairement à MySQL)
- ✅ Compatible avec MySQL (facile de migrer)
- ✅ Très populaire dans le monde du web
- ✅ Plus simple à administrer

**Pour qui ?**
- Projets web (PHP, WordPress, etc.)
- Besoin de compatibilité MySQL
- Préférence pour la simplicité

### Mon conseil

**Pour ce tutoriel, je recommande PostgreSQL** car :
1. Meilleur pour apprendre les concepts avancés
2. Plus strict (vous apprend les bonnes pratiques)
3. Sera un excellent atout professionnel

Mais **les deux sections sont fournies**. Choisissez selon vos besoins !

---

# PARTIE A : PostgreSQL

## Installation de PostgreSQL

### Sur Windows

#### Étape 1 : Télécharger l'installateur

1. Allez sur https://www.postgresql.org/download/windows/
2. Cliquez sur "Download the installer"
3. Choisissez la dernière version stable (par exemple, PostgreSQL 16.x)
4. Téléchargez la version 64-bit pour Windows

**Fichier téléchargé :** `postgresql-16.x-windows-x64.exe` (environ 300 Mo)

#### Étape 2 : Lancer l'installation

1. **Double-cliquez** sur l'installateur
2. Cliquez sur **Next**

#### Étape 3 : Répertoire d'installation

- Laisser le répertoire par défaut : `C:\Program Files\PostgreSQL\16`
- Cliquez sur **Next**

#### Étape 4 : Sélection des composants

**Cochez :**
- ☑ PostgreSQL Server (obligatoire)
- ☑ pgAdmin 4 (interface graphique - recommandé)
- ☑ Stack Builder (optionnel)
- ☑ Command Line Tools (recommandé)

Cliquez sur **Next**

#### Étape 5 : Répertoire des données

- Laisser le défaut : `C:\Program Files\PostgreSQL\16\data`
- Cliquez sur **Next**

#### Étape 6 : Mot de passe du superutilisateur

**IMPORTANT !** Choisissez un mot de passe pour l'utilisateur `postgres` (administrateur).

**Exemple :** `admin123` (pour le développement local)

**⚠️ Notez ce mot de passe**, vous en aurez besoin !

Cliquez sur **Next**

#### Étape 7 : Port

- Laisser le port par défaut : **5432**
- Cliquez sur **Next**

#### Étape 8 : Locale

- Laisser le défaut (généralement "French, France")
- Cliquez sur **Next**

#### Étape 9 : Résumé et installation

- Vérifiez les paramètres
- Cliquez sur **Next**
- L'installation commence (quelques minutes)
- Cliquez sur **Finish**

#### Étape 10 : Vérifier l'installation

1. Ouvrez le menu Démarrer
2. Cherchez "pgAdmin 4"
3. Lancez pgAdmin 4
4. Entrez le mot de passe que vous avez défini
5. Cliquez sur "Servers" → "PostgreSQL 16" → "Databases"
6. Vous devriez voir la base "postgres" (base par défaut)

**✅ PostgreSQL est installé et fonctionne !**

### Sur Ubuntu/Linux

#### Étape 1 : Mettre à jour les paquets

```bash
sudo apt update
```

#### Étape 2 : Installer PostgreSQL

```bash
sudo apt install postgresql postgresql-contrib
```

Cette commande installe :
- Le serveur PostgreSQL
- Les outils en ligne de commande
- Des extensions utiles

**Durée :** 2-3 minutes

#### Étape 3 : Vérifier l'installation

```bash
sudo systemctl status postgresql
```

Vous devriez voir :
```
● postgresql.service - PostgreSQL RDBMS
   Active: active (running)
```

Si ce n'est pas le cas :
```bash
sudo systemctl start postgresql
```

#### Étape 4 : Configurer l'utilisateur postgres

Par défaut, PostgreSQL crée un utilisateur système `postgres`. Définissez un mot de passe :

```bash
sudo -u postgres psql
```

Vous êtes maintenant dans l'interface PostgreSQL. Tapez :

```sql
ALTER USER postgres PASSWORD 'admin123';
\q
```

**Explication :**
- `ALTER USER postgres PASSWORD 'admin123';` : définit le mot de passe
- `\q` : quitte psql

#### Étape 5 : Autoriser les connexions locales

Éditez le fichier de configuration :

```bash
sudo nano /etc/postgresql/*/main/pg_hba.conf
```

Trouvez la ligne :
```
local   all             postgres                                peer
```

Changez `peer` en `md5` :
```
local   all             postgres                                md5
```

Sauvegardez (Ctrl+O, Entrée, Ctrl+X)

Redémarrez PostgreSQL :
```bash
sudo systemctl restart postgresql
```

**✅ PostgreSQL est installé et configuré !**

## Configuration de PostgreSQL

### Créer une base de données pour vos tests

#### Avec pgAdmin (Windows)

1. Ouvrez **pgAdmin 4**
2. Connectez-vous au serveur
3. Clic droit sur **Databases** → **Create** → **Database...**
4. **Database:** `ma_base_test`
5. **Owner:** `postgres`
6. Cliquez sur **Save**

#### En ligne de commande (Windows et Linux)

**Windows :**
Ouvrez "SQL Shell (psql)" depuis le menu Démarrer.

**Linux :**
```bash
sudo -u postgres psql
```

**Puis :**
```sql
CREATE DATABASE ma_base_test;
\l
```

La commande `\l` liste les bases. Vous devriez voir `ma_base_test`.

### Créer un utilisateur pour votre application

**Bonne pratique :** Ne pas utiliser le superutilisateur `postgres` dans vos applications.

```sql
-- Créer un utilisateur
CREATE USER mon_app WITH PASSWORD 'motdepasse_app';

-- Donner les droits sur la base
GRANT ALL PRIVILEGES ON DATABASE ma_base_test TO mon_app;

-- Quitter
\q
```

**Vous avez maintenant :**
- Base de données : `ma_base_test`
- Utilisateur : `mon_app`
- Mot de passe : `motdepasse_app`

## Connexion depuis Lazarus

### Installer le composant PostgreSQL

#### Vérifier l'installation

1. Ouvrez Lazarus
2. Menu **Paquets** → **Ouvrir un paquet chargé (.lpk)**
3. Cherchez **PQConnection** ou **SQLDBLaz**

Si vous ne le trouvez pas, installez-le :

1. **Paquets** → **Ouvrir un fichier de paquet (.lpk)**
2. Naviguez vers `[lazarus]/components/sqldb/`
3. Ouvrez `sqldblaz.lpk`
4. Cliquez sur **Compiler** puis **Utiliser** → **Installer**
5. Lazarus se recompilera

#### Installer les bibliothèques PostgreSQL

**Windows :**
Les bibliothèques sont installées avec PostgreSQL. Ajoutez le dossier bin au PATH :

`C:\Program Files\PostgreSQL\16\bin`

Ou copiez `libpq.dll` dans le dossier de votre projet.

**Ubuntu :**
```bash
sudo apt install libpq-dev
```

### Composants nécessaires

Dans Lazarus, vous utiliserez :
- `TPQConnection` (au lieu de TSQLite3Connection)
- `TSQLTransaction` (même composant)
- `TSQLQuery` (même composant)
- `TDataSource` (même composant)

**La bonne nouvelle :** Seul le composant de connexion change !

### Premier projet avec PostgreSQL

#### Étape 1 : Créer le projet

1. **Projet** → **Nouveau projet** → **Application**
2. Enregistrez dans un dossier `ProjetPostgreSQL`

#### Étape 2 : Ajouter les composants

**Palette SQLdb :**
- `TPQConnection` → renommez en `PQConnection1`
- `TSQLTransaction` → `SQLTransaction1`
- `TSQLQuery` → `SQLQuery1`

**Palette Data Access :**
- `TDataSource` → `DataSource1`

#### Étape 3 : Configurer TPQConnection

**Propriétés dans l'Inspecteur d'objets :**

**HostName :** `localhost` (ou l'adresse de votre serveur)  
**DatabaseName :** `ma_base_test`  
**UserName :** `mon_app`  
**Password :** `motdepasse_app`  
**CharSet :** `UTF8`  
**Transaction :** `SQLTransaction1`  

**Ne PAS définir Password dans l'Inspecteur pour la sécurité !** Définissez-le en code.

#### Étape 4 : Code de connexion

```pascal
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls,
  sqldb, pqconnection;

type
  TForm1 = class(TForm)
    PQConnection1: TPQConnection;
    SQLTransaction1: TSQLTransaction;
    SQLQuery1: TSQLQuery;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Configuration de la connexion
  PQConnection1.HostName := 'localhost';
  PQConnection1.DatabaseName := 'ma_base_test';
  PQConnection1.UserName := 'mon_app';
  PQConnection1.Password := 'motdepasse_app';
  PQConnection1.CharSet := 'UTF8';

  // Lier les composants
  PQConnection1.Transaction := SQLTransaction1;
  SQLTransaction1.Database := PQConnection1;
  SQLQuery1.Database := PQConnection1;

  try
    // Ouvrir la connexion
    PQConnection1.Open;
    Memo1.Lines.Add('✅ Connexion réussie à PostgreSQL !');
  except
    on E: Exception do
    begin
      Memo1.Lines.Add('❌ Erreur : ' + E.Message);
      ShowMessage('Impossible de se connecter : ' + E.Message);
    end;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);  
begin  
  try
    // Créer une table de test
    SQLQuery1.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS test (' +
      '  id SERIAL PRIMARY KEY,' +
      '  nom VARCHAR(100),' +
      '  date_creation TIMESTAMP DEFAULT CURRENT_TIMESTAMP' +
      ')';
    SQLQuery1.ExecSQL;
    SQLTransaction1.Commit;

    // Insérer des données
    SQLQuery1.SQL.Text :=
      'INSERT INTO test (nom) VALUES (:nom)';
    SQLQuery1.ParamByName('nom').AsString := 'Test PostgreSQL';
    SQLQuery1.ExecSQL;
    SQLTransaction1.Commit;

    // Lire les données
    SQLQuery1.SQL.Text := 'SELECT * FROM test ORDER BY id';
    SQLQuery1.Open;

    Memo1.Lines.Add('');
    Memo1.Lines.Add('Données dans la table :');
    while not SQLQuery1.EOF do
    begin
      Memo1.Lines.Add(Format('ID: %d, Nom: %s',
        [SQLQuery1.FieldByName('id').AsInteger,
         SQLQuery1.FieldByName('nom').AsString]));
      SQLQuery1.Next;
    end;

    SQLQuery1.Close;

  except
    on E: Exception do
    begin
      SQLTransaction1.Rollback;
      ShowMessage('Erreur : ' + E.Message);
    end;
  end;
end;

end.
```

#### Étape 5 : Tester

1. Compilez et exécutez (F9)
2. Si tout va bien, vous verrez "✅ Connexion réussie à PostgreSQL !"
3. Cliquez sur le bouton pour créer une table et insérer des données

**✅ Votre première application PostgreSQL fonctionne !**

---

# PARTIE B : MariaDB / MySQL

## Installation de MariaDB

### Sur Windows

#### Étape 1 : Télécharger MariaDB

1. Allez sur https://mariadb.org/download/
2. Choisissez la dernière version stable (par exemple, 11.x)
3. **Package :** MSI Package
4. Téléchargez pour Windows (64-bit)

**Fichier :** `mariadb-11.x-winx64.msi` (environ 100 Mo)

#### Étape 2 : Installation

1. Double-cliquez sur l'installateur
2. Cliquez sur **Next**
3. Acceptez la licence → **Next**
4. Laissez les options par défaut → **Next**

#### Étape 3 : Mot de passe root

**Définissez un mot de passe** pour l'utilisateur `root` (administrateur)

**Exemple :** `admin123`

☑ **Cochez :** "Enable access from remote machines" (pour le développement)

Cliquez sur **Next**

#### Étape 4 : Configuration

- **Service Name :** MySQL (laisser par défaut)
- ☑ **Cochez :** "Install as service"
- ☑ **Cochez :** "Enable networking"
- **Port :** 3306 (défaut)

Cliquez sur **Next** puis **Install**

#### Étape 5 : Fin de l'installation

Cliquez sur **Finish**

#### Étape 6 : Vérifier

Ouvrez l'invite de commande et tapez :

```cmd
mysql -u root -p
```

Entrez le mot de passe. Vous devriez voir :
```
Welcome to the MariaDB monitor.  
MariaDB [(none)]>  
```

Tapez `exit` pour quitter.

**✅ MariaDB est installé !**

### Sur Ubuntu/Linux

#### Étape 1 : Installer MariaDB

```bash
sudo apt update  
sudo apt install mariadb-server mariadb-client  
```

#### Étape 2 : Sécuriser l'installation

```bash
sudo mysql_secure_installation
```

Répondez aux questions :
- **Enter current password for root:** (Appuyez sur Entrée si aucun)
- **Switch to unix_socket authentication?** N
- **Change the root password?** Y → Entrez `admin123`
- **Remove anonymous users?** Y
- **Disallow root login remotely?** N (pour développement)
- **Remove test database?** Y
- **Reload privilege tables?** Y

#### Étape 3 : Démarrer MariaDB

```bash
sudo systemctl start mariadb  
sudo systemctl enable mariadb  
```

#### Étape 4 : Vérifier

```bash
sudo mysql -u root -p
```

Entrez le mot de passe `admin123`.

**✅ MariaDB est installé !**

## Configuration de MariaDB

### Créer une base de données

```sql
CREATE DATABASE ma_base_test CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;  
SHOW DATABASES;  
```

### Créer un utilisateur

```sql
-- Créer l'utilisateur
CREATE USER 'mon_app'@'%' IDENTIFIED BY 'motdepasse_app';

-- Donner tous les droits sur la base
GRANT ALL PRIVILEGES ON ma_base_test.* TO 'mon_app'@'%';

-- Appliquer les changements
FLUSH PRIVILEGES;

-- Quitter
exit
```

**Notes :**
- `'mon_app'@'%'` : utilisateur accessible depuis n'importe où
- `'mon_app'@'localhost'` : uniquement depuis la machine locale

## Connexion depuis Lazarus

### Installer le composant MySQL

#### Vérifier

Comme pour PostgreSQL, vérifiez que **SQLDBLaz** est installé.

#### Bibliothèques

**Windows :**
Copiez `libmysql.dll` (ou `libmariadb.dll`) dans le dossier de votre projet.

Trouvez-la dans : `C:\Program Files\MariaDB\lib\`

**Ubuntu :**
```bash
sudo apt install libmariadb-dev
```

### Composants nécessaires

- `TMySQL80Connection` (ou TMySQL57Connection selon version)
- `TSQLTransaction`
- `TSQLQuery`
- `TDataSource`

### Premier projet avec MariaDB

#### Configuration

```pascal
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls,
  sqldb, mysql80conn;  // ou mysql57conn

type
  TForm1 = class(TForm)
    MySQL80Connection1: TMySQL80Connection;
    SQLTransaction1: TSQLTransaction;
    SQLQuery1: TSQLQuery;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Configuration
  MySQL80Connection1.HostName := 'localhost';
  MySQL80Connection1.DatabaseName := 'ma_base_test';
  MySQL80Connection1.UserName := 'mon_app';
  MySQL80Connection1.Password := 'motdepasse_app';
  MySQL80Connection1.CharSet := 'utf8mb4';

  // Liaisons
  MySQL80Connection1.Transaction := SQLTransaction1;
  SQLTransaction1.Database := MySQL80Connection1;
  SQLQuery1.Database := MySQL80Connection1;

  try
    MySQL80Connection1.Open;
    Memo1.Lines.Add('✅ Connexion réussie à MariaDB !');
  except
    on E: Exception do
    begin
      Memo1.Lines.Add('❌ Erreur : ' + E.Message);
      ShowMessage('Connexion impossible : ' + E.Message);
    end;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);  
begin  
  try
    // Créer une table
    SQLQuery1.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS test (' +
      '  id INT AUTO_INCREMENT PRIMARY KEY,' +
      '  nom VARCHAR(100),' +
      '  date_creation TIMESTAMP DEFAULT CURRENT_TIMESTAMP' +
      ')';
    SQLQuery1.ExecSQL;
    SQLTransaction1.Commit;

    // Insérer
    SQLQuery1.SQL.Text :=
      'INSERT INTO test (nom) VALUES (:nom)';
    SQLQuery1.ParamByName('nom').AsString := 'Test MariaDB';
    SQLQuery1.ExecSQL;
    SQLTransaction1.Commit;

    // Lire
    SQLQuery1.SQL.Text := 'SELECT * FROM test ORDER BY id';
    SQLQuery1.Open;

    Memo1.Lines.Add('');
    Memo1.Lines.Add('Données :');
    while not SQLQuery1.EOF do
    begin
      Memo1.Lines.Add(Format('ID: %d, Nom: %s',
        [SQLQuery1.FieldByName('id').AsInteger,
         SQLQuery1.FieldByName('nom').AsString]));
      SQLQuery1.Next;
    end;

    SQLQuery1.Close;

  except
    on E: Exception do
    begin
      SQLTransaction1.Rollback;
      ShowMessage('Erreur : ' + E.Message);
    end;
  end;
end;

end.
```

**✅ Votre application MariaDB fonctionne !**

---

# Différences avec SQLite

## Types de données

### PostgreSQL

```sql
-- SQLite
id INTEGER PRIMARY KEY AUTOINCREMENT

-- PostgreSQL
id SERIAL PRIMARY KEY
-- ou
id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY
```

### MariaDB/MySQL

```sql
-- SQLite
id INTEGER PRIMARY KEY AUTOINCREMENT

-- MariaDB
id INT AUTO_INCREMENT PRIMARY KEY
```

## Fonctions de date

### SQLite

```sql
SELECT date('now');  
SELECT datetime('now');  
```

### PostgreSQL

```sql
SELECT CURRENT_DATE;  
SELECT CURRENT_TIMESTAMP;  
SELECT NOW();  
```

### MariaDB

```sql
SELECT CURDATE();  
SELECT NOW();  
```

## Limite de résultats

### SQLite et PostgreSQL

```sql
SELECT * FROM clients LIMIT 10;
```

### MariaDB (identique)

```sql
SELECT * FROM clients LIMIT 10;
```

## Concaténation de chaînes

### SQLite

```sql
SELECT prenom || ' ' || nom FROM clients;
```

### PostgreSQL (identique)

```sql
SELECT prenom || ' ' || nom FROM clients;
```

### MariaDB

```sql
SELECT CONCAT(prenom, ' ', nom) FROM clients;
```

## Résolution de problèmes

### Erreur : "Can't connect to server"

**Cause :** Le serveur n'est pas démarré ou ne répond pas

**Solution Windows :**
```
Services → PostgreSQL / MySQL → Démarrer
```

**Solution Linux :**
```bash
# PostgreSQL
sudo systemctl start postgresql

# MariaDB
sudo systemctl start mariadb
```

### Erreur : "Access denied for user"

**Cause :** Mauvais utilisateur/mot de passe ou permissions

**Solution :**
1. Vérifiez l'utilisateur et le mot de passe
2. Vérifiez les permissions (`GRANT`)
3. Pour PostgreSQL, vérifiez `pg_hba.conf`

### Erreur : "Library not found"

**Cause :** Bibliothèques client manquantes

**Solution Windows :**
Copiez `libpq.dll` (PostgreSQL) ou `libmysql.dll` (MariaDB) dans le dossier de votre .exe

**Solution Linux :**
```bash
# PostgreSQL
sudo apt install libpq-dev

# MariaDB
sudo apt install libmariadb-dev
```

### Erreur de port déjà utilisé

**Cause :** Le port est occupé par un autre programme

**Solution :**
Changez le port dans la configuration du serveur ou arrêtez l'autre programme.

## Bonnes pratiques

### 1. Ne jamais utiliser root/postgres en production

Créez toujours un utilisateur dédié avec permissions limitées.

### 2. Stocker les mots de passe de façon sécurisée

```pascal
// MAL : mot de passe en dur
MySQL80Connection1.Password := 'motdepasse';

// MIEUX : fichier de configuration
MySQL80Connection1.Password := LireMotDePasseConfig();

// ENCORE MIEUX : variable d'environnement
MySQL80Connection1.Password := GetEnvironmentVariable('DB_PASSWORD');
```

### 3. Toujours utiliser les paramètres

```pascal
// MAL
SQLQuery1.SQL.Text := 'SELECT * FROM users WHERE name = ''' + Edit1.Text + '''';

// BIEN
SQLQuery1.SQL.Text := 'SELECT * FROM users WHERE name = :name';  
SQLQuery1.ParamByName('name').AsString := Edit1.Text;  
```

### 4. Gérer les erreurs de connexion

```pascal
procedure TForm1.SeConnecter;  
var  
  Tentatives: Integer;
begin
  Tentatives := 0;

  while Tentatives < 3 do
  begin
    try
      PQConnection1.Open;
      ShowMessage('Connecté !');
      Break;
    except
      on E: Exception do
      begin
        Inc(Tentatives);
        if Tentatives >= 3 then
        begin
          ShowMessage('Impossible de se connecter après 3 tentatives');
          Application.Terminate;
        end
        else
          Sleep(1000);  // Attendre 1 seconde avant de réessayer
      end;
    end;
  end;
end;
```

### 5. Fermer proprement les connexions

```pascal
procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);  
begin  
  if PQConnection1.Connected then
  begin
    try
      if SQLTransaction1.Active then
        SQLTransaction1.Commit;
    except
      SQLTransaction1.Rollback;
    end;

    PQConnection1.Close;
  end;
end;
```

## Résumé

**Installation :**
- PostgreSQL : installateur Windows ou `apt install postgresql`
- MariaDB : installateur Windows ou `apt install mariadb-server`

**Configuration :**
- Définir un mot de passe pour postgres/root
- Créer une base de données
- Créer un utilisateur applicatif

**Connexion Lazarus :**
- `TPQConnection` pour PostgreSQL
- `TMySQL80Connection` pour MariaDB
- Même logique qu'avec SQLite (changement minimal)

**Différences principales :**
- `AUTOINCREMENT` → `SERIAL` (PostgreSQL) ou `AUTO_INCREMENT` (MariaDB)
- Fonctions de dates différentes
- Concaténation avec `CONCAT()` pour MariaDB

**Avantages acquis :**
- ✅ Multi-utilisateurs simultanés
- ✅ Meilleures performances pour gros volumes
- ✅ Sécurité avancée (utilisateurs, permissions)
- ✅ Accès réseau natif
- ✅ Compétence professionnelle valorisée

Vous maîtrisez maintenant les trois types de bases de données :
1. **SQLite** : Simple et embarqué
2. **PostgreSQL** : Puissant et conforme aux standards
3. **MariaDB** : Populaire et performant

Félicitations ! Vous êtes prêt pour des applications professionnelles !

---

*De SQLite à PostgreSQL/MariaDB : vous êtes maintenant un développeur base de données complet !*

⏭️ [Gestion avancée des transactions (BEGIN, COMMIT, ROLLBACK)](/16-bases-donnees-maitrise-approfondie/12-gestion-avancee-transactions.md)
