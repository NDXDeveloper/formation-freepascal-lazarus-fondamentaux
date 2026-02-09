🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.3 SQLite : base embarquée

## Qu'est-ce que SQLite ?

**SQLite** est un système de gestion de base de données relationnelle (SGBD) léger, rapide et autonome. C'est le moteur de base de données le plus déployé au monde, présent dans des milliards d'appareils.

### L'analogie du carnet de notes

Pour comprendre SQLite, imaginez la différence entre :

**Une bibliothèque municipale** (base de données traditionnelle) :
- Bâtiment dédié avec personnel
- Système centralisé
- Plusieurs personnes peuvent y accéder simultanément
- Nécessite une infrastructure complexe

**Votre carnet personnel** (SQLite) :
- Simple cahier que vous gardez avec vous
- Autonome et portable
- Vous seul y accédez
- Aucune infrastructure nécessaire

SQLite, c'est le "carnet personnel" des bases de données : simple, portable, et suffisant pour la plupart des besoins !

## Pourquoi SQLite est parfait pour débuter ?

### 1. Zéro configuration

Contrairement à MySQL, PostgreSQL ou SQL Server qui nécessitent :
- Installation d'un serveur
- Configuration de ports réseau
- Gestion d'utilisateurs et de mots de passe
- Services qui tournent en arrière-plan

**SQLite ne nécessite RIEN de tout cela !**

Votre base de données SQLite est simplement un fichier sur votre disque dur. Pas de serveur, pas de configuration compliquée.

### 2. Un simple fichier

Une base de données SQLite est stockée dans **un seul fichier** avec l'extension `.db`, `.sqlite`, ou `.sqlite3`.

**Exemple :**
```
mes_contacts.db        (3 Ko)
ma_bibliotheque.db     (145 Ko)
mon_application.db     (2,5 Mo)
```

Vous pouvez :
- Le copier comme n'importe quel fichier
- Le sauvegarder facilement
- L'envoyer par email
- Le mettre sur une clé USB
- Le partager entre Windows et Linux sans conversion

### 3. Multi-plateforme natif

Le même fichier `.db` fonctionne sans modification sur :
- Windows (32 et 64 bits)
- Linux (toutes distributions)
- macOS
- Android
- iOS
- Et bien d'autres plateformes

C'est parfait pour notre formation multi-plateforme !

### 4. Performances excellentes

Malgré sa simplicité, SQLite est très rapide :
- Milliers d'opérations par seconde
- Idéal pour des bases jusqu'à plusieurs Go
- Utilisé dans Firefox, Chrome, Skype, WhatsApp, etc.

### 5. SQL standard

SQLite utilise le SQL que vous venez d'apprendre. Si vous savez écrire une requête SQL pour SQLite, vous saurez le faire pour PostgreSQL ou MySQL !

## Base embarquée vs Base Client-Serveur

Il est important de comprendre la différence entre ces deux architectures.

### Base de données embarquée (SQLite)

```
┌─────────────────────────────────────┐
│   Votre Application Pascal          │
│   ┌──────────────────────────┐      │
│   │   Code Pascal            │      │
│   │   ├─ Interface           │      │
│   │   ├─ Logique métier      │      │
│   │   └─ Accès BDD (SQLite)  │      │
│   └──────────────────────────┘      │
│   ┌──────────────────────────┐      │
│   │   Bibliothèque SQLite    │      │
│   │   (intégrée)             │      │
│   └──────────────────────────┘      │
│                │                    │
│                ↓                    │
│   ┌──────────────────────────┐      │
│   │   Fichier .db            │      │
│   │   (sur le disque)        │      │
│   └──────────────────────────┘      │
└─────────────────────────────────────┘
```

**Caractéristiques :**
- Tout dans un seul processus
- Accès direct au fichier
- Pas de réseau
- Un seul utilisateur à la fois (généralement)
- Démarrage instantané

### Base de données Client-Serveur (PostgreSQL, MySQL, etc.)

```
┌──────────────────────┐         ┌──────────────────────┐
│  Application Client  │         │  Serveur de BDD      │
│  ┌────────────────┐  │         │  ┌────────────────┐  │
│  │  Code Pascal   │  │         │  │  PostgreSQL    │  │
│  │  Interface     │  │         │  │  MySQL         │  │
│  └────────────────┘  │         │  │  etc.          │  │
│         │            │         │  └────────────────┘  │
│         │ Requêtes   │ Réseau  │         │            │
│         └────────────┼─────────┼─────────┘            │
│                      │         │         │            │
│                      │         │  ┌────────────────┐  │
│                      │         │  │  Fichiers BDD  │  │
│                      │         │  │  (gérés par    │  │
│                      │         │  │   le serveur)  │  │
│                      │         │  └────────────────┘  │
└──────────────────────┘         └──────────────────────┘
```

**Caractéristiques :**
- Processus séparés (client et serveur)
- Communication via le réseau (même en local)
- Plusieurs clients simultanés
- Nécessite un serveur démarré
- Gestion centralisée

### Quand utiliser SQLite ?

**✅ Utilisez SQLite pour :**
- Applications desktop mono-utilisateur
- Prototypes et apprentissage
- Applications mobiles
- Applications embarquées
- Fichiers de configuration avancés
- Applications avec bases de données < 100 Go
- Stockage local de données

**❌ Évitez SQLite pour :**
- Applications web avec beaucoup d'utilisateurs simultanés
- Données nécessitant un accès concurrent intensif
- Besoin de contrôle d'accès très fin
- Bases de données dépassant plusieurs dizaines de Go
- Charge d'écriture très élevée depuis plusieurs sources

**Pour cette formation** : SQLite est parfait ! Il vous permet d'apprendre tous les concepts sans la complexité d'installation d'un serveur.

## Installation et utilisation de SQLite

### Sur Windows

#### Option 1 : Via Lazarus (recommandé)

La bonne nouvelle : **Lazarus inclut déjà le support SQLite** ! Vous n'avez rien à installer de spécial pour l'utiliser dans vos programmes.

Les composants nécessaires sont :
- `TSQLite3Connection` : déjà disponible dans Lazarus
- Bibliothèque SQLite : généralement incluse ou facile à ajouter

#### Option 2 : Outil en ligne de commande (optionnel)

Si vous voulez manipuler vos bases de données en dehors de votre programme, vous pouvez installer l'outil en ligne de commande SQLite :

1. Allez sur https://sqlite.org/download.html
2. Téléchargez "sqlite-tools-win32-x86-*.zip"
3. Décompressez dans un dossier (ex: `C:\sqlite`)
4. Ajoutez ce dossier au PATH (optionnel)

**Utilisation :**
```bash
# Ouvrir une base de données
sqlite3.exe ma_base.db

# Exécuter du SQL
sqlite> SELECT * FROM Clients;
sqlite> .exit
```

### Sur Ubuntu/Linux

#### Option 1 : Via Lazarus (recommandé)

Comme sur Windows, Lazarus inclut le support SQLite. Vous devrez juste installer la bibliothèque SQLite3 :

```bash
sudo apt update
sudo apt install libsqlite3-dev
```

C'est tout ! Vos programmes Lazarus pourront maintenant utiliser SQLite.

#### Option 2 : Outil en ligne de commande

SQLite est souvent déjà installé sur Linux. Vérifiez :

```bash
sqlite3 --version
```

Si ce n'est pas le cas :

```bash
sudo apt install sqlite3
```

**Utilisation :**
```bash
# Ouvrir/créer une base de données
sqlite3 ma_base.db

# Exécuter du SQL
sqlite> CREATE TABLE test (id INTEGER, nom TEXT);
sqlite> INSERT INTO test VALUES (1, 'Bonjour');
sqlite> SELECT * FROM test;
sqlite> .exit
```

## Créer votre première base de données SQLite

### Méthode 1 : En ligne de commande

C'est la méthode la plus simple pour comprendre le concept.

**Sur Windows :**
```bash
cd C:\MesProjets
sqlite3.exe ma_premiere_base.db
```

**Sur Linux :**
```bash
cd ~/MesProjets
sqlite3 ma_premiere_base.db
```

Vous entrez dans le shell SQLite :
```
SQLite version 3.x.x
Enter ".help" for usage hints.
sqlite>
```

**Créons notre première table :**

```sql
CREATE TABLE Contacts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    nom TEXT NOT NULL,
    prenom TEXT,
    email TEXT,
    telephone TEXT
);
```

**Ajoutons des données :**

```sql
INSERT INTO Contacts (nom, prenom, email)
VALUES ('Dupont', 'Pierre', 'pierre.dupont@email.fr');

INSERT INTO Contacts (nom, prenom, email)
VALUES ('Martin', 'Marie', 'marie.martin@email.fr');

INSERT INTO Contacts (nom, prenom, email)
VALUES ('Durand', 'Jacques', 'j.durand@email.fr');
```

**Vérifions :**

```sql
SELECT * FROM Contacts;
```

**Résultat :**
```
1|Dupont|Pierre|pierre.dupont@email.fr|
2|Martin|Marie|marie.martin@email.fr|
3|Durand|Jacques|j.durand@email.fr|
```

**Quittons :**
```
.exit
```

✅ Félicitations ! Vous venez de créer votre première base de données SQLite avec des données dedans !

Si vous regardez votre dossier, vous verrez le fichier `ma_premiere_base.db` qui a été créé.

### Méthode 2 : Via un outil graphique (optionnel)

Si vous préférez une interface graphique, plusieurs outils gratuits existent :

#### DB Browser for SQLite (recommandé)

**Téléchargement :**
- Windows : https://sqlitebrowser.org/dl/
- Ubuntu : `sudo apt install sqlitebrowser`

**Avantages :**
- Interface intuitive
- Création de tables en mode graphique
- Visualisation des données
- Exécution de requêtes SQL
- Multi-plateforme

**Utilisation basique :**
1. Lancez DB Browser
2. "Nouvelle base de données" ou "Ouvrir une base de données"
3. Onglet "Exécuter le SQL" pour taper vos requêtes
4. Onglet "Parcourir les données" pour voir les tables

#### DBeaver Community (plus avancé)

Un outil universel qui supporte SQLite et bien d'autres SGBD. Plus complexe mais très puissant.

## Structure d'un fichier SQLite

### Le fichier .db

Un fichier SQLite contient **tout** :
- La structure des tables
- Les données
- Les index
- Les vues
- Les triggers (déclencheurs)
- Les contraintes

C'est un format binaire propriétaire (mais ouvert). Vous ne pouvez pas l'ouvrir avec un éditeur de texte.

### Taille du fichier

- **Minimum** : environ 4 Ko (base vide)
- **Maximum théorique** : 281 To (téraoctets)
- **Maximum pratique recommandé** : quelques Go

La taille grandit automatiquement quand vous ajoutez des données.

### Fichiers auxiliaires

Lors de l'utilisation, SQLite peut créer des fichiers temporaires :

- `ma_base.db-journal` : journal des transactions (temporaire)
- `ma_base.db-wal` : Write-Ahead Logging (mode WAL)
- `ma_base.db-shm` : mémoire partagée (mode WAL)

Ces fichiers sont normaux et gérés automatiquement par SQLite. Ne les supprimez pas manuellement !

## Commandes spéciales du shell SQLite

Quand vous utilisez `sqlite3` en ligne de commande, certaines commandes spéciales (préfixées par `.`) sont disponibles :

### Informations sur la base

```sql
-- Liste toutes les tables
.tables

-- Affiche la structure d'une table
.schema Contacts

-- Affiche toutes les structures
.schema

-- Informations sur les bases attachées
.databases
```

### Formatage de l'affichage

```sql
-- Mode colonne (plus lisible)
.mode column
.headers on
SELECT * FROM Contacts;

-- Mode ligne
.mode line
SELECT * FROM Contacts;

-- Mode CSV
.mode csv
SELECT * FROM Contacts;
```

### Import/Export

```sql
-- Exporter en SQL
.output export.sql
.dump

-- Exporter une table en CSV
.mode csv
.output contacts.csv
SELECT * FROM Contacts;
.output stdout

-- Importer un CSV
.mode csv
.import contacts.csv Contacts
```

### Autres commandes utiles

```sql
-- Aide
.help

-- Quitter
.exit
-- ou
.quit

-- Afficher le temps d'exécution
.timer on

-- Sauvegarder les modifications
-- (automatique, mais pour forcer :)
.backup ma_sauvegarde.db
```

## Types de données dans SQLite

SQLite a une approche unique des types de données : il est **dynamiquement typé**.

### Les 5 classes de stockage

SQLite stocke les données dans 5 classes :

1. **NULL** : valeur nulle
2. **INTEGER** : entier signé (1 à 8 octets)
3. **REAL** : nombre à virgule flottante (8 octets)
4. **TEXT** : chaîne de caractères (UTF-8, UTF-16)
5. **BLOB** : données binaires brutes

### Types de données déclarés

Quand vous créez une table, vous déclarez des types qui sont **recommandés** mais pas strictement imposés :

```sql
CREATE TABLE Exemple (
    id INTEGER PRIMARY KEY,
    nom TEXT,
    age INTEGER,
    salaire REAL,
    actif BOOLEAN,      -- Stocké comme INTEGER (0 ou 1)
    date_naissance DATE -- Stocké comme TEXT ou INTEGER
);
```

**Particularité SQLite :** Vous pouvez insérer un texte dans une colonne INTEGER ! SQLite essaiera de convertir, mais ne générera pas d'erreur stricte.

```sql
-- Ceci fonctionne dans SQLite (mais c'est une mauvaise pratique !)
INSERT INTO Exemple (age) VALUES ('vingt-cinq');
```

**Conseil :** Même si SQLite est flexible, respectez les types que vous déclarez pour garder un code propre et portable !

### Dates et heures

SQLite n'a pas de type DATE natif. Il utilise :

- **TEXT** : format ISO 8601 `"2025-10-15 14:30:00"`
- **INTEGER** : timestamp Unix (secondes depuis 1970)
- **REAL** : jour Julien

**Fonctions de dates :**

```sql
-- Date actuelle
SELECT date('now');           -- 2025-10-15

-- Date et heure
SELECT datetime('now');       -- 2025-10-15 14:30:00

-- Timestamp Unix
SELECT strftime('%s', 'now'); -- 1729001400

-- Formatage personnalisé
SELECT strftime('%d/%m/%Y', 'now'); -- 15/10/2025

-- Calculs de dates
SELECT date('now', '+7 days');      -- Dans 7 jours
SELECT date('now', '-1 month');     -- Il y a 1 mois
SELECT date('now', 'start of year'); -- 1er janvier
```

## Contraintes et intégrité dans SQLite

### PRIMARY KEY AUTOINCREMENT

```sql
CREATE TABLE Produits (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    nom TEXT NOT NULL
);
```

`AUTOINCREMENT` garantit que l'ID ne sera jamais réutilisé, même après suppression.

### UNIQUE

```sql
CREATE TABLE Utilisateurs (
    id INTEGER PRIMARY KEY,
    email TEXT UNIQUE,
    username TEXT UNIQUE
);
```

### CHECK

```sql
CREATE TABLE Produits (
    id INTEGER PRIMARY KEY,
    nom TEXT NOT NULL,
    prix REAL CHECK(prix > 0),
    stock INTEGER CHECK(stock >= 0)
);
```

### FOREIGN KEY

**Important :** Les clés étrangères sont **désactivées par défaut** dans SQLite !

Pour les activer :

```sql
PRAGMA foreign_keys = ON;
```

Puis créez vos tables :

```sql
CREATE TABLE Clients (
    id INTEGER PRIMARY KEY,
    nom TEXT NOT NULL
);

CREATE TABLE Commandes (
    id INTEGER PRIMARY KEY,
    date TEXT,
    id_client INTEGER,
    FOREIGN KEY (id_client) REFERENCES Clients(id)
);
```

**À exécuter à chaque connexion :**
```sql
PRAGMA foreign_keys = ON;
```

### DEFAULT

```sql
CREATE TABLE Articles (
    id INTEGER PRIMARY KEY,
    titre TEXT NOT NULL,
    date_creation TEXT DEFAULT (datetime('now')),
    vues INTEGER DEFAULT 0,
    publie INTEGER DEFAULT 1
);
```

## Transactions dans SQLite

Par défaut, SQLite enveloppe chaque commande dans une transaction automatique.

### Transaction manuelle

```sql
-- Commencer une transaction
BEGIN TRANSACTION;

-- Vos commandes SQL
INSERT INTO Comptes (id, solde) VALUES (1, 1000);
UPDATE Comptes SET solde = solde - 100 WHERE id = 1;
INSERT INTO Historique (id_compte, montant) VALUES (1, -100);

-- Valider
COMMIT;

-- Ou annuler en cas d'erreur
-- ROLLBACK;
```

**Avantages :**
- Atomicité : tout ou rien
- Performance : beaucoup plus rapide que des commandes séparées

**Exemple pratique :**

```sql
BEGIN TRANSACTION;

-- Si une erreur se produit, toute la transaction est annulée
UPDATE Comptes SET solde = solde - 500 WHERE id = 1;
UPDATE Comptes SET solde = solde + 500 WHERE id = 2;

COMMIT; -- Les deux comptes sont modifiés, ou aucun
```

## Limitations de SQLite

Il est important de connaître les limitations pour faire les bons choix :

### Limitations techniques

- **Pas de modification de colonnes** : ALTER TABLE est limité
- **Pas de procédures stockées** : la logique doit être dans votre application
- **Concurrence limitée** : un seul écrivain à la fois
- **Pas de gestion d'utilisateurs** : pas de système de permissions intégré
- **Typage dynamique** : peut être déroutant si on vient d'autres SGBD

### Limitations pratiques

- **Performance en écriture** : si beaucoup d'utilisateurs écrivent simultanément
- **Réseau** : pas conçu pour l'accès réseau (NFS, partages Windows)
- **Très grandes bases** : au-delà de quelques Go, un serveur est plus adapté

**Mais pour 95% des applications desktop, ces limitations n'ont aucun impact !**

## Avantages de SQLite récapitulés

### Pour l'apprentissage

✅ Aucune configuration  
✅ Pas de serveur à gérer  
✅ Erreurs claires et simples  
✅ Démarrage instantané  
✅ Parfait pour expérimenter

### Pour le développement

✅ Déploiement simple (un fichier)  
✅ Multi-plateforme natif  
✅ Pas de dépendances externes  
✅ Tests faciles (copier/supprimer des fichiers)  
✅ Sauvegarde simple (copie de fichier)

### Pour la production

✅ Fiable et mature (20+ ans)  
✅ Très bien testé  
✅ Performances excellentes  
✅ Footprint minimal  
✅ Domaine public (aucune licence)

## Outils de développement recommandés

### 1. Lazarus + Composants SQLdb

C'est ce que nous utiliserons principalement dans cette formation. Tout est intégré !

### 2. DB Browser for SQLite

Pour explorer et tester vos bases de données visuellement.

### 3. SQLite CLI

Pour des opérations rapides en ligne de commande.

### 4. Plugins navigateurs web

- **Firefox** : SQLite Manager (extension)
- **Chrome** : Several SQLite viewers

Utile pour examiner les bases SQLite utilisées par les applications.

## Exemple complet : Base de données bibliothèque

Créons ensemble une base de données complète pour gérer une bibliothèque personnelle.

### Création de la base

```bash
sqlite3 ma_bibliotheque.db
```

### Activation des clés étrangères

```sql
PRAGMA foreign_keys = ON;
```

### Création des tables

```sql
-- Table des auteurs
CREATE TABLE Auteurs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    nom TEXT NOT NULL,
    prenom TEXT,
    nationalite TEXT,
    date_naissance TEXT
);

-- Table des genres
CREATE TABLE Genres (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    nom TEXT NOT NULL UNIQUE
);

-- Table des livres
CREATE TABLE Livres (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    titre TEXT NOT NULL,
    isbn TEXT UNIQUE,
    annee INTEGER,
    pages INTEGER,
    id_auteur INTEGER NOT NULL,
    id_genre INTEGER,
    date_ajout TEXT DEFAULT (date('now')),
    lu INTEGER DEFAULT 0,
    FOREIGN KEY (id_auteur) REFERENCES Auteurs(id),
    FOREIGN KEY (id_genre) REFERENCES Genres(id)
);
```

### Insertion de données

```sql
-- Ajout d'auteurs
INSERT INTO Auteurs (nom, prenom, nationalite) VALUES
    ('Hugo', 'Victor', 'Française'),
    ('Dumas', 'Alexandre', 'Française'),
    ('Orwell', 'George', 'Britannique');

-- Ajout de genres
INSERT INTO Genres (nom) VALUES
    ('Roman'),
    ('Science-Fiction'),
    ('Dystopie'),
    ('Aventure');

-- Ajout de livres
INSERT INTO Livres (titre, isbn, annee, pages, id_auteur, id_genre) VALUES
    ('Les Misérables', '978-2070409228', 1862, 1664, 1, 1),
    ('Notre-Dame de Paris', '978-2070413089', 1831, 752, 1, 1),
    ('Le Comte de Monte-Cristo', '978-2253098058', 1844, 1504, 2, 4),
    ('1984', '978-0451524935', 1949, 328, 3, 3);
```

### Requêtes utiles

```sql
-- Tous les livres avec nom d'auteur et genre
SELECT
    L.titre,
    A.prenom || ' ' || A.nom AS auteur,
    G.nom AS genre,
    L.annee,
    L.lu
FROM Livres L
INNER JOIN Auteurs A ON L.id_auteur = A.id
LEFT JOIN Genres G ON L.id_genre = G.id
ORDER BY L.titre;

-- Livres non lus
SELECT titre, annee
FROM Livres
WHERE lu = 0;

-- Nombre de livres par auteur
SELECT
    A.nom,
    COUNT(*) AS nb_livres
FROM Auteurs A
INNER JOIN Livres L ON A.id = L.id_auteur
GROUP BY A.id, A.nom
ORDER BY nb_livres DESC;

-- Marquer un livre comme lu
UPDATE Livres SET lu = 1 WHERE id = 1;
```

### Sauvegarde de la structure

```sql
.output schema.sql
.schema
.output stdout
```

Vous avez maintenant une base de données complète et fonctionnelle !

## Transition vers FreePascal/Lazarus

Dans les sections suivantes, nous verrons comment :

1. **Se connecter** à cette base depuis un programme Pascal
2. **Exécuter** ces requêtes SQL depuis le code
3. **Afficher** les données dans une interface graphique
4. **Créer des formulaires** pour ajouter/modifier/supprimer des données

Mais avant de passer au code, assurez-vous de bien comprendre :

- ✓ Ce qu'est SQLite et pourquoi c'est pratique
- ✓ Comment créer une base de données
- ✓ Les types de données SQLite
- ✓ Les commandes SQL de base
- ✓ Le concept de transactions

## Résumé

**SQLite est :**
- Un SGBD embarqué (pas de serveur)
- Stocké dans un simple fichier
- Multi-plateforme
- Gratuit et open source
- Parfait pour débuter et pour de nombreuses applications

**Pour l'utiliser :**
- Rien à installer avec Lazarus (support intégré)
- Optionnellement : sqlite3 CLI ou DB Browser
- Créer une base = créer un fichier `.db`

**Points clés :**
- Activer `PRAGMA foreign_keys = ON`
- Utiliser les transactions pour les performances
- Respecter les types même si SQLite est flexible
- Sauvegarder = copier le fichier `.db`

Vous êtes maintenant prêt à intégrer SQLite dans vos applications Pascal/Lazarus !

---

*SQLite sera votre compagnon fidèle pour tous vos projets nécessitant une base de données. Simple mais puissant !*

⏭️ [Composants de connexion](/16-bases-donnees-maitrise-approfondie/04-composants-connexion.md)
