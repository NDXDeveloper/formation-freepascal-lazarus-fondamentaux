🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.2 Introduction au SQL

## Qu'est-ce que le SQL ?

**SQL** (Structured Query Language, prononcé "S-Q-L" ou "Siquel") est le langage universel pour communiquer avec les bases de données relationnelles. C'est un peu comme l'anglais dans le monde de l'aviation : peu importe où vous êtes, c'est le langage standard que tout le monde comprend.

### Une analogie simple

Imaginez que votre base de données soit une bibliothèque et que le bibliothécaire soit le système de gestion de base de données (SGBD). Vous ne pouvez pas entrer directement dans les réserves pour chercher un livre. Vous devez demander au bibliothécaire, et pour cela, vous devez parler sa langue : le SQL.

**Exemples de "conversations" en SQL :**

- "Donne-moi tous les livres de Victor Hugo" → `SELECT * FROM Livres WHERE auteur = 'Victor Hugo'`
- "Ajoute ce nouveau client dans le registre" → `INSERT INTO Clients VALUES (...)`
- "Change le prix de ce produit" → `UPDATE Produits SET prix = 19.99 WHERE id = 42`
- "Supprime cette commande annulée" → `DELETE FROM Commandes WHERE id = 101`

### Un langage, pas un programme

**Important :** SQL n'est pas un langage de programmation comme Pascal. C'est un **langage de requête** spécialisé. Vous ne pouvez pas créer une application complète en SQL seul, mais vous en avez besoin pour dialoguer avec vos données.

C'est pourquoi vous utiliserez SQL **depuis** vos programmes Pascal : votre code Pascal contrôle la logique de l'application, et envoie des commandes SQL pour manipuler les données.

## Pourquoi SQL est-il si important ?

### 1. Universalité

SQL est standardisé (norme ISO/ANSI). Cela signifie que :

- Le même SQL fonctionne sur SQLite, PostgreSQL, MySQL, MariaDB, Oracle, SQL Server...
- Une fois appris, vous pouvez travailler avec n'importe quelle base de données
- Votre code SQL peut être transféré d'une base à une autre avec peu de modifications

### 2. Simplicité relative

Contrairement aux langages de programmation, SQL utilise des mots proches de l'anglais courant :

- `SELECT` (sélectionne)
- `FROM` (depuis)
- `WHERE` (où)
- `INSERT` (insère)
- `UPDATE` (met à jour)
- `DELETE` (supprime)

C'est presque de l'anglais lisible !

### 3. Puissance

Avec quelques lignes de SQL, vous pouvez :

- Rechercher parmi des millions d'enregistrements en quelques millisecondes
- Combiner des données de plusieurs tables
- Effectuer des calculs complexes (sommes, moyennes, comptages)
- Trier et filtrer les données selon vos besoins

### 4. Compétence professionnelle

Dans le monde professionnel, SQL est omniprésent :

- Développeurs web, mobiles, desktop
- Analystes de données
- Administrateurs de bases de données
- Data scientists

C'est l'une des compétences les plus demandées en informatique.

## Les catégories de commandes SQL

SQL est divisé en plusieurs catégories selon le type d'opération effectuée. Pas de panique, nous allons les voir progressivement !

### DDL - Data Definition Language (Langage de Définition de Données)

Ces commandes servent à **créer et modifier la structure** de la base de données.

**Commandes principales :**

- `CREATE TABLE` : créer une nouvelle table
- `ALTER TABLE` : modifier une table existante
- `DROP TABLE` : supprimer une table
- `CREATE DATABASE` : créer une base de données

**Exemple :**
```sql
CREATE TABLE Clients (
    id INTEGER PRIMARY KEY,
    nom TEXT,
    email TEXT
);
```

### DML - Data Manipulation Language (Langage de Manipulation de Données)

Ces commandes servent à **manipuler les données** dans les tables.

**Commandes principales :**

- `SELECT` : lire/interroger des données
- `INSERT` : ajouter de nouvelles données
- `UPDATE` : modifier des données existantes
- `DELETE` : supprimer des données

**Exemple :**
```sql
SELECT nom, email FROM Clients WHERE ville = 'Paris';
```

### DCL - Data Control Language (Langage de Contrôle de Données)

Ces commandes gèrent les **permissions et droits d'accès**.

- `GRANT` : donner des permissions
- `REVOKE` : retirer des permissions

*Note : Nous n'utiliserons pas beaucoup ces commandes en tant que débutants.*

### TCL - Transaction Control Language (Langage de Contrôle de Transaction)

Ces commandes gèrent les **transactions**.

- `BEGIN` : commencer une transaction
- `COMMIT` : valider une transaction
- `ROLLBACK` : annuler une transaction

*Note : Nous verrons les transactions dans une section ultérieure.*

## La syntaxe SQL de base

### Règles générales

Avant de plonger dans les commandes, voici quelques règles importantes :

1. **Les commandes SQL ne sont pas sensibles à la casse**
   - `SELECT` = `select` = `Select`
   - Convention : on écrit les mots-clés SQL en MAJUSCULES pour la lisibilité

2. **Le point-virgule termine une instruction**
   ```sql
   SELECT * FROM Clients;
   ```

3. **Les espaces et retours à la ligne sont ignorés**
   ```sql
   -- Ces deux requêtes sont identiques :
   SELECT nom FROM Clients;

   SELECT nom
   FROM Clients;
   ```

4. **Les commentaires**
   ```sql
   -- Ceci est un commentaire sur une ligne

   /* Ceci est un commentaire
      sur plusieurs lignes */
   ```

5. **Les chaînes de caractères sont entre apostrophes simples**
   ```sql
   WHERE nom = 'Dupont'  -- Correct
   WHERE nom = "Dupont"  -- Incorrect dans SQL standard
   ```

## SELECT : Lire des données

`SELECT` est la commande la plus utilisée en SQL. Elle permet de **lire** (interroger) des données.

### Syntaxe de base

```sql
SELECT colonnes  
FROM table  
WHERE condition;  
```

### Exemple 1 : Tout sélectionner

```sql
SELECT * FROM Clients;
```

**Explication :**
- `SELECT *` : sélectionne toutes les colonnes
- `FROM Clients` : dans la table Clients
- L'astérisque `*` signifie "tout"

**Résultat :** Affiche tous les clients avec toutes leurs informations.

### Exemple 2 : Sélectionner des colonnes spécifiques

```sql
SELECT nom, email FROM Clients;
```

**Résultat :** Affiche uniquement le nom et l'email de tous les clients.

### Exemple 3 : Filtrer avec WHERE

```sql
SELECT nom, email  
FROM Clients  
WHERE ville = 'Paris';  
```

**Résultat :** Affiche le nom et l'email uniquement des clients habitant Paris.

### Exemple 4 : Opérateurs de comparaison

```sql
-- Égalité
SELECT * FROM Produits WHERE prix = 19.99;

-- Différent
SELECT * FROM Produits WHERE prix <> 19.99;
-- ou
SELECT * FROM Produits WHERE prix != 19.99;

-- Supérieur, inférieur
SELECT * FROM Produits WHERE prix > 50;  
SELECT * FROM Produits WHERE prix <= 100;  
```

### Exemple 5 : Opérateurs logiques

```sql
-- ET (AND)
SELECT * FROM Produits  
WHERE prix > 10 AND prix < 50;  

-- OU (OR)
SELECT * FROM Clients  
WHERE ville = 'Paris' OR ville = 'Lyon';  

-- NON (NOT)
SELECT * FROM Produits  
WHERE NOT categorie = 'Électronique';  
```

### Exemple 6 : Recherche de motif avec LIKE

```sql
-- Commence par 'Dup'
SELECT * FROM Clients WHERE nom LIKE 'Dup%';

-- Contient 'mart'
SELECT * FROM Clients WHERE nom LIKE '%mart%';

-- Deuxième lettre est 'a'
SELECT * FROM Clients WHERE nom LIKE '_a%';
```

**Explications des jokers :**
- `%` : n'importe quelle séquence de caractères (0 ou plus)
- `_` : exactement un caractère

### Exemple 7 : Trier avec ORDER BY

```sql
-- Ordre croissant (par défaut)
SELECT * FROM Clients ORDER BY nom;

-- Ordre décroissant
SELECT * FROM Clients ORDER BY nom DESC;

-- Tri multiple
SELECT * FROM Clients ORDER BY ville, nom;
```

### Exemple 8 : Limiter les résultats

```sql
-- Les 10 premiers clients
SELECT * FROM Clients LIMIT 10;

-- Les 5 produits les plus chers
SELECT * FROM Produits  
ORDER BY prix DESC  
LIMIT 5;  
```

### Exemple 9 : Fonctions d'agrégation

```sql
-- Compter les clients
SELECT COUNT(*) FROM Clients;

-- Prix moyen
SELECT AVG(prix) FROM Produits;

-- Prix minimum et maximum
SELECT MIN(prix), MAX(prix) FROM Produits;

-- Somme totale
SELECT SUM(montant) FROM Commandes;
```

### Exemple 10 : Grouper avec GROUP BY

```sql
-- Nombre de clients par ville
SELECT ville, COUNT(*)  
FROM Clients  
GROUP BY ville;  

-- Montant total des commandes par client
SELECT id_client, SUM(montant)  
FROM Commandes  
GROUP BY id_client;  
```

## INSERT : Ajouter des données

`INSERT` permet d'**ajouter** de nouveaux enregistrements dans une table.

### Syntaxe de base

```sql
INSERT INTO table (colonne1, colonne2, ...)  
VALUES (valeur1, valeur2, ...);  
```

### Exemple 1 : Insérer un enregistrement complet

```sql
INSERT INTO Clients (id, nom, prenom, email, ville)  
VALUES (1, 'Dupont', 'Pierre', 'pierre.dupont@email.fr', 'Paris');  
```

### Exemple 2 : Insérer sans spécifier toutes les colonnes

```sql
-- Les colonnes non mentionnées seront NULL ou auront leur valeur par défaut
INSERT INTO Clients (nom, prenom, email)  
VALUES ('Martin', 'Marie', 'marie.martin@email.fr');  
```

### Exemple 3 : Insérer plusieurs enregistrements

```sql
INSERT INTO Clients (nom, prenom, email)  
VALUES  
    ('Durand', 'Jacques', 'j.durand@email.fr'),
    ('Bernard', 'Sophie', 's.bernard@email.fr'),
    ('Petit', 'Luc', 'l.petit@email.fr');
```

### Exemple 4 : Insérer depuis une autre table

```sql
-- Copier des données d'une table à une autre
INSERT INTO Clients_Archive  
SELECT * FROM Clients WHERE date_creation < '2020-01-01';  
```

## UPDATE : Modifier des données

`UPDATE` permet de **modifier** des enregistrements existants.

### Syntaxe de base

```sql
UPDATE table  
SET colonne1 = valeur1, colonne2 = valeur2  
WHERE condition;  
```

**⚠️ ATTENTION :** Si vous oubliez le `WHERE`, **TOUS** les enregistrements seront modifiés !

### Exemple 1 : Modifier un enregistrement

```sql
UPDATE Clients  
SET email = 'nouveau.email@email.fr'  
WHERE id = 1;  
```

### Exemple 2 : Modifier plusieurs colonnes

```sql
UPDATE Produits  
SET prix = 29.99, stock = 150  
WHERE id = 42;  
```

### Exemple 3 : Modifier plusieurs enregistrements

```sql
-- Augmenter tous les prix de 10%
UPDATE Produits  
SET prix = prix * 1.10  
WHERE categorie = 'Électronique';  
```

### Exemple 4 : Modifier avec des conditions complexes

```sql
UPDATE Clients  
SET statut = 'VIP'  
WHERE total_achats > 1000 AND nb_commandes > 10;  
```

## DELETE : Supprimer des données

`DELETE` permet de **supprimer** des enregistrements.

### Syntaxe de base

```sql
DELETE FROM table  
WHERE condition;  
```

**⚠️ DANGER :** Si vous oubliez le `WHERE`, **TOUS** les enregistrements seront supprimés !

### Exemple 1 : Supprimer un enregistrement

```sql
DELETE FROM Clients  
WHERE id = 1;  
```

### Exemple 2 : Supprimer plusieurs enregistrements

```sql
DELETE FROM Commandes  
WHERE statut = 'Annulée';  
```

### Exemple 3 : Supprimer avec des conditions

```sql
DELETE FROM Produits  
WHERE stock = 0 AND date_creation < '2020-01-01';  
```

### Exemple 4 : Tout supprimer (avec prudence !)

```sql
-- Vide complètement la table
DELETE FROM Clients;
```

**Alternative plus rapide :**
```sql
-- Vide et réinitialise la table (plus rapide que DELETE)
TRUNCATE TABLE Clients;
```

## CREATE TABLE : Créer une structure

`CREATE TABLE` permet de **créer** une nouvelle table.

### Syntaxe de base

```sql
CREATE TABLE nom_table (
    colonne1 type contraintes,
    colonne2 type contraintes,
    ...
);
```

### Types de données courants

**Types numériques :**
- `INTEGER` : nombres entiers
- `REAL` / `FLOAT` : nombres décimaux
- `DECIMAL(p,s)` : décimaux avec précision

**Types textuels :**
- `TEXT` : texte de longueur variable
- `VARCHAR(n)` : texte avec longueur maximale n
- `CHAR(n)` : texte de longueur fixe n

**Types temporels :**
- `DATE` : date (année-mois-jour)
- `TIME` : heure
- `DATETIME` / `TIMESTAMP` : date et heure

**Autres :**
- `BOOLEAN` : vrai/faux
- `BLOB` : données binaires (images, fichiers)

### Exemple 1 : Table simple

```sql
CREATE TABLE Clients (
    id INTEGER PRIMARY KEY,
    nom TEXT,
    prenom TEXT,
    email TEXT
);
```

### Exemple 2 : Avec contraintes

```sql
CREATE TABLE Produits (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    nom TEXT NOT NULL,
    description TEXT,
    prix REAL NOT NULL,
    stock INTEGER DEFAULT 0,
    date_creation DATE DEFAULT CURRENT_DATE
);
```

**Contraintes expliquées :**
- `PRIMARY KEY` : clé primaire (unique et non nulle)
- `AUTOINCREMENT` : valeur générée automatiquement (1, 2, 3...)
- `NOT NULL` : la valeur est obligatoire
- `DEFAULT` : valeur par défaut si non spécifiée
- `UNIQUE` : la valeur doit être unique dans la table

### Exemple 3 : Avec clé étrangère

```sql
CREATE TABLE Commandes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    date DATE NOT NULL,
    montant REAL NOT NULL,
    id_client INTEGER,
    FOREIGN KEY (id_client) REFERENCES Clients(id)
);
```

### Exemple 4 : Avec plusieurs contraintes

```sql
CREATE TABLE Utilisateurs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT NOT NULL UNIQUE,
    email TEXT NOT NULL UNIQUE,
    password TEXT NOT NULL,
    date_inscription DATETIME DEFAULT CURRENT_TIMESTAMP,
    actif BOOLEAN DEFAULT 1,
    CHECK (length(password) >= 8)
);
```

## DROP TABLE : Supprimer une structure

`DROP TABLE` **supprime complètement** une table et toutes ses données.

```sql
DROP TABLE Clients;
```

**⚠️ IRRÉVERSIBLE :** Cette opération ne peut pas être annulée !

**Version sécurisée :**
```sql
-- Ne génère pas d'erreur si la table n'existe pas
DROP TABLE IF EXISTS Clients;
```

## ALTER TABLE : Modifier une structure

`ALTER TABLE` permet de **modifier** la structure d'une table existante.

### Ajouter une colonne

```sql
ALTER TABLE Clients  
ADD COLUMN telephone TEXT;  
```

### Supprimer une colonne

```sql
ALTER TABLE Clients  
DROP COLUMN telephone;  
```

*Note : Certaines bases de données (comme SQLite) ont des limitations sur les modifications de structure.*

## Les jointures : combiner plusieurs tables

Les **jointures** (JOIN) permettent de combiner des données de plusieurs tables. C'est l'une des fonctionnalités les plus puissantes de SQL.

### INNER JOIN : intersection

Retourne uniquement les lignes qui ont une correspondance dans les deux tables.

```sql
SELECT Clients.nom, Commandes.montant  
FROM Clients  
INNER JOIN Commandes ON Clients.id = Commandes.id_client;  
```

**Résultat :** Liste des clients qui ont passé au moins une commande, avec le montant.

### LEFT JOIN : tout à gauche

Retourne toutes les lignes de la table de gauche, avec les correspondances de droite si elles existent.

```sql
SELECT Clients.nom, Commandes.montant  
FROM Clients  
LEFT JOIN Commandes ON Clients.id = Commandes.id_client;  
```

**Résultat :** Tous les clients, même ceux sans commande (montant sera NULL).

### Exemple pratique complet

```sql
-- Liste des commandes avec nom du client et nom du produit
SELECT
    Clients.nom AS client,
    Produits.nom AS produit,
    Commandes.quantite,
    Commandes.montant
FROM Commandes  
INNER JOIN Clients ON Commandes.id_client = Clients.id  
INNER JOIN Produits ON Commandes.id_produit = Produits.id  
ORDER BY Commandes.date DESC;  
```

## Bonnes pratiques SQL

### 1. Toujours utiliser WHERE avec UPDATE et DELETE

```sql
-- MAL : modifie tout !
UPDATE Produits SET prix = 10;

-- BIEN : modifie un seul produit
UPDATE Produits SET prix = 10 WHERE id = 42;
```

### 2. Utiliser des noms de colonnes explicites

```sql
-- Moins bien
SELECT * FROM Clients;

-- Mieux (plus clair et plus efficace)
SELECT id, nom, email FROM Clients;
```

### 3. Utiliser des alias pour la lisibilité

```sql
SELECT
    c.nom AS nom_client,
    p.nom AS nom_produit,
    co.montant
FROM Commandes co  
INNER JOIN Clients c ON co.id_client = c.id  
INNER JOIN Produits p ON co.id_produit = p.id;  
```

### 4. Commenter vos requêtes complexes

```sql
-- Récupère les 10 meilleurs clients de l'année
SELECT c.nom, SUM(co.montant) AS total  
FROM Clients c  
INNER JOIN Commandes co ON c.id = co.id_client  
WHERE co.date >= '2025-01-01'  
GROUP BY c.id, c.nom  
ORDER BY total DESC  
LIMIT 10;  
```

### 5. Tester sur un petit échantillon d'abord

```sql
-- Testez d'abord avec LIMIT
UPDATE Produits SET prix = prix * 1.10 LIMIT 1;

-- Si OK, exécutez sur tout
UPDATE Produits SET prix = prix * 1.10;
```

## Différences entre SGBD

Bien que SQL soit standardisé, chaque système de base de données a ses particularités :

### SQLite
- `AUTOINCREMENT` pour l'auto-incrémentation
- Types de données flexibles
- Pas de `TRUNCATE`

### PostgreSQL
- `SERIAL` pour l'auto-incrémentation
- Types de données riches (JSON, tableaux, etc.)
- Support des fonctions avancées

### MySQL/MariaDB
- `AUTO_INCREMENT` pour l'auto-incrémentation
- Backticks ` pour les noms de tables/colonnes
- Dialecte légèrement différent

**Conseil :** Commencez avec SQLite pour apprendre, les concepts resteront valables partout !

## Récapitulatif des commandes essentielles

| Commande | Usage | Exemple |
|----------|-------|---------|
| `SELECT` | Lire des données | `SELECT * FROM Clients;` |
| `INSERT` | Ajouter des données | `INSERT INTO Clients VALUES (...);` |
| `UPDATE` | Modifier des données | `UPDATE Clients SET nom = '...' WHERE id = 1;` |
| `DELETE` | Supprimer des données | `DELETE FROM Clients WHERE id = 1;` |
| `CREATE TABLE` | Créer une table | `CREATE TABLE Clients (...);` |
| `DROP TABLE` | Supprimer une table | `DROP TABLE Clients;` |
| `ALTER TABLE` | Modifier une table | `ALTER TABLE Clients ADD COLUMN ...;` |

## Pour aller plus loin

Dans les prochaines sections, nous verrons :

- Comment utiliser SQL depuis FreePascal/Lazarus
- Comment créer une vraie base de données SQLite
- Comment afficher les données dans votre interface graphique
- Comment gérer les erreurs et les transactions

## Conclusion

SQL est un langage puissant mais accessible. Les commandes de base (`SELECT`, `INSERT`, `UPDATE`, `DELETE`) couvrent 90% des besoins quotidiens.

**Points clés à retenir :**

- SQL est un langage de requête, pas de programmation
- Il est standardisé et universel
- Les commandes ressemblent à de l'anglais
- Toujours tester prudemment UPDATE et DELETE
- Les jointures permettent de combiner plusieurs tables

Avec ces bases, vous êtes prêt à créer et manipuler vos premières bases de données !

---

*N'hésitez pas à revenir sur cette section de référence quand vous aurez besoin de vous rappeler une syntaxe SQL.*

⏭️ [SQLite : base embarquée](/16-bases-donnees-maitrise-approfondie/03-sqlite-base-embarquee.md)
