🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.10 Introduction aux bases Client/Serveur (concepts)

## Introduction : au-delà de SQLite

Vous avez appris à utiliser SQLite, une base de données **embarquée** parfaite pour débuter et pour de nombreuses applications. Mais dans le monde professionnel, vous rencontrerez souvent des bases de données **client/serveur** comme PostgreSQL, MySQL/MariaDB, Oracle ou SQL Server.

Pourquoi existe-t-il deux types de bases de données ? Quelles sont les différences ? Quand utiliser l'une ou l'autre ? Cette section répond à ces questions.

### L'analogie des bibliothèques

**SQLite = Bibliothèque personnelle**
- Vous avez vos livres chez vous, dans votre bibliothèque
- Vous seul y avez accès
- Simple : aucune organisation complexe nécessaire
- Pratique pour une personne
- Limité en nombre de livres

**Client/Serveur = Bibliothèque municipale**
- Les livres sont dans un bâtiment central (le serveur)
- Plusieurs personnes peuvent y accéder simultanément (les clients)
- Organisation professionnelle avec bibliothécaires (administrateurs)
- Beaucoup plus de livres possibles
- Accès contrôlé avec carte de bibliothèque (authentification)
- Horaires d'ouverture (disponibilité du serveur)

## Qu'est-ce qu'une base de données client/serveur ?

### Définition

Une base de données **client/serveur** est un système où :

**Le SERVEUR** :
- Programme qui tourne en permanence (service/daemon)
- Gère la base de données
- Écoute les demandes sur le réseau
- Traite les requêtes SQL
- Renvoie les résultats
- Gère les accès simultanés

**Le(s) CLIENT(S)** :
- Votre application Pascal/Lazarus
- Se connecte au serveur via le réseau
- Envoie des requêtes SQL
- Reçoit les résultats
- Peut être sur la même machine ou à distance

### Schéma de fonctionnement

```
┌─────────────────────────────────────────────────────────┐
│                    SERVEUR                              │
│  Machine : serveur-bdd.entreprise.com                   │
│  ┌───────────────────────────────────────────────────┐  │
│  │  Processus PostgreSQL / MySQL / MariaDB           │  │
│  │  - Écoute sur le port 5432 (PostgreSQL)           │  │
│  │  - ou port 3306 (MySQL/MariaDB)                   │  │
│  │  - Gère les connexions                            │  │
│  │  - Exécute les requêtes SQL                       │  │
│  │  - Contrôle les permissions                       │  │
│  └─────────────┬─────────────────────────────────────┘  │
│                │                                        │
│  ┌─────────────▼─────────────────────────────────────┐  │
│  │  Fichiers de la base de données                   │  │
│  │  /var/lib/postgresql/data/                        │  │
│  │  ou /var/lib/mysql/                               │  │
│  └───────────────────────────────────────────────────┘  │
└──────────────────────┬──────────────────────────────────┘
                       │
                       │ RÉSEAU (TCP/IP)
                       │ (local ou Internet)
       ┌───────────────┼───────────────┬─────────────────┐
       │               │               │                 │
┌──────▼──────┐ ┌──────▼──────┐ ┌─────▼──────┐  ┌────────▼────┐
│  CLIENT 1   │ │  CLIENT 2   │ │ CLIENT 3   │  │  CLIENT N   │
│             │ │             │ │            │  │             │
│ Application │ │ Application │ │ Navigateur │  │ Application │
│ Lazarus     │ │ Web PHP     │ │ Web        │  │ Mobile      │
└─────────────┘ └─────────────┘ └────────────┘  └─────────────┘
  PC Bureau       Serveur Web     Tablette         Smartphone
```

### Communication réseau

Les clients et le serveur communiquent via le **réseau** (même si tout est sur la même machine, on passe par le réseau local).

**Protocole :** TCP/IP  
**Port :** Chaque SGBD écoute sur un port spécifique  
- PostgreSQL : port 5432 (par défaut)
- MySQL/MariaDB : port 3306 (par défaut)
- SQL Server : port 1433 (par défaut)

**Connexion typique :**
```
Hôte     : serveur-bdd.entreprise.com (ou 192.168.1.100, ou localhost)  
Port     : 5432  
Base     : ma_base  
Utilisateur : mon_utilisateur  
Mot de passe : mon_mot_de_passe  
```

## Différences fondamentales : Embarqué vs Client/Serveur

### Architecture

**SQLite (Embarquée)**
```
┌────────────────────────────────┐
│  Votre Application             │
│  ┌──────────────────────────┐  │
│  │  Bibliothèque SQLite     │  │
│  │  (liée à l'application)  │  │
│  └───────────┬──────────────┘  │
│              │                 │
│              ▼                 │
│  ┌──────────────────────────┐  │
│  │  Fichier .db             │  │
│  │  (accès direct)          │  │
│  └──────────────────────────┘  │
└────────────────────────────────┘
    Tout dans le même processus
```

**PostgreSQL/MySQL (Client/Serveur)**
```
┌─────────────────┐         ┌──────────────────┐
│  Application    │         │  Serveur BDD     │
│  (Client)       │ ◄────► │  (Processus       │
│                 │ Réseau  │   séparé)        │
└─────────────────┘         └────────┬─────────┘
                                     │
                            ┌────────▼─────────┐
                            │  Fichiers BDD    │
                            └──────────────────┘
        Deux processus distincts qui dialoguent
```

### Accès concurrent

**SQLite**
- **Un seul écrivain** à la fois
- Plusieurs lecteurs simultanés possibles
- Verrous au niveau du fichier
- Adapté aux applications mono-utilisateur ou avec peu d'écritures simultanées

**Client/Serveur**
- **Plusieurs écrivains** simultanés
- Gestion fine des verrous (au niveau de la ligne)
- Optimisé pour de nombreux utilisateurs simultanés
- Adapté aux applications multi-utilisateurs

**Exemple concret :**

Imaginons 10 employés qui utilisent simultanément une application de gestion de stocks :

**Avec SQLite :**
```
Employé 1 : Modifier le stock article 42   → Attend...  
Employé 2 : Ajouter une commande          → Attend...  
Employé 3 : Consulter les stocks          → OK (lecture)  
Employé 4 : Modifier le stock article 15   → Attend...  
```
→ Un seul peut écrire à la fois = **goulot d'étranglement**

**Avec PostgreSQL/MySQL :**
```
Employé 1 : Modifier le stock article 42   → OK  
Employé 2 : Ajouter une commande          → OK  
Employé 3 : Consulter les stocks          → OK  
Employé 4 : Modifier le stock article 15   → OK  
```
→ Tous travaillent en parallèle (sur des données différentes) = **pas de blocage**

### Localisation

**SQLite**
- Fichier .db sur le **même ordinateur** que l'application
- Accès via le système de fichiers
- Partage de fichier réseau (NFS, SMB) **déconseillé** et problématique

**Client/Serveur**
- Serveur peut être **n'importe où** :
  - Sur la même machine (localhost)
  - Sur le réseau local (192.168.x.x)
  - Sur Internet (serveur-bdd.exemple.com)
- Accès via connexion réseau (TCP/IP)
- Conçu pour le réseau

### Sécurité et authentification

**SQLite**
- Pas d'authentification intégrée
- Sécurité = permissions du système de fichiers
- Qui peut lire le fichier .db peut lire toutes les données

**Client/Serveur**
- **Authentification** : utilisateur + mot de passe
- **Autorisations** granulaires :
  - Par utilisateur
  - Par base de données
  - Par table
  - Par opération (SELECT, INSERT, UPDATE, DELETE)
- Exemple : utilisateur "lecteur" peut lire mais pas modifier

**Exemple de permissions :**
```sql
-- Créer un utilisateur
CREATE USER 'jean'@'%' IDENTIFIED BY 'motdepasse123';

-- Donner seulement le droit de lecture sur une table
GRANT SELECT ON ma_base.clients TO 'jean'@'%';

-- Jean peut maintenant :
SELECT * FROM clients;  -- ✅ OK

-- Mais pas :
DELETE FROM clients;    -- ❌ Erreur : permission refusée
```

### Administration

**SQLite**
- Aucune administration nécessaire
- Pas de serveur à démarrer/arrêter
- Pas de configuration réseau
- Pas d'utilisateurs à gérer
- Sauvegarde = copie du fichier .db

**Client/Serveur**
- **Serveur** à installer et configurer
- **Service** à démarrer (démarre avec le système)
- **Utilisateurs** et permissions à gérer
- **Sauvegardes** régulières (dumps)
- **Surveillance** (monitoring) recommandée
- **Optimisations** possibles (tuning)
- Rôle d'**administrateur de base de données** (DBA)

### Performance

**SQLite**
- ✅ Très rapide pour les petites/moyennes bases
- ✅ Pas de latence réseau
- ✅ Accès direct au fichier
- ❌ Ralentit avec beaucoup d'écritures simultanées
- ❌ Moins d'optimisations avancées

**Client/Serveur**
- ✅ Optimisé pour les grandes bases de données
- ✅ Gère très bien la concurrence
- ✅ Nombreuses options d'optimisation (index avancés, cache, etc.)
- ✅ Peut utiliser beaucoup de RAM
- ❌ Latence réseau (même minime en local)
- ❌ Plus de ressources nécessaires

### Taille et volume

**SQLite**
- Maximum théorique : 281 To
- Maximum pratique recommandé : quelques Go
- Idéal pour : < 1 Go

**Client/Serveur**
- Limite très élevée (plusieurs To faciles)
- Bases de plusieurs centaines de Go courantes
- Optimisé pour les gros volumes

## Les principaux SGBD client/serveur

### PostgreSQL

**"The World's Most Advanced Open Source Database"**

**Caractéristiques :**
- ✅ 100% gratuit et open source
- ✅ Très riche en fonctionnalités
- ✅ Excellente conformité aux standards SQL
- ✅ Support des types avancés (JSON, tableaux, géométrie, etc.)
- ✅ Extensible (on peut ajouter des fonctions, des types)
- ✅ Excellente documentation
- ✅ Communauté active

**Idéal pour :**
- Applications complexes
- Données structurées et semi-structurées
- Projets nécessitant l'intégrité des données
- Startups et grandes entreprises

**Utilisé par :** Instagram, Spotify, Reddit, Apple, Cisco

### MySQL / MariaDB

**MySQL :** Le plus populaire au monde  
**MariaDB :** Fork open source de MySQL (100% compatible)  

**Caractéristiques :**
- ✅ Très populaire (énorme communauté)
- ✅ Facile à apprendre
- ✅ Très rapide pour les lectures
- ✅ Bien intégré aux hébergeurs web
- ✅ MariaDB est 100% open source

**Différences MySQL / MariaDB :**
- MySQL appartient à Oracle (mais version gratuite existe)
- MariaDB est le fork libre créé par le fondateur original de MySQL
- MariaDB plus de nouvelles fonctionnalités
- Compatibles pour la plupart des usages

**Idéal pour :**
- Applications web (PHP, WordPress, etc.)
- Projets nécessitant de la vitesse
- Débutants (documentation abondante)

**Utilisé par :** Facebook, Twitter, YouTube, Wikipedia, Booking.com

### Autres SGBD (non couverts dans ce tutoriel)

**Microsoft SQL Server**
- ❌ Windows uniquement (principalement)
- ❌ Payant (version express gratuite mais limitée)
- ✅ Excellente intégration avec .NET
- ✅ Outils puissants

**Oracle Database**
- ❌ Très cher (entreprises uniquement)
- ✅ Extrêmement robuste et performant
- ✅ Fonctionnalités avancées

## Quand utiliser SQLite vs Client/Serveur ?

### Utilisez SQLite si :

✅ **Application desktop mono-utilisateur**
- Éditeur de texte avec base de données
- Gestionnaire de photos personnel
- Application de prise de notes

✅ **Prototypes et apprentissage**
- Pas besoin de serveur
- Configuration immédiate
- Idéal pour apprendre

✅ **Applications mobiles**
- Android, iOS utilisent SQLite
- Base de données locale

✅ **Fichiers de configuration avancés**
- Alternative aux fichiers INI/XML/JSON
- Requêtes SQL sur la config

✅ **Bases de données embarquées**
- Navigateurs (Firefox, Chrome)
- Logiciels qui incluent leur BDD

✅ **Peu d'écritures simultanées**
- Principalement de la lecture
- Modifications occasionnelles

✅ **Données < 1-2 Go**
- Petites à moyennes bases

### Utilisez Client/Serveur (PostgreSQL/MySQL) si :

✅ **Application multi-utilisateurs**
- Plusieurs personnes travaillent simultanément
- Application web avec trafic
- ERP, CRM d'entreprise

✅ **Nombreuses écritures simultanées**
- Plusieurs utilisateurs modifient les données en même temps
- Application collaborative

✅ **Gros volumes de données**
- Bases > 10 Go
- Millions d'enregistrements
- Croissance importante prévue

✅ **Sécurité avancée**
- Contrôle d'accès fin (utilisateurs, rôles, permissions)
- Audit des opérations
- Chiffrement

✅ **Accès réseau nécessaire**
- Clients sur plusieurs machines
- Application distribuée
- API REST pour applications mobiles

✅ **Fonctionnalités avancées**
- Procédures stockées
- Triggers complexes
- Réplication
- Clustering

✅ **Administration professionnelle**
- Équipe de DBA
- Monitoring
- Optimisation fine

✅ **Haute disponibilité**
- Serveur 24/7
- Réplication master/slave
- Failover automatique

## Exemple de scénarios

### Scénario 1 : Application de gestion de contacts personnelle

**Besoin :**
- Un seul utilisateur
- 500 contacts maximum
- Application desktop

**Choix : SQLite** ✅
- Simple
- Pas de serveur nécessaire
- Performance largement suffisante
- Portable (fichier .db)

### Scénario 2 : Site e-commerce

**Besoin :**
- Centaines d'utilisateurs simultanés
- Commandes, paiements, stocks
- 50 000+ produits
- Disponibilité 24/7

**Choix : PostgreSQL ou MySQL** ✅
- Gère la concurrence
- Sécurité (utilisateurs, permissions)
- Performance pour gros volumes
- Disponibilité garantie

### Scénario 3 : Application de facturation pour petite entreprise

**Besoin :**
- 3 à 5 utilisateurs simultanés
- Base de données partagée
- Clients, factures, produits
- Réseau local

**Choix : PostgreSQL/MySQL** ✅ (ou SQLite avec précautions)
- **PostgreSQL/MySQL recommandé** : meilleure gestion de la concurrence
- **SQLite possible** si peu d'écritures simultanées et taille modeste

### Scénario 4 : Jeu vidéo offline

**Besoin :**
- Sauvegardes de jeu
- Inventaire du joueur
- Statistiques
- Un seul joueur

**Choix : SQLite** ✅
- Léger
- Embarqué dans le jeu
- Pas de connexion réseau nécessaire
- Rapide

### Scénario 5 : Système de réservation en ligne

**Besoin :**
- Réservations en temps réel
- Éviter les doubles réservations
- Centaines de requêtes/minute
- Haute disponibilité

**Choix : PostgreSQL** ✅
- Verrous transactionnels robustes
- Concurrence forte
- Fonctionnalités avancées
- Fiabilité

## Migration : de SQLite vers Client/Serveur

Si vous commencez avec SQLite et devez migrer vers PostgreSQL/MySQL :

### Ce qui reste identique (ou presque)

✅ **SQL standard**
- SELECT, INSERT, UPDATE, DELETE fonctionnent pareil
- WHERE, ORDER BY, GROUP BY, JOIN identiques

✅ **Concepts**
- Tables, colonnes, clés primaires/étrangères
- Transactions, index
- Contraintes

### Ce qui change

⚠️ **Connexion**
- Hôte, port, utilisateur, mot de passe nécessaires
- Pas juste un chemin de fichier

⚠️ **Types de données**
- Syntaxe légèrement différente
- `AUTOINCREMENT` → `SERIAL` (PostgreSQL) ou `AUTO_INCREMENT` (MySQL)

⚠️ **Fonctions**
- Certaines fonctions SQL diffèrent
- Compatibilité à vérifier

⚠️ **Configuration**
- Serveur à installer et configurer
- Utilisateurs à créer
- Permissions à définir

### Stratégie de migration

1. **Développer avec SQLite** (rapide, simple)
2. **Quand la concurrence devient un problème** → migrer
3. **Tests** : vérifier que tout fonctionne
4. **Ajuster** : optimiser pour le nouveau SGBD

**Bonne nouvelle :** Le code Lazarus change **très peu** ! Principalement le composant de connexion.

## Coûts et ressources

### SQLite

**Coûts :**
- 💰 Licence : Gratuit (domaine public)
- 💰 Hébergement : Aucun (fichier local)
- 💰 Administration : Aucun

**Ressources nécessaires :**
- 💻 RAM : Très peu (quelques Mo)
- 💻 CPU : Minimal
- 💻 Disque : Taille de la base uniquement

### PostgreSQL / MySQL

**Coûts :**
- 💰 Licence : Gratuit (open source)
- 💰 Hébergement : Variable (serveur dédié ou mutualisé)
  - Local : gratuit (votre machine)
  - VPS : 5-50€/mois
  - Cloud : pay-as-you-go
- 💰 Administration : Temps ou DBA si complexe

**Ressources nécessaires :**
- 💻 RAM : Minimum 1-2 Go, recommandé 4-8 Go ou plus
- 💻 CPU : 1-4 cores minimum
- 💻 Disque : Espace pour données + logs + sauvegardes

## Préparation pour les sections suivantes

Dans les prochaines sections, nous verrons :

**Section 16.11 : Connexion à PostgreSQL ou MariaDB**
- Installation du serveur
- Configuration
- Connexion depuis Lazarus
- Premier projet client/serveur

**Section 16.12 : Gestion avancée des transactions**
- Niveaux d'isolation
- Deadlocks
- Transactions longues

**Section 16.13 : Gestion des erreurs de connexion et résilience**
- Déconnexions réseau
- Reconnexion automatique
- Gestion des timeouts

## Résumé

**Deux architectures :**
- **Embarquée (SQLite)** : tout dans l'application
- **Client/Serveur (PostgreSQL/MySQL)** : processus séparés

**SQLite = Simplicité**
- ✅ Pas de serveur
- ✅ Un fichier
- ✅ Parfait pour débuter
- ❌ Un seul écrivain

**Client/Serveur = Puissance**
- ✅ Multi-utilisateurs
- ✅ Concurrence forte
- ✅ Sécurité avancée
- ❌ Plus complexe

**Choisir selon :**
- Nombre d'utilisateurs simultanés
- Volume de données
- Besoins de sécurité
- Complexité acceptable

**Ne pas avoir peur :**
- Client/Serveur n'est pas si difficile
- Les concepts restent les mêmes
- Le SQL est identique à 95%
- Lazarus facilite le travail

**Progression naturelle :**
1. Apprendre avec SQLite ✅ (vous êtes ici)
2. Comprendre les concepts client/serveur ✅ (cette section)
3. Pratiquer avec PostgreSQL/MySQL (sections suivantes)
4. Choisir le bon outil pour chaque projet

Vous êtes maintenant prêt à découvrir PostgreSQL et MariaDB dans les sections suivantes !

---

*SQLite vous a appris les bases. Les SGBD client/serveur vous ouvrent les portes des applications professionnelles !*

⏭️ [**Connexion à PostgreSQL ou MariaDB**](/16-bases-donnees-maitrise-approfondie/11-connexion-postgresql-mariadb.md)
