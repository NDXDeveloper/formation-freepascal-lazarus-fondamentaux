🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.12 Gestion avancée des transactions (BEGIN, COMMIT, ROLLBACK)

## Introduction : au-delà des bases

Dans la section 16.9, vous avez découvert les **fondamentaux** des transactions : BEGIN, COMMIT, ROLLBACK et les propriétés ACID. Maintenant que vous utilisez PostgreSQL ou MariaDB, il est temps d'explorer des **concepts plus avancés** qui vous permettront de créer des applications robustes et performantes dans des environnements multi-utilisateurs.

### Ce que vous allez apprendre

Cette section couvre :
- 🔒 Les **niveaux d'isolation** des transactions
- ⚔️ Les **deadlocks** (interblocages) et comment les éviter
- 💾 Les **points de sauvegarde** (SAVEPOINT)
- ⏱️ La gestion des **transactions longues**
- 🔐 Les **verrous** (locks) et la concurrence
- 🎯 Les **bonnes pratiques** avancées

### Prérequis

Avant de continuer, assurez-vous d'avoir compris :
- ✅ Les transactions de base (section 16.9)
- ✅ La connexion à PostgreSQL ou MariaDB (section 16.11)
- ✅ Les opérations CRUD (section 16.8)

## Rappel : Le cycle de vie d'une transaction

```
BEGIN TRANSACTION
    ↓
[Opérations SQL]
    ↓
    ├─→ COMMIT (tout est validé) ✅
    │
    └─→ ROLLBACK (tout est annulé) ❌
```

**Important :** Entre BEGIN et COMMIT/ROLLBACK, les modifications sont **visibles uniquement par votre transaction**. Les autres utilisateurs ne les voient pas encore.

## Les niveaux d'isolation

### Qu'est-ce que l'isolation ?

L'**isolation** détermine comment les transactions simultanées interagissent entre elles. C'est le "I" de ACID.

### L'analogie des cabines d'essayage

Imaginez un magasin avec des cabines d'essayage :

**Isolation totale (SERIALIZABLE)**
- Cabines complètement fermées et opaques
- Personne ne voit ce que vous faites
- Une seule personne à la fois dans le magasin
- ✅ Maximum de confidentialité
- ❌ File d'attente longue

**Isolation partielle (READ COMMITTED)**
- Cabines avec rideaux
- On voit vos pieds, mais pas ce que vous essayez
- Plusieurs personnes dans le magasin
- ✅ Plus rapide
- ⚠️ Risque de voir des changements

**Isolation minimale (READ UNCOMMITTED)**
- Pas de cabines, tout le monde voit tout
- Très rapide
- ❌ Pas de confidentialité
- ❌ Confusions possibles

### Les quatre niveaux d'isolation SQL

Du moins isolé au plus isolé :

#### 1. READ UNCOMMITTED (Lecture non validée)

**Description :**
- Peut lire les données **non encore validées** d'autres transactions
- Le plus bas niveau d'isolation

**Problème : Dirty Read (lecture sale)**

Transaction A :
```sql
BEGIN;  
UPDATE comptes SET solde = solde - 100 WHERE id = 1;  
-- Pas encore de COMMIT
```

Transaction B :
```sql
BEGIN;  
SELECT solde FROM comptes WHERE id = 1;  
-- Voit la modification de A (solde - 100) alors qu'elle n'est pas validée !
```

Si A fait ROLLBACK, B a lu une valeur **qui n'existe plus**.

**Utilisation :** Presque jamais. Trop dangereux.

#### 2. READ COMMITTED (Lecture validée)

**Description :**
- Ne lit que les données **validées** (COMMIT)
- Niveau par défaut dans PostgreSQL et MySQL

**Problème : Non-Repeatable Read (lecture non répétable)**

Transaction A :
```sql
BEGIN;  
SELECT solde FROM comptes WHERE id = 1;  -- Résultat : 1000€  

-- Pendant ce temps, Transaction B :
-- UPDATE comptes SET solde = 500 WHERE id = 1;
-- COMMIT;

SELECT solde FROM comptes WHERE id = 1;  -- Résultat : 500€ (différent !)  
COMMIT;  
```

La même requête dans la même transaction donne des résultats différents.

**Utilisation :** Défaut et suffisant pour la plupart des applications.

#### 3. REPEATABLE READ (Lecture répétable)

**Description :**
- Les lectures sont **stables** : même résultat pendant toute la transaction
- Une fois qu'une ligne est lue, sa valeur ne change pas pour cette transaction

**Problème : Phantom Read (lecture fantôme)**

Transaction A :
```sql
BEGIN;  
SELECT COUNT(*) FROM clients WHERE ville = 'Paris';  -- Résultat : 10  

-- Transaction B insère un nouveau client à Paris et fait COMMIT

SELECT COUNT(*) FROM clients WHERE ville = 'Paris';  -- Résultat : 11 (!)  
COMMIT;  
```

De nouvelles lignes peuvent apparaître (fantômes).

**Utilisation :** Quand vous avez besoin de stabilité des données.

#### 4. SERIALIZABLE (Sérialisable)

**Description :**
- Isolation **totale**
- Les transactions s'exécutent comme si elles étaient **séquentielles** (une après l'autre)
- Aucun problème de concurrence possible

**Avantages :**
- ✅ Sécurité maximale
- ✅ Cohérence garantie

**Inconvénients :**
- ❌ Performance réduite
- ❌ Risque de blocages (deadlocks)
- ❌ Rejets de transactions plus fréquents

**Utilisation :** Opérations financières critiques, inventaires.

### Définir le niveau d'isolation

#### PostgreSQL

```sql
-- Au début de la transaction
BEGIN TRANSACTION ISOLATION LEVEL READ COMMITTED;
-- ou
BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ;
-- ou
BEGIN TRANSACTION ISOLATION LEVEL SERIALIZABLE;

-- Vos opérations SQL
COMMIT;
```

**Ou pour toute la session :**
```sql
SET SESSION CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL SERIALIZABLE;
```

#### MariaDB / MySQL

```sql
-- Pour la transaction courante
SET TRANSACTION ISOLATION LEVEL READ COMMITTED;  
BEGIN;  
-- Vos opérations
COMMIT;

-- Ou pour toute la session
SET SESSION TRANSACTION ISOLATION LEVEL REPEATABLE READ;
```

#### Depuis Lazarus

```pascal
procedure TForm1.TransactionAvecIsolation;  
begin  
  try
    // PostgreSQL
    SQLQuery1.SQL.Text :=
      'BEGIN TRANSACTION ISOLATION LEVEL SERIALIZABLE';
    SQLQuery1.ExecSQL;

    // Vos opérations
    SQLQuery2.SQL.Text := 'UPDATE comptes SET solde = solde - 100 WHERE id = 1';
    SQLQuery2.ExecSQL;

    SQLTransaction1.Commit;
  except
    SQLTransaction1.Rollback;
    raise;
  end;
end;
```

### Quel niveau choisir ?

**Tableau de décision :**

| Cas d'usage | Niveau recommandé |
|-------------|-------------------|
| Application web standard | READ COMMITTED |
| Lecture de rapports | READ COMMITTED |
| Transfert bancaire | SERIALIZABLE |
| Réservation de places | SERIALIZABLE |
| Gestion de stock | REPEATABLE READ ou SERIALIZABLE |
| Analytics (lecture seule) | READ COMMITTED |

**Règle générale :** Commencez avec READ COMMITTED, montez uniquement si nécessaire.

## Les Deadlocks (Interblocages)

### Qu'est-ce qu'un deadlock ?

Un **deadlock** se produit quand deux transactions s'attendent mutuellement et se bloquent indéfiniment.

### L'analogie de la porte

Imaginez deux personnes qui veulent passer une porte en même temps :
- Personne A tient la porte à gauche, attend que B libère la droite
- Personne B tient la porte à droite, attend que A libère la gauche
- **Blocage total** : personne ne peut avancer !

### Exemple de deadlock

**Transaction A :**
```sql
BEGIN;  
UPDATE comptes SET solde = solde - 100 WHERE id = 1;  -- Verrouille ligne 1  
-- Attend pour accéder à la ligne 2...
UPDATE comptes SET solde = solde + 100 WHERE id = 2;  -- ⏳ BLOQUÉ  
COMMIT;  
```

**Transaction B (simultanée) :**
```sql
BEGIN;  
UPDATE comptes SET solde = solde - 50 WHERE id = 2;   -- Verrouille ligne 2  
-- Attend pour accéder à la ligne 1...
UPDATE comptes SET solde = solde + 50 WHERE id = 1;   -- ⏳ BLOQUÉ  
COMMIT;  
```

**Résultat :** Deadlock ! 💀

- A attend que B libère la ligne 2
- B attend que A libère la ligne 1
- Personne n'avance

### Détection et résolution par le SGBD

**Bonne nouvelle :** PostgreSQL et MySQL **détectent** automatiquement les deadlocks et **annulent** une des transactions.

**Message d'erreur typique :**
```
ERROR: deadlock detected  
DETAIL: Process 1234 waits for ShareLock on transaction 5678...  
```

### Comment éviter les deadlocks ?

#### 1. Toujours accéder aux tables dans le même ordre

**MAL :**
```pascal
// Transaction A
UPDATE table1 ...  
UPDATE table2 ...  

// Transaction B (ordre inversé)
UPDATE table2 ...  
UPDATE table1 ...  
// Risque de deadlock !
```

**BIEN :**
```pascal
// Transaction A
UPDATE table1 ...  
UPDATE table2 ...  

// Transaction B (même ordre)
UPDATE table1 ...  
UPDATE table2 ...  
// Pas de deadlock
```

#### 2. Garder les transactions courtes

```pascal
// MAL : transaction trop longue
BEGIN;  
UPDATE comptes ...  
Sleep(5000);  // Attente = mauvais !  
CalculComplexe();  // Calcul long = mauvais !  
UPDATE autre_table ...  
COMMIT;  

// BIEN : transaction courte
BEGIN;  
UPDATE comptes ...  
UPDATE autre_table ...  
COMMIT;  
// Puis faire les calculs longs
CalculComplexe();
```

#### 3. Verrouiller explicitement au début

```sql
BEGIN;
-- Verrouiller toutes les lignes nécessaires dès le début
SELECT * FROM comptes WHERE id IN (1, 2) FOR UPDATE;
-- Maintenant, on peut les modifier sans risque
UPDATE comptes SET solde = solde - 100 WHERE id = 1;  
UPDATE comptes SET solde = solde + 100 WHERE id = 2;  
COMMIT;  
```

#### 4. Gérer l'erreur et réessayer

```pascal
procedure TForm1.OperationAvecRetry;  
var  
  Tentatives: Integer;
  Success: Boolean;
begin
  Tentatives := 0;
  Success := False;

  while (Tentatives < 3) and (not Success) do
  begin
    try
      SQLTransaction1.StartTransaction;

      // Vos opérations
      SQLQuery1.ExecSQL;

      SQLTransaction1.Commit;
      Success := True;

    except
      on E: EDatabaseError do
      begin
        SQLTransaction1.Rollback;

        // Vérifier si c'est un deadlock
        if Pos('deadlock', LowerCase(E.Message)) > 0 then
        begin
          Inc(Tentatives);
          Sleep(100 * Tentatives);  // Attente progressive

          if Tentatives >= 3 then
          begin
            ShowMessage('Opération échouée après 3 tentatives');
            raise;
          end;
        end
        else
          raise;  // Autre erreur, on propage
      end;
    end;
  end;
end;
```

## Les Points de Sauvegarde (SAVEPOINT)

### Qu'est-ce qu'un SAVEPOINT ?

Un **point de sauvegarde** (savepoint) est un **point de retour partiel** dans une transaction. Vous pouvez annuler jusqu'à ce point sans annuler toute la transaction.

### L'analogie du jeu vidéo

C'est comme les **points de sauvegarde** dans un jeu vidéo :
- Vous sauvegardez à différents moments
- Si vous mourez, vous revenez au dernier point de sauvegarde
- Mais vous ne recommencez pas tout le jeu !

### Syntaxe

```sql
BEGIN;

-- Opération 1
INSERT INTO clients (nom) VALUES ('Dupont');

-- Créer un point de sauvegarde
SAVEPOINT etape1;

-- Opération 2
INSERT INTO commandes (id_client) VALUES (1);

-- Problème ! Annuler uniquement l'opération 2
ROLLBACK TO etape1;

-- Opération 2 bis (correction)
INSERT INTO commandes (id_client, montant) VALUES (1, 100);

-- Tout valider
COMMIT;
```

**Résultat :**
- Le client Dupont est créé ✅
- La première commande (sans montant) est annulée ❌
- La deuxième commande (avec montant) est créée ✅

### Exemple pratique : Importation de données

```pascal
procedure TForm1.ImporterDonnees(Fichier: string);  
var  
  Ligne: string;
  NumLigne: Integer;
begin
  NumLigne := 0;

  try
    SQLTransaction1.StartTransaction;

    // Lire le fichier
    while LireLigne(Fichier, Ligne) do
    begin
      Inc(NumLigne);

      // Point de sauvegarde pour chaque ligne
      SQLQuery1.SQL.Text := Format('SAVEPOINT ligne_%d', [NumLigne]);
      SQLQuery1.ExecSQL;

      try
        // Importer cette ligne
        ImporterLigne(Ligne);

      except
        on E: Exception do
        begin
          // Annuler seulement cette ligne
          SQLQuery1.SQL.Text := Format('ROLLBACK TO ligne_%d', [NumLigne]);
          SQLQuery1.ExecSQL;

          // Logger l'erreur
          Memo1.Lines.Add(Format('Ligne %d ignorée : %s',
            [NumLigne, E.Message]));
        end;
      end;
    end;

    // Valider toutes les lignes réussies
    SQLTransaction1.Commit;
    ShowMessage(Format('Import terminé : %d lignes traitées', [NumLigne]));

  except
    SQLTransaction1.Rollback;
    raise;
  end;
end;
```

**Avantage :** Si une ligne est invalide, on continue avec les autres au lieu de tout annuler.

### Libérer un SAVEPOINT

```sql
-- Libérer un point de sauvegarde (on ne peut plus y revenir)
RELEASE SAVEPOINT etape1;
```

### Limites des SAVEPOINT

- ✅ PostgreSQL : support complet
- ⚠️ MySQL/MariaDB : support limité (InnoDB uniquement)
- ✅ SQLite : support natif depuis la version 3.6.8

## Les Verrous (Locks)

### Types de verrous

Les SGBD utilisent des **verrous** pour gérer l'accès concurrent aux données.

#### Verrous de lecture (Shared Lock)

Plusieurs transactions peuvent **lire** la même donnée simultanément.

```sql
SELECT * FROM clients WHERE id = 1;
-- Verrou de lecture (partagé)
-- D'autres transactions peuvent aussi lire
```

#### Verrous d'écriture (Exclusive Lock)

Une seule transaction peut **modifier** une donnée à la fois.

```sql
UPDATE clients SET nom = 'Nouveau' WHERE id = 1;
-- Verrou d'écriture (exclusif)
-- Les autres transactions doivent attendre
```

### Verrouillage explicite

#### SELECT ... FOR UPDATE

**Verrouille les lignes** pour les modifier plus tard dans la même transaction.

```sql
BEGIN;

-- Verrouiller les lignes pour empêcher d'autres modifications
SELECT * FROM comptes WHERE id = 1 FOR UPDATE;

-- Maintenant, on peut modifier en toute sécurité
UPDATE comptes SET solde = solde - 100 WHERE id = 1;

COMMIT;
```

**Utilisation typique :** Éviter les conditions de course (race conditions).

**Exemple : Réservation de place**

```pascal
function TForm1.ReserverPlace(NumPlace: Integer): Boolean;  
begin  
  Result := False;

  try
    SQLTransaction1.StartTransaction;

    // Verrouiller la place
    SQLQuery1.SQL.Text :=
      'SELECT * FROM places WHERE numero = :num FOR UPDATE';
    SQLQuery1.ParamByName('num').AsInteger := NumPlace;
    SQLQuery1.Open;

    // Vérifier si disponible
    if SQLQuery1.FieldByName('disponible').AsBoolean then
    begin
      SQLQuery1.Close;

      // Réserver
      SQLQuery1.SQL.Text :=
        'UPDATE places SET disponible = FALSE, ' +
        'id_client = :id WHERE numero = :num';
      SQLQuery1.ParamByName('id').AsInteger := IDClientActuel;
      SQLQuery1.ParamByName('num').AsInteger := NumPlace;
      SQLQuery1.ExecSQL;

      SQLTransaction1.Commit;
      Result := True;
      ShowMessage('Place réservée !');
    end
    else
    begin
      SQLQuery1.Close;
      SQLTransaction1.Rollback;
      ShowMessage('Place déjà prise');
    end;

  except
    SQLTransaction1.Rollback;
    raise;
  end;
end;
```

#### SELECT ... FOR SHARE

Verrou de lecture partagé, empêche les modifications mais permet d'autres lectures.

```sql
BEGIN;  
SELECT * FROM clients WHERE id = 1 FOR SHARE;  
-- D'autres peuvent lire, mais pas modifier
COMMIT;
```

#### LOCK TABLE

Verrouiller une **table entière** (à utiliser avec précaution).

```sql
BEGIN;  
LOCK TABLE clients IN EXCLUSIVE MODE;  
-- Personne d'autre ne peut accéder à la table
-- Faire les opérations
COMMIT;
```

**Attention :** Très bloquant, à éviter si possible.

## Transactions longues : problèmes et solutions

### Les dangers des transactions longues

**Problèmes :**
- 🔒 Verrous maintenus longtemps → blocage des autres utilisateurs
- 💾 Logs de transaction qui grossissent
- ❌ Risque accru de deadlocks
- ⚠️ Rollback très long si erreur

### Exemple de transaction trop longue (MAL)

```pascal
procedure TForm1.TraitementLong_MAUVAIS;  
var  
  i: Integer;
begin
  SQLTransaction1.StartTransaction;

  for i := 1 to 10000 do
  begin
    SQLQuery1.SQL.Text := 'INSERT INTO logs (message) VALUES (:msg)';
    SQLQuery1.ParamByName('msg').AsString := 'Message ' + IntToStr(i);
    SQLQuery1.ExecSQL;

    // Calcul complexe pour chaque enregistrement
    CalculComplexe(i);  // 1 seconde
  end;

  SQLTransaction1.Commit;
  // Transaction de 10000 secondes = 2h45 ! 😱
end;
```

**Problèmes :**
- Transaction de plusieurs heures
- Bloque l'accès à la table logs
- Impossible d'annuler proprement

### Solution 1 : Transactions par lots

```pascal
procedure TForm1.TraitementLong_BIEN;  
const  
  TAILLE_LOT = 100;
var
  i: Integer;
begin
  for i := 1 to 10000 do
  begin
    // Nouvelle transaction tous les 100 enregistrements
    if (i mod TAILLE_LOT) = 1 then
      SQLTransaction1.StartTransaction;

    SQLQuery1.SQL.Text := 'INSERT INTO logs (message) VALUES (:msg)';
    SQLQuery1.ParamByName('msg').AsString := 'Message ' + IntToStr(i);
    SQLQuery1.ExecSQL;

    // Commit tous les 100
    if (i mod TAILLE_LOT) = 0 then
      SQLTransaction1.Commit;
  end;

  // Commit du reste
  if SQLTransaction1.Active then
    SQLTransaction1.Commit;
end;
```

**Avantages :**
- Transactions courtes (100 insertions)
- Libération régulière des verrous
- En cas d'erreur, perte de max 100 enregistrements

### Solution 2 : Séparer calcul et base de données

```pascal
procedure TForm1.TraitementLong_OPTIMAL;  
var  
  i: Integer;
  Messages: TStringList;
begin
  Messages := TStringList.Create;
  try
    // 1. Préparer les données (hors transaction)
    for i := 1 to 10000 do
    begin
      CalculComplexe(i);
      Messages.Add('Message ' + IntToStr(i));
    end;

    // 2. Insérer rapidement (une seule transaction)
    SQLTransaction1.StartTransaction;
    try
      for i := 0 to Messages.Count - 1 do
      begin
        SQLQuery1.SQL.Text := 'INSERT INTO logs (message) VALUES (:msg)';
        SQLQuery1.ParamByName('msg').AsString := Messages[i];
        SQLQuery1.ExecSQL;
      end;

      SQLTransaction1.Commit;
    except
      SQLTransaction1.Rollback;
      raise;
    end;

  finally
    Messages.Free;
  end;
end;
```

**Avantage :** Transaction très courte, calculs faits avant.

## Mode Autocommit

### Qu'est-ce que l'autocommit ?

En mode **autocommit**, chaque commande SQL est **automatiquement** validée (COMMIT).

**PostgreSQL et MySQL :** Autocommit activé par défaut en ligne de commande.

**Lazarus :** Pas d'autocommit par défaut avec TSQLTransaction.

### Activer/Désactiver

#### PostgreSQL (psql)

```sql
-- Vérifier
SHOW autocommit;

-- Désactiver
\set AUTOCOMMIT off

-- Activer
\set AUTOCOMMIT on
```

#### MySQL (mysql)

```sql
-- Désactiver
SET autocommit = 0;

-- Activer
SET autocommit = 1;
```

### Autocommit et Lazarus

**Par défaut**, avec `TSQLTransaction`, vous devez **explicitement** faire COMMIT. C'est une bonne chose pour la sécurité !

```pascal
// Autocommit simulé (déconseillé)
SQLQuery1.ExecSQL;  
SQLTransaction1.Commit;  // Commit immédiat après chaque opération  

// Mode normal (recommandé)
SQLQuery1.ExecSQL;  
SQLQuery2.ExecSQL;  
SQLQuery3.ExecSQL;  
SQLTransaction1.Commit;  // Un seul commit pour tout  
```

## Différences entre PostgreSQL et MySQL/MariaDB

### Niveaux d'isolation par défaut

| SGBD | Niveau par défaut |
|------|-------------------|
| PostgreSQL | READ COMMITTED |
| MySQL/MariaDB | REPEATABLE READ |

### SAVEPOINT

**PostgreSQL :** Support complet et robuste

**MySQL/MariaDB :** Support limité à InnoDB, pas avec MyISAM

### Détection de deadlocks

**PostgreSQL :**
- Délai configurable (deadlock_timeout)
- Par défaut : 1 seconde

**MySQL/MariaDB :**
- Détection immédiate
- Rollback automatique de la transaction la plus récente

### Verrouillage

**PostgreSQL :**
- Verrous au niveau ligne (row-level)
- Très efficace pour la concurrence

**MySQL/MariaDB :**
- InnoDB : verrous au niveau ligne ✅
- MyISAM : verrous au niveau table ❌ (ancien moteur)

## Bonnes pratiques avancées

### 1. Choisir le bon niveau d'isolation

```pascal
// Niveau de base pour la plupart des cas
BEGIN TRANSACTION ISOLATION LEVEL READ COMMITTED;

// Niveau strict pour opérations financières
BEGIN TRANSACTION ISOLATION LEVEL SERIALIZABLE;
```

### 2. Toujours gérer les deadlocks

```pascal
function TForm1.ExecuterAvecRetry(MaxRetries: Integer): Boolean;  
var  
  Retry: Integer;
begin
  Result := False;
  Retry := 0;

  while Retry < MaxRetries do
  begin
    try
      SQLTransaction1.StartTransaction;
      // Opérations
      SQLTransaction1.Commit;
      Result := True;
      Break;
    except
      on E: EDatabaseError do
      begin
        SQLTransaction1.Rollback;
        if IsDeadlock(E) then
        begin
          Inc(Retry);
          Sleep(Random(100) + 50);  // Attente aléatoire
        end
        else
          raise;
      end;
    end;
  end;
end;

function TForm1.IsDeadlock(E: Exception): Boolean;  
begin  
  Result := (Pos('deadlock', LowerCase(E.Message)) > 0) or
            (Pos('lock wait timeout', LowerCase(E.Message)) > 0);
end;
```

### 3. Utiliser FOR UPDATE quand nécessaire

```pascal
// Réservation de ressource
SQLQuery1.SQL.Text :=
  'SELECT * FROM ressources WHERE id = :id FOR UPDATE';
SQLQuery1.ParamByName('id').AsInteger := IDRessource;  
SQLQuery1.Open;  

if SQLQuery1.FieldByName('disponible').AsBoolean then  
begin  
  // Modification sûre
  SQLQuery2.SQL.Text :=
    'UPDATE ressources SET disponible = FALSE WHERE id = :id';
  SQLQuery2.ParamByName('id').AsInteger := IDRessource;
  SQLQuery2.ExecSQL;
end;
```

### 4. Logger les transactions problématiques

```pascal
procedure TForm1.LogTransaction(Debut: TDateTime; Erreur: string);  
var  
  Duree: Integer;
begin
  Duree := MilliSecondsBetween(Now, Debut);

  WriteLn(Format('[%s] Transaction - Durée: %dms - Erreur: %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
     Duree,
     Erreur]));
end;
```

### 5. Définir des timeouts

**PostgreSQL :**
```sql
-- Timeout de 30 secondes pour les requêtes
SET statement_timeout = '30s';

-- Timeout pour les verrous
SET lock_timeout = '10s';
```

**MySQL/MariaDB :**
```sql
-- Timeout pour les verrous
SET innodb_lock_wait_timeout = 10;
```

### 6. Monitorer les transactions longues

**PostgreSQL :**
```sql
-- Voir les transactions en cours
SELECT
  pid,
  now() - pg_stat_activity.query_start AS duration,
  query,
  state
FROM pg_stat_activity  
WHERE state != 'idle'  
ORDER BY duration DESC;  
```

**MySQL/MariaDB :**
```sql
-- Voir les processus
SHOW PROCESSLIST;

-- Transactions en cours
SELECT * FROM information_schema.innodb_trx;
```

## Résumé

**Niveaux d'isolation :**
- READ UNCOMMITTED : dangereux, éviter
- READ COMMITTED : défaut, suffisant pour la plupart
- REPEATABLE READ : pour stabilité des lectures
- SERIALIZABLE : maximum de sécurité, pour opérations critiques

**Deadlocks :**
- Inévitables en environnement concurrent
- Détectés automatiquement par le SGBD
- Gérer avec retry et attente aléatoire
- Éviter : même ordre d'accès, transactions courtes

**SAVEPOINT :**
- Points de retour partiels
- Utiles pour imports de données
- Support complet PostgreSQL

**Verrous :**
- SELECT ... FOR UPDATE : verrouillage explicite
- Nécessaire pour éviter les race conditions
- Utiliser avec parcimonie

**Transactions longues :**
- Éviter absolument
- Découper en lots
- Séparer calculs et BDD

**Bonnes pratiques :**
- ✅ Transactions courtes
- ✅ Gérer les deadlocks
- ✅ Utiliser le bon niveau d'isolation
- ✅ Logger les problèmes
- ✅ Définir des timeouts
- ✅ Monitorer

Vous maîtrisez maintenant la gestion avancée des transactions. Vos applications seront robustes même en environnement multi-utilisateurs intensif !

---

*Des transactions simples aux concepts avancés : vous êtes maintenant un expert !*

⏭️ [Gestion des erreurs de connexion et résilience](/16-bases-donnees-maitrise-approfondie/13-gestion-erreurs-connexion-resilience.md)
