🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.9 Transactions basics

## Introduction : le concept de transaction

Imaginez que vous êtes à la banque et que vous voulez transférer 100€ de votre compte vers celui de votre ami. Cette opération nécessite **deux actions** :

1. **Débiter** 100€ de votre compte
2. **Créditer** 100€ sur le compte de votre ami

Que se passerait-il si l'ordinateur plante juste après avoir débité votre compte mais **avant** de créditer celui de votre ami ? Vous auriez perdu 100€ !

C'est exactement ce problème que les **transactions** résolvent.

### L'analogie du "tout ou rien"

Une transaction, c'est comme dire à la base de données :

> "Voici plusieurs opérations. Soit tu fais **TOUTES** ces opérations avec succès, soit tu n'en fais **AUCUNE**. Pas de demi-mesure !"

C'est le principe du **"tout ou rien"** (all-or-nothing).

**Sans transaction :**
```
1. Débiter 100€ du compte A    ✅ Réussi
2. CRASH ! 💥
3. Créditer 100€ au compte B   ❌ Jamais exécuté
→ Résultat : L'argent a disparu !
```

**Avec transaction :**
```
BEGIN TRANSACTION
1. Débiter 100€ du compte A    ✅ Réussi
2. CRASH ! 💥
→ ROLLBACK automatique
→ Résultat : Tout est annulé, les deux comptes sont intacts
```

## Qu'est-ce qu'une transaction ?

Une **transaction** est un **groupe d'opérations SQL** qui doivent être exécutées ensemble, comme une unité indivisible.

### Définition formelle

Une transaction garantit que :
- **Soit** toutes les opérations réussissent → on les **valide** (COMMIT)
- **Soit** au moins une échoue → on **annule tout** (ROLLBACK)

### Les trois commandes fondamentales

**1. BEGIN TRANSACTION** (ou START TRANSACTION)
- Démarre une transaction
- Dit : "À partir de maintenant, ne valide rien dans la base"

**2. COMMIT**
- Valide toutes les modifications
- Dit : "Tout s'est bien passé, enregistre définitivement"

**3. ROLLBACK**
- Annule toutes les modifications
- Dit : "Il y a eu un problème, annule tout et reviens en arrière"

### Exemple simple

```sql
-- Démarrer la transaction
BEGIN TRANSACTION;

-- Opération 1
UPDATE Comptes SET solde = solde - 100 WHERE id = 1;

-- Opération 2
UPDATE Comptes SET solde = solde + 100 WHERE id = 2;

-- Si tout s'est bien passé :
COMMIT;

-- Ou en cas d'erreur :
-- ROLLBACK;
```

## Les propriétés ACID

Les transactions garantissent quatre propriétés importantes, résumées par l'acronyme **ACID** :

### A - Atomicité (Atomicity)

**Tout ou rien** : une transaction est indivisible.
- Soit toutes les opérations réussissent
- Soit aucune n'est appliquée

**Analogie :** Comme un atome qu'on ne peut pas diviser.

### C - Cohérence (Consistency)

Les données passent d'un **état cohérent** à un autre **état cohérent**.
- Les contraintes sont respectées
- Les règles métier sont préservées

**Exemple :** La somme des soldes de tous les comptes reste constante après un virement.

### I - Isolation (Isolation)

Chaque transaction s'exécute **comme si elle était seule**.
- Une transaction ne voit pas les modifications non validées des autres
- Pas d'interférence entre transactions simultanées

**Analogie :** Comme des cabines d'essayage séparées.

### D - Durabilité (Durability)

Une fois validée (COMMIT), une transaction est **définitive**.
- Même si le système plante juste après
- Les données sont sauvegardées sur le disque

**Analogie :** Comme graver dans le marbre.

## Transactions dans SQLite

SQLite gère les transactions automatiquement, mais vous pouvez (et devez) les contrôler explicitement.

### Mode automatique par défaut

**Par défaut**, SQLite enveloppe **chaque commande** dans une transaction automatique :

```sql
INSERT INTO Clients (nom) VALUES ('Dupont');
-- SQLite fait automatiquement :
-- BEGIN
-- INSERT INTO Clients (nom) VALUES ('Dupont');
-- COMMIT
```

**Problème avec le mode automatique :**

Si vous exécutez plusieurs commandes séparément, chacune est une transaction indépendante :

```sql
UPDATE Comptes SET solde = solde - 100 WHERE id = 1;  -- Transaction 1
-- Si crash ici, le débit est validé mais pas le crédit !
UPDATE Comptes SET solde = solde + 100 WHERE id = 2;  -- Transaction 2
```

### Mode manuel (recommandé)

Contrôlez explicitement vos transactions pour grouper les opérations :

```sql
BEGIN TRANSACTION;

UPDATE Comptes SET solde = solde - 100 WHERE id = 1;  
UPDATE Comptes SET solde = solde + 100 WHERE id = 2;  

COMMIT;
```

Maintenant, les deux opérations forment **une seule transaction atomique**.

### BEGIN, COMMIT, ROLLBACK en SQLite

**Commencer une transaction :**
```sql
BEGIN TRANSACTION;
-- ou simplement
BEGIN;
```

**Valider (enregistrer) :**
```sql
COMMIT;
```

**Annuler (revenir en arrière) :**
```sql
ROLLBACK;
```

### Exemple complet avec gestion d'erreur

```sql
BEGIN TRANSACTION;

-- Vérifier le solde avant débit
SELECT solde FROM Comptes WHERE id = 1;
-- Supposons que le résultat est 150€

-- Si le solde est suffisant (≥ 100€)
UPDATE Comptes SET solde = solde - 100 WHERE id = 1;  
UPDATE Comptes SET solde = solde + 100 WHERE id = 2;  

-- Tout va bien
COMMIT;

-- Si erreur ou solde insuffisant
-- ROLLBACK;
```

## Transactions avec TSQLTransaction dans Lazarus

Dans Lazarus, le composant `TSQLTransaction` gère les transactions pour vous.

### Le composant TSQLTransaction

Vous avez déjà utilisé `TSQLTransaction` dans les sections précédentes. Voyons maintenant son rôle en détail.

**Rappel de la configuration :**
```pascal
// Lier la transaction à la connexion
SQLTransaction1.Database := SQLite3Connection1;

// Lier la connexion à la transaction
SQLite3Connection1.Transaction := SQLTransaction1;

// Lier les requêtes à la transaction
SQLQuery1.Transaction := SQLTransaction1;
```

### Fonctionnement automatique

**Par défaut**, `TSQLTransaction` démarre automatiquement une transaction lors de la première opération SQL.

```pascal
// Automatiquement, TSQLTransaction fait :
// BEGIN TRANSACTION

SQLQuery1.SQL.Text := 'UPDATE Clients SET nom = :nom WHERE id = :id';  
SQLQuery1.ParamByName('nom').AsString := 'Nouveau Nom';  
SQLQuery1.ParamByName('id').AsInteger := 1;  
SQLQuery1.ExecSQL;  

// La transaction est en cours, mais pas encore validée !
```

### Commit : Valider la transaction

```pascal
SQLTransaction1.Commit;
```

**Effet :**
- Valide toutes les modifications depuis le BEGIN
- Les données sont définitivement enregistrées dans la base
- Une nouvelle transaction démarre automatiquement

**Exemple complet :**
```pascal
procedure TForm1.ModifierClient;  
begin  
  try
    SQLQuery1.SQL.Text := 'UPDATE Clients SET nom = :nom WHERE id = :id';
    SQLQuery1.ParamByName('nom').AsString := 'Nouveau Nom';
    SQLQuery1.ParamByName('id').AsInteger := 1;
    SQLQuery1.ExecSQL;

    SQLTransaction1.Commit;  // ← IMPORTANT !
    ShowMessage('Modification enregistrée');
  except
    on E: Exception do
    begin
      SQLTransaction1.Rollback;  // Annuler en cas d'erreur
      ShowMessage('Erreur : ' + E.Message);
    end;
  end;
end;
```

### Rollback : Annuler la transaction

```pascal
SQLTransaction1.Rollback;
```

**Effet :**
- Annule toutes les modifications depuis le BEGIN
- Les données reviennent à leur état d'avant la transaction
- Une nouvelle transaction démarre automatiquement

**Exemple :**
```pascal
procedure TForm1.OperationRisquee;  
begin  
  try
    // Plusieurs opérations
    SQLQuery1.SQL.Text := 'DELETE FROM Commandes WHERE id = :id';
    SQLQuery1.ParamByName('id').AsInteger := 100;
    SQLQuery1.ExecSQL;

    SQLQuery1.SQL.Text := 'UPDATE Stats SET nb_commandes = nb_commandes - 1';
    SQLQuery1.ExecSQL;

    // Si tout va bien
    SQLTransaction1.Commit;
  except
    on E: Exception do
    begin
      // En cas d'erreur, annuler TOUT
      SQLTransaction1.Rollback;
      ShowMessage('Opération annulée : ' + E.Message);
    end;
  end;
end;
```

### StartTransaction : Démarrer explicitement

Normalement, vous n'avez pas besoin d'appeler `StartTransaction` car c'est automatique. Mais vous pouvez le faire :

```pascal
SQLTransaction1.StartTransaction;  
try  
  // Vos opérations
  SQLQuery1.ExecSQL;

  SQLTransaction1.Commit;
except
  SQLTransaction1.Rollback;
  raise;
end;
```

### Propriété Active

Indique si une transaction est en cours :

```pascal
if SQLTransaction1.Active then
  ShowMessage('Transaction en cours')
else
  ShowMessage('Pas de transaction');
```

## Exemple pratique : Transfert d'argent

Voici un exemple complet d'utilisation de transactions pour un transfert bancaire sécurisé.

### Version sans transaction (DANGEREUX !)

```pascal
procedure TForm1.TransfererArgent_MAUVAIS(
  IDSource, IDDest: Integer;
  Montant: Double);
begin
  // Débiter le compte source
  SQLQuery1.SQL.Text :=
    'UPDATE Comptes SET solde = solde - :montant WHERE id = :id';
  SQLQuery1.ParamByName('montant').AsFloat := Montant;
  SQLQuery1.ParamByName('id').AsInteger := IDSource;
  SQLQuery1.ExecSQL;
  SQLTransaction1.Commit;  // ← Validé trop tôt !

  // Si crash ici, l'argent est perdu ! 💥

  // Créditer le compte destination
  SQLQuery1.SQL.Text :=
    'UPDATE Comptes SET solde = solde + :montant WHERE id = :id';
  SQLQuery1.ParamByName('montant').AsFloat := Montant;
  SQLQuery1.ParamByName('id').AsInteger := IDDest;
  SQLQuery1.ExecSQL;
  SQLTransaction1.Commit;
end;
```

**Problème :** Si une erreur survient entre les deux opérations, le premier commit est déjà fait !

### Version avec transaction (SÉCURISÉE)

```pascal
procedure TForm1.TransfererArgent(
  IDSource, IDDest: Integer;
  Montant: Double);
var
  SoldeSource: Double;
begin
  try
    // 1. Vérifier le solde source
    SQLQuery1.SQL.Text := 'SELECT solde FROM Comptes WHERE id = :id';
    SQLQuery1.ParamByName('id').AsInteger := IDSource;
    SQLQuery1.Open;

    if SQLQuery1.IsEmpty then
    begin
      ShowMessage('Compte source introuvable');
      Exit;
    end;

    SoldeSource := SQLQuery1.FieldByName('solde').AsFloat;
    SQLQuery1.Close;

    // 2. Vérifier que le solde est suffisant
    if SoldeSource < Montant then
    begin
      ShowMessage(Format('Solde insuffisant : %.2f € disponibles', [SoldeSource]));
      Exit;
    end;

    // 3. Débiter le compte source
    SQLQuery1.SQL.Text :=
      'UPDATE Comptes SET solde = solde - :montant WHERE id = :id';
    SQLQuery1.ParamByName('montant').AsFloat := Montant;
    SQLQuery1.ParamByName('id').AsInteger := IDSource;
    SQLQuery1.ExecSQL;

    // 4. Créditer le compte destination
    SQLQuery1.SQL.Text :=
      'UPDATE Comptes SET solde = solde + :montant WHERE id = :id';
    SQLQuery1.ParamByName('montant').AsFloat := Montant;
    SQLQuery1.ParamByName('id').AsInteger := IDDest;
    SQLQuery1.ExecSQL;

    // 5. Valider TOUT en une seule fois
    SQLTransaction1.Commit;

    ShowMessage(Format('Transfert de %.2f € effectué avec succès', [Montant]));

  except
    on E: Exception do
    begin
      // En cas d'erreur, annuler TOUT
      SQLTransaction1.Rollback;
      ShowMessage('Erreur lors du transfert : ' + E.Message);
    end;
  end;
end;
```

**Avantages :**
- ✅ Atomicité : les deux opérations réussissent ou aucune
- ✅ Cohérence : le total des soldes reste constant
- ✅ Pas de perte d'argent possible
- ✅ Rollback automatique en cas d'erreur

## Cas d'usage courants des transactions

### 1. Opérations multiples liées

**Exemple : Créer une commande avec ses lignes**

```pascal
procedure TForm1.CreerCommande(IDClient: Integer; Articles: TListeArticles);  
var  
  IDCommande: Integer;
  i: Integer;
begin
  try
    // 1. Insérer la commande
    SQLQuery1.SQL.Text :=
      'INSERT INTO Commandes (id_client, date, montant_total) ' +
      'VALUES (:id_client, :date, :montant)';
    SQLQuery1.ParamByName('id_client').AsInteger := IDClient;
    SQLQuery1.ParamByName('date').AsDateTime := Now;
    SQLQuery1.ParamByName('montant').AsFloat := Articles.CalculerTotal;
    SQLQuery1.ExecSQL;

    // 2. Récupérer l'ID de la commande insérée
    SQLQuery1.SQL.Text := 'SELECT last_insert_rowid() AS id';
    SQLQuery1.Open;
    IDCommande := SQLQuery1.FieldByName('id').AsInteger;
    SQLQuery1.Close;

    // 3. Insérer chaque ligne de commande
    for i := 0 to Articles.Count - 1 do
    begin
      SQLQuery1.SQL.Text :=
        'INSERT INTO LignesCommande (id_commande, id_article, quantite, prix) ' +
        'VALUES (:id_cmd, :id_art, :qte, :prix)';
      SQLQuery1.ParamByName('id_cmd').AsInteger := IDCommande;
      SQLQuery1.ParamByName('id_art').AsInteger := Articles[i].ID;
      SQLQuery1.ParamByName('qte').AsInteger := Articles[i].Quantite;
      SQLQuery1.ParamByName('prix').AsFloat := Articles[i].Prix;
      SQLQuery1.ExecSQL;

      // 4. Décrémenter le stock
      SQLQuery1.SQL.Text :=
        'UPDATE Articles SET stock = stock - :qte WHERE id = :id';
      SQLQuery1.ParamByName('qte').AsInteger := Articles[i].Quantite;
      SQLQuery1.ParamByName('id').AsInteger := Articles[i].ID;
      SQLQuery1.ExecSQL;
    end;

    // 5. Tout valider en une fois
    SQLTransaction1.Commit;

    ShowMessage(Format('Commande n°%d créée avec succès', [IDCommande]));

  except
    on E: Exception do
    begin
      SQLTransaction1.Rollback;
      ShowMessage('Erreur lors de la création de la commande : ' + E.Message);
    end;
  end;
end;
```

**Sans transaction :** Si une erreur survient lors de l'insertion d'une ligne, la commande principale existe déjà (incohérence), et certains stocks ont été décrémentés (perte de données).

**Avec transaction :** Tout est annulé proprement en cas d'erreur.

### 2. Suppression en cascade

**Exemple : Supprimer un client et toutes ses données**

```pascal
procedure TForm1.SupprimerClientComplet(IDClient: Integer);  
var  
  NomClient: string;
begin
  // Récupérer le nom pour confirmation
  SQLQuery1.SQL.Text := 'SELECT nom FROM Clients WHERE id = :id';
  SQLQuery1.ParamByName('id').AsInteger := IDClient;
  SQLQuery1.Open;

  if SQLQuery1.IsEmpty then
  begin
    ShowMessage('Client introuvable');
    SQLQuery1.Close;
    Exit;
  end;

  NomClient := SQLQuery1.FieldByName('nom').AsString;
  SQLQuery1.Close;

  // Demander confirmation
  if MessageDlg(
    Format('Supprimer le client "%s" et toutes ses données ?', [NomClient]),
    mtWarning, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  try
    // 1. Supprimer les lignes de commande
    SQLQuery1.SQL.Text :=
      'DELETE FROM LignesCommande ' +
      'WHERE id_commande IN (SELECT id FROM Commandes WHERE id_client = :id)';
    SQLQuery1.ParamByName('id').AsInteger := IDClient;
    SQLQuery1.ExecSQL;

    // 2. Supprimer les commandes
    SQLQuery1.SQL.Text := 'DELETE FROM Commandes WHERE id_client = :id';
    SQLQuery1.ParamByName('id').AsInteger := IDClient;
    SQLQuery1.ExecSQL;

    // 3. Supprimer les factures
    SQLQuery1.SQL.Text := 'DELETE FROM Factures WHERE id_client = :id';
    SQLQuery1.ParamByName('id').AsInteger := IDClient;
    SQLQuery1.ExecSQL;

    // 4. Supprimer le client
    SQLQuery1.SQL.Text := 'DELETE FROM Clients WHERE id = :id';
    SQLQuery1.ParamByName('id').AsInteger := IDClient;
    SQLQuery1.ExecSQL;

    // Valider tout
    SQLTransaction1.Commit;

    ShowMessage('Client et toutes ses données supprimés');

  except
    on E: Exception do
    begin
      SQLTransaction1.Rollback;
      ShowMessage('Erreur lors de la suppression : ' + E.Message);
    end;
  end;
end;
```

### 3. Traitement par lot

**Exemple : Mise à jour de prix en masse**

```pascal
procedure TForm1.AugmenterPrixCategorie(
  Categorie: string;
  PourcentageAugmentation: Double);
var
  NbArticles: Integer;
begin
  try
    // Compter les articles concernés
    SQLQuery1.SQL.Text :=
      'SELECT COUNT(*) AS nb FROM Articles WHERE categorie = :cat';
    SQLQuery1.ParamByName('cat').AsString := Categorie;
    SQLQuery1.Open;
    NbArticles := SQLQuery1.FieldByName('nb').AsInteger;
    SQLQuery1.Close;

    if NbArticles = 0 then
    begin
      ShowMessage('Aucun article dans cette catégorie');
      Exit;
    end;

    // Demander confirmation
    if MessageDlg(
      Format('Augmenter de %.1f%% le prix de %d article(s) ?',
             [PourcentageAugmentation, NbArticles]),
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;

    // Effectuer la mise à jour
    SQLQuery1.SQL.Text :=
      'UPDATE Articles ' +
      'SET prix = prix * :facteur ' +
      'WHERE categorie = :cat';
    SQLQuery1.ParamByName('facteur').AsFloat := 1 + (PourcentageAugmentation / 100);
    SQLQuery1.ParamByName('cat').AsString := Categorie;
    SQLQuery1.ExecSQL;

    // Valider
    SQLTransaction1.Commit;

    ShowMessage(Format('%d article(s) mis à jour', [NbArticles]));

  except
    on E: Exception do
    begin
      SQLTransaction1.Rollback;
      ShowMessage('Erreur : ' + E.Message);
    end;
  end;
end;
```

## Transactions imbriquées et points de sauvegarde

### SQLite et les transactions imbriquées

**Important :** SQLite ne supporte **pas** les transactions imbriquées classiques.

```sql
-- Ceci ne fonctionne PAS comme prévu dans SQLite
BEGIN TRANSACTION;
  -- Opérations
  BEGIN TRANSACTION;  -- Ignoré !
    -- Plus d'opérations
  COMMIT;  -- Ignoré !
COMMIT;  -- Seul celui-ci compte
```

### Alternative : SAVEPOINT

SQLite offre les **points de sauvegarde** (savepoints) pour des transactions partielles :

```sql
BEGIN TRANSACTION;

-- Opération 1
INSERT INTO Clients (nom) VALUES ('Dupont');

-- Créer un point de sauvegarde
SAVEPOINT etape1;

-- Opération 2
INSERT INTO Commandes (id_client) VALUES (1);

-- Si problème, revenir au savepoint
ROLLBACK TO etape1;  -- Annule uniquement l'opération 2

-- Ou continuer
COMMIT;  -- Valide tout (opération 1 et peut-être 2)
```

**En pratique pour les débutants :** Évitez les savepoints pour l'instant. Structurez plutôt votre code pour avoir des transactions simples et complètes.

## Performance et transactions

### Les transactions améliorent les performances !

Contre-intuitivement, utiliser des transactions **explicites** peut grandement **améliorer** les performances.

**Pourquoi ?**

Sans transaction explicite, SQLite crée une transaction pour **chaque** commande :

```pascal
// LENT : 1000 transactions (une par INSERT)
for i := 1 to 1000 do  
begin  
  SQLQuery1.SQL.Text := 'INSERT INTO Logs (message) VALUES (:msg)';
  SQLQuery1.ParamByName('msg').AsString := 'Message ' + IntToStr(i);
  SQLQuery1.ExecSQL;
  SQLTransaction1.Commit;  // ← 1000 commits = TRÈS LENT
end;
```

Avec une seule transaction :

```pascal
// RAPIDE : 1 seule transaction
SQLTransaction1.StartTransaction;  
try  
  for i := 1 to 1000 do
  begin
    SQLQuery1.SQL.Text := 'INSERT INTO Logs (message) VALUES (:msg)';
    SQLQuery1.ParamByName('msg').AsString := 'Message ' + IntToStr(i);
    SQLQuery1.ExecSQL;
    // Pas de Commit ici
  end;

  SQLTransaction1.Commit;  // ← Un seul commit = RAPIDE
except
  SQLTransaction1.Rollback;
  raise;
end;
```

**Gain de performance :** Facilement **100 fois plus rapide** !

### Compromis : transactions par lots

Pour de très grandes quantités de données, un compromis est de faire des transactions par lots :

```pascal
const
  TAILLE_LOT = 1000;

var
  i: Integer;
begin
  for i := 1 to 100000 do
  begin
    // Démarrer une transaction tous les 1000 enregistrements
    if (i mod TAILLE_LOT) = 1 then
      SQLTransaction1.StartTransaction;

    SQLQuery1.SQL.Text := 'INSERT INTO Logs (message) VALUES (:msg)';
    SQLQuery1.ParamByName('msg').AsString := 'Message ' + IntToStr(i);
    SQLQuery1.ExecSQL;

    // Commiter tous les 1000 enregistrements
    if (i mod TAILLE_LOT) = 0 then
      SQLTransaction1.Commit;
  end;

  // Commiter le reste
  if SQLTransaction1.Active then
    SQLTransaction1.Commit;
end;
```

**Avantages :**
- ✅ Performance excellente
- ✅ Moins de mémoire utilisée
- ✅ En cas d'erreur, on perd au maximum 1000 enregistrements

## Gestion d'erreurs robuste

### Pattern recommandé

Voici le pattern à utiliser systématiquement :

```pascal
procedure TForm1.OperationAvecTransaction;  
begin  
  try
    // Vos opérations SQL
    SQLQuery1.ExecSQL;
    // Plus d'opérations...

    // Si tout va bien
    SQLTransaction1.Commit;

  except
    on E: Exception do
    begin
      // En cas d'erreur, annuler
      SQLTransaction1.Rollback;

      // Logger l'erreur (optionnel)
      LogError('OperationAvecTransaction', E.Message);

      // Informer l'utilisateur
      ShowMessage('Erreur : ' + E.Message);

      // Propager l'exception si nécessaire
      // raise;
    end;
  end;
end;
```

### Vérifier l'état de la transaction

```pascal
procedure TForm1.OperationSecurisee;  
begin  
  try
    // Vos opérations
    SQLQuery1.ExecSQL;

    // Vérifier si une transaction est active avant de commiter
    if SQLTransaction1.Active then
      SQLTransaction1.Commit;

  except
    on E: Exception do
    begin
      if SQLTransaction1.Active then
        SQLTransaction1.Rollback;
      ShowMessage('Erreur : ' + E.Message);
    end;
  end;
end;
```

## Bonnes pratiques

### 1. Une transaction = une unité logique

Groupez les opérations qui doivent réussir ou échouer ensemble.

```pascal
// BIEN : tout dans une transaction
try
  CreerCommande();
  DecrementerStock();
  EnvoyerEmail();
  SQLTransaction1.Commit;
except
  SQLTransaction1.Rollback;
end;
```

### 2. Transactions courtes

Gardez vos transactions aussi **courtes** que possible.

```pascal
// MAL : transaction trop longue
try
  SQLQuery1.ExecSQL;
  Sleep(5000);  // Attente = mauvais !
  TraitementLong();  // Calcul long = mauvais !
  SQLTransaction1.Commit;
except
  SQLTransaction1.Rollback;
end;

// BIEN : transaction courte
try
  SQLQuery1.ExecSQL;
  SQLTransaction1.Commit;
except
  SQLTransaction1.Rollback;
end;
// Puis faire les traitements longs
TraitementLong();
```

**Pourquoi ?** Une longue transaction peut bloquer d'autres accès à la base.

### 3. Toujours gérer les erreurs

```pascal
// MAL : pas de gestion d'erreur
SQLQuery1.ExecSQL;  
SQLTransaction1.Commit;  

// BIEN : gestion d'erreur
try
  SQLQuery1.ExecSQL;
  SQLTransaction1.Commit;
except
  SQLTransaction1.Rollback;
  raise;
end;
```

### 4. Commit après Post/Delete

Avec les composants data-aware :

```pascal
procedure TForm1.SQLQuery1AfterPost(DataSet: TDataSet);  
begin  
  try
    SQLTransaction1.Commit;
  except
    SQLTransaction1.Rollback;
    raise;
  end;
end;

procedure TForm1.SQLQuery1AfterDelete(DataSet: TDataSet);  
begin  
  try
    SQLTransaction1.Commit;
  except
    SQLTransaction1.Rollback;
    raise;
  end;
end;
```

### 5. Éviter les transactions dans les boucles de lecture

```pascal
// MAL : Commit dans une boucle de lecture
SQLQuery1.Open;  
while not SQLQuery1.EOF do  
begin  
  // Traitement
  SQLTransaction1.Commit;  // ← Inutile et lent
  SQLQuery1.Next;
end;

// BIEN : Pas de Commit pour la lecture
SQLQuery1.Open;  
while not SQLQuery1.EOF do  
begin  
  // Traitement de lecture uniquement
  SQLQuery1.Next;
end;
```

### 6. Documenter les transactions complexes

```pascal
procedure TForm1.TransactionComplexe;  
begin  
  try
    // Transaction groupée :
    // 1. Création facture
    // 2. Génération des lignes
    // 3. Mise à jour des stocks
    // Tout ou rien !

    CreerFacture();
    GenererLignes();
    MettreAJourStocks();

    SQLTransaction1.Commit;
  except
    SQLTransaction1.Rollback;
    raise;
  end;
end;
```

### 7. Tester le comportement en cas d'erreur

```pascal
procedure TForm1.TesterRollback;  
begin  
  try
    SQLQuery1.SQL.Text := 'INSERT INTO Clients (nom) VALUES (''Test'')';
    SQLQuery1.ExecSQL;

    // Forcer une erreur pour tester le rollback
    raise Exception.Create('Test de rollback');

    SQLTransaction1.Commit;
  except
    SQLTransaction1.Rollback;
    ShowMessage('Rollback effectué : le client Test n''a pas été ajouté');
  end;
end;
```

## Résumé

**Transaction = groupe d'opérations atomiques**
- Tout réussit ou tout échoue
- Garantit la cohérence des données

**Trois commandes :**
- `BEGIN` : démarrer
- `COMMIT` : valider
- `ROLLBACK` : annuler

**ACID :**
- **A**tomicité : tout ou rien
- **C**ohérence : état valide → état valide
- **I**solation : pas d'interférence
- **D**urabilité : permanent après commit

**TSQLTransaction :**
- `StartTransaction` : démarrer (automatique par défaut)
- `Commit` : valider
- `Rollback` : annuler

**Pattern fondamental :**
```pascal
try
  // Opérations SQL
  SQLTransaction1.Commit;
except
  SQLTransaction1.Rollback;
  raise;
end;
```

**Utilisez les transactions pour :**
- ✅ Opérations multiples liées
- ✅ Garantir la cohérence
- ✅ Améliorer les performances (insertions en masse)
- ✅ Permettre l'annulation en cas d'erreur

**Bonnes pratiques :**
- Transactions courtes
- Toujours gérer les erreurs
- Grouper les opérations logiquement liées
- Commit après chaque Post/Delete

Les transactions sont la **clé de voûte** de la fiabilité d'une application de base de données !

---

*Avec les transactions, vos données sont protégées contre les incohérences et les erreurs. Tout ou rien !*

⏭️ [Introduction aux bases Client/Serveur (concepts)](/16-bases-donnees-maitrise-approfondie/10-introduction-bases-client-serveur.md)
