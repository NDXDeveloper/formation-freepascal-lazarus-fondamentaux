# Chapitre 16 : Bases de donnees — Maitrise approfondie — Exemples

## Fichiers

### Programmes console (7)

| Fichier | Section | Description |
|---|---|---|
| `04-connexion-sqlite.pas` | 16.4 | Connexion SQLite, config (foreign_keys, journal_mode), verification Connected, chemins cross-platform, gestion erreurs |
| `05-requetes-parametrees.pas` | 16.5 | CREATE TABLE, INSERT/SELECT/UPDATE/DELETE avec parametres, protection injection SQL, acces champs par index, gestion NULL |
| `07-navigation-donnees.pas` | 16.7 | First/Last/Next/Prior, MoveBy, recherche SQL (equivalent Locate), Lookup, Bookmarks, DisableControls, export CSV |
| `08-crud-operations.pas` | 16.8 | INSERT/UPDATE/DELETE via SQL parametre, RowsAffected, contrainte UNIQUE, etats dataset |
| `09-transactions.pas` | 16.9 | ACID demo, commit/rollback, transfert bancaire, performance (1 transaction vs N commits), batch par lots |
| `12-savepoints-batch.pas` | 16.12 | SAVEPOINT/ROLLBACK TO/RELEASE, import tolerant aux erreurs, batch par blocs, retry avec backoff |
| `13-resilience-erreurs.pas` | 16.13 | Classification erreurs, retry backoff exponentiel, test connexion, monitoring requetes, logging structure |

### Projet Lazarus (1 projet, 4 fichiers)

| Fichier | Description |
|---|---|
| `06-data-aware/DataAwareDemo.lpr` | Programme principal |
| `06-data-aware/DataAwareDemo.lpi` | Fichier projet (RequiredPackages: LCL + SQLDBLaz) |
| `06-data-aware/unit1.pas` | Formulaire avec TDBGrid, TDBNavigator, TDBEdit, TDBMemo, validation BeforePost |
| `06-data-aware/unit1.lfm` | Design formulaire |

### Sections sans exemples (theorie)

- **16.1** Concepts BDD relationnelles (theorie pure)
- **16.2** Introduction SQL (reference SQL)
- **16.3** SQLite base embarquee (theorie + CLI)
- **16.10** Introduction Client/Serveur (theorie architecture)
- **16.11** Connexion PostgreSQL/MariaDB (necessite serveur externe)

## Prerequis

- Free Pascal Compiler (fpc) 3.2.2+
- Lazarus 3.0+ (pour le projet 06-data-aware uniquement)
- Bibliotheque SQLite3 installee (`libsqlite3-0` sous Debian/Ubuntu)

## Compilation

### Programmes console

```bash
cd exemples/
fpc 04-connexion-sqlite.pas
fpc 05-requetes-parametrees.pas
fpc 07-navigation-donnees.pas
fpc 08-crud-operations.pas
fpc 09-transactions.pas
fpc 12-savepoints-batch.pas
fpc 13-resilience-erreurs.pas
```

### Projet Lazarus

```bash
cd exemples/06-data-aware
lazbuild DataAwareDemo.lpi
```

## Execution

Chaque programme console cree une base SQLite temporaire dans `/tmp/`, affiche ses resultats et supprime la base a la fin.

```bash
./04-connexion-sqlite
./05-requetes-parametrees
./07-navigation-donnees
./08-crud-operations
./09-transactions
./12-savepoints-batch
./13-resilience-erreurs
```

## Nettoyage

```bash
cd exemples/
rm -f *.o *.ppu
rm -f 04-connexion-sqlite 05-requetes-parametrees 07-navigation-donnees
rm -f 08-crud-operations 09-transactions 12-savepoints-batch 13-resilience-erreurs
rm -rf 06-data-aware/lib 06-data-aware/*.res 06-data-aware/*.o 06-data-aware/*.ppu
find 06-data-aware -maxdepth 1 -type f -executable -delete
```

## Notes techniques

- Les colonnes TEXT de SQLite sont rapportees comme `ftMemo` par FPC, ce qui empeche `Locate`/`Lookup` de fonctionner. Les exemples utilisent des requetes SQL `WHERE` a la place.
- `Trans.Commit` ferme automatiquement les datasets ouverts (`TSQLQuery.Open`). Les operations CRUD utilisent `ExecSQL` avec parametres pour eviter ce probleme.
- Les fichiers `.db`, `-wal` et `-shm` temporaires sont nettoyes automatiquement par chaque programme.
