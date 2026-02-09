# Compilation des exemples - Chapitre 08 : Gestion des fichiers et I/O

## Compilateur

```bash
fpc fichier.pas
```

Version testée : Free Pascal Compiler 3.2.2+dfsg-32 (Linux x86-64)

## Nettoyage après compilation

```bash
rm -f *.o
# Supprimer les exécutables (même nom que les .pas sans extension)
```

## Liste des exemples (30 fichiers)

### Section 8.1 : Types de fichiers (texte, binaire, typé)

Aucun exemple compilable (section théorique).

### Section 8.2 : Fichiers texte - ouverture, lecture, écriture

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `02-ecrire-fichier.pas` | Écriture dans un fichier texte (nombres.txt) | Non |
| `02-lire-fichier.pas` | Lecture d'un fichier texte ligne par ligne | Non (*) |
| `02-ajouter-fichier.pas` | Ajout de lignes dans un fichier existant (Append) | Non (*) |
| `02-lecture-securisee.pas` | Lecture avec gestion d'erreurs ({$I-} / IOResult) | Non |

(*) `02-lire-fichier.pas` nécessite que `02-ecrire-fichier` ait été exécuté avant (crée nombres.txt).
(*) `02-ajouter-fichier.pas` nécessite un fichier journal.txt existant.

### Section 8.3 : Fichiers binaires et accès direct

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `03-ecrire-lire-binaire.pas` | Écriture/lecture binaire avec BlockWrite/BlockRead | Non |
| `03-copier-fichier.pas` | Copie de fichier binaire (auto-contenu) | Non |
| `03-base-de-donnees.pas` | Base de données de contacts binaire | Oui (ReadLn) |

### Section 8.4 : Fichiers typés

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `04-gestion-employes.pas` | Gestion d'employés avec File of TEmploye | Non |
| `04-carnet-adresses.pas` | Carnet d'adresses interactif | Oui (ReadLn) |

### Section 8.5 : Gestion des erreurs I/O

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `05-ouverture-securisee.pas` | Gestion d'erreurs avec {$I-} et IOResult | Non |
| `05-ecriture-securisee.pas` | Écriture sécurisée avec vérification IOResult | Non |
| `05-gestion-moderne.pas` | Gestion d'erreurs moderne avec try-except | Non |
| `05-config-robuste.pas` | Lecteur de configuration robuste avec valeurs par défaut | Non |

### Section 8.6 : Manipulation de répertoires

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `06-repertoire-courant.pas` | Afficher le répertoire courant (GetCurrentDir) | Non |
| `06-lister-fichiers.pas` | Lister le contenu d'un répertoire (FindFirst/FindNext) | Non |
| `06-chercher-fichiers-texte.pas` | Rechercher des fichiers par extension (*.pas) | Non |
| `06-gestionnaire-repertoires.pas` | Gestionnaire interactif de répertoires | Oui (ReadLn) |

### Section 8.7 : Chemins et noms de fichiers

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `07-decomposer-chemin.pas` | Décomposition d'un chemin en ses composants | Non |
| `07-constantes-portabilite.pas` | Constantes de portabilité Windows/Linux | Non |
| `07-construire-chemin.pas` | Construction portable de chemins | Non |
| `07-valider-nom.pas` | Validation d'un nom de fichier | Non |
| `07-gestionnaire-chemin.pas` | Gestionnaire interactif de chemins | Oui (ReadLn) |

### Section 8.8 : Fichiers INI pour configuration

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `08-demo-ini.pas` | Démonstration de TIniFile (écriture, lecture, sections) | Non |
| `08-config-connexion.pas` | Configuration de connexion avec TMemIniFile | Non |
| `08-gestion-preferences.pas` | Gestionnaire interactif de préférences | Oui (ReadLn) |

### Section 8.9 : Introduction aux streams

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `09-ecrire-lire-stream.pas` | Écriture et lecture avec TFileStream | Non |
| `09-stream-memoire.pas` | TMemoryStream : données en RAM, SaveToFile/LoadFromFile | Non |
| `09-stream-chaine.pas` | TStringStream : manipulation de chaînes | Non |
| `09-ecrire-structures.pas` | Écriture/lecture de records dans un TFileStream | Non |
| `09-copier-stream.pas` | Copie de fichier avec TFileStream.CopyFrom | Non |

## Notes

- Tous les fichiers utilisent `{$mode objfpc}{$H+}` pour le support des AnsiStrings et du mot-clé `Result`.
- Les programmes auto-contenus créent et nettoient leurs fichiers temporaires automatiquement.
- Les programmes interactifs (ReadLn) ne peuvent pas être testés automatiquement.

## Corrections apportées aux fichiers .md

### Corrections de contenu

1. **`04-fichiers-types.md`** : L'affichage attendu montrait des chaînes alignées à gauche, mais le spécificateur Pascal `:20` aligne à droite. Corrigé.
2. **`07-chemins-noms-fichiers.md`** : `ExeExt` n'existe pas en FPC 3.2.2. Remplacé par `SharedSuffix` (suffixe des bibliothèques partagées).
3. **`09-introduction-streams.md`** : `TStringStream.WriteString` après `Create` écrit à la position 0 (écrase le contenu). Ajouté `Stream.Seek(0, soFromEnd)` avant l'appel.

### Programmes rendus auto-contenus (création de données test + nettoyage)

4. **`03-fichiers-binaires-acces-direct.md`** : `CopierFichier` utilisait `image.jpg` inexistant. Remplacé par création d'un fichier test + nettoyage.
5. **`04-fichiers-types.md`** : `GestionEmployes` ne nettoyait pas `employes.dat`. Ajouté `Erase(F)`.
6. **`05-gestion-erreurs-io.md`** : `EcritureSecurisee` ne nettoyait pas `sortie.txt`. Ajouté nettoyage avec `Erase`.
7. **`09-introduction-streams.md`** : `CopierFichier` renommé `CopierStream`, rendu auto-contenu avec création de fichier test et nettoyage.
8. **`09-introduction-streams.md`** : `EcrireStructures` ajouté nettoyage de `personnes.dat`.

### Ajout de `{$mode objfpc}{$H+}` (requis pour Classes, SysUtils, Result, try-except)

9. **`05-gestion-erreurs-io.md`** : `GestionModerne`, `ConfigRobuste`.
10. **`06-manipulation-repertoires.md`** : `AfficherRepertoireCourant`, `ListerFichiers`, `ChercherFichiersTexte`, `GestionnaireRepertoires`.
11. **`07-chemins-noms-fichiers.md`** : `DecomposerChemin`, `GestionnaireChemin`.
12. **`08-fichiers-ini-configuration.md`** : `GestionPreferences`, `ConfigConnexion`.
13. **`09-introduction-streams.md`** : `EcrireStream`, `LireStream`, `StreamMemoire`, `StreamChaine`, `CopierStream`, `LireFichierTexte`, `EcrireStructures`, `ConcatenerFichiers`, `LectureBufferisee`.

### Corrections de noms et symboles

14. **`05-gestion-erreurs-io.md`** : Noms de programmes avec accents (`OuvertureSécurisée`, `LectureSécurisée`, `EcritureSécurisée`) remplacés par des noms ASCII.
15. **`05-gestion-erreurs-io.md`** : Paramètre `Config` renommé `Cfg` dans `ChargerConfig` pour éviter le shadowing de la variable globale.
16. **`06-manipulation-repertoires.md`** : `ChercherFichiersTexte` cherchait `*.txt`, changé en `*.pas` pour correspondre au .pas.
17. **`07-chemins-noms-fichiers.md`** : Chemins Windows changés en chemins Linux dans `DecomposerChemin`.
18. **`07-chemins-noms-fichiers.md`** : `ConstruireChemin` renommé `ConstruireCheminInteractif` dans `GestionnaireChemin`.
19. **`07-chemins-noms-fichiers.md`** et **`08-fichiers-ini-configuration.md`** : Caractères ✓/✗ remplacés par V/X.
20. **`08-fichiers-ini-configuration.md`** : `ConfigConnexion` rendu auto-contenu (ajout `SauvegarderConfigConnexion`, nettoyage `DeleteFile`, suppression `ReadLn` final).
