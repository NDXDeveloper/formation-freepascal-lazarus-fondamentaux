# Chapitre 19 — Développement Multi-plateforme en Pratique

## Fichiers exemples : 42 fichiers (29 programs + 2 units + 2 .inc + 1 unit associée + 2 projets Lazarus (8 fichiers))

### Section 19.1 : Différences fondamentales Windows/Linux (3 programs)
| Fichier | Description | Compilation |
|---|---|---|
| `01-constantes-portables.pas` | Affiche PathDelim, DirectorySeparator, PathSeparator, LineEnding | `fpc 01-constantes-portables.pas` |
| `01-repertoires-systeme.pas` | Répertoires home, temp, config, courant de manière portable | `fpc 01-repertoires-systeme.pas` |
| `01-execution-commande.pas` | Exécution portable de commande via TProcess (ls/dir) | `fpc 01-execution-commande.pas` |

### Section 19.2 : Gestion portable des chemins (5 programs)
| Fichier | Description | Compilation |
|---|---|---|
| `02-config-app.pas` | Configuration d'application avec PathDelim | `fpc 02-config-app.pas` |
| `02-analyse-chemin.pas` | Analyse d'un chemin avec ExtractFilePath, ExtractFileName, etc. | `fpc 02-analyse-chemin.pas` |
| `02-chargement-config.pas` | Chargement d'un fichier de configuration | `fpc 02-chargement-config.pas` |
| `02-sauvegarde-utilisateur.pas` | Sauvegarde dans un répertoire utilisateur portable | `fpc 02-sauvegarde-utilisateur.pas` |
| `02-parcours-repertoire.pas` | Parcours récursif d'un arbre de répertoires | `fpc 02-parcours-repertoire.pas` |

### Section 19.3 : Directives de compilation conditionnelle (6 programs)
| Fichier | Description | Compilation |
|---|---|---|
| `03-detection-os.pas` | Détection du système d'exploitation avec IFDEF | `fpc 03-detection-os.pas` |
| `03-detection-architecture.pas` | Détection de l'architecture CPU | `fpc 03-detection-architecture.pas` |
| `03-conditions-avancees.pas` | Conditions multiples avec IF DEFINED et ELSEIF | `fpc 03-conditions-avancees.pas` |
| `03-symboles-personnalises.pas` | Définition et utilisation de symboles personnalisés | `fpc 03-symboles-personnalises.pas` |
| `03-undef-example.pas` | Annulation d'un symbole avec UNDEF | `fpc 03-undef-example.pas` |
| `03-appli-multi-plateforme.pas` | Application complète multi-plateforme | `fpc 03-appli-multi-plateforme.pas` |

### Section 19.4 : Unités spécifiques à chaque plateforme (15 programs + 2 units + 2 .inc)
| Fichier | Description | Compilation |
|---|---|---|
| `04-utilisation-conditionnelle.pas` | Clause uses conditionnelle | `fpc 04-utilisation-conditionnelle.pas` |
| `04-nom-ordinateur.pas` | Obtenir le nom de l'ordinateur | `fpc 04-nom-ordinateur.pas` |
| `04-info-systeme.pas` | Informations sur le système d'exploitation | `fpc 04-info-systeme.pas` |
| `04-variables-env.pas` | Accès au répertoire temporaire | `fpc 04-variables-env.pas` |
| `04-lecture-registre.pas` | Lecture du registre Windows (alternative Linux) | `fpc 04-lecture-registre.pas` |
| `04-ecriture-registre.pas` | Écriture dans le registre Windows (alternative Linux) | `fpc 04-ecriture-registre.pas` |
| `04-config-portable.pas` | Alternative portable avec fichiers INI | `fpc 04-config-portable.pas` |
| `04-exemple-base-unix.pas` | Appels système Unix (PID, UID, GID) | `fpc 04-exemple-base-unix.pas` |
| `04-exemple-unix.pas` | Lister les variables d'environnement (Unix) | `fpc 04-exemple-unix.pas` |
| `04-permissions-fichier.pas` | Vérification des permissions de fichiers Unix | `fpc 04-permissions-fichier.pas` |
| `04-appli-avec-threads.pas` | Utilisation de CThreads sous Unix | `fpc 04-appli-avec-threads.pas` |
| `PlatformUtils.pas` | Unité wrapper portable (nom utilisateur, PID) | compilée via `04-utilisation-wrapper.pas` |
| `04-utilisation-wrapper.pas` | Programme utilisant PlatformUtils | `fpc 04-utilisation-wrapper.pas` |
| `SystemInfo.pas` | Unité avec implémentation séparée par plateforme | `fpc SystemInfo.pas` |
| `SystemInfo.Unix.inc` | Implémentation Unix de SystemInfo | inclus par SystemInfo.pas |
| `SystemInfo.Windows.inc` | Implémentation Windows de SystemInfo | inclus par SystemInfo.pas |
| `04-liste-disques.pas` | Liste des disques/points de montage | `fpc 04-liste-disques.pas` |

### Section 19.5 : Configuration de projets multi-cibles (1 projet Lazarus)

Sous-dossier : `05-build-modes/`

| Fichier | Description | Compilation |
|---|---|---|
| `05-build-modes/BuildModes.lpr` | Programme principal Lazarus | `lazbuild 05-build-modes/BuildModes.lpi` |
| `05-build-modes/BuildModes.lpi` | Fichier projet avec 2 build modes (Default + Release) | — |
| `05-build-modes/unit1.pas` | Formulaire affichant OS, architecture, mode, version FPC | — |
| `05-build-modes/unit1.lfm` | Design du formulaire avec labels informatifs | — |

Ce projet illustre les build modes, macros `{$I %FPCTARGETOS%}` et defines `{$IFDEF DEBUG}`.

### Section 19.6 : Cross-compilation : théorie et pratique (2 programs + 1 projet Lazarus)
| Fichier | Description | Compilation |
|---|---|---|
| `06-hello-cross.pas` | Application console avec détection plateforme/CPU | `fpc 06-hello-cross.pas` |
| `06-file-manager.pas` | Gestionnaire de fichiers multi-plateforme | `fpc 06-file-manager.pas` |

Sous-dossier : `06-cross-gui/`

| Fichier | Description | Compilation |
|---|---|---|
| `06-cross-gui/CrossGUI.lpr` | Programme principal Lazarus (code du .md) | `lazbuild 06-cross-gui/CrossGUI.lpi` |
| `06-cross-gui/CrossGUI.lpi` | Fichier projet Lazarus | — |
| `06-cross-gui/mainform.pas` | Formulaire affichant la plateforme, bouton ShowMessage | — |
| `06-cross-gui/mainform.lfm` | Design du formulaire avec Label1 + Button1 | — |

### Section 19.7 : Gestion des dépendances externes (1 program)
| Fichier | Description | Compilation |
|---|---|---|
| `07-verify-deps.pas` | Script de vérification des dépendances système | `fpc 07-verify-deps.pas` |

### Section 19.8 : Tests sur différentes plateformes (2 programs)
| Fichier | Description | Compilation |
|---|---|---|
| `08-test-fonctionnel.pas` | Tests fonctionnels multi-plateformes | `fpc 08-test-fonctionnel.pas` |
| `08-benchmark-multi-plateforme.pas` | Benchmark de performance multi-plateforme | `fpc 08-benchmark-multi-plateforme.pas` |

### Section 19.9 : Empaquetage et distribution
Aucun exemple compilable (théorie sur l'empaquetage et la distribution).

## Compilation en lot

```bash
cd exemples/

# Section 19.1
fpc 01-constantes-portables.pas  
fpc 01-repertoires-systeme.pas  
fpc 01-execution-commande.pas  

# Section 19.2
fpc 02-config-app.pas  
fpc 02-analyse-chemin.pas  
fpc 02-chargement-config.pas  
fpc 02-sauvegarde-utilisateur.pas  
fpc 02-parcours-repertoire.pas  

# Section 19.3
fpc 03-detection-os.pas  
fpc 03-detection-architecture.pas  
fpc 03-conditions-avancees.pas  
fpc 03-symboles-personnalises.pas  
fpc 03-undef-example.pas  
fpc 03-appli-multi-plateforme.pas  

# Section 19.4
fpc 04-utilisation-conditionnelle.pas  
fpc 04-nom-ordinateur.pas  
fpc 04-info-systeme.pas  
fpc 04-variables-env.pas  
fpc 04-lecture-registre.pas  
fpc 04-ecriture-registre.pas  
fpc 04-config-portable.pas  
fpc 04-exemple-base-unix.pas  
fpc 04-exemple-unix.pas  
fpc 04-permissions-fichier.pas  
fpc 04-appli-avec-threads.pas  
fpc 04-utilisation-wrapper.pas  
fpc SystemInfo.pas  
fpc 04-liste-disques.pas  

# Section 19.5 (projet Lazarus)
lazbuild 05-build-modes/BuildModes.lpi

# Section 19.6
fpc 06-hello-cross.pas  
fpc 06-file-manager.pas  
lazbuild 06-cross-gui/CrossGUI.lpi  

# Section 19.7
fpc 07-verify-deps.pas

# Section 19.8
fpc 08-test-fonctionnel.pas  
fpc 08-benchmark-multi-plateforme.pas  
```

## Nettoyage

```bash
rm -f *.o *.ppu  
rm -f 01-constantes-portables 01-repertoires-systeme 01-execution-commande  
rm -f 02-config-app 02-analyse-chemin 02-chargement-config 02-sauvegarde-utilisateur 02-parcours-repertoire  
rm -f 03-detection-os 03-detection-architecture 03-conditions-avancees 03-symboles-personnalises 03-undef-example 03-appli-multi-plateforme  
rm -f 04-utilisation-conditionnelle 04-nom-ordinateur 04-info-systeme 04-variables-env 04-lecture-registre 04-ecriture-registre 04-config-portable  
rm -f 04-exemple-base-unix 04-exemple-unix 04-permissions-fichier 04-appli-avec-threads 04-utilisation-wrapper 04-liste-disques  
rm -f 06-hello-cross 06-file-manager  
rm -f 07-verify-deps  
rm -f 08-test-fonctionnel 08-benchmark-multi-plateforme  
rm -rf 05-build-modes/lib 05-build-modes/BuildModes  
rm -rf 06-cross-gui/lib 06-cross-gui/CrossGUI  
```

## Corrections apportées au .md

1. **04-unites-specifiques-plateforme.md** : Ajout de `SysUtils` dans la clause `uses` des fichiers `SystemInfo.Windows.inc` et `SystemInfo.Unix.inc` (nécessaire pour la fonction `Format`).
2. **07-gestion-dependances-externes.md** : Correction du bug de double appel à `TestDependency` dans la boucle de vérification.

## Notes

- Tous les programmes utilisent `{$IFDEF}` pour le code spécifique à chaque plateforme. Sous Linux, les branches Windows ne sont pas compilées et inversement.
- Les programmes qui créent des fichiers/répertoires (02-config-app, 02-sauvegarde-utilisateur, 03-appli-multi-plateforme, 04-config-portable) nettoient pas après eux ; pensez à supprimer les répertoires créés.
- `07-verify-deps.pas` retourne un code de sortie 1 si des dépendances requises manquent (ex: OpenSSL 1.1 sur les systèmes avec OpenSSL 3.x).
- Les unités FileOpener et SystemNotifications du .md n'ont pas été extraites car ce sont des unités sans programme principal associé dans le .md.
- Le projet `05-build-modes` a 2 build modes : Default (Debug avec heaptrc) et Release (optimisé, strippé). Il illustre les macros `{$I %FPCTARGETOS%}`, `{$I %FPCTARGETCPU%}`, `{$I %FPCVERSION%}` et les defines `{$IFDEF DEBUG}`.
- Le projet `06-cross-gui` est extrait de la section 19.6 (exemple 2 du .md). Il illustre une application graphique cross-compilable.
