# Compilation des exemples - Chapitre 07

## Prérequis

- Free Pascal Compiler (fpc) version 3.2.2 ou supérieure

## Compilation

Pour compiler un programme, utilisez :

```bash
fpc nom-du-programme.pas
```

Les unités associées sont compilées automatiquement par fpc grâce à la clause `uses`.

## Nettoyage

```bash
rm -f *.o *.ppu  
rm -f 01-utilisation-unites 02-test-cercles 03-test-dependances 03-test-ordre-uses  
rm -f 05-client-banque 05-test-compteur 06-test-compteur-init 06-test-ordre-init  
rm -f 07-classes-stringlist 07-math-fonctions 07-strutils-chaines 07-sysutils-chaines  
rm -f 07-sysutils-exceptions 07-variants 08-test-validation  
```

## Liste des fichiers (27 fichiers)

### Section 7.1 - Concept d'unité en Pascal (1 fichier)

| Fichier | Type | Description | Interactif |
|---------|------|-------------|------------|
| `01-utilisation-unites.pas` | programme | Utilisation des unités SysUtils et Math | Non |

### Section 7.2 - Structure d'une unité (2 fichiers)

| Fichier | Type | Description | Interactif |
|---------|------|-------------|------------|
| `UniteCercles.pas` | unité | Unité de calculs de cercles (périmètre, surface) | - |
| `02-test-cercles.pas` | programme | Test de UniteCercles | Oui (ReadLn) |

### Section 7.3 - Clauses uses et dépendances (6 fichiers)

| Fichier | Type | Description | Interactif |
|---------|------|-------------|------------|
| `UniteA.pas` | unité | Unité avec procedure Afficher ("Version A") | - |
| `UniteB.pas` | unité | Unité avec procedure Afficher ("Version B") | - |
| `03-test-ordre-uses.pas` | programme | Démonstration de l'ordre des uses | Non |
| `UniteMaths.pas` | unité | Unité avec fonction Carre | - |
| `UniteCalculs.pas` | unité | Unité dépendant de UniteMaths | - |
| `03-test-dependances.pas` | programme | Démonstration de la chaîne de dépendances | Non |

### Section 7.4 - Ordre de compilation (0 fichier)

Section théorique, pas d'exemples compilables distincts.

### Section 7.5 - Variables et procédures publiques/privées (4 fichiers)

| Fichier | Type | Description | Interactif |
|---------|------|-------------|------------|
| `UniteCompteur.pas` | unité | Variable privée avec accesseurs publics | - |
| `05-test-compteur.pas` | programme | Test du compteur encapsulé | Non |
| `CompteEnBanque.pas` | unité | Encapsulation d'un compte en banque | - |
| `05-client-banque.pas` | programme | Test du compte en banque | Non |

### Section 7.6 - Sections initialization et finalization (6 fichiers)

| Fichier | Type | Description | Interactif |
|---------|------|-------------|------------|
| `UniteCompteurInit.pas` | unité | Compteur avec initialization/finalization | - |
| `06-test-compteur-init.pas` | programme | Démonstration init/final du compteur | Non |
| `UniteInitA.pas` | unité | Unité A avec init/final (chaîne de dépendances) | - |
| `UniteInitB.pas` | unité | Unité B avec init/final (dépend de A) | - |
| `UniteInitC.pas` | unité | Unité C avec init/final (dépend de B) | - |
| `06-test-ordre-init.pas` | programme | Démonstration de l'ordre init/final | Non |

### Section 7.7 - Unités standard du RTL (6 fichiers)

| Fichier | Type | Description | Interactif |
|---------|------|-------------|------------|
| `07-sysutils-chaines.pas` | programme | Manipulation de chaînes avec SysUtils | Non |
| `07-sysutils-exceptions.pas` | programme | Gestion des exceptions avec SysUtils | Non |
| `07-classes-stringlist.pas` | programme | Utilisation de TStringList | Non |
| `07-math-fonctions.pas` | programme | Fonctions mathématiques (System + Math) | Non |
| `07-strutils-chaines.pas` | programme | Manipulation avancée de chaînes (StrUtils) | Non |
| `07-variants.pas` | programme | Utilisation du type Variant | Non |

### Section 7.8 - Création de bibliothèques réutilisables (2 fichiers)

| Fichier | Type | Description | Interactif |
|---------|------|-------------|------------|
| `UniteValidation.pas` | unité | Bibliothèque de validation de données | - |
| `08-test-validation.pas` | programme | Test de la bibliothèque de validation | Non |

### Section 7.9 - Documentation des unités (0 fichier)

Section théorique sur les bonnes pratiques de documentation.

## Notes importantes

### Conflits de nommage entre sections
- **Section 06** : Les unités ont été renommées pour éviter les conflits :
  - `UniteCompteur` → `UniteCompteurInit` (conflit avec section 05)
  - `UniteA/B` → `UniteInitA/B/C` (conflit avec section 03)

### Directive `{$mode objfpc}{$H+}`
Les fichiers utilisant `Result` dans les fonctions ou les chaînes longues (`String` = `AnsiString`) nécessitent cette directive. Concernés : `UniteCercles.pas`, `UniteMaths.pas`, `UniteCalculs.pas`, `UniteCompteur.pas`, `CompteEnBanque.pas`, `UniteCompteurInit.pas`, `UniteValidation.pas`, `07-sysutils-chaines.pas`, `07-sysutils-exceptions.pas`, `07-classes-stringlist.pas`, `07-strutils-chaines.pas`, `07-variants.pas`, `08-test-validation.pas`.

### Finalization et Flush(Output)
Les sections `finalization` des unités nécessitent un `Flush(Output)` après `WriteLn` pour que la sortie apparaisse, car le tampon stdout peut être fermé avant l'exécution des sections finalization.

### Corrections apportées au .md
- `01-concept-unite-pascal.md` : `WriteLn(resultat)` corrigé en `WriteLn(resultat:0:0)` car `WriteLn` sur un `Real` affiche en notation scientifique par défaut.

### Programmes interactifs (ReadLn)
- `02-test-cercles.pas` : Demande un rayon au clavier.
