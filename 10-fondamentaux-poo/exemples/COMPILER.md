# Compilation des exemples - Chapitre 10 : Fondamentaux de la POO

## Compilateur

```bash
fpc fichier.pas
```

Version testée :
- Free Pascal Compiler 3.2.2+dfsg-32 (Linux x86-64)

## Nettoyage après compilation

```bash
rm -f *.o *.ppu
# Supprimer les exécutables (même nom que les .pas sans extension)
```

## Liste des exemples (14 fichiers + 1 unité)

### Section 10.1 : Concepts : Classes et Objets

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `01-exemple-classe.pas` | Classe TCompteur avec attribut et méthodes | Non |

### Section 10.2 : Encapsulation et visibilité

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `02-exemple-encapsulation.pas` | Classe TCompteBancaire illustrant l'encapsulation | Non |

### Section 10.3 : Déclaration de classes

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `03-gestion-personnes.pas` | Programme avec classe TPersonne (getters/setters) | Non |
| `03-exemple-rectangle.pas` | Classe TRectangle avec bonnes pratiques | Non |
| `UPersonne.pas` | Unité avec une classe TPersonne | - |

### Section 10.4 : Attributs et méthodes

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `04-exemple-temperature.pas` | Classe TTemperature avec conversions C/F/K | Non |

### Section 10.5 : Constructeurs (Create)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `05-exemple-constructeur.pas` | Classe TArticle avec surcharge de constructeurs | Non |

### Section 10.6 : Destructeurs (Destroy, Free)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `06-exemple-destructeur.pas` | Classe TFichierLog avec destructeur (crée application.log) | Non |

### Section 10.7 : Self et référence à l'objet courant

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `07-exemple-self.pas` | Classe TPoint avec chaînage de méthodes via Self | Non |

### Section 10.8 : Visibilité : private, protected, public, published

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `08-systeme-notes.pas` | Système de notes avec héritage TPersonne/TEtudiant | Non |

### Section 10.9 : Propriétés (properties) simples

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `09-exemple-properties.pas` | Classe TCompteBancaire avec propriétés et historique | Non |

### Section 10.10 : Comparaison procédural vs objet

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `10-rectangle-procedural.pas` | Rectangle en approche procédurale (record) | Non |
| `10-rectangle-objet.pas` | Rectangle en approche orientée objet (classe) | Non |
| `10-compte-procedural.pas` | Comptes bancaires en approche procédurale | Non |
| `10-compte-objet.pas` | Comptes bancaires en approche orientée objet | Non |

### Section 10.11 : UML et diagrammes de classes basics

Aucun exemple compilable (section théorique/descriptive).

## Notes

- Tous les programmes utilisent `{$mode objfpc}{$H+}` car les classes et `Result` nécessitent ce mode.
- `UPersonne.pas` est une unité qui se compile seule (`fpc UPersonne.pas` crée un `.ppu`).
- Le programme `06-exemple-destructeur.pas` crée un fichier `application.log` lors de l'exécution.
- Aucun programme n'est interactif (pas de ReadLn), tous peuvent être testés automatiquement.

## Corrections apportées aux fichiers .md

1. **`01-concepts-classes-objets.md`** : Ajout de `{$mode objfpc}{$H+}` au programme `ExempleClasse`.
2. **`02-encapsulation-visibilite.md`** : Ajout de `{$mode objfpc}{$H+}` au programme `ExempleEncapsulation` (nécessaire pour `Result`).
## Corrections apportées aux fichiers .pas

Harmonisation des commentaires : remplacement des commentaires ASCII (sans accents) par des commentaires en français correct (avec accents) dans les fichiers des sections 10.2 à 10.10, pour correspondre exactement aux blocs de code des fichiers .md.
