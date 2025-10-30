🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.1 Types de fichiers (texte, binaire, typé)

## Introduction

En programmation, un **fichier** est un espace de stockage sur le disque dur qui permet de conserver des données de manière permanente, même après la fermeture du programme. Pascal propose trois types de fichiers différents, chacun adapté à des besoins spécifiques.

Imaginez que vous voulez sauvegarder des informations :
- Une lettre ou un document → **fichier texte**
- Une photo ou un son → **fichier binaire**
- Une liste de contacts avec nom, téléphone, email → **fichier typé**

## Les trois types de fichiers en Pascal

Pascal distingue trois catégories de fichiers, chacune avec ses caractéristiques propres :

### 1. Les fichiers texte (Text Files)

**Définition :** Un fichier texte contient des caractères lisibles par l'humain, organisés en lignes. C'est le type de fichier le plus simple et le plus courant.

**Déclaration :**
```pascal
var
  MonFichier: TextFile;  // ou simplement Text
```

**Caractéristiques :**
- Contenu lisible avec un éditeur de texte (Notepad, gedit, etc.)
- Organisé en lignes séparées par des retours à la ligne
- Parfait pour stocker du texte brut, des logs, des fichiers de configuration
- Lecture/écriture séquentielle ligne par ligne

**Exemples d'utilisation :**
- Fichiers de configuration (.ini, .conf)
- Fichiers de logs
- Fichiers CSV (valeurs séparées par des virgules)
- Documents texte simples

**Analogie :** C'est comme un cahier où vous écrivez phrase après phrase, ligne après ligne.

---

### 2. Les fichiers binaires non typés (Untyped Files)

**Définition :** Un fichier binaire contient des données brutes sous forme d'octets, sans structure particulière imposée par Pascal.

**Déclaration :**
```pascal
var
  MonFichier: File;  // Fichier binaire non typé
```

**Caractéristiques :**
- Contenu illisible avec un éditeur de texte classique
- Manipulation au niveau des octets
- Accès direct et rapide aux données
- Flexible mais nécessite une gestion manuelle de la structure
- Idéal pour copier des fichiers, traiter de gros volumes de données

**Exemples d'utilisation :**
- Images (JPEG, PNG, BMP)
- Fichiers audio ou vidéo
- Fichiers exécutables
- Copie bit à bit de fichiers
- Fichiers de sauvegarde bruts

**Analogie :** C'est comme une bande magnétique où les données sont enregistrées sous forme de signaux bruts, sans organisation visible.

---

### 3. Les fichiers typés (Typed Files)

**Définition :** Un fichier typé est un fichier binaire structuré qui contient des données d'un type précis (Integer, Real, Record, etc.). Chaque élément du fichier a la même taille et la même structure.

**Déclaration :**
```pascal
type
  TPersonne = record
    Nom: string[50];
    Age: Integer;
    Salaire: Real;
  end;

var
  FichierEntiers: File of Integer;
  FichierPersonnes: File of TPersonne;
```

**Caractéristiques :**
- Structure homogène : tous les éléments sont du même type
- Accès direct possible : on peut lire le 10ème élément sans lire les 9 premiers
- Très efficace pour les bases de données simples
- Stockage compact et rapide

**Exemples d'utilisation :**
- Base de données simple (liste de contacts, inventaire)
- Fichiers de configuration binaires
- Sauvegarde de structures complexes
- Tableaux persistants sur disque

**Analogie :** C'est comme un classeur où chaque fiche a exactement le même format (même nombre de cases, même type d'information).

---

## Tableau comparatif

| Critère | Fichier Texte | Fichier Binaire Non Typé | Fichier Typé |
|---------|---------------|--------------------------|--------------|
| **Déclaration** | `TextFile` ou `Text` | `File` | `File of TypeDonnee` |
| **Lisibilité** | Lisible directement | Illisible | Illisible |
| **Structure** | Lignes de texte | Octets bruts | Éléments d'un type précis |
| **Accès** | Séquentiel (ligne par ligne) | Séquentiel par blocs | Séquentiel ou direct |
| **Taille** | Variable (dépend du texte) | Variable | Fixe par élément |
| **Performance** | Moyenne | Très rapide | Rapide |
| **Facilité d'usage** | Très facile | Complexe | Moyenne |

---

## Comment choisir le bon type de fichier ?

**Utilisez un fichier texte si :**
- Vous voulez pouvoir lire/éditer le fichier manuellement
- Vous stockez des données textuelles simples
- La compatibilité et la portabilité sont importantes
- La taille du fichier n'est pas critique

**Utilisez un fichier binaire non typé si :**
- Vous devez manipuler des fichiers existants (copie, traitement)
- Vous travaillez avec de très gros volumes de données
- Vous avez besoin de performances maximales
- La structure des données est variable ou complexe

**Utilisez un fichier typé si :**
- Vous créez une petite base de données
- Vous stockez des enregistrements structurés
- Vous avez besoin d'un accès direct aux éléments
- Tous vos éléments ont la même structure

---

## Résumé

Pascal offre trois types de fichiers pour répondre à différents besoins :

1. **Fichiers texte** (`TextFile`) : simples, lisibles, pour du texte
2. **Fichiers binaires** (`File`) : rapides, flexibles, pour des données brutes
3. **Fichiers typés** (`File of Type`) : structurés, efficaces, pour des données homogènes

Le choix du type de fichier dépend de la nature de vos données et de l'usage que vous en ferez. Dans les sections suivantes, nous verrons en détail comment manipuler chacun de ces types de fichiers.

---

> **Note importante :** Quelle que soit le type de fichier utilisé, n'oubliez jamais de fermer vos fichiers après utilisation avec la procédure `Close()` pour éviter la perte de données et les problèmes d'accès.

⏭️ [Fichiers texte : ouverture, lecture, écriture](/08-gestion-fichiers-io/02-fichiers-texte-ouverture-lecture-ecriture.md)
