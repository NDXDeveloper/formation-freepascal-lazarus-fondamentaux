🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.2 Différences avec Turbo Pascal

## Introduction

FreePascal a été conçu pour être **compatible** avec Turbo Pascal, ce qui signifie que la plupart des programmes écrits pour Turbo Pascal peuvent être compilés avec FreePascal sans modification. Cependant, plus de 30 ans séparent la dernière version de Turbo Pascal (1992) de FreePascal moderne. Pendant ce temps, l'informatique a énormément évolué, et FreePascal a dû s'adapter.

Cette section vous présente les principales différences entre ces deux environnements. Même si vous n'avez jamais utilisé Turbo Pascal, ces informations vous seront utiles car de nombreux tutoriels et livres de Pascal font encore référence à Turbo Pascal.

## Vue d'ensemble : compatibilité et évolution

### Le principe de compatibilité

**La bonne nouvelle :** FreePascal peut compiler la grande majorité du code Turbo Pascal sans modification. Les créateurs de FreePascal ont fait un énorme travail pour préserver cette compatibilité.

**La réalité :** Certaines différences existent, principalement parce que :
- Les systèmes d'exploitation modernes fonctionnent différemment
- Les ordinateurs ont évolué (64 bits, multi-cœurs, plus de mémoire...)
- De nouvelles fonctionnalités ont été ajoutées
- Certaines anciennes techniques sont devenues obsolètes

### Les modes de compilation

FreePascal offre plusieurs **modes de compilation** pour gérer la compatibilité :

```pascal
{$MODE TP}        // Mode Turbo Pascal (maximum de compatibilité)
{$MODE FPC}       // Mode FreePascal (par défaut)
{$MODE DELPHI}    // Mode compatible Delphi
{$MODE OBJFPC}    // Mode objet FreePascal
```

**Pour le débutant :** Si vous suivez un ancien tutoriel Turbo Pascal, vous pouvez ajouter `{$MODE TP}` au début de votre programme pour maximiser la compatibilité.

## 1. Différences liées au système d'exploitation

### 1.1 L'écran en mode texte

**Turbo Pascal (DOS)**
- Fonctionnait en "mode texte" : écran de 80 colonnes × 25 lignes
- Accès direct à la mémoire vidéo
- Unité `Crt` pour gérer l'écran, les couleurs, le positionnement du curseur

**FreePascal (Windows/Linux modernes)**
- Fonctionne dans une fenêtre console moderne
- Pas d'accès direct à la mémoire vidéo (interdit par les systèmes modernes)
- L'unité `Crt` existe mais avec des limitations

**Exemple de code Turbo Pascal qui pose problème :**

```pascal
uses Crt;
begin
  TextBackground(Blue);      // Fonctionne en Turbo Pascal
  TextColor(Yellow);         // Fonctionne en Turbo Pascal
  ClrScr;                    // Efface l'écran - fonctionne
  GotoXY(40, 12);           // Positionne le curseur - fonctionne partiellement
  Write('Bonjour');
end.
```

**En FreePascal :** Les couleurs de fond et certaines fonctions avancées peuvent ne pas fonctionner selon le terminal utilisé.

### 1.2 L'accès aux interruptions matérielles

**Turbo Pascal** permettait d'accéder directement au matériel via les interruptions (par exemple pour programmer la carte son ou contrôler le clavier au niveau matériel).

**FreePascal** ne permet plus cet accès direct car :
- Les systèmes modernes protègent le matériel (sécurité)
- C'est le système d'exploitation qui gère le matériel
- Il faut passer par des API système

**Pour le débutant :** Ces fonctionnalités très techniques ne vous concernent pas pour l'instant. Les programmes modernes utilisent d'autres méthodes pour gérer le son, le clavier, etc.

### 1.3 Les chemins de fichiers

**Turbo Pascal (DOS)**
```pascal
Assign(F, 'C:\DATA\FICHIER.TXT');    // Toujours le séparateur \
```

**FreePascal (multi-plateforme)**
```pascal
// Mieux : utiliser les constantes portables
Assign(F, 'data' + DirectorySeparator + 'fichier.txt');
// Sous Windows : \
// Sous Linux : /
```

**Pour le débutant :** Si vous voulez que votre programme fonctionne sur Windows et Linux, utilisez les constantes comme `DirectorySeparator` ou `PathDelim` au lieu d'écrire directement `\` ou `/`.

## 2. Différences dans les types de données

### 2.1 Taille des types entiers

**Turbo Pascal (architecture 16 bits)**
- `Integer` = 16 bits (valeurs de -32768 à 32767)
- `Word` = 16 bits non signé (0 à 65535)
- `LongInt` = 32 bits

**FreePascal (architecture moderne, souvent 64 bits)**
- `Integer` = dépend du mode de compilation (16 bits en mode TP, 32 bits dans les autres modes)
- `Word` = 16 bits
- `LongInt` = 32 bits
- `Int64` = 64 bits (nouveau type)
- `NativeInt` = dépend de la plateforme (32 ou 64 bits)

**Recommandation pour le débutant :**
Si vous voulez une taille précise, utilisez les types explicites :
```pascal
var
  petit: SmallInt;    // Toujours 16 bits signé
  moyen: LongInt;     // Toujours 32 bits signé
  grand: Int64;       // Toujours 64 bits signé
```

### 2.2 Les chaînes de caractères

C'est l'une des différences les plus importantes !

**Turbo Pascal**
```pascal
var
  s: String;    // Limité à 255 caractères maximum
```

**FreePascal**
```pascal
var
  s: String;         // Chaîne dynamique, taille illimitée (AnsiString)
  s: ShortString;    // Compatible Turbo Pascal (255 caractères max)
```

**En mode `{$MODE TP}` :** Le type `String` revient automatiquement à `ShortString` pour compatibilité.

**Pour le débutant :** Utilisez simplement `String` dans vos programmes. FreePascal gérera automatiquement la mémoire et vous n'aurez pas de limite de 255 caractères.

### 2.3 Les pointeurs et la mémoire

**Turbo Pascal**
- Modèle de mémoire segmentée (limites de 64 Ko par segment)
- Types de pointeurs spéciaux : `Near`, `Far`, `Huge`
- Mémoire limitée (640 Ko de RAM utilisable)

**FreePascal**
- Modèle de mémoire moderne linéaire
- Pas de distinction `Near`/`Far`
- Accès à toute la mémoire de l'ordinateur

**Pour le débutant :** Ces concepts de mémoire segmentée appartiennent au passé. En FreePascal, utilisez simplement les pointeurs normaux (`^`) et ne vous souciez pas de ces complications.

## 3. Différences dans les unités (bibliothèques)

### 3.1 Unités standard modifiées

Certaines unités Turbo Pascal existent encore mais avec des changements :

**Unité `Crt` (contrôle de la console)**
- Existe en FreePascal mais limitée
- Certaines fonctions de couleur ne marchent pas partout
- `Delay()` fonctionne différemment

**Unité `Dos` (fonctions système)**
- Existe mais obsolète
- Utilisez plutôt les unités modernes : `SysUtils`, `Unix` (Linux), `Windows` (Windows)

**Unité `Graph` (graphiques)**
- N'existe pas dans FreePascal standard
- Il existe des portages non officiels
- Pour les graphiques modernes, utilisez Lazarus (LCL)

### 3.2 Nouvelles unités disponibles

FreePascal apporte de nombreuses nouvelles unités modernes :

```pascal
uses
  SysUtils,      // Fonctions système modernes (dates, fichiers, chaînes...)
  Classes,       // Classes et listes (programmation objet)
  StrUtils,      // Manipulation avancée de chaînes
  DateUtils,     // Manipulation de dates et heures
  Math,          // Fonctions mathématiques étendues
  RegExpr;       // Expressions régulières
```

**Pour le débutant :** Ces nouvelles unités vous offrent des outils puissants et modernes. Nous les découvrirons progressivement dans la formation.

## 4. Différences dans la syntaxe et les fonctionnalités

### 4.1 Déclarations de variables plus flexibles

**Turbo Pascal** : Toutes les variables doivent être déclarées au début du programme/procédure.

```pascal
procedure Test;
var
  i: Integer;
  s: String;
begin
  i := 5;
  // Impossible de déclarer une nouvelle variable ici
end;
```

**FreePascal (mode moderne)** : Avec certaines configurations, vous pouvez déclarer des variables presque partout.

```pascal
{$mode delphi}  // Nécessaire pour les déclarations inline
procedure Test;
var
  i: Integer;
begin
  i := 5;
  var s: String;   // Déclaration inline (mode Delphi, FPC récent)
  s := 'Bonjour';
end;
```

**Pour le débutant :** Les déclarations inline nécessitent `{$mode delphi}` et une version récente de FreePascal. En mode standard (`{$MODE OBJFPC}`), déclarez toutes vos variables dans la section `var` avant `begin`, comme en Turbo Pascal.

### 4.2 Opérateurs supplémentaires

FreePascal ajoute de nouveaux opérateurs :

```pascal
// Opérateurs d'affectation composés (comme en C)
i += 5;      // Équivalent à : i := i + 5;
i -= 3;      // Équivalent à : i := i - 3;
i *= 2;      // Équivalent à : i := i * 2;

// Opérateurs de puissance
x := 2 ** 3;  // x = 8 (2 puissance 3)
```

**Pour le débutant :** Ces nouveaux opérateurs sont pratiques mais optionnels. Les anciennes méthodes fonctionnent toujours.

### 4.3 Support complet de la POO (Programmation Orientée Objet)

**Turbo Pascal 7.0** : POO basique avec le mot-clé `Object`

```pascal
type
  TPoint = object
    X, Y: Integer;
  end;
```

**FreePascal** : POO complète et moderne avec `Class`

```pascal
type
  TPoint = class
    private
      FX, FY: Integer;
    public
      constructor Create(AX, AY: Integer);
      property X: Integer read FX write FX;
      property Y: Integer read FY write FY;
  end;
```

**Pour le débutant :** Nous verrons la programmation orientée objet en détail dans les chapitres 10 à 12. FreePascal offre bien plus de possibilités que Turbo Pascal dans ce domaine.

### 4.4 Gestion des exceptions

**Turbo Pascal** : Pas de gestion d'exceptions intégrée

**FreePascal** : Gestion complète des exceptions

```pascal
try
  // Code qui peut causer une erreur
  Resultat := Diviser(10, 0);
except
  on E: Exception do
    WriteLn('Erreur : ', E.Message);
end;
```

**Pour le débutant :** Les exceptions sont un mécanisme moderne pour gérer les erreurs proprement. Nous les étudierons au chapitre 13.

## 5. Différences dans l'environnement de développement

### 5.1 Interface graphique

**Turbo Pascal**
- IDE (environnement de développement) en mode texte bleu
- Interface DOS avec menus déroulants
- Éditeur simple mais efficace pour l'époque

**FreePascal + Lazarus**
- IDE graphique moderne et coloré
- Multiples fenêtres, onglets
- Éditeur avec coloration syntaxique avancée
- Outils visuels pour créer des interfaces graphiques

### 5.2 Compilation et vitesse

**Turbo Pascal**
- Compilation très rapide (pour l'époque)
- Programmes 16 bits pour DOS
- Fichiers exécutables petits

**FreePascal**
- Compilation encore plus rapide (ordinateurs modernes)
- Programmes 32 ou 64 bits pour systèmes modernes
- Support de multiples cibles (Windows, Linux, macOS...)
- Optimisations avancées disponibles

### 5.3 Débogueur

**Turbo Pascal**
- Débogueur intégré basique
- Affichage des variables limité

**FreePascal + Lazarus**
- Débogueur graphique puissant (basé sur GDB)
- Inspection complète des variables
- Points d'arrêt conditionnels
- Pile d'appels, threads...

## 6. Limites disparues

Plusieurs limitations de Turbo Pascal n'existent plus :

| Limitation Turbo Pascal | FreePascal |
|------------------------|------------|
| Programmes limités à 640 Ko de code | Aucune limite pratique |
| Segments de 64 Ko maximum | Aucune limite de segment |
| Chaînes limitées à 255 caractères | Chaînes de taille illimitée |
| Tableaux limités en taille | Tableaux de taille limitée seulement par la RAM |
| Pas de multi-threading | Support complet du multi-threading |
| DOS uniquement | Multi-plateforme (Windows, Linux, macOS...) |

## 7. Code de compatibilité : conseils pratiques

### Si vous portez un programme Turbo Pascal vers FreePascal

**Étape 1 :** Ajoutez le mode Turbo Pascal au début du fichier
```pascal
{$MODE TP}
program MonAncienProgramme;
```

**Étape 2 :** Remplacez les unités obsolètes si nécessaire
- `Dos` → `SysUtils` pour les fonctions modernes
- `Graph` → Considérez de refaire l'interface avec Lazarus
- `Overlay` → N'existe plus (inutile sur systèmes modernes)

**Étape 3 :** Vérifiez les accès matériels
- Si le programme utilise des interruptions, il faudra le réécrire
- Les accès directs à la mémoire vidéo ne fonctionneront pas

**Étape 4 :** Testez et adaptez
- Compilez et corrigez les erreurs éventuelles
- Testez le comportement du programme

### Si vous écrivez un nouveau programme

**Recommandation :** N'utilisez PAS le mode Turbo Pascal !

Utilisez plutôt le mode moderne :
```pascal
{$MODE OBJFPC}{$H+}
program MonNouveauProgramme;

uses
  SysUtils, Classes;

// Votre code moderne ici
```

**Pourquoi ?**
- Vous profitez de toutes les fonctionnalités modernes
- Vous apprenez les bonnes pratiques actuelles
- Votre code sera plus facile à maintenir
- Vous serez compatible avec Lazarus

## 8. Tableau récapitulatif des principales différences

| Aspect | Turbo Pascal | FreePascal |
|--------|-------------|------------|
| **Année de sortie** | 1983-1992 | 1993-aujourd'hui |
| **Licence** | Propriétaire (payant) | GPL (gratuit) |
| **Système d'exploitation** | DOS uniquement | Multi-plateforme |
| **Architecture** | 16 bits | 32 et 64 bits |
| **Mémoire disponible** | 640 Ko max | Toute la RAM |
| **Type String** | 255 caractères max | Illimité |
| **POO** | Basique (Object) | Complète (Class) |
| **Exceptions** | Non | Oui |
| **Multi-threading** | Non | Oui |
| **Unicode** | Non | Oui |
| **Unités modernes** | Limitées | Très nombreuses |
| **IDE** | Mode texte | Graphique (Lazarus) |

## Conclusion

FreePascal est un **sur-ensemble** de Turbo Pascal : il contient tout ce que Turbo Pascal offrait, plus énormément de fonctionnalités supplémentaires adaptées aux ordinateurs et systèmes modernes.

**Pour vous, débutant :**
- Si vous suivez un ancien tutoriel Turbo Pascal, utilisez `{$MODE TP}` pour compatibilité
- Si vous apprenez de zéro, utilisez le mode moderne (`{$MODE OBJFPC}{$H+}`)
- Ne vous inquiétez pas trop des différences : FreePascal gère la compatibilité intelligemment
- Concentrez-vous sur l'apprentissage des concepts, pas sur les détails historiques

Les différences sont là principalement pour des raisons d'évolution technique et de modernisation. FreePascal a su préserver l'esprit et la philosophie du Pascal tout en s'adaptant au monde moderne.

---

**À retenir :**
- FreePascal est largement compatible avec Turbo Pascal
- Les différences viennent de l'évolution des systèmes et des technologies
- Les modes de compilation (`{$MODE}`) permettent de gérer la compatibilité
- FreePascal offre bien plus de possibilités que Turbo Pascal
- Pour apprendre aujourd'hui, utilisez les fonctionnalités modernes de FreePascal
- La vaste majorité des tutoriels Turbo Pascal restent valides et utilisables

⏭️ [L'écosystème Lazarus](/09-introduction-freepascal-lazarus/03-ecosysteme-lazarus.md)
