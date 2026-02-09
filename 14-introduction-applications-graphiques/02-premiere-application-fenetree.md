🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.2 Première application fenêtrée

## Introduction

Maintenant que vous comprenez les concepts de la programmation événementielle, il est temps de créer votre **première application graphique** avec Lazarus !

Dans cette section, nous allons :
- Créer un nouveau projet d'application GUI
- Comprendre ce que Lazarus génère automatiquement
- Découvrir la structure des fichiers
- Compiler et exécuter votre première fenêtre

---

## Créer un nouveau projet GUI

### Étape 1 : Lancer Lazarus

Ouvrez Lazarus IDE. Vous arrivez sur l'écran principal avec plusieurs fenêtres :
- **L'éditeur de code** (grande fenêtre centrale)
- **L'inspecteur d'objets** (généralement à gauche)
- **La palette de composants** (en haut, avec des onglets)
- **Le concepteur de formulaires** (la fenêtre vide avec des points)

### Étape 2 : Créer un nouveau projet

**Menu : Projet → Nouveau projet...**

Une boîte de dialogue s'ouvre avec plusieurs types de projets. Sélectionnez :

```
┌─────────────────────────────────────┐
│   Créer un nouveau projet           │
│                                     │
│  [🖥️] Application                   │  ← Sélectionnez celui-ci
│  [ ] Programme console              │
│  [ ] Bibliothèque                   │
│  [ ] Application CGI                │
│  ...                                │
│                                     │
│        [Créer]  [Annuler]           │
└─────────────────────────────────────┘
```

Cliquez sur **"Application"** puis sur **"Créer"**.

### Étape 3 : Enregistrer votre projet

Lazarus vous demande immédiatement où enregistrer votre projet.

**Conseil pratique :** Créez un dossier dédié pour chaque projet !

**Exemple d'organisation :**
```
MesProjetsLazarus/
└── MonPremierGUI/
    ├── project1.lpr      (fichier principal du projet)
    ├── unit1.pas         (unité du formulaire)
    ├── unit1.lfm         (définition du formulaire)
    └── ...
```

**Noms suggérés :**
- **Nom du projet :** `MonPremierGUI.lpr`
- **Nom de l'unité :** `unit1.pas` (nom par défaut, vous pouvez le changer)

---

## Comprendre ce que Lazarus a créé

### Vue d'ensemble

Lazarus a généré automatiquement :
1. Un **fichier projet** (.lpr)
2. Une **unité de formulaire** (.pas)
3. Un **fichier de formulaire** (.lfm)
4. Un **formulaire vide** affiché à l'écran

### Le formulaire vide

Vous voyez maintenant une fenêtre grise vide avec des points (grille d'alignement) :

```
┌─────────────────────────────────┐
│ Form1                     _ □ ✕ │
├─────────────────────────────────┤
│                                 │
│    . . . . . . . . . . . . .    │
│    . . . . . . . . . . . . .    │
│    . . . . . . . . . . . . .    │
│    . . . . . . . . . . . . .    │
│    . . . . . . . . . . . . .    │
│                                 │
└─────────────────────────────────┘
```

C'est votre **premier formulaire** (fenêtre). C'est ici que vous allez placer vos composants (boutons, zones de texte, etc.).

---

## Structure des fichiers générés

### 1. Le fichier projet (.lpr)

**Nom :** `project1.lpr` (ou `MonPremierGUI.lpr`)

C'est le **point d'entrée** de votre application. Lazarus l'a généré automatiquement :

```pascal
program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // Cette unité initialise la LCL
  Forms, Unit1
  { vous pouvez ajouter des unités ici };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
```

**Décortiquons ce code :**

| Ligne | Explication |
|-------|-------------|
| `program project1;` | Déclaration du programme |
| `{$mode objfpc}{$H+}` | Directives du compilateur (mode objet, chaînes longues) |
| `Interfaces` | Initialise la bibliothèque LCL (interface graphique) |
| `Forms` | Unité pour gérer les formulaires |
| `Unit1` | Votre unité de formulaire (générée automatiquement) |
| `Application.Initialize` | Initialise l'application |
| `Application.CreateForm(TForm1, Form1)` | Crée le formulaire principal |
| `Application.Run` | **Lance la boucle d'événements !** |

**Point important :** `Application.Run` démarre la boucle d'événements dont nous avons parlé au chapitre 14.1. C'est ici que votre programme attend les interactions de l'utilisateur.

### 2. Le fichier unité (.pas)

**Nom :** `unit1.pas`

C'est le code de votre formulaire :

```pascal
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type
  TForm1 = class(TForm)
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.
```

**Décortiquons ce code :**

| Élément | Explication |
|---------|-------------|
| `TForm1 = class(TForm)` | Définition de la classe de votre formulaire (hérite de TForm) |
| `private` | Section pour les méthodes/attributs privés |
| `public` | Section pour les méthodes/attributs publics |
| `Form1: TForm1;` | Instance globale de votre formulaire |
| `{$R *.lfm}` | Directive pour inclure le fichier .lfm |

**Ce que vous allez faire ici :**
- Ajouter des gestionnaires d'événements
- Déclarer des variables pour le formulaire
- Écrire la logique de votre application

### 3. Le fichier formulaire (.lfm)

**Nom :** `unit1.lfm`

C'est un fichier texte qui décrit visuellement votre formulaire :

```
object Form1: TForm1
  Left = 300
  Height = 240
  Top = 200
  Width = 320
  Caption = 'Form1'
  ClientHeight = 240
  ClientWidth = 320
end
```

**Vous ne modifiez généralement PAS ce fichier directement !**
- Lazarus le met à jour automatiquement quand vous modifiez le formulaire visuellement
- Il contient les propriétés de tous les composants

---

## Propriétés du formulaire

### L'Inspecteur d'Objets

Quand vous cliquez sur le formulaire, l'**Inspecteur d'Objets** (à gauche) affiche ses propriétés :

```
┌─────────────────────────┐
│ Inspecteur d'Objets     │
├─────────────────────────┤
│ Form1: TForm1           │
├─────────────────────────┤
│ Propriétés  | Événements│
├─────────────────────────┤
│ Caption    = 'Form1'    │
│ Width      = 320        │
│ Height     = 240        │
│ Position   = poDesigned │
│ BorderStyle= bsSizeable │
│ Color      = clBtnFace  │
│ ...                     │
└─────────────────────────┘
```

### Propriétés importantes à connaître

| Propriété | Type | Description |
|-----------|------|-------------|
| **Caption** | String | Titre affiché dans la barre de titre |
| **Width** | Integer | Largeur de la fenêtre (en pixels) |
| **Height** | Integer | Hauteur de la fenêtre (en pixels) |
| **Position** | Enum | Position à l'ouverture (centrée, défaut, etc.) |
| **BorderStyle** | Enum | Type de bordure (redimensionnable, fixe, etc.) |
| **Color** | Color | Couleur de fond du formulaire |
| **Name** | String | Nom de l'objet dans le code (ex: Form1) |

### Modifier une propriété

**Exemple : Changer le titre de la fenêtre**

1. Cliquez sur le formulaire
2. Dans l'Inspecteur d'Objets, trouvez la propriété `Caption`
3. Changez `'Form1'` en `'Ma première fenêtre'`
4. Appuyez sur Entrée

Le titre de la fenêtre change immédiatement !

---

## Compiler et exécuter

### Méthode 1 : Via le menu

**Menu : Exécuter → Exécuter** (ou appuyez sur **F9**)

Lazarus va :
1. **Compiler** votre projet
2. **Créer** l'exécutable
3. **Lancer** l'application

### Méthode 2 : Via la barre d'outils

Cliquez sur le bouton ▶️ (triangle vert) dans la barre d'outils.

### Résultat

Une fenêtre s'ouvre ! 🎉

```
┌─────────────────────────────────┐
│ Ma première fenêtre        _ □ ✕ │
├─────────────────────────────────┤
│                                 │
│                                 │
│         (fenêtre vide)          │
│                                 │
│                                 │
└─────────────────────────────────┘
```

**Testez l'interaction :**
- ✅ Vous pouvez déplacer la fenêtre
- ✅ Vous pouvez la redimensionner
- ✅ Vous pouvez la réduire/agrandir
- ✅ Vous pouvez la fermer

**Tout ça sans écrire une seule ligne de code !** La LCL gère automatiquement ces comportements standard.

---

## Comprendre ce qui se passe

### Au lancement de l'application

Voici le cycle de vie simplifié :

```
1. programme project1.lpr démarre
   ↓
2. Application.Initialize
   • Initialise le système graphique
   ↓
3. Application.CreateForm(TForm1, Form1)
   • Crée l'instance du formulaire
   • Lit le fichier .lfm
   • Construit les composants
   ↓
4. Le formulaire s'affiche
   ↓
5. Application.Run
   • Démarre la BOUCLE D'ÉVÉNEMENTS
   • Attend les interactions utilisateur
   ↓
6. L'utilisateur ferme la fenêtre
   • Événement OnClose déclenché
   • Libération des ressources
   ↓
7. Fin du programme
```

### La boucle d'événements en action

Pendant que votre fenêtre est ouverte, `Application.Run` tourne en boucle :

```pascal
// Pseudo-code de ce que fait Application.Run
while not ApplicationTerminated do
begin
  if EventAvailable then
  begin
    Event := GetNextEvent;
    DispatchEventToHandler(Event);
  end;
  ProcessMessages;
end;
```

C'est exactement le concept expliqué au chapitre 14.1 !

---

## Où sont les fichiers compilés ?

Lazarus crée plusieurs fichiers lors de la compilation :

```
MonPremierGUI/
├── project1.lpr           (votre code source)
├── unit1.pas              (votre code source)
├── unit1.lfm              (définition visuelle)
│
├── lib/                   (dossier créé automatiquement)
│   └── x86_64-win64/      (dépend de votre plateforme)
│       ├── project1.exe   ← L'EXÉCUTABLE !
│       ├── unit1.o        (fichier objet)
│       ├── unit1.ppu      (unité compilée)
│       └── ...
│
└── backup/                (sauvegardes automatiques)
```

**Sur Linux :** le dossier sera `x86_64-linux` et l'exécutable `project1` (sans .exe)

---

## Différences Windows / Linux

### Apparence

Votre fenêtre aura automatiquement l'apparence native :
- **Windows :** Style Windows (bordures Windows, boutons Windows)
- **Linux :** Style du gestionnaire de fenêtres (GTK, Qt, etc.)

**C'est la magie de la LCL !** Le même code produit une apparence native sur chaque plateforme.

### Compilation

Le processus est identique, seuls les fichiers générés diffèrent :

| Aspect | Windows | Linux |
|--------|---------|-------|
| Exécutable | `project1.exe` | `project1` |
| Dossier | `x86_64-win64` | `x86_64-linux` |
| Taille | ~2-3 MB | ~3-4 MB |

---

## Personnaliser votre première fenêtre

### Changez quelques propriétés

Essayez de modifier ces propriétés dans l'Inspecteur d'Objets :

**1. Caption**
```
'Ma première fenêtre' → 'Application de démonstration'
```

**2. Width et Height**
```
Width: 320 → 640
Height: 240 → 480
```

**3. Position**
```
poDesigned → poScreenCenter
```
Maintenant la fenêtre s'ouvrira centrée sur l'écran !

**4. Color**
```
clBtnFace → clSkyBlue
```
Le fond devient bleu ciel !

**5. BorderStyle**
```
bsSizeable → bsDialog
```
La fenêtre ne peut plus être redimensionnée.

### Recompilez et testez

Appuyez sur **F9** pour voir vos modifications !

---

## Structure d'un projet GUI vs Console

### Projet Console (ancien style)

```pascal
program HelloConsole;
begin
  WriteLn('Hello World!');
  ReadLn;
end.
```

**Caractéristiques :**
- Code linéaire
- Pas de boucle d'événements
- Interaction textuelle
- Terminé après quelques instructions

### Projet GUI (nouveau style)

```pascal
program HelloGUI;
uses
  Interfaces, Forms, Unit1;
begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;  // Boucle infinie !
end.
```

**Caractéristiques :**
- Structure événementielle
- Boucle d'événements active
- Interaction graphique
- Reste actif jusqu'à fermeture

---

## Conseils pour débuter

### 1. Sauvegardez souvent
**Ctrl+S** ou **Menu : Fichier → Tout enregistrer**

Lazarus peut parfois se fermer inopinément. Prenez l'habitude de sauvegarder régulièrement.

### 2. Un dossier par projet
Ne mélangez jamais les fichiers de plusieurs projets dans le même dossier.

### 3. Noms de fichiers clairs
Au lieu de `project1.lpr` et `unit1.pas`, utilisez des noms descriptifs :
- `GestionClients.lpr`
- `FormulairePrincipal.pas`

### 4. Versionning
Considérez utiliser Git dès le début pour suivre vos modifications.

### 5. Testez sur les deux plateformes
Si vous développez pour Windows et Linux, testez régulièrement sur les deux.

---

## Résolution de problèmes courants

### "Cannot find unit Interfaces"
**Cause :** Lazarus n'arrive pas à trouver la LCL.  
**Solution :** Vérifiez que Lazarus est correctement installé et que le chemin vers les sources LCL est configuré.  

### La fenêtre ne s'affiche pas
**Cause :** Erreur dans le code ou mauvaise configuration.  
**Solution :** Vérifiez que `Application.Run` est bien appelé dans le .lpr  

### "Project1.lpr(21,1) Fatal: Cannot open file"  
**Cause :** Chemin de fichier incorrect ou fichier manquant.  
**Solution :** Vérifiez que tous les fichiers du projet sont dans le bon dossier.  

### La fenêtre clignote et disparaît  
**Cause :** Exception non gérée au démarrage.  
**Solution :** Exécutez en mode debug (F7) pour voir l'erreur.  

---

## Ce que vous avez appris

✅ Créer un nouveau projet GUI avec Lazarus  
✅ Comprendre la structure des fichiers (.lpr, .pas, .lfm)  
✅ Utiliser l'Inspecteur d'Objets pour modifier les propriétés  
✅ Compiler et exécuter une application fenêtrée  
✅ Comprendre le cycle de vie d'une application GUI  
✅ Différencier projet console et projet GUI  

---

## Prochaines étapes

Vous avez maintenant une fenêtre vide fonctionnelle. Dans les prochaines sections, vous apprendrez à :

- **14.3 Formulaires (TForm)** : Aller plus loin avec les formulaires
- **14.4 Composants de base** : Ajouter des boutons, zones de texte, labels
- **14.5 Événements et handlers** : Faire réagir votre application aux clics

Votre fenêtre vide va bientôt devenir une vraie application interactive ! 🚀

---

**Félicitations !** Vous venez de créer votre première application graphique. C'est une étape majeure dans votre parcours de programmeur !

⏭️ [Formulaires (TForm)](/14-introduction-applications-graphiques/03-formulaires-tform.md)
