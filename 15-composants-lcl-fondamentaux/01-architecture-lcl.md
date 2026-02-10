🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.1 Architecture de la LCL

## Introduction

La **LCL** (Lazarus Component Library) est la bibliothèque de composants visuels qui constitue le cœur de Lazarus. C'est grâce à elle que vous pouvez créer des interfaces graphiques en plaçant des boutons, des zones de texte, des menus et bien d'autres éléments sur vos formulaires.

Dans ce chapitre, nous allons comprendre comment la LCL est organisée et pourquoi cette architecture la rend si puissante et flexible.

---

## Qu'est-ce que la LCL ?

La LCL est une bibliothèque de composants qui permet de créer des applications graphiques multi-plateformes. Elle fournit :

- **Des composants visuels** : boutons, zones de texte, listes, etc.
- **Des classes de base** : pour gérer les fenêtres, les événements, le dessin
- **Une abstraction** : qui permet à votre code de fonctionner sur Windows, Linux, macOS sans modification

### Analogie simple

Imaginez la LCL comme une **boîte à outils universelle** pour construire des interfaces graphiques. Peu importe que vous construisiez sur Windows ou Linux, vous utilisez les mêmes outils (composants), mais la LCL adapte automatiquement leur apparence et leur comportement au système d'exploitation.

---

## L'Architecture en Couches

La LCL est organisée en plusieurs couches distinctes, chacune ayant un rôle précis :

```
┌─────────────────────────────────────────┐
│     Votre Application                   │
│  (Formulaires, composants, code)        │
└─────────────────────────────────────────┘
                  ↓
┌─────────────────────────────────────────┐
│     LCL (Lazarus Component Library)     │
│  (Composants abstraits, TButton, etc.)  │
└─────────────────────────────────────────┘
                  ↓
┌─────────────────────────────────────────┐
│         Widgetset Interface             │
│  (Couche d'abstraction plateforme)      │
└─────────────────────────────────────────┘
                  ↓
┌─────────────────────────────────────────┐
│    Widgetsets Spécifiques               │
│  (Win32, GTK2, Qt5, Cocoa, etc.)        │
└─────────────────────────────────────────┘
                  ↓
┌─────────────────────────────────────────┐
│   Système d'Exploitation                │
│  (Windows, Linux, macOS)                │
└─────────────────────────────────────────┘
```

### Explication des couches

#### 1. Votre Application
C'est le code que vous écrivez : vos formulaires, vos gestionnaires d'événements, votre logique métier.

#### 2. La LCL (Couche principale)
La LCL définit tous les composants de manière **abstraite** et **indépendante de la plateforme**. Quand vous placez un `TButton` sur votre formulaire, vous utilisez la classe LCL qui ne sait pas (encore) sur quel système elle va s'exécuter.

**Classes principales de la LCL :**
- `TControl` : classe de base pour tous les contrôles visuels
- `TWinControl` : contrôles qui peuvent recevoir le focus
- `TCustomForm` : base des formulaires
- `TButton`, `TEdit`, `TLabel`, etc. : composants concrets

#### 3. Widgetset Interface
C'est une **couche d'abstraction** qui définit les méthodes que chaque plateforme doit implémenter. Elle fait le lien entre le code générique de la LCL et les implémentations spécifiques.

#### 4. Widgetsets Spécifiques
Ce sont les implémentations concrètes pour chaque plateforme :

- **Win32/Win64** : pour Windows (utilise l'API Windows native)
- **GTK2/GTK3** : pour Linux (utilise la bibliothèque GTK)
- **Qt5/Qt6** : alternative pour Linux/Windows (utilise Qt)
- **Cocoa** : pour macOS
- **fpGUI** : widgetset entièrement en Pascal (portable)
- **WinCE** : pour Windows CE/Mobile

---

## Le Principe du Widgetset

### Qu'est-ce qu'un Widgetset ?

Un **widgetset** est l'implémentation concrète des composants visuels pour une plateforme spécifique. C'est lui qui "sait" comment dessiner un bouton Windows ou un bouton GTK.

### Comment ça fonctionne ?

Quand vous écrivez :

```pascal
Button1.Caption := 'Cliquez-moi';
```

Voici ce qui se passe en coulisses :

1. **Votre code** appelle la propriété `Caption` de `TButton` (classe LCL)
2. **La LCL** délègue cette demande au widgetset actif
3. **Le widgetset** (par exemple Win32) traduit cela en appel API Windows
4. **Windows** affiche le texte sur le bouton natif

### Avantage : Write Once, Run Anywhere

Grâce à cette architecture, vous écrivez votre code **une seule fois**, et il fonctionne sur toutes les plateformes supportées. La LCL se charge de traduire vos intentions en appels natifs pour chaque système.

---

## Organisation des Unités LCL

La LCL est organisée en plusieurs unités (fichiers `.pas`) regroupées par fonctionnalité :

### Unités Fondamentales

- **`LCLType`** : types de base, constantes, énumérations
- **`LCLIntf`** : interface avec le widgetset
- **`LMessages`** : gestion des messages système
- **`Graphics`** : dessin (canvas, brush, pen)
- **`Controls`** : classes de base pour les contrôles

### Unités de Composants

- **`StdCtrls`** : composants standards (TButton, TEdit, TLabel, etc.)
- **`ExtCtrls`** : composants étendus (TPanel, TImage, TTimer, etc.)
- **`ComCtrls`** : composants communs (TTreeView, TListView, TProgressBar, etc.)
- **`Dialogs`** : boîtes de dialogue (TOpenDialog, TSaveDialog, etc.)
- **`Forms`** : formulaires (TForm, TApplication)
- **`Menus`** : menus (TMainMenu, TPopupMenu)
- **`Grids`** : grilles (TStringGrid, TDrawGrid)

### Unités Utilitaires

- **`FileUtil`** : manipulation de fichiers multi-plateforme
- **`LCLProc`** : procédures utilitaires
- **`LazFileUtils`** : utilitaires fichiers Lazarus
- **`LazUTF8`** : gestion de l'Unicode UTF-8

---

## La Classe TControl : Racine de Tout

Presque tous les composants visuels de la LCL héritent (directement ou indirectement) de la classe `TControl`.

### Hiérarchie simplifiée

```
TObject
  ↓
TComponent
  ↓
TControl ← Tous les contrôles visuels
  ↓
  ├─ TGraphicControl (contrôles sans fenêtre : TLabel, TImage)
  └─ TWinControl (contrôles avec fenêtre : TButton, TEdit)
       ↓
       ├─ TCustomControl (contrôles personnalisés avec canvas)
       │    ↓
       │    └─ TPanel, etc.
       └─ TCustomForm
            ↓
            └─ TForm
```

### Propriétés héritées de TControl

Tous les composants héritent de ces propriétés de base :

- **Position et taille** : `Left`, `Top`, `Width`, `Height`
- **Visibilité** : `Visible`, `Enabled`
- **Apparence** : `Color`, `Font`, `Cursor`
- **Ancrage** : `Align`, `Anchors`, `BorderSpacing`
- **Événements** : `OnClick`, `OnMouseMove`, `OnKeyPress`, etc.

---

## TWinControl vs TGraphicControl

Il existe deux grandes familles de contrôles visuels :

### TWinControl (Contrôles avec Handle)

Ce sont des contrôles qui possèdent un **handle** (identifiant de fenêtre du système).

**Caractéristiques :**
- Peuvent recevoir le focus clavier
- Peuvent contenir d'autres contrôles (conteneurs)
- Plus lourds en ressources système
- Gérés directement par le système d'exploitation

**Exemples :** `TButton`, `TEdit`, `TPanel`, `TForm`, `TListBox`

### TGraphicControl (Contrôles sans Handle)

Ce sont des contrôles **dessinés** par leur parent, sans fenêtre système propre.

**Caractéristiques :**
- Ne peuvent pas recevoir le focus
- Plus légers en ressources
- Dessinés via le Canvas du parent
- Gérés par la LCL uniquement

**Exemples :** `TLabel`, `TImage`, `TShape`, `TSpeedButton`

### Quand utiliser l'un ou l'autre ?

- **TWinControl** : quand vous avez besoin d'interaction clavier, de conteneur, ou de fonctionnalités système avancées
- **TGraphicControl** : pour de l'affichage simple, des indicateurs visuels, des étiquettes

---

## Le Système d'Événements

La LCL utilise un système d'événements pour gérer les interactions utilisateur.

### Comment ça fonctionne ?

1. **L'utilisateur** clique sur un bouton
2. **Le système d'exploitation** détecte le clic et envoie un message
3. **Le widgetset** capture ce message natif
4. **La LCL** transforme le message en événement Pascal
5. **Votre gestionnaire d'événement** (`OnClick`) est appelé

### Types d'événements

- **Souris** : `OnClick`, `OnDblClick`, `OnMouseDown`, `OnMouseMove`, `OnMouseUp`
- **Clavier** : `OnKeyPress`, `OnKeyDown`, `OnKeyUp`
- **Focus** : `OnEnter`, `OnExit`
- **Peinture** : `OnPaint`
- **Changement** : `OnChange`, `OnResize`

---

## Le Canvas : Surface de Dessin

Tous les contrôles qui héritent de `TCustomControl` ou `TGraphicControl` possèdent une surface de dessin appelée **Canvas**.

### Qu'est-ce que le Canvas ?

Le Canvas est une abstraction qui vous permet de dessiner (lignes, rectangles, texte, images) sans vous soucier des détails du système graphique sous-jacent.

**Outils du Canvas :**
- **Pen** (stylo) : pour les lignes et les contours
- **Brush** (pinceau) : pour remplir les surfaces
- **Font** (police) : pour le texte

### Exemple simple

```pascal
procedure TForm1.FormPaint(Sender: TObject);  
begin  
  // Dessiner un rectangle bleu avec contour rouge
  Canvas.Pen.Color := clRed;
  Canvas.Brush.Color := clBlue;
  Canvas.Rectangle(10, 10, 100, 100);
end;
```

---

## Le Rôle de TApplication

`TApplication` est une classe singleton (une seule instance) qui représente votre application entière.

### Responsabilités de TApplication

- **Initialisation** : créer la fenêtre principale
- **Boucle de messages** : gérer les événements système
- **Gestion des formulaires** : ouvrir/fermer les fenêtres
- **Propriétés globales** : titre, icône, hints

### Utilisation typique

```pascal
program MonProjet;

uses
  Forms, Unit1;

begin
  Application.Initialize;
  Application.Title := 'Mon Application';
  Application.CreateForm(TForm1, Form1);
  Application.Run;  // Lance la boucle de messages
end.
```

---

## Multi-plateforme : Comment c'est possible ?

### Le Secret : L'Abstraction

La LCL ne fait **jamais** d'appels directs au système d'exploitation. Elle passe toujours par le widgetset.

### Compilation conditionnelle

À la compilation, Lazarus inclut uniquement le widgetset correspondant à la plateforme cible :

- Sous Windows → widgetset Win32
- Sous Linux → widgetset GTK2/GTK3
- Sous macOS → widgetset Cocoa

### Directives de compilation

Dans le code source de la LCL, vous trouverez souvent :

```pascal
{$IFDEF WINDOWS}
  // Code spécifique Windows
{$ENDIF}

{$IFDEF LINUX}
  // Code spécifique Linux
{$ENDIF}
```

Mais en tant qu'utilisateur de la LCL, vous n'avez **pas besoin** d'utiliser ces directives. La LCL s'en charge pour vous !

---

## Avantages de l'Architecture LCL

### 1. Portabilité
Votre code fonctionne sur plusieurs systèmes sans modification.

### 2. Apparence native
Les applications ont l'apparence du système d'exploitation sur lequel elles s'exécutent.

### 3. Performance
Les composants utilisent les APIs natives, garantissant de bonnes performances.

### 4. Extensibilité
Vous pouvez créer vos propres composants en héritant des classes de base.

### 5. Cohérence
L'API est cohérente quelle que soit la plateforme.

---

## Différences avec la VCL de Delphi

Si vous connaissez Delphi et sa VCL (Visual Component Library), voici les principales différences :

| Aspect | VCL (Delphi) | LCL (Lazarus) |
|--------|--------------|---------------|
| Plateformes | Windows uniquement | Multi-plateforme |
| Architecture | Accès direct API Windows | Couche d'abstraction (widgetset) |
| Apparence | Windows native | Native à chaque plateforme |
| Compatibilité | Propriétaire | Open Source |
| Compatibilité code | La LCL est largement compatible avec la VCL | Migration possible mais pas 100% |

---

## Organisation Physique des Fichiers

Dans votre installation Lazarus, la LCL se trouve dans le dossier :

```
<lazarus>/lcl/
  ├── include/         (fichiers d'implémentation .inc)
  ├── interfaces/      (widgetsets)
  │   ├── win32/
  │   ├── gtk2/
  │   ├── qt5/
  │   └── ...
  ├── forms.pp         (unité Forms)
  ├── controls.pp      (unité Controls)
  ├── stdctrls.pp      (unité StdCtrls)
  └── ...
```

Vous n'avez normalement **pas besoin** de modifier ces fichiers, mais il peut être instructif de les consulter pour comprendre comment les choses fonctionnent.

---

## Cycle de Vie d'un Composant

Comprendre le cycle de vie d'un composant aide à mieux utiliser la LCL.

### 1. Création
```pascal
Button1 := TButton.Create(Self);  // Self = propriétaire (souvent le Form)
```

### 2. Configuration
```pascal
Button1.Parent := Self;  // Attaché au formulaire  
Button1.Caption := 'OK';  
Button1.Left := 10;  
Button1.Top := 10;  
```

### 3. Affichage
Le composant est dessiné lors du premier `Show` ou `Application.Run`.

### 4. Utilisation
L'utilisateur interagit, les événements sont déclenchés.

### 5. Destruction
```pascal
Button1.Free;  // ou automatique si Owner défini
```

### Le rôle du Owner (propriétaire)

Quand vous créez un composant avec un propriétaire :
```pascal
TButton.Create(Form1);
```

Le formulaire `Form1` devient responsable de libérer automatiquement le bouton quand il sera lui-même détruit. C'est une gestion automatique de la mémoire très pratique !

---

## Composants Visuels vs Non-Visuels

### Composants Visuels
Ils apparaissent à l'exécution : `TButton`, `TEdit`, `TPanel`, etc.

### Composants Non-Visuels
Ils n'apparaissent que dans l'IDE (en mode design) mais sont invisibles à l'exécution :

- `TTimer` : déclenche des actions périodiques
- `TOpenDialog` : ouvre une boîte de dialogue
- `TDataSource` : connecteur de données
- `TImageList` : collection d'images

Ces composants héritent de `TComponent` mais pas de `TControl`.

---

## Points Clés à Retenir

1. **La LCL est une bibliothèque multi-plateforme** qui abstrait les différences entre systèmes d'exploitation.

2. **L'architecture en couches** sépare votre code de l'implémentation système via les widgetsets.

3. **Tous les composants visuels héritent de TControl**, qui fournit les propriétés et méthodes de base.

4. **TWinControl** (avec handle) vs **TGraphicControl** (dessiné) : deux familles de composants.

5. **Le système d'événements** transforme les messages système en événements Pascal que vous pouvez gérer.

6. **Le Canvas** permet de dessiner sur les contrôles de manière portable.

7. **TApplication** gère la boucle de messages et l'application globale.

8. **Write Once, Run Anywhere** : le même code fonctionne sur Windows, Linux et macOS.

---

## Conclusion

Comprendre l'architecture de la LCL vous permet de :

- Mieux appréhender comment fonctionnent vos applications
- Choisir les bons composants pour vos besoins
- Déboguer plus efficacement
- Créer vos propres composants personnalisés (chapitre suivant)
- Écrire du code véritablement multi-plateforme

Dans les sections suivantes, nous explorerons la **hiérarchie des composants** en détail, puis nous apprendrons à utiliser les composants les plus courants de la LCL.

---

**Prochaine étape :** 15.2 Hiérarchie des composants

⏭️ [Hiérarchie des composants](/15-composants-lcl-fondamentaux/02-hierarchie-composants.md)
