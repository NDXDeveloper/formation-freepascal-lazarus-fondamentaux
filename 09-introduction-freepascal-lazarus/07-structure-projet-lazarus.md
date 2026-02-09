🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.7 Structure d'un projet Lazarus

## Introduction

Dans la section précédente, vous avez créé vos premiers projets avec Lazarus. Vous avez vu apparaître plusieurs fichiers avec différentes extensions (.lpr, .lpi, .pas, .lfm...). Mais à quoi servent exactement tous ces fichiers ? Comment sont-ils organisés ? Comment interagissent-ils ?

Cette section vous explique en détail la structure d'un projet Lazarus. Comprendre cette structure est essentiel pour :
- Savoir quels fichiers sauvegarder
- Comprendre comment votre projet est organisé
- Pouvoir partager votre code avec d'autres
- Résoudre les problèmes de compilation
- Travailler en équipe sur un projet

**Ne vous inquiétez pas !** Même si cela semble technique, nous allons tout expliquer simplement, avec des exemples concrets.

## Vue d'ensemble : les deux types de projets

Lazarus peut créer deux grands types de projets, avec des structures différentes.

### Projet simple (console)

Un programme console, comme notre "Hello World", est le plus simple :

```
HelloWorld/
  ├── HelloWorld.lpr         # Fichier source principal
  ├── HelloWorld.lps         # Session (optionnel)
  ├── lib/                   # Dossier de compilation
  │   └── ...fichiers .o, .ppu...
  └── HelloWorld.exe         # Exécutable final (Windows)
      ou HelloWorld          # (Linux)
```

**Caractéristiques :**
- Un seul fichier source (.lpr)
- Pas de fichier projet (.lpi)
- Pas d'interface graphique

### Projet application (graphique)

Une application avec interface graphique est plus complexe :

```
MonAppli/
  ├── MonAppli.lpi           # Fichier projet (Project Information)
  ├── MonAppli.lpr           # Programme principal
  ├── MonAppli.lps           # Session
  ├── MonAppli.res           # Ressources (icône, etc.)
  ├── unit1.pas              # Code du formulaire 1
  ├── unit1.lfm              # Description visuelle du formulaire 1
  ├── unit2.pas              # Code du formulaire 2 (si plusieurs)
  ├── unit2.lfm              # Description visuelle du formulaire 2
  ├── lib/                   # Dossier de compilation
  │   ├── x86_64-win64/      # Plateforme cible
  │   │   ├── unit1.o
  │   │   ├── unit1.ppu
  │   │   └── ...
  │   └── ...
  ├── backup/                # Sauvegardes automatiques (optionnel)
  │   └── ...
  └── MonAppli.exe           # Exécutable final
```

**Caractéristiques :**
- Fichier projet (.lpi) obligatoire
- Programme principal (.lpr)
- Une ou plusieurs unités (.pas + .lfm)
- Fichiers de ressources (.res)
- Organisation plus structurée

## Les fichiers essentiels d'un projet

### 1. Le fichier .lpi (Lazarus Project Information)

**C'est le cœur de votre projet !**

#### Rôle et importance

Le fichier .lpi est un fichier **XML** qui contient toute la configuration de votre projet :
- La liste des fichiers sources
- Les options de compilation
- Les paramètres de l'application (icône, version, titre...)
- Les packages requis
- Les paramètres de débogage
- Les chemins de sortie

**Analogie :** C'est comme le "plan de montage" d'un meuble IKEA : il dit quelles pièces utiliser et comment les assembler.

#### Contenu typique (simplifié)

```xml
<?xml version="1.0" encoding="UTF-8"?>
<CONFIG>
  <ProjectOptions>
    <Version Value="12"/>
    <General>
      <Title Value="MonAppli"/>
      <Icon Value="0"/>
    </General>
    <BuildModes Count="1">
      <Item1 Name="Default">
        <CompilerOptions>
          <Target>
            <Filename Value="MonAppli"/>
          </Target>
        </CompilerOptions>
      </Item1>
    </BuildModes>
    <Units Count="2">
      <Unit0>
        <Filename Value="MonAppli.lpr"/>
        <IsPartOfProject Value="True"/>
      </Unit0>
      <Unit1>
        <Filename Value="unit1.pas"/>
        <IsPartOfProject Value="True"/>
      </Unit1>
    </Units>
  </ProjectOptions>
</CONFIG>
```

**Points importants :**
- `<Title>` : nom de l'application
- `<Filename>` : nom de l'exécutable
- `<Units>` : liste des fichiers du projet

#### Ouverture d'un projet

**Pour ouvrir un projet dans Lazarus :**
1. Menu **File** → **Open Project...**
2. Naviguez vers votre dossier projet
3. **Sélectionnez le fichier .lpi**
4. Cliquez **Ouvrir**

Lazarus charge alors automatiquement tous les fichiers associés.

**⚠️ Important :** Ne modifiez JAMAIS le .lpi manuellement avec un éditeur de texte ! Utilisez toujours Lazarus pour le gérer.

### 2. Le fichier .lpr (Lazarus Program)

**C'est le programme principal de votre application.**

#### Rôle et importance

Le .lpr est le point d'entrée de votre programme. C'est le fichier qui contient le code principal qui sera exécuté au démarrage.

**Analogie :** C'est comme la fonction `main()` en C ou Java : tout commence ici.

#### Exemple pour un programme console

```pascal
program HelloWorld;

{$mode objfpc}{$H+}

uses
  SysUtils;

begin
  WriteLn('Hello World !');
  WriteLn('Appuyez sur Entrée...');
  ReadLn;
end.
```

**Structure :**
1. `program HelloWorld;` : déclaration du nom du programme
2. `{$mode objfpc}{$H+}` : directives de compilation
3. `uses SysUtils;` : unités utilisées
4. `begin...end.` : code principal

#### Exemple pour une application graphique

```pascal
program MonAppli;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unit1;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
```

**Structure :**
1. `program MonAppli;` : nom du programme
2. `uses Interfaces, Forms, unit1;` : unités nécessaires
3. `{$R *.res}` : inclusion des ressources
4. `Application.Initialize` : initialisation de l'application
5. `Application.CreateForm(TForm1, Form1);` : création du formulaire principal
6. `Application.Run;` : lancement de la boucle d'événements

**Note :** Pour les applications graphiques, Lazarus génère et gère automatiquement ce fichier. Vous n'avez généralement pas besoin de le modifier.

#### Quand modifier le .lpr ?

**Rarement pour les applications graphiques !** Lazarus le gère.

**Pour les programmes console :** C'est là que vous écrivez votre code principal.

**Modifications possibles (avancé) :**
- Ajouter du code d'initialisation global
- Configurer des options avant le lancement de l'application
- Gérer des exceptions globales

### 3. Les fichiers .pas (Pascal Unit)

**Ce sont vos unités de code : le "vrai" travail se fait ici !**

#### Rôle et importance

Les fichiers .pas contiennent :
- Le code de vos formulaires
- Vos procédures et fonctions personnalisées
- Vos classes et objets
- Vos types de données personnalisés
- La logique métier de votre application

**Analogie :** C'est comme les chapitres d'un livre : chaque unité traite d'un sujet particulier.

#### Structure d'une unité

Chaque fichier .pas a toujours la même structure en 3 parties :

```pascal
unit unit1;  // 1. En-tête : nom de l'unité

{$mode objfpc}{$H+}

// 2. INTERFACE : partie publique (visible de l'extérieur)
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    // Déclarations privées (non visibles de l'extérieur)
  public
    // Déclarations publiques (visibles de l'extérieur)
  end;

var
  Form1: TForm1;

// 3. IMPLEMENTATION : partie privée (code réel)
implementation

{$R *.lfm}  // Charge le formulaire visuel

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Bouton cliqué !');
end;

// 4. INITIALIZATION (optionnel)
initialization
  // Code exécuté au chargement de l'unité

// 5. FINALIZATION (optionnel)
finalization
  // Code exécuté à la fermeture du programme

end.  // Fin de l'unité (notez le point !)
```

#### Détail des sections

**1. Interface (partie publique)**
- Déclare ce qui est visible par les autres unités
- Types, classes, procédures, fonctions publiques
- Comme une "vitrine" : on voit ce qui est disponible

**2. Implementation (code réel)**
- Contient le code effectif des procédures et fonctions
- Peut avoir des éléments privés (non visibles de l'extérieur)
- C'est ici que le travail est fait

**3. Initialization (optionnel)**
- Code exécuté automatiquement au chargement de l'unité
- Utilisé pour initialiser des variables globales, charger des configurations, etc.

**4. Finalization (optionnel)**
- Code exécuté automatiquement à la fermeture du programme
- Utilisé pour libérer des ressources, sauvegarder des données, etc.

#### Exemple concret : unité de calculs

```pascal
unit UnitCalculs;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

// Fonctions publiques (utilisables depuis d'autres unités)
function Additionner(a, b: Integer): Integer;
function Multiplier(a, b: Integer): Integer;
function Diviser(a, b: Real): Real;

implementation

// Code des fonctions
function Additionner(a, b: Integer): Integer;
begin
  Result := a + b;
end;

function Multiplier(a, b: Integer): Integer;
begin
  Result := a * b;
end;

function Diviser(a, b: Real): Real;
begin
  if b = 0 then
    raise Exception.Create('Division par zéro !')
  else
    Result := a / b;
end;

end.
```

**Utilisation dans une autre unité :**

```pascal
unit unit1;

interface

uses
  UnitCalculs;  // On inclut notre unité de calculs

// ...

implementation

procedure TForm1.Button1Click(Sender: TObject);
var
  Resultat: Integer;
begin
  Resultat := Additionner(5, 3);  // Utilise la fonction de UnitCalculs
  ShowMessage('5 + 3 = ' + IntToStr(Resultat));
end;

end.
```

### 4. Les fichiers .lfm (Lazarus Form)

**C'est la description visuelle de vos formulaires.**

#### Rôle et importance

Le fichier .lfm décrit la structure visuelle d'un formulaire :
- Position et taille de la fenêtre
- Tous les composants (boutons, labels, etc.)
- Leurs propriétés (couleur, texte, taille...)
- Leurs positions relatives

**Important :** Le .lfm est lié à un fichier .pas du même nom.
- `unit1.pas` ↔ `unit1.lfm`
- `FormPrincipal.pas` ↔ `FormPrincipal.lfm`

#### Format du fichier

Le .lfm est un fichier texte dans un format propriétaire Lazarus (similaire au .dfm de Delphi).

**Exemple :**

```
object Form1: TForm1
  Left = 300
  Height = 400
  Top = 200
  Width = 600
  Caption = 'Ma Première Application'
  ClientHeight = 400
  ClientWidth = 600
  Color = clSkyBlue
  Position = poScreenCenter
  LCLVersion = '3.0.0.0'

  object Label1: TLabel
    Left = 150
    Height = 20
    Top = 50
    Width = 300
    Caption = 'Bienvenue dans mon application !'
    Font.Height = -16
    Font.Style = [fsBold]
  end

  object Button1: TButton
    Left = 220
    Height = 30
    Top = 150
    Width = 160
    Caption = 'Cliquez-moi !'
    OnClick = Button1Click
    TabOrder = 0
  end
end
```

**Lecture du fichier :**
- `object Form1: TForm1` : début de la définition du formulaire
- Propriétés du formulaire (Left, Height, Caption, Color...)
- Objets enfants (Label1, Button1) avec leurs propriétés
- `end` : fin de chaque objet

#### Lien avec le fichier .pas

Dans le fichier .pas correspondant, vous trouvez :

```pascal
{$R *.lfm}  // Cette directive charge le .lfm
```

Au démarrage, Lazarus :
1. Lit le fichier .lfm
2. Crée les objets visuels décrits
3. Configure leurs propriétés
4. Les associe au code du .pas

**⚠️ Ne modifiez JAMAIS le .lfm manuellement !**
- Utilisez toujours le concepteur visuel de Lazarus
- Modifications manuelles = risque de corruption
- Lazarus gère automatiquement ce fichier

### 5. Le fichier .lps (Lazarus Project Session)

**C'est la mémorisation de votre session de travail.**

#### Rôle et importance

Le .lps sauvegarde :
- Quels fichiers étaient ouverts
- Position du curseur dans chaque fichier
- Points d'arrêt (breakpoints) du débogueur
- Fenêtres ouvertes/fermées
- Disposition de l'espace de travail

**Analogie :** C'est comme les onglets ouverts dans votre navigateur : ça vous permet de reprendre où vous vous êtes arrêté.

#### Caractéristiques

**Format :** XML (similaire au .lpi)

**Contenu typique :**

```xml
<?xml version="1.0" encoding="UTF-8"?>
<CONFIG>
  <ProjectSession>
    <Version Value="12"/>
    <Units Count="2">
      <Unit0>
        <Filename Value="MonAppli.lpr"/>
        <IsPartOfProject Value="True"/>
        <EditorIndex Value="1"/>
        <CursorPos X="15" Y="18"/>
        <UsageCount Value="25"/>
      </Unit0>
      <Unit1>
        <Filename Value="unit1.pas"/>
        <IsPartOfProject Value="True"/>
        <IsVisibleTab Value="True"/>
        <EditorIndex Value="0"/>
        <TopLine Value="15"/>
        <CursorPos X="8" Y="32"/>
        <UsageCount Value="25"/>
      </Unit1>
    </Units>
  </ProjectSession>
</CONFIG>
```

**Informations sauvegardées :**
- Fichiers ouverts dans l'éditeur
- Position du curseur (ligne, colonne)
- Ligne visible en haut de l'éditeur
- Onglet actif

#### Faut-il sauvegarder le .lps ?

**Non, pas nécessaire !**

**Avantages de le sauvegarder :**
- ✅ Garde votre espace de travail personnel
- ✅ Reprend exactement où vous étiez

**Inconvénients :**
- ❌ Spécifique à votre machine
- ❌ Peut causer des conflits en équipe
- ❌ Lazarus le recrée automatiquement

**Recommandation :**
- Travail solo : sauvegardez si vous voulez
- Travail en équipe : ajoutez `.lps` au `.gitignore`
- Si absent : Lazarus en crée un nouveau automatiquement

### 6. Le fichier .res (Resources)

**Contient les ressources binaires de l'application.**

#### Rôle et importance

Le fichier .res contient des ressources embarquées :
- **Icône de l'application** (visible dans la barre des tâches)
- Manifest Windows (informations sur l'application)
- Version de l'application
- Autres ressources binaires (images, sons...)

**Pour les applications graphiques :** Lazarus crée automatiquement ce fichier.

#### Contenu typique

```
{$R *.res}
```

Cette directive dans le .lpr indique d'inclure le fichier .res.

**Le fichier .res est binaire**, vous ne pouvez pas l'ouvrir avec un éditeur de texte.

#### Personnaliser l'icône de l'application

**Méthode 1 : Via les options du projet**
1. Menu **Project** → **Project Options**
2. Section **Application** → **Icon**
3. Cliquez sur **Load Icon**
4. Sélectionnez votre fichier .ico
5. Cliquez **OK**
6. Recompilez (F9)

**Méthode 2 : Via le Project Inspector**
1. Menu **View** → **Project Inspector**
2. Double-cliquez sur le fichier .res
3. Un éditeur de ressources s'ouvre
4. Modifiez l'icône

**Format de l'icône :**
- Windows : fichier .ico (16×16, 32×32, 48×48 pixels)
- Linux : fichier .png ou .xpm

## Les dossiers d'un projet

### 1. Le dossier lib/

**Contient les fichiers temporaires de compilation.**

#### Contenu

```
lib/
  ├── x86_64-win64/         # Plateforme cible (exemple : Windows 64 bits)
  │   ├── unit1.o           # Fichier objet (code compilé)
  │   ├── unit1.ppu         # Pascal Precompiled Unit
  │   ├── MonAppli.res      # Ressources compilées
  │   └── ...
  ├── x86_64-linux/         # Autre plateforme (si compilation croisée)
  │   └── ...
  └── ...
```

**Types de fichiers :**

| Extension | Description |
|-----------|-------------|
| **.o** | Fichier objet : code machine intermédiaire |
| **.ppu** | Pascal Precompiled Unit : unité précompilée |
| **.compiled** | Marque de compilation réussie |
| **.res** | Ressources compilées |
| **.or** | Fichier objet pour les ressources |

**Organisation :**
- Un sous-dossier par plateforme cible
- Exemples : `x86_64-win64`, `i386-linux`, `arm-android`

#### Peut-on supprimer lib/ ?

**Oui, sans problème !**

- Ces fichiers seront recréés à la prochaine compilation
- Utile pour "nettoyer" un projet
- Gain d'espace disque (lib/ peut être volumineux)

**Comment nettoyer :**
1. Menu **Run** → **Clean up and Build**
2. Ou supprimer manuellement le dossier lib/

**Quand nettoyer :**
- Erreurs de compilation mystérieuses
- Changement d'options de compilation
- Avant de partager le projet (réduit la taille)

### 2. Le dossier backup/

**Sauvegardes automatiques des fichiers modifiés.**

#### Rôle

Lazarus peut sauvegarder automatiquement vos fichiers avant modification.

**Configuration :**
1. Menu **Tools** → **Options**
2. Section **Environment** → **Backup**
3. Cochez **Create backup of files**
4. Choisissez le sous-dossier : `backup/` (recommandé)

#### Contenu

```
backup/
  ├── unit1.pas.bak
  ├── unit1.lfm.bak
  ├── MonAppli.lpr.bak
  └── ...
```

Chaque fichier sauvegardé a l'extension `.bak` ajoutée.

#### Utilisation

**Pour restaurer un fichier :**
1. Allez dans `backup/`
2. Renommez `unit1.pas.bak` en `unit1.pas`
3. Remplacez le fichier actuel (attention, sauvegardez-le d'abord !)

**Avantages :**
- Protection contre les erreurs
- Annulation de modifications importantes
- Historique limité des changements

**Inconvénients :**
- Prend de l'espace disque
- Pas aussi sophistiqué qu'un vrai système de versions (Git)

**Recommandation :**
- Activez les backups pour débuter
- Plus tard, passez à Git pour un vrai versioning

### 3. Autres dossiers (avancé)

**docs/** : Documentation du projet  
**images/** : Images, icônes pour l'interface  
**data/** : Fichiers de données (configuration, bases de données...)  
**tests/** : Tests unitaires (FPCUnit)  

**Organisation personnelle :** Vous pouvez créer vos propres dossiers selon vos besoins.

## Hiérarchie et dépendances

### Dépendances entre fichiers

**Ordre d'importance :**

```
.lpi (Fichier projet)
  ↓
.lpr (Programme principal)
  ↓
.pas (Unités de code)
  ↓
.lfm (Formulaires visuels)
```

**Le .lpi coordonne tout :**
- Liste tous les fichiers du projet
- Définit l'ordre de compilation
- Configure les options

**Le .lpr démarre l'application :**
- Point d'entrée du programme
- Initialise l'application
- Crée les formulaires

**Les .pas contiennent la logique :**
- Code métier
- Gestionnaires d'événements
- Fonctions et procédures

**Les .lfm définissent l'apparence :**
- Structure visuelle
- Propriétés des composants
- Liés aux .pas correspondants

### Clause uses : gérer les dépendances

**Dans chaque unité .pas, la clause `uses` liste les unités nécessaires :**

```pascal
unit unit1;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;
```

**Unités standards courantes :**

| Unité | Contenu |
|-------|---------|
| **Classes** | Listes, streams, classes de base |
| **SysUtils** | Fonctions système, manipulation de chaînes, dates |
| **Forms** | Formulaires et applications graphiques |
| **Controls** | Contrôles de base (TControl) |
| **Graphics** | Dessin, couleurs, polices |
| **Dialogs** | Boîtes de dialogue standard |
| **StdCtrls** | Contrôles standard (boutons, labels...) |
| **ExtCtrls** | Contrôles étendus (panels, timers...) |

**Unités personnalisées :**

```pascal
uses
  unit1, UnitCalculs, UnitBaseDonnees;
```

**Ordre d'importance :**
1. Unités système d'abord
2. Puis unités LCL
3. Enfin unités personnalisées

**⚠️ Attention aux dépendances circulaires !**

**Exemple de problème :**
```pascal
// unit1.pas
uses unit2;

// unit2.pas
uses unit1;  // Erreur : dépendance circulaire !
```

**Solution :** Utiliser la section `implementation` pour les dépendances secondaires :

```pascal
// unit1.pas
interface
uses Classes;

implementation
uses unit2;  // OK : dans implementation seulement
```

## Organisation d'un projet multi-formulaires

Pour une application avec plusieurs fenêtres :

```
MonGrandeAppli/
  ├── MonGrandeAppli.lpi
  ├── MonGrandeAppli.lpr
  ├── MonGrandeAppli.res
  │
  ├── FormPrincipal.pas          # Formulaire principal
  ├── FormPrincipal.lfm
  │
  ├── FormOptions.pas            # Fenêtre d'options
  ├── FormOptions.lfm
  │
  ├── FormAPropos.pas            # Fenêtre "À propos"
  ├── FormAPropos.lfm
  │
  ├── UnitBaseDonnees.pas        # Unité pour la BD (sans formulaire)
  ├── UnitCalculs.pas            # Unité de calculs (sans formulaire)
  │
  └── lib/
```

**Programme principal (.lpr) :**

```pascal
program MonGrandeAppli;

uses
  Interfaces, Forms,
  FormPrincipal,      // Formulaire principal
  FormOptions,        // Autres formulaires
  FormAPropos,
  UnitBaseDonnees,    // Unités sans interface
  UnitCalculs;

begin
  Application.Initialize;
  Application.CreateForm(TFormPrincipal, FormPrincipal);  // Création du form principal
  Application.Run;
end.
```

**Notes :**
- Le formulaire principal est créé avec `CreateForm`
- Les autres formulaires sont créés à la demande dans le code
- Les unités sans interface sont juste listées dans `uses`

## Fichiers à sauvegarder et partager

### Fichiers essentiels (INDISPENSABLES)

✅ À sauvegarder absolument :

| Fichier | Pourquoi |
|---------|----------|
| **.lpi** | Configuration du projet |
| **.lpr** | Programme principal |
| **.pas** | Tout votre code |
| **.lfm** | Structure des formulaires |
| **.res** | Ressources (icône...) |

**Sans ces fichiers, votre projet ne peut pas être recompilé !**

### Fichiers optionnels

🔶 Peuvent être sauvegardés :

| Fichier | Utilité |
|---------|---------|
| **.lps** | Session de travail personnelle |
| **backup/** | Sauvegardes automatiques |
| **README.md** | Documentation du projet |

### Fichiers à ne PAS sauvegarder

❌ Ne sauvegardez pas (seront recréés) :

| Fichier/Dossier | Raison |
|-----------------|--------|
| **lib/** | Fichiers de compilation temporaires |
| **.exe** / exécutable | Résultat de la compilation |
| **.o, .ppu** | Fichiers objets temporaires |
| **.compiled** | Marque de compilation |

**Avantage :** Économise beaucoup d'espace !

### Utiliser un fichier .gitignore (avancé)

Si vous utilisez Git pour le versioning :

**.gitignore :**
```
# Fichiers de compilation
lib/
*.exe
*.o
*.ppu
*.compiled
*.or

# Session
*.lps

# Backup
backup/

# Fichiers temporaires
*~
*.bak
*.tmp
```

## Comprendre la compilation : du code au programme

### Processus de compilation

**Étape 1 : Analyse syntaxique**
- Lazarus lit les fichiers .pas
- Vérifie la syntaxe (begin/end, point-virgules...)
- Construit un arbre syntaxique

**Étape 2 : Compilation**
- Transformation du Pascal en code machine
- Création des fichiers .o (fichiers objets)
- Chaque unité .pas → un fichier .o

**Étape 3 : Linking (édition de liens)**
- Rassemblement de tous les .o
- Ajout des bibliothèques système
- Inclusion des ressources (.res, .lfm)
- Création de l'exécutable final

**Résultat :** Un fichier .exe (Windows) ou un exécutable (Linux) prêt à l'emploi.

### Ordre de compilation

Lazarus compile dans cet ordre :

1. **Unités sans dépendances** en premier
2. **Unités avec dépendances** ensuite (selon la clause `uses`)
3. **Programme principal** (.lpr) en dernier

**Exemple :**
```
UnitCalculs (pas de dépendance)
  ↓
UnitBaseDonnees (utilise UnitCalculs)
  ↓
unit1 (utilise UnitCalculs et UnitBaseDonnees)
  ↓
MonAppli.lpr (utilise unit1)
```

**Lazarus détermine automatiquement l'ordre** en analysant les clauses `uses`.

### Modes de compilation

**Debug (par défaut pour développement)**
- Informations de débogage incluses
- Optimisations désactivées
- Fichier plus gros, mais débogage possible
- Performance réduite

**Release (pour distribution)**
- Pas d'informations de débogage
- Optimisations activées
- Fichier plus petit
- Performance maximale
- Pas de débogage possible

**Configuration :**
1. Menu **Project** → **Project Options**
2. Section **Compiler Options**
3. Modes de build : **Debug** ou **Release**

## Bonnes pratiques d'organisation

### 1. Un dossier = un projet

**Toujours créer un dossier dédié pour chaque projet !**

✅ Bon :
```
MesProjets/
  ├── Calculatrice/
  │   └── ...tous les fichiers de la calculatrice...
  ├── JeuMemoire/
  │   └── ...tous les fichiers du jeu...
  └── GestionContacts/
      └── ...tous les fichiers de gestion...
```

❌ Mauvais :
```
MesProjets/
  ├── calculatrice.lpi
  ├── unit1.pas
  ├── jeu.lpi
  ├── unit1.pas  ← Conflit de noms !
  └── ...
```

### 2. Noms de fichiers clairs

**Donnez des noms descriptifs :**

✅ Bon :
- `FormPrincipal.pas` / `FormPrincipal.lfm`
- `FormOptions.pas` / `FormOptions.lfm`
- `UnitBaseDonnees.pas`
- `UnitCalculs.pas`

❌ Mauvais :
- `unit1.pas` / `unit1.lfm`
- `unit2.pas` / `unit2.lfm`
- `tmp.pas`

**Comment renommer :**
1. Fermez le projet dans Lazarus
2. Renommez les fichiers .pas ET .lfm ensemble
3. Éditez le .lpi pour corriger les références
4. Réouvrez le projet

**Plus simple :** Renommez dès la création !

### 3. Commentaires dans le code

**Documentez chaque unité :**

```pascal
unit UnitCalculs;

{*****************************************************************************
 * UnitCalculs.pas
 *
 * Description : Fonctions mathématiques pour l'application
 * Auteur : Votre Nom
 * Date création : 15/01/2025
 * Dernière modification : 15/01/2025
 *
 * Fonctions principales :
 *   - Additionner : addition de deux nombres
 *   - Multiplier : multiplication de deux nombres
 *   - Diviser : division avec gestion de la division par zéro
 ****************************************************************************}

{$mode objfpc}{$H+}

interface

// Reste du code...
```

### 4. Organiser avec des sous-dossiers (projets avancés)

Pour les grands projets :

```
MonGrosProjet/
  ├── MonGrosProjet.lpi
  ├── MonGrosProjet.lpr
  │
  ├── src/                    # Code source
  │   ├── forms/              # Tous les formulaires
  │   │   ├── FormPrincipal.*
  │   │   ├── FormOptions.*
  │   │   └── ...
  │   ├── units/              # Unités métier
  │   │   ├── UnitBaseDonnees.pas
  │   │   └── UnitCalculs.pas
  │   └── classes/            # Classes personnalisées
  │       └── ...
  │
  ├── resources/              # Ressources
  │   ├── images/
  │   ├── icons/
  │   └── data/
  │
  ├── docs/                   # Documentation
  │   └── README.md
  │
  └── lib/                    # Compilation
```

**Note :** Nécessite de configurer les chemins dans les options du projet.

### 5. Versioning avec Git (recommandé)

**Git permet de :**
- Sauvegarder l'historique des modifications
- Travailler en équipe
- Revenir à une version antérieure
- Gérer les branches de développement

**Fichiers à suivre :**
```bash
git add *.lpi *.lpr *.pas *.lfm *.res README.md
git commit -m "Version initiale"
```

**Fichiers à ignorer :** Voir le .gitignore plus haut.

## Conclusion

Vous comprenez maintenant en profondeur la structure d'un projet Lazarus ! Cette connaissance est fondamentale pour travailler efficacement.

**Ce que vous avez appris dans cette section :**
- ✅ Rôle et contenu de chaque type de fichier (.lpi, .lpr, .pas, .lfm, .lps, .res)
- ✅ Organisation des dossiers (lib/, backup/)
- ✅ Hiérarchie et dépendances entre fichiers
- ✅ Processus de compilation du code au programme
- ✅ Fichiers essentiels vs fichiers temporaires
- ✅ Bonnes pratiques d'organisation
- ✅ Gestion des dépendances (clause uses)
- ✅ Organisation de projets multi-formulaires

**Compétences acquises :**
- Savoir quels fichiers sauvegarder
- Comprendre les messages de compilation
- Organiser proprement vos projets
- Travailler avec plusieurs unités
- Nettoyer et maintenir un projet

**Points clés à retenir :**
- **Le .lpi est le cœur du projet** : c'est lui qu'on ouvre dans Lazarus
- **Le .lpr est le point d'entrée** : le programme commence ici
- **Les .pas contiennent votre code** : c'est là que vous travaillez
- **Les .lfm sont générés automatiquement** : ne les modifiez jamais manuellement
- **Le dossier lib/ peut être supprimé** : il sera recréé à la compilation
- **Un projet = un dossier dédié** : règle d'or de l'organisation

**Prochaines étapes :**
- Section 9.8 : Configuration avancée de Lazarus
- Section 9.9 : Maîtriser la compilation et l'exécution
- Section 9.10 : Utiliser l'aide et la documentation

**Conseil pratique :** Prenez le temps d'explorer vos propres projets avec ces nouvelles connaissances. Ouvrez les fichiers, regardez leur contenu, expérimentez !

---

**En résumé :**
- `.lpi` = configuration du projet (XML)
- `.lpr` = programme principal (code Pascal)
- `.pas` = unités de code (votre travail)
- `.lfm` = description des formulaires (généré automatiquement)
- `.lps` = session (optionnel, peut être ignoré)
- `.res` = ressources (icône, manifest...)
- `lib/` = compilation temporaire (peut être supprimé)
- `backup/` = sauvegardes automatiques (optionnel)

⏭️ [Compilation et exécution](/09-introduction-freepascal-lazarus/08-compilation-execution.md)
