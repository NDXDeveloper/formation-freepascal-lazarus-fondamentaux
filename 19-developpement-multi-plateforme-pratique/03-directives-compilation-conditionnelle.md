🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.3 Directives de compilation conditionnelle {$IFDEF}

## Introduction

Imaginez que vous écrivez un livre qui doit être publié en deux versions : une pour les enfants et une pour les adultes. Certains chapitres seront identiques, mais d'autres devront être adaptés selon le public. Les directives de compilation conditionnelle fonctionnent de la même manière : elles permettent d'inclure ou d'exclure du code selon la plateforme de compilation.

**La magie :** Le compilateur ne garde que le code pertinent pour la plateforme cible. Le code inutile n'est même pas compilé, ce qui rend vos exécutables plus petits et plus rapides.

---

## 1. Le Concept de Base

### Qu'est-ce qu'une Directive de Compilation ?

Une **directive de compilation** est une instruction spéciale qui commence par `{$` et se termine par `}`. Elle ne fait pas partie du code exécuté par votre programme, mais elle dit au **compilateur** quoi faire pendant la compilation.

```pascal
{$IFDEF WINDOWS}
  // Ce code sera compilé SEULEMENT sous Windows
{$ENDIF}
```

**Analogie :** C'est comme des notes dans la marge d'un livre destinées à l'imprimeur, pas au lecteur.

### Pourquoi C'est Utile ?

Sans directives conditionnelles, vous devriez créer deux versions différentes de votre programme :
- Une version pour Windows
- Une version pour Linux

Avec les directives, vous avez **un seul fichier source** qui s'adapte automatiquement !

---

## 2. Syntaxe de Base : {$IFDEF}

### Structure Simple

```pascal
{$IFDEF SYMBOLE}
  // Code à compiler si SYMBOLE est défini
{$ENDIF}
```

**Exemple concret :**

```pascal
program MonPremierIfdef;

begin
  WriteLn('Ce message apparaît toujours');

  {$IFDEF WINDOWS}
  WriteLn('Vous êtes sous Windows !');
  {$ENDIF}

  {$IFDEF LINUX}
  WriteLn('Vous êtes sous Linux !');
  {$ENDIF}

  WriteLn('Fin du programme');
end.
```

**Résultat :**
- **Compilé sous Windows :**
  ```
  Ce message apparaît toujours
  Vous êtes sous Windows !
  Fin du programme
  ```

- **Compilé sous Linux :**
  ```
  Ce message apparaît toujours
  Vous êtes sous Linux !
  Fin du programme
  ```

### Avec Alternative : {$ELSE}

```pascal
{$IFDEF SYMBOLE}
  // Code si SYMBOLE est défini
{$ELSE}
  // Code si SYMBOLE n'est PAS défini
{$ENDIF}
```

**Exemple :**

```pascal
program AvecElse;

var
  Separateur: string;
begin
  {$IFDEF WINDOWS}
  Separateur := '\';
  WriteLn('Système : Windows');
  {$ELSE}
  Separateur := '/';
  WriteLn('Système : Unix/Linux');
  {$ENDIF}

  WriteLn('Séparateur : ', Separateur);
end.
```

### Négation : {$IFNDEF}

**IFNDEF = IF NOT DEFined** (si NON défini)

```pascal
{$IFNDEF SYMBOLE}
  // Code si SYMBOLE n'est PAS défini
{$ENDIF}
```

**Exemple :**

```pascal
program AvecIfndef;

begin
  {$IFNDEF DEBUG}
  WriteLn('Mode production - optimisations activées');
  {$ENDIF}

  {$IFDEF DEBUG}
  WriteLn('Mode debug - informations de débogage incluses');
  {$ENDIF}
end.
```

---

## 3. Symboles Prédéfinis par FreePascal

FreePascal définit automatiquement plusieurs symboles selon la plateforme de compilation. Vous n'avez rien à faire, ils sont toujours disponibles !

### Symboles de Système d'Exploitation

| Symbole | Signification | Quand est-il défini ? |
|---------|---------------|----------------------|
| `WINDOWS` | Microsoft Windows | Compilation pour Windows (toutes versions) |
| `LINUX` | Linux | Compilation pour Linux |
| `UNIX` | Unix-like | Linux, macOS, FreeBSD, etc. |
| `DARWIN` | macOS | Compilation pour macOS |
| `FREEBSD` | FreeBSD | Compilation pour FreeBSD |
| `ANDROID` | Android | Compilation pour Android |

**Exemple pratique :**

```pascal
program DetectionOS;

begin
  WriteLn('Système d''exploitation détecté :');

  {$IFDEF WINDOWS}
  WriteLn('  - Windows');
  {$ENDIF}

  {$IFDEF LINUX}
  WriteLn('  - Linux');
  {$ENDIF}

  {$IFDEF DARWIN}
  WriteLn('  - macOS');
  {$ENDIF}

  {$IFDEF UNIX}
  WriteLn('  - Système de type Unix');
  {$ENDIF}
end.
```

**Note importante :** Sous Linux, DEUX symboles sont définis : `LINUX` ET `UNIX`. Sous macOS, `DARWIN` ET `UNIX` sont définis.

### Symboles d'Architecture

| Symbole | Architecture |
|---------|-------------|
| `CPU32` | Processeur 32 bits |
| `CPU64` | Processeur 64 bits |
| `CPUI386` | Intel x86 32 bits |
| `CPUX86_64` | Intel/AMD x64 64 bits |
| `CPUARM` | ARM (mobiles, Raspberry Pi) |

**Exemple :**

```pascal
program DetectionArchitecture;

begin
  {$IFDEF CPU64}
  WriteLn('Version 64 bits');
  {$ELSE}
  WriteLn('Version 32 bits');
  {$ENDIF}

  {$IFDEF CPUARM}
  WriteLn('Processeur ARM détecté (ex: Raspberry Pi)');
  {$ENDIF}
end.
```

### Autres Symboles Utiles

| Symbole | Description |
|---------|-------------|
| `FPC` | Toujours défini (Free Pascal Compiler) |
| `DEBUG` | Mode debug (si compilé avec `-g`) |
| `RELEASE` | Mode release (si compilé avec `-O2` ou plus) |
| `VER3_2_2` | Version de FPC (exemple : 3.2.2) |

---

## 4. Cas d'Usage Pratiques

### Cas 1 : Chemins Système Différents

```pascal
program GestionChemin;

uses
  SysUtils;

function ObtenirRepertoireConfig: string;  
begin  
  {$IFDEF WINDOWS}
  // Windows : AppData\Roaming
  Result := GetEnvironmentVariable('APPDATA') + PathDelim + 'MonApp';
  {$ENDIF}

  {$IFDEF LINUX}
  // Linux : ~/.config
  Result := GetEnvironmentVariable('HOME') + PathDelim + '.config' + PathDelim + 'MonApp';
  {$ENDIF}

  {$IFDEF DARWIN}
  // macOS : ~/Library/Application Support
  Result := GetEnvironmentVariable('HOME') + PathDelim + 'Library' + PathDelim +
            'Application Support' + PathDelim + 'MonApp';
  {$ENDIF}

  // Créer le répertoire s'il n'existe pas
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

begin
  WriteLn('Répertoire de configuration : ', ObtenirRepertoireConfig);
end.
```

### Cas 2 : Commandes Système Différentes

```pascal
program ExecutionCommande;

uses
  Process, SysUtils;

procedure ViderEcran;  
var  
  Proc: TProcess;
begin
  Proc := TProcess.Create(nil);
  try
    {$IFDEF WINDOWS}
    Proc.Executable := 'cmd.exe';
    Proc.Parameters.Add('/c');
    Proc.Parameters.Add('cls');
    {$ENDIF}

    {$IFDEF UNIX}
    Proc.Executable := '/usr/bin/clear';
    {$ENDIF}

    Proc.Execute;
    Proc.WaitOnExit;
  finally
    Proc.Free;
  end;
end;

begin
  WriteLn('Avant le nettoyage');
  Sleep(2000);
  ViderEcran;
  WriteLn('Après le nettoyage');
end.
```

### Cas 3 : Bibliothèques Externes

```pascal
program ChargementBibliotheque;

uses
  DynLibs;

const
  {$IFDEF WINDOWS}
  NomBibliotheque = 'sqlite3.dll';
  {$ENDIF}

  {$IFDEF LINUX}
  NomBibliotheque = 'libsqlite3.so';
  {$ENDIF}

  {$IFDEF DARWIN}
  NomBibliotheque = 'libsqlite3.dylib';
  {$ENDIF}

var
  Handle: TLibHandle;
begin
  WriteLn('Tentative de chargement de : ', NomBibliotheque);

  Handle := LoadLibrary(NomBibliotheque);
  if Handle <> 0 then
  begin
    WriteLn('Bibliothèque chargée avec succès !');
    UnloadLibrary(Handle);
  end
  else
    WriteLn('Échec du chargement de la bibliothèque');
end.
```

### Cas 4 : Mode Debug vs Production

```pascal
program ModeDebug;

procedure Log(const Message: string);  
begin  
  {$IFDEF DEBUG}
  // En mode debug, afficher tous les messages
  WriteLn('[LOG] ', Message);
  {$ENDIF}

  // En mode production (release), ne rien faire
  // Le code n'est même pas compilé !
end;

procedure FonctionComplexe;  
begin  
  Log('Début de la fonction');

  // ... traitement ...

  Log('Fin de la fonction');
end;

begin
  Log('Démarrage du programme');
  FonctionComplexe;
  Log('Arrêt du programme');
end.
```

---

## 5. Conditions Multiples et Imbriquées

### Conditions Multiples : {$IF}

FreePascal supporte aussi `{$IF}` avec des expressions booléennes plus complexes :

```pascal
program ConditionsAvancees;

begin
  {$IF DEFINED(WINDOWS) AND DEFINED(CPU64)}
  WriteLn('Windows 64 bits');
  {$ELSEIF DEFINED(WINDOWS) AND DEFINED(CPU32)}
  WriteLn('Windows 32 bits');
  {$ELSEIF DEFINED(LINUX)}
  WriteLn('Linux');
  {$ELSE}
  WriteLn('Autre système');
  {$ENDIF}
end.
```

**Opérateurs disponibles :**
- `DEFINED(symbole)` : vérifie si un symbole est défini
- `AND` : et logique
- `OR` : ou logique
- `NOT` : négation
- `=`, `<>`, `<`, `>`, `<=`, `>=` : comparaisons

### Conditions Imbriquées

```pascal
program ConditionsImbriquees;

begin
  {$IFDEF WINDOWS}
    WriteLn('Système : Windows');

    {$IFDEF CPU64}
    WriteLn('Architecture : 64 bits');
    {$ELSE}
    WriteLn('Architecture : 32 bits');
    {$ENDIF}

  {$ELSE}
    WriteLn('Système : Non-Windows');

    {$IFDEF LINUX}
    WriteLn('Précisément : Linux');
    {$ENDIF}

  {$ENDIF}
end.
```

**Attention à la lisibilité !** Trop d'imbrication rend le code difficile à lire. Préférez des fonctions séparées.

---

## 6. Définir Vos Propres Symboles

### Avec {$DEFINE}

Vous pouvez créer vos propres symboles pour contrôler la compilation :

```pascal
program SymbolesPersonnalises;

// Définir un symbole
{$DEFINE VERSION_DEMO}
{$DEFINE AFFICHAGE_COULEUR}

begin
  WriteLn('Mon Application');

  {$IFDEF VERSION_DEMO}
  WriteLn('VERSION DÉMO - Fonctionnalités limitées');
  WriteLn('Achetez la version complète sur notre site !');
  {$ELSE}
  WriteLn('Version complète enregistrée');
  {$ENDIF}

  {$IFDEF AFFICHAGE_COULEUR}
  WriteLn('Mode couleur activé');
  {$ENDIF}
end.
```

### Annuler un Symbole : {$UNDEF}

```pascal
program UndefExample;

{$DEFINE MA_FONCTIONNALITE}

begin
  {$IFDEF MA_FONCTIONNALITE}
  WriteLn('Fonctionnalité activée');
  {$ENDIF}

  // Désactiver le symbole
  {$UNDEF MA_FONCTIONNALITE}

  {$IFDEF MA_FONCTIONNALITE}
  WriteLn('Cette ligne ne sera jamais affichée');
  {$ENDIF}
end.
```

### Définir via la Ligne de Commande

Vous pouvez aussi définir des symboles lors de la compilation :

```bash
# Compiler avec un symbole personnalisé
fpc -dDEBUG -dVERSION_PRO MonProgramme.pas

# Équivalent à avoir {$DEFINE DEBUG} et {$DEFINE VERSION_PRO} dans le code
```

**Dans Lazarus :**
1. Projet → Options du projet
2. Options du compilateur
3. Autres → Définitions personnalisées
4. Ajouter vos symboles (ex: `DEBUG;VERSION_PRO`)

---

## 7. Inclusion Conditionnelle de Fichiers

### {$I} ou {$INCLUDE}

Vous pouvez inclure des fichiers différents selon la plateforme :

```pascal
program InclusionConditionnelle;

{$IFDEF WINDOWS}
  {$I windows_specifique.inc}
{$ENDIF}

{$IFDEF LINUX}
  {$I linux_specifique.inc}
{$ENDIF}

begin
  // Le code des fichiers .inc est inséré ici
  FonctionSpecifique;
end.
```

**Fichier `windows_specifique.inc` :**
```pascal
procedure FonctionSpecifique;  
begin  
  WriteLn('Code spécifique Windows');
end;
```

**Fichier `linux_specifique.inc` :**
```pascal
procedure FonctionSpecifique;  
begin  
  WriteLn('Code spécifique Linux');
end;
```

---

## 8. Bonnes Pratiques

### ✅ 1. Minimiser le Code Conditionnel

**Mauvais :** Code conditionnel partout

```pascal
procedure SauvegarderFichier(const Nom: string);  
var  
  Chemin: string;
begin
  {$IFDEF WINDOWS}
  Chemin := 'C:\Data\' + Nom;
  {$ELSE}
  Chemin := '/home/user/data/' + Nom;
  {$ENDIF}

  // ... sauvegarde ...
end;
```

**Bon :** Isoler les différences dans des fonctions dédiées

```pascal
function ObtenirRepertoireDonnees: string;  
begin  
  {$IFDEF WINDOWS}
  Result := 'C:\Data\';
  {$ELSE}
  Result := '/home/user/data/';
  {$ENDIF}
end;

procedure SauvegarderFichier(const Nom: string);  
var  
  Chemin: string;
begin
  Chemin := ObtenirRepertoireDonnees + Nom;
  // ... sauvegarde ...
end;
```

### ✅ 2. Toujours Prévoir un {$ELSE}

**Mauvais :**

```pascal
{$IFDEF WINDOWS}
NomBib := 'sqlite3.dll';
{$ENDIF}

{$IFDEF LINUX}
NomBib := 'libsqlite3.so';
{$ENDIF}

// Et si on compile pour macOS ? NomBib n'est pas défini !
```

**Bon :**

```pascal
{$IFDEF WINDOWS}
NomBib := 'sqlite3.dll';
{$ELSE}
  {$IFDEF LINUX}
  NomBib := 'libsqlite3.so';
  {$ELSE}
    {$IFDEF DARWIN}
    NomBib := 'libsqlite3.dylib';
    {$ELSE}
    {$ERROR Plateforme non supportée}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
```

### ✅ 3. Utiliser {$ERROR} et {$WARNING}

**Générer une erreur de compilation :**

```pascal
{$IFNDEF WINDOWS}
  {$IFNDEF LINUX}
    {$ERROR Ce programme nécessite Windows ou Linux}
  {$ENDIF}
{$ENDIF}
```

**Générer un avertissement :**

```pascal
{$IFDEF CPU32}
  {$WARNING Cette application fonctionne mieux en 64 bits}
{$ENDIF}
```

### ✅ 4. Commenter Vos Conditions

```pascal
{$IFDEF WINDOWS}
  // Windows utilise CRLF pour les fins de lignes
  FinLigne := #13#10;
{$ELSE}
  // Unix/Linux utilisent seulement LF
  FinLigne := #10;
{$ENDIF}
```

### ✅ 5. Tester Toutes les Plateformes

Même avec des directives conditionnelles, testez régulièrement votre code sur toutes les plateformes cibles !

---

## 9. Pièges à Éviter

### ❌ Piège 1 : Oublier {$ENDIF}

```pascal
{$IFDEF WINDOWS}
  WriteLn('Windows');
// ERREUR : manque {$ENDIF}

begin
  // Le compilateur sera perdu !
end.
```

**Solution :** Toujours fermer vos `{$IFDEF}` avec `{$ENDIF}`. Indentez le code pour voir la structure.

### ❌ Piège 2 : Confondre IFDEF et IF

```pascal
// FAUX - IF est pour les conditions à l'exécution
{$IF WINDOWS}  // Erreur de syntaxe !

// CORRECT - IFDEF pour la compilation conditionnelle
{$IFDEF WINDOWS}
```

Pour les conditions complexes, utilisez `{$IF DEFINED(...)}` :

```pascal
{$IF DEFINED(WINDOWS) OR DEFINED(DARWIN)}
```

### ❌ Piège 3 : Code Mort Non Détecté

```pascal
{$DEFINE NOUVELLE_VERSION}

procedure AncienCode;  
begin  
  {$IFNDEF NOUVELLE_VERSION}
  // Ce code ne sera JAMAIS compilé
  // Difficile de savoir s'il fonctionne encore !
  {$ENDIF}
end;
```

**Solution :** Supprimez régulièrement le code mort au lieu de le désactiver indéfiniment.

### ❌ Piège 4 : Trop de Symboles Personnalisés

```pascal
{$DEFINE FEATURE_A}
{$DEFINE FEATURE_B}
{$DEFINE FEATURE_C}
{$DEFINE MODE_DEMO}
{$DEFINE DEBUG_NIVEAU_2}
// ... 20 autres symboles ...

// Le code devient ingérable !
```

**Solution :** Utilisez un système de configuration à l'exécution pour les fonctionnalités optionnelles.

---

## 10. Exemple Complet : Application Multi-Plateforme

```pascal
program AppliMultiPlateforme;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes;

// Définir la version
{$DEFINE VERSION_1_5}

// Configuration selon la plateforme
{$IFDEF WINDOWS}
  {$APPTYPE CONSOLE}
  {$R *.res}
const
  NomPlateforme = 'Windows';
  ExtensionExe = '.exe';
{$ENDIF}

{$IFDEF LINUX}
const
  NomPlateforme = 'Linux';
  ExtensionExe = '';
{$ENDIF}

{$IFDEF DARWIN}
const
  NomPlateforme = 'macOS';
  ExtensionExe = '';
{$ENDIF}

// Fonction pour obtenir le répertoire de configuration
function ObtenirDirConfig: string;  
begin  
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('APPDATA') + PathDelim + 'MonApp';
  {$ENDIF}

  {$IFDEF LINUX}
  Result := GetEnvironmentVariable('HOME') + PathDelim + '.config' + PathDelim + 'MonApp';
  {$ENDIF}

  {$IFDEF DARWIN}
  Result := GetEnvironmentVariable('HOME') + PathDelim +
            'Library' + PathDelim + 'Application Support' + PathDelim + 'MonApp';
  {$ENDIF}

  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

// Fonction de log conditionnelle
procedure Log(const Message: string);  
begin  
  {$IFDEF DEBUG}
  WriteLn('[', FormatDateTime('hh:nn:ss', Now), '] ', Message);
  {$ENDIF}
end;

// Programme principal
var
  DirConfig: string;
begin
  WriteLn('=================================');
  WriteLn('Application Multi-Plateforme');

  {$IFDEF VERSION_1_5}
  WriteLn('Version 1.5');
  {$ELSE}
  WriteLn('Version 1.0');
  {$ENDIF}

  WriteLn('=================================');
  WriteLn;

  WriteLn('Plateforme : ', NomPlateforme);

  {$IFDEF CPU64}
  WriteLn('Architecture : 64 bits');
  {$ELSE}
  WriteLn('Architecture : 32 bits');
  {$ENDIF}

  WriteLn;

  DirConfig := ObtenirDirConfig;
  WriteLn('Répertoire de configuration :');
  WriteLn('  ', DirConfig);

  Log('Programme initialisé');

  // ... logique de l'application ...

  Log('Programme terminé');

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

**Compilation :**

```bash
# Windows (Debug)
fpc -dDEBUG AppliMultiPlateforme.pas

# Linux (Release)
fpc -O2 AppliMultiPlateforme.pas

# Avec symbole personnalisé
fpc -dDEBUG -dVERSION_PRO AppliMultiPlateforme.pas
```

---

## 11. Directives Additionnelles Utiles

### Contrôle d'Inclusion

```pascal
{$IFOPT O+}
// Code si les optimisations sont activées
{$ENDIF}

{$IFDEF FPC_FULLVERSION}
  {$IF FPC_FULLVERSION >= 30200}
  // Code nécessitant FreePascal 3.2.0 ou plus
  {$ENDIF}
{$ENDIF}
```

### Messages de Compilation

```pascal
{$MESSAGE 'Compilation pour Windows'}
{$HINT 'Pensez à tester sous Linux aussi'}
{$NOTE 'Version expérimentale'}
```

### Désactiver Warnings Spécifiques

```pascal
{$WARN SYMBOL_DEPRECATED OFF}
// Utiliser une fonction dépréciée sans warning
AncienneFonction;
{$WARN SYMBOL_DEPRECATED ON}
```

---

## Résumé

Les directives de compilation conditionnelle vous permettent de :

1. **Écrire un seul code source** pour plusieurs plateformes
2. **Adapter le comportement** selon Windows, Linux, macOS, etc.
3. **Contrôler les fonctionnalités** (version demo/pro, debug/release)
4. **Optimiser la compilation** (seul le code nécessaire est inclus)

**Règles d'or :**
- Minimisez le code conditionnel
- Isolez les différences dans des fonctions dédiées
- Testez sur toutes les plateformes cibles
- Commentez vos conditions
- Utilisez `{$ERROR}` pour les cas non supportés

Avec ces outils, vous êtes maintenant capable de créer de vraies applications multi-plateformes professionnelles !

Dans le prochain chapitre, nous verrons comment configurer Lazarus pour la cross-compilation et tester efficacement sur différentes plateformes.

⏭️ [Unités spécifiques par plateforme](/19-developpement-multi-plateforme-pratique/04-unites-specifiques-plateforme.md)
