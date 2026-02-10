🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.8 Fichiers INI pour configuration

## Introduction

Les **fichiers INI** (Initialization files) sont un format simple et lisible pour stocker des paramètres de configuration d'applications. Ils sont parfaits pour sauvegarder les préférences utilisateur, les paramètres d'applications, et autres données de configuration.

**Analogie :**
Un fichier INI, c'est comme un carnet de notes organisé par thèmes :
- **Sections** = chapitres du carnet (entre crochets)
- **Clés** = questions
- **Valeurs** = réponses

```ini
[Général]
Langue=Français  
Theme=Sombre  

[Fenêtre]
Largeur=1024  
Hauteur=768  
```

---

## Qu'est-ce qu'un fichier INI ?

### Structure d'un fichier INI

Un fichier INI est un fichier texte organisé en **sections**, chacune contenant des paires **clé=valeur**.

**Exemple de fichier `config.ini` :**
```ini
; Ceci est un commentaire
; Les commentaires commencent par ; ou #

[Application]
Nom=MonApplication  
Version=1.0.5  
Auteur=Jean Dupont  

[Interface]
Langue=Français  
Theme=Clair  
TaillePolice=12  

[Fenêtre]
Largeur=1024  
Hauteur=768  
Maximisee=false  
PosX=100  
PosY=50  

[Connexion]
Serveur=192.168.1.100  
Port=8080  
Timeout=30  
```

### Syntaxe et règles

**Sections :**
- Délimitées par des crochets : `[NomSection]`
- Organisent les paramètres par catégories
- Les noms ne sont généralement pas sensibles à la casse

**Clés et valeurs :**
- Format : `Clé=Valeur`
- Pas d'espaces obligatoires autour du `=`
- Les valeurs peuvent être des nombres, du texte, des booléens

**Commentaires :**
- Commencent par `;` ou `#`
- Ignorés lors de la lecture
- Utiles pour documenter la configuration

**Types de valeurs supportées :**
- **Chaînes** : `Nom=Jean Dupont`
- **Nombres entiers** : `Port=8080`
- **Nombres décimaux** : `Prix=19.99`
- **Booléens** : `Actif=true` ou `Actif=1`

---

## Pourquoi utiliser des fichiers INI ?

### Avantages

✅ **Lisible** : format texte, facile à éditer manuellement

✅ **Simple** : structure intuitive (sections, clés, valeurs)

✅ **Portable** : fonctionne sur tous les systèmes

✅ **Léger** : pas de dépendances complexes

✅ **Standardisé** : format bien connu et supporté

✅ **Éditable** : l'utilisateur peut modifier avec un éditeur de texte

### Inconvénients

❌ **Limité** : pas de structure hiérarchique complexe

❌ **Pas typé** : tout est stocké comme texte

❌ **Pas de validation** : aucune vérification de format

❌ **Sensible aux erreurs** : une faute de frappe peut causer des problèmes

### Quand utiliser des fichiers INI ?

**Utilisez des fichiers INI pour :**
- Paramètres de configuration simples
- Préférences utilisateur
- Options d'applications
- Connexions à des serveurs/bases de données

**N'utilisez PAS de fichiers INI pour :**
- Structures de données complexes → utilisez JSON ou XML
- Grandes quantités de données → utilisez une base de données
- Données sensibles (mots de passe) → utilisez un stockage sécurisé

---

## Unités nécessaires

Pour manipuler les fichiers INI en Pascal, utilisez l'unité `IniFiles` :

```pascal
uses
  IniFiles, SysUtils;
```

---

## Classe TIniFile : Manipulation de base

Pascal fournit la classe **TIniFile** pour lire et écrire facilement dans les fichiers INI.

### Créer un objet TIniFile

```pascal
uses
  IniFiles, SysUtils;

var
  IniFile: TIniFile;

begin
  // Créer l'objet (ouvre ou crée le fichier)
  IniFile := TIniFile.Create('config.ini');

  try
    // Utiliser l'objet ici

  finally
    IniFile.Free;  // Toujours libérer !
  end;
end.
```

**Important :** Toujours utiliser `try-finally` pour garantir que l'objet est libéré.

---

## Lire des valeurs

TIniFile offre plusieurs méthodes pour lire différents types de données.

### ReadString : Lire une chaîne

```pascal
uses
  IniFiles, SysUtils;

var
  IniFile: TIniFile;
  Langue: string;

begin
  IniFile := TIniFile.Create('config.ini');
  try
    // Lire : ReadString(Section, Clé, ValeurParDéfaut)
    Langue := IniFile.ReadString('Interface', 'Langue', 'Français');
    WriteLn('Langue : ', Langue);
  finally
    IniFile.Free;
  end;
end.
```

**Paramètres :**
- `Section` : nom de la section
- `Clé` : nom de la clé
- `ValeurParDéfaut` : valeur retournée si la clé n'existe pas

### ReadInteger : Lire un entier

```pascal
var
  IniFile: TIniFile;
  Port: Integer;

begin
  IniFile := TIniFile.Create('config.ini');
  try
    Port := IniFile.ReadInteger('Connexion', 'Port', 8080);
    WriteLn('Port : ', Port);
  finally
    IniFile.Free;
  end;
end.
```

### ReadBool : Lire un booléen

```pascal
var
  IniFile: TIniFile;
  Maximisee: Boolean;

begin
  IniFile := TIniFile.Create('config.ini');
  try
    Maximisee := IniFile.ReadBool('Fenêtre', 'Maximisee', False);
    WriteLn('Maximisée : ', Maximisee);
  finally
    IniFile.Free;
  end;
end.
```

**Note :** Accepte `true/false`, `1/0`, `yes/no` comme valeurs booléennes.

### ReadFloat : Lire un nombre décimal

```pascal
var
  IniFile: TIniFile;
  Prix: Double;

begin
  IniFile := TIniFile.Create('config.ini');
  try
    Prix := IniFile.ReadFloat('Produit', 'Prix', 0.0);
    WriteLn('Prix : ', Prix:0:2);
  finally
    IniFile.Free;
  end;
end.
```

### ReadDate, ReadTime, ReadDateTime : Lire des dates/heures

```pascal
var
  IniFile: TIniFile;
  DerniereUtilisation: TDateTime;

begin
  IniFile := TIniFile.Create('config.ini');
  try
    DerniereUtilisation := IniFile.ReadDateTime('Application', 'DerniereUtilisation', Now);
    WriteLn('Dernière utilisation : ', DateTimeToStr(DerniereUtilisation));
  finally
    IniFile.Free;
  end;
end.
```

---

## Écrire des valeurs

### WriteString : Écrire une chaîne

```pascal
var
  IniFile: TIniFile;

begin
  IniFile := TIniFile.Create('config.ini');
  try
    IniFile.WriteString('Interface', 'Langue', 'Anglais');
    WriteLn('Langue mise à jour');
  finally
    IniFile.Free;
  end;
end.
```

### WriteInteger : Écrire un entier

```pascal
var
  IniFile: TIniFile;

begin
  IniFile := TIniFile.Create('config.ini');
  try
    IniFile.WriteInteger('Connexion', 'Port', 9000);
    WriteLn('Port mis à jour');
  finally
    IniFile.Free;
  end;
end.
```

### WriteBool : Écrire un booléen

```pascal
var
  IniFile: TIniFile;

begin
  IniFile := TIniFile.Create('config.ini');
  try
    IniFile.WriteBool('Fenêtre', 'Maximisee', True);
    WriteLn('État maximisé enregistré');
  finally
    IniFile.Free;
  end;
end.
```

### WriteFloat, WriteDate, WriteTime, WriteDateTime

```pascal
var
  IniFile: TIniFile;

begin
  IniFile := TIniFile.Create('config.ini');
  try
    IniFile.WriteFloat('Produit', 'Prix', 29.99);
    IniFile.WriteDateTime('Application', 'DerniereUtilisation', Now);
    WriteLn('Valeurs enregistrées');
  finally
    IniFile.Free;
  end;
end.
```

---

## Opérations sur les sections et clés

### Lister toutes les sections

```pascal
uses
  IniFiles, SysUtils, Classes;

var
  IniFile: TIniFile;
  Sections: TStringList;
  i: Integer;

begin
  IniFile := TIniFile.Create('config.ini');
  Sections := TStringList.Create;

  try
    IniFile.ReadSections(Sections);

    WriteLn('Sections du fichier INI :');
    for i := 0 to Sections.Count - 1 do
      WriteLn('  [', Sections[i], ']');

  finally
    Sections.Free;
    IniFile.Free;
  end;
end.
```

### Lister toutes les clés d'une section

```pascal
uses
  IniFiles, SysUtils, Classes;

var
  IniFile: TIniFile;
  Cles: TStringList;
  i: Integer;

begin
  IniFile := TIniFile.Create('config.ini');
  Cles := TStringList.Create;

  try
    IniFile.ReadSection('Interface', Cles);

    WriteLn('Clés de la section [Interface] :');
    for i := 0 to Cles.Count - 1 do
      WriteLn('  ', Cles[i]);

  finally
    Cles.Free;
    IniFile.Free;
  end;
end.
```

### Lire toutes les valeurs d'une section

```pascal
uses
  IniFiles, SysUtils, Classes;

var
  IniFile: TIniFile;
  Valeurs: TStringList;
  i: Integer;

begin
  IniFile := TIniFile.Create('config.ini');
  Valeurs := TStringList.Create;

  try
    IniFile.ReadSectionValues('Interface', Valeurs);

    WriteLn('Contenu de [Interface] :');
    for i := 0 to Valeurs.Count - 1 do
      WriteLn('  ', Valeurs[i]);  // Format : Clé=Valeur

  finally
    Valeurs.Free;
    IniFile.Free;
  end;
end.
```

### Vérifier l'existence

```pascal
var
  IniFile: TIniFile;

begin
  IniFile := TIniFile.Create('config.ini');
  try
    // Vérifier si une section existe
    if IniFile.SectionExists('Interface') then
      WriteLn('La section [Interface] existe');

    // Vérifier si une clé existe
    if IniFile.ValueExists('Interface', 'Langue') then
      WriteLn('La clé "Langue" existe dans [Interface]');

  finally
    IniFile.Free;
  end;
end.
```

### Supprimer des éléments

```pascal
var
  IniFile: TIniFile;

begin
  IniFile := TIniFile.Create('config.ini');
  try
    // Supprimer une clé
    IniFile.DeleteKey('Interface', 'Theme');

    // Supprimer une section entière
    IniFile.EraseSection('Obsolete');

    WriteLn('Éléments supprimés');
  finally
    IniFile.Free;
  end;
end.
```

---

## Exemple complet : Gestion des préférences

Créons une application qui charge et sauvegarde ses préférences.

```pascal
{$mode objfpc}{$H+}
program GestionPreferences;

uses
  IniFiles, SysUtils;

type
  TPreferences = record
    Langue: string;
    Theme: string;
    TaillePolice: Integer;
    FenetreMaximisee: Boolean;
    FenetreLargeur: Integer;
    FenetreHauteur: Integer;
    DernierFichierOuvert: string;
  end;

var
  Prefs: TPreferences;

procedure ChargerPreferences(var Prefs: TPreferences);  
var  
  IniFile: TIniFile;
  CheminIni: string;
begin
  // Créer le fichier INI dans le répertoire de l'application
  CheminIni := ExtractFilePath(ParamStr(0)) + 'config.ini';

  IniFile := TIniFile.Create(CheminIni);
  try
    WriteLn('Chargement des préférences depuis : ', CheminIni);
    WriteLn;

    // Section Interface
    Prefs.Langue := IniFile.ReadString('Interface', 'Langue', 'Français');
    Prefs.Theme := IniFile.ReadString('Interface', 'Theme', 'Clair');
    Prefs.TaillePolice := IniFile.ReadInteger('Interface', 'TaillePolice', 11);

    // Section Fenêtre
    Prefs.FenetreMaximisee := IniFile.ReadBool('Fenetre', 'Maximisee', False);
    Prefs.FenetreLargeur := IniFile.ReadInteger('Fenetre', 'Largeur', 1024);
    Prefs.FenetreHauteur := IniFile.ReadInteger('Fenetre', 'Hauteur', 768);

    // Section Fichiers
    Prefs.DernierFichierOuvert := IniFile.ReadString('Fichiers', 'DernierOuvert', '');

    WriteLn('Préférences chargées avec succès');
  finally
    IniFile.Free;
  end;
end;

procedure SauvegarderPreferences(const Prefs: TPreferences);  
var  
  IniFile: TIniFile;
  CheminIni: string;
begin
  CheminIni := ExtractFilePath(ParamStr(0)) + 'config.ini';

  IniFile := TIniFile.Create(CheminIni);
  try
    WriteLn('Sauvegarde des préférences...');

    // Section Interface
    IniFile.WriteString('Interface', 'Langue', Prefs.Langue);
    IniFile.WriteString('Interface', 'Theme', Prefs.Theme);
    IniFile.WriteInteger('Interface', 'TaillePolice', Prefs.TaillePolice);

    // Section Fenêtre
    IniFile.WriteBool('Fenetre', 'Maximisee', Prefs.FenetreMaximisee);
    IniFile.WriteInteger('Fenetre', 'Largeur', Prefs.FenetreLargeur);
    IniFile.WriteInteger('Fenetre', 'Hauteur', Prefs.FenetreHauteur);

    // Section Fichiers
    IniFile.WriteString('Fichiers', 'DernierOuvert', Prefs.DernierFichierOuvert);

    WriteLn('Préférences sauvegardées avec succès');
  finally
    IniFile.Free;
  end;
end;

procedure AfficherPreferences(const Prefs: TPreferences);  
begin  
  WriteLn;
  WriteLn('=== PRÉFÉRENCES ACTUELLES ===');
  WriteLn('Langue          : ', Prefs.Langue);
  WriteLn('Thème           : ', Prefs.Theme);
  WriteLn('Taille police   : ', Prefs.TaillePolice);
  WriteLn('Fenêtre maximisée : ', Prefs.FenetreMaximisee);
  WriteLn('Largeur         : ', Prefs.FenetreLargeur);
  WriteLn('Hauteur         : ', Prefs.FenetreHauteur);
  WriteLn('Dernier fichier : ', Prefs.DernierFichierOuvert);
  WriteLn('============================');
end;

procedure ModifierPreferences(var Prefs: TPreferences);  
var  
  Choix: Integer;
begin
  WriteLn;
  WriteLn('Que voulez-vous modifier ?');
  WriteLn('1. Langue');
  WriteLn('2. Thème');
  WriteLn('3. Taille de police');
  WriteLn('0. Retour');
  Write('Choix : ');
  ReadLn(Choix);

  case Choix of
    1: begin
         Write('Nouvelle langue : ');
         ReadLn(Prefs.Langue);
       end;
    2: begin
         Write('Nouveau thème : ');
         ReadLn(Prefs.Theme);
       end;
    3: begin
         Write('Nouvelle taille de police : ');
         ReadLn(Prefs.TaillePolice);
       end;
  end;
end;

var
  Choix: Integer;

begin
  WriteLn('=================================');
  WriteLn('  GESTIONNAIRE DE PRÉFÉRENCES   ');
  WriteLn('=================================');

  ChargerPreferences(Prefs);

  repeat
    WriteLn;
    WriteLn('1. Afficher les préférences');
    WriteLn('2. Modifier les préférences');
    WriteLn('3. Sauvegarder');
    WriteLn('0. Quitter');
    Write('Choix : ');
    ReadLn(Choix);

    case Choix of
      1: AfficherPreferences(Prefs);
      2: ModifierPreferences(Prefs);
      3: SauvegarderPreferences(Prefs);
      0: begin
           WriteLn('Sauvegarde automatique avant de quitter...');
           SauvegarderPreferences(Prefs);
           WriteLn('Au revoir !');
         end;
    else
      WriteLn('Choix invalide !');
    end;

  until Choix = 0;
end.
```

---

## TMemIniFile : Fichier INI en mémoire

La classe **TMemIniFile** charge tout le fichier en mémoire, ce qui est plus rapide pour de nombreuses opérations.

### Différences avec TIniFile

| Caractéristique | TIniFile | TMemIniFile |
|-----------------|----------|-------------|
| **Stockage** | Disque (direct) | Mémoire (cache) |
| **Vitesse lecture** | Plus lent | Très rapide |
| **Vitesse écriture** | Immédiate | Différée |
| **Usage mémoire** | Faible | Plus élevé |
| **Sauvegarde** | Automatique | Manuelle (UpdateFile) |

### Utilisation de TMemIniFile

```pascal
uses
  IniFiles, SysUtils;

var
  IniFile: TMemIniFile;

begin
  // Créer et charger en mémoire
  IniFile := TMemIniFile.Create('config.ini');

  try
    // Lire (très rapide, depuis la mémoire)
    WriteLn('Langue : ', IniFile.ReadString('Interface', 'Langue', 'Français'));

    // Écrire (en mémoire seulement)
    IniFile.WriteString('Interface', 'Theme', 'Sombre');

    // Sauvegarder sur le disque (important !)
    IniFile.UpdateFile;

    WriteLn('Fichier mis à jour');
  finally
    IniFile.Free;  // Sauvegarde automatique lors de Free
  end;
end.
```

**Important :**
- `UpdateFile` est nécessaire pour écrire les modifications sur le disque
- `Free` appelle automatiquement `UpdateFile`

### Quand utiliser TMemIniFile ?

**Utilisez TMemIniFile quand :**
- Vous faites beaucoup de lectures
- Vous lisez/écrivez plusieurs valeurs d'un coup
- Les performances sont critiques

**Utilisez TIniFile quand :**
- Vous faites peu d'opérations
- Vous voulez que les changements soient immédiats
- Le fichier INI est très gros

---

## Exemple avancé : Configuration de connexion

```pascal
{$mode objfpc}{$H+}
program ConfigConnexion;

uses
  IniFiles, SysUtils;

type
  TConfigConnexion = record
    Serveur: string;
    Port: Integer;
    BaseDeDonnees: string;
    Utilisateur: string;
    Timeout: Integer;
    SSL: Boolean;
  end;

function ChargerConfigConnexion(Fichier: string): TConfigConnexion;  
var  
  Ini: TMemIniFile;
begin
  if not FileExists(Fichier) then
  begin
    WriteLn('ATTENTION : Fichier de configuration introuvable !');
    WriteLn('Utilisation des valeurs par défaut.');

    // Valeurs par défaut
    Result.Serveur := 'localhost';
    Result.Port := 5432;
    Result.BaseDeDonnees := 'mabase';
    Result.Utilisateur := 'admin';
    Result.Timeout := 30;
    Result.SSL := False;
    Exit;
  end;

  Ini := TMemIniFile.Create(Fichier);
  try
    Result.Serveur := Ini.ReadString('Connexion', 'Serveur', 'localhost');
    Result.Port := Ini.ReadInteger('Connexion', 'Port', 5432);
    Result.BaseDeDonnees := Ini.ReadString('Connexion', 'BaseDeDonnees', 'mabase');
    Result.Utilisateur := Ini.ReadString('Connexion', 'Utilisateur', 'admin');
    Result.Timeout := Ini.ReadInteger('Connexion', 'Timeout', 30);
    Result.SSL := Ini.ReadBool('Connexion', 'SSL', False);
  finally
    Ini.Free;
  end;
end;

procedure SauvegarderConfigConnexion(Fichier: string; const Config: TConfigConnexion);  
var  
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(Fichier);
  try
    // Ajouter un en-tête avec des commentaires
    Ini.WriteString('_INFO', 'Description', 'Configuration de connexion base de données');
    Ini.WriteString('_INFO', 'Version', '1.0');
    Ini.WriteString('_INFO', 'DateModif', DateTimeToStr(Now));

    // Écrire la configuration
    Ini.WriteString('Connexion', 'Serveur', Config.Serveur);
    Ini.WriteInteger('Connexion', 'Port', Config.Port);
    Ini.WriteString('Connexion', 'BaseDeDonnees', Config.BaseDeDonnees);
    Ini.WriteString('Connexion', 'Utilisateur', Config.Utilisateur);
    Ini.WriteInteger('Connexion', 'Timeout', Config.Timeout);
    Ini.WriteBool('Connexion', 'SSL', Config.SSL);

    Ini.UpdateFile;
    WriteLn('Configuration sauvegardée dans : ', Fichier);
  finally
    Ini.Free;
  end;
end;

procedure AfficherConfig(const Config: TConfigConnexion);  
begin  
  WriteLn;
  WriteLn('=== CONFIGURATION DE CONNEXION ===');
  WriteLn('Serveur        : ', Config.Serveur);
  WriteLn('Port           : ', Config.Port);
  WriteLn('Base de données: ', Config.BaseDeDonnees);
  WriteLn('Utilisateur    : ', Config.Utilisateur);
  WriteLn('Timeout        : ', Config.Timeout, ' secondes');
  WriteLn('SSL            : ', Config.SSL);
  WriteLn('==================================');
end;

function TesterConnexion(const Config: TConfigConnexion): Boolean;  
begin  
  WriteLn;
  WriteLn('Test de connexion à ', Config.Serveur, ':', Config.Port, '...');

  // Ici, vous mettriez le vrai code de connexion
  // Pour l'exemple, on simule
  WriteLn('Connexion simulée réussie !');
  Result := True;
end;

var
  Config: TConfigConnexion;
  FichierConfig: string;

begin
  FichierConfig := 'database.ini';

  WriteLn('===================================');
  WriteLn('  CONFIGURATION BASE DE DONNÉES   ');
  WriteLn('===================================');
  WriteLn;

  // Charger la configuration
  Config := ChargerConfigConnexion(FichierConfig);
  AfficherConfig(Config);

  // Sauvegarder pour créer le fichier
  SauvegarderConfigConnexion(FichierConfig, Config);

  // Tester la connexion
  if TesterConnexion(Config) then
    WriteLn('V Connexion OK')
  else
    WriteLn('X Échec de connexion');

  // Nettoyage du fichier de demo
  if FileExists(FichierConfig) then
    DeleteFile(FichierConfig);
end.
```

**Fichier `database.ini` créé :**
```ini
[_INFO]
Description=Configuration de connexion base de données  
Version=1.0  
DateModif=13/10/2025 14:30:52  

[Connexion]
Serveur=localhost  
Port=5432  
BaseDeDonnees=mabase  
Utilisateur=admin  
Timeout=30  
SSL=0  
```

---

## Gestion des erreurs et validation

### Validation des valeurs lues

```pascal
function ChargerConfigValidee(Fichier: string): Boolean;  
var  
  Ini: TIniFile;
  Port: Integer;
begin
  Result := False;

  if not FileExists(Fichier) then
  begin
    WriteLn('Erreur : Fichier introuvable');
    Exit;
  end;

  Ini := TIniFile.Create(Fichier);
  try
    // Lire et valider le port
    Port := Ini.ReadInteger('Connexion', 'Port', 0);

    if (Port < 1) or (Port > 65535) then
    begin
      WriteLn('Erreur : Port invalide (', Port, ')');
      WriteLn('Le port doit être entre 1 et 65535');
      Exit;
    end;

    // Autres validations...

    Result := True;
  finally
    Ini.Free;
  end;
end;
```

### Gestion des fichiers INI corrompus

```pascal
function ChargementSecurise(Fichier: string): Boolean;  
var  
  Ini: TIniFile;
begin
  Result := False;

  try
    if FileExists(Fichier) then
    begin
      // Tenter de charger
      // Si le fichier est corrompu, TIniFile peut lever une exception
      Ini := TIniFile.Create(Fichier);
      try
        // Test de lecture
        Ini.ReadString('Test', 'Test', '');
        Result := True;
      finally
        Ini.Free;
      end;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Erreur lors du chargement : ', E.Message);
      WriteLn('Le fichier INI est peut-être corrompu');
    end;
  end;
end;
```

---

## Bonnes pratiques

### ✅ À faire

**Toujours utiliser try-finally** pour libérer l'objet TIniFile

**Fournir des valeurs par défaut** sensées dans les Read...()

**Organiser logiquement** les sections (Interface, Connexion, Fichiers, etc.)

**Documenter le fichier INI** avec des commentaires

**Valider les valeurs** après lecture (plages, formats)

**Créer le fichier** automatiquement s'il n'existe pas

**Utiliser des chemins portables** pour le fichier INI

**Préférer TMemIniFile** pour de nombreuses opérations

**Versionner le format** du fichier INI (section [Version])

**Gérer les migrations** quand vous changez le format

### ❌ À éviter

**Ne jamais oublier Free** après Create

**Ne pas supposer** que toutes les clés existent

**Ne pas stocker** de mots de passe en clair

**Ne pas utiliser** de caractères spéciaux dans les noms de sections/clés

**Ne pas imbriquer** les sections (INI est plat)

**Ne pas stocker** de grandes quantités de données

**Ne pas compter** sur l'ordre des sections/clés

**Ne pas mélanger** majuscules/minuscules de façon incohérente

---

## Emplacement du fichier INI

### Dans le répertoire de l'application

```pascal
var
  CheminIni: string;
begin
  CheminIni := ExtractFilePath(ParamStr(0)) + 'config.ini';
end.
```

### Dans le répertoire utilisateur (recommandé)

```pascal
uses
  SysUtils, FileUtil;

var
  CheminIni: string;
begin
  // Windows : C:\Users\Jean\AppData\Local\MonApp\
  // Linux   : /home/jean/.config/MonApp/

  CheminIni := GetAppConfigDir(False) + 'config.ini';

  // Créer le répertoire s'il n'existe pas
  ForceDirectories(ExtractFilePath(CheminIni));
end.
```

### Dans un répertoire système

```pascal
{$IFDEF WINDOWS}
  CheminIni := 'C:\ProgramData\MonApp\config.ini';
{$ELSE}
  CheminIni := '/etc/monapp/config.ini';
{$ENDIF}
```

---

## Alternatives aux fichiers INI

### JSON (données structurées)

**Avantages :** Hiérarchique, typé, standard web  
**Usage :** API, échange de données, structures complexes  

### XML (documents structurés)

**Avantages :** Hiérarchique, validable, extensible  
**Usage :** Documents, données métier complexes  

### SQLite (base de données)

**Avantages :** Requêtes puissantes, transactions, performances  
**Usage :** Grandes quantités de données, relations complexes  

### Registry (Windows uniquement)

**Avantages :** Intégré au système  
**Inconvénients :** Non portable, difficile à sauvegarder  

---

## Tableau récapitulatif

| Méthode | Description | Paramètres |
|---------|-------------|------------|
| `Create(fichier)` | Créer l'objet | Chemin du fichier INI |
| `ReadString(s, k, def)` | Lire une chaîne | Section, Clé, Défaut |
| `ReadInteger(s, k, def)` | Lire un entier | Section, Clé, Défaut |
| `ReadBool(s, k, def)` | Lire un booléen | Section, Clé, Défaut |
| `ReadFloat(s, k, def)` | Lire un réel | Section, Clé, Défaut |
| `WriteString(s, k, val)` | Écrire une chaîne | Section, Clé, Valeur |
| `WriteInteger(s, k, val)` | Écrire un entier | Section, Clé, Valeur |
| `WriteBool(s, k, val)` | Écrire un booléen | Section, Clé, Valeur |
| `SectionExists(s)` | Section existe ? | Section |
| `ValueExists(s, k)` | Clé existe ? | Section, Clé |
| `ReadSections(list)` | Lister sections | TStringList |
| `ReadSection(s, list)` | Lister clés | Section, TStringList |
| `DeleteKey(s, k)` | Supprimer clé | Section, Clé |
| `EraseSection(s)` | Supprimer section | Section |
| `UpdateFile` | Sauvegarder (TMemIniFile) | - |
| `Free` | Libérer l'objet | - |

---

## Résumé

Les fichiers INI sont parfaits pour stocker des configurations simples :

**Format :**
- Sections entre crochets : `[Section]`
- Paires clé=valeur : `Clé=Valeur`
- Commentaires : `;` ou `#`

**Classes Pascal :**
- `TIniFile` : accès direct au disque
- `TMemIniFile` : cache en mémoire (plus rapide)

**Opérations principales :**
- `ReadString/Integer/Bool` : lire avec valeur par défaut
- `WriteString/Integer/Bool` : écrire
- `SectionExists/ValueExists` : vérifier

**Bonnes pratiques :**
- Toujours utiliser try-finally
- Fournir des valeurs par défaut
- Valider les données lues
- Documenter avec des commentaires

Dans la prochaine section, nous découvrirons les streams pour des manipulations plus avancées !

---

> **Conseil pratique :** Les fichiers INI sont le moyen le plus simple de sauvegarder les préférences de vos applications. Commencez par les maîtriser avant d'explorer des formats plus complexes comme JSON ou XML !

⏭️ [Introduction aux streams](/08-gestion-fichiers-io/09-introduction-streams.md)
