🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.1 Différences Windows/Linux à connaître

## Introduction

Lorsque vous développez des applications multi-plateformes avec FreePascal et Lazarus, il est essentiel de comprendre les différences fondamentales entre Windows et Linux. Ces différences peuvent sembler techniques au premier abord, mais nous allons les expliquer de manière simple et pratique.

Le principal avantage de FreePascal/Lazarus est que la plupart de ces différences sont gérées automatiquement par le compilateur et la bibliothèque standard. Cependant, connaître ces différences vous évitera bien des surprises et vous permettra d'écrire du code vraiment portable.

---

## 1. Les Chemins de Fichiers (Paths)

### Le Séparateur de Répertoires

**C'est LA différence la plus importante à connaître !**

**Windows utilise :** le backslash `\`
```
C:\Users\Pierre\Documents\MonFichier.txt
```

**Linux utilise :** le slash `/`
```
/home/pierre/Documents/MonFichier.txt
```

### Solution en Pascal

FreePascal fournit des constantes qui s'adaptent automatiquement :

```pascal
uses
  SysUtils;

var
  CheminFichier: string;
begin
  // MAUVAIS : codé en dur pour Windows
  CheminFichier := 'C:\Data\fichier.txt';  // Ne fonctionnera PAS sous Linux !

  // BON : utilisation du séparateur portable
  CheminFichier := 'Data' + PathDelim + 'fichier.txt';

  // ENCORE MIEUX : utilisation de fonctions dédiées
  CheminFichier := IncludeTrailingPathDelimiter(GetCurrentDir) + 'fichier.txt';
end;
```

**Constantes utiles :**
- `PathDelim` : le bon séparateur selon la plateforme (`\` ou `/`)
- `DirectorySeparator` : identique à PathDelim
- `LineEnding` : fin de ligne correcte selon la plateforme

---

## 2. Sensibilité à la Casse (Majuscules/Minuscules)

### Noms de Fichiers

**Windows :** insensible à la casse
```
MonFichier.txt = monfichier.txt = MONFICHIER.TXT
```

**Linux :** sensible à la casse
```
MonFichier.txt ≠ monfichier.txt ≠ MONFICHIER.TXT
```

### Conséquences Pratiques

```pascal
// Sous Windows, ces trois lignes ouvrent le MÊME fichier :
AssignFile(F, 'config.ini');  
AssignFile(F, 'Config.ini');  
AssignFile(F, 'CONFIG.INI');  

// Sous Linux, ce sont trois fichiers DIFFÉRENTS !
```

**Bonne pratique :** Utilisez toujours la même casse de manière cohérente dans votre code. Par convention, privilégiez les minuscules pour les noms de fichiers.

---

## 3. Extensions des Exécutables

**Windows :** les programmes ont l'extension `.exe`
```
MonProgramme.exe
```

**Linux :** les programmes n'ont généralement pas d'extension
```
MonProgramme
```

### Dans Votre Code

FreePascal gère cela automatiquement lors de la compilation. Vous n'avez rien à faire ! Le compilateur produira automatiquement :
- `MonProjet.exe` sous Windows
- `MonProjet` sous Linux

---

## 4. Fins de Lignes dans les Fichiers Texte

### Les Caractères Invisibles

**Windows :** utilise deux caractères : CR+LF (Carriage Return + Line Feed)
- Code : `#13#10`
- Représentation : `\r\n`

**Linux :** utilise un seul caractère : LF (Line Feed)
- Code : `#10`
- Représentation : `\n`

### Dans Votre Code

```pascal
uses
  SysUtils;

var
  Texte: string;
begin
  // MAUVAIS : codé en dur
  Texte := 'Ligne 1' + #13#10 + 'Ligne 2';  // Ne fonctionnera pas bien sous Linux

  // BON : utilisation de la constante portable
  Texte := 'Ligne 1' + LineEnding + 'Ligne 2';

  // Pour les fichiers texte
  WriteLn(MonFichier, 'Ma ligne');  // WriteLn gère automatiquement la fin de ligne !
end;
```

**Important :** Lorsque vous utilisez `WriteLn`, FreePascal ajoute automatiquement la bonne fin de ligne selon la plateforme. Vous n'avez généralement pas à vous en soucier !

---

## 5. Structure des Chemins Système

### Emplacement des Répertoires Utilisateur

**Windows :**
```
C:\Users\NomUtilisateur\  
C:\Users\NomUtilisateur\Documents\  
C:\Users\NomUtilisateur\AppData\Local\  
```

**Linux :**
```
/home/NomUtilisateur/
/home/NomUtilisateur/Documents/
/home/NomUtilisateur/.config/
/home/NomUtilisateur/.local/share/
```

### Récupération du Répertoire Utilisateur

```pascal
uses
  SysUtils;

var
  RepUtilisateur: string;
begin
  {$IFDEF WINDOWS}
  RepUtilisateur := GetEnvironmentVariable('USERPROFILE');
  {$ENDIF}

  {$IFDEF LINUX}
  RepUtilisateur := GetEnvironmentVariable('HOME');
  {$ENDIF}

  WriteLn('Répertoire utilisateur : ', RepUtilisateur);
end;
```

---

## 6. Permissions et Droits d'Accès

### Concept

**Windows :** Système de permissions basé sur les utilisateurs et les groupes (ACL - Access Control Lists). Moins strict dans l'usage quotidien.

**Linux :** Système de permissions strict basé sur propriétaire/groupe/autres avec lecture/écriture/exécution.

### Exécution des Programmes

**Windows :** Un fichier `.exe` est automatiquement exécutable.

**Linux :** Un fichier doit avoir le droit d'exécution activé :
```bash
chmod +x MonProgramme
```

**Important pour la distribution :** Si vous distribuez votre application Linux, pensez à documenter que l'utilisateur doit rendre le fichier exécutable, ou fournissez un script d'installation qui le fait automatiquement.

---

## 7. Variables d'Environnement

### Syntaxe

**Windows :**
```
%USERPROFILE%
%TEMP%
%PATH%
```

**Linux :**
```
$HOME
$TMPDIR ou $TEMP
$PATH
```

### Lecture en Pascal

```pascal
uses
  SysUtils;

var
  CheminTemp: string;
begin
  // FreePascal unifie l'accès :
  CheminTemp := GetTempDir;  // Fonctionne sur les deux plateformes !

  // Ou pour une variable spécifique :
  CheminTemp := GetEnvironmentVariable('TEMP');
end;
```

---

## 8. Répertoires de Configuration

### Où Sauvegarder les Préférences ?

**Windows :**
- Fichiers de configuration : `%APPDATA%` ou le répertoire du programme
- Exemple : `C:\Users\Pierre\AppData\Roaming\MonApp\`

**Linux :**
- Fichiers de configuration : `~/.config/MonApp/` ou `~/.MonApp/`
- Le point `.` devant le nom indique un répertoire caché

### Code Portable

```pascal
uses
  SysUtils;

function GetConfigDir: string;  
begin  
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('APPDATA') + PathDelim + 'MonApp' + PathDelim;
  {$ENDIF}

  {$IFDEF LINUX}
  Result := GetEnvironmentVariable('HOME') + PathDelim + '.config' + PathDelim + 'MonApp' + PathDelim;
  {$ENDIF}

  // Créer le répertoire s'il n'existe pas
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;
```

---

## 9. Bibliothèques Dynamiques

### Extensions et Noms

**Windows :** DLL (Dynamic Link Library)
```
MaBibliotheque.dll
```

**Linux :** SO (Shared Object)
```
libMaBibliotheque.so
```

### Chargement Dynamique

```pascal
uses
  DynLibs;

var
  Handle: TLibHandle;
  NomBib: string;
begin
  {$IFDEF WINDOWS}
  NomBib := 'MaBibliotheque.dll';
  {$ENDIF}

  {$IFDEF LINUX}
  NomBib := 'libMaBibliotheque.so';
  {$ENDIF}

  Handle := LoadLibrary(NomBib);
  if Handle <> 0 then
    WriteLn('Bibliothèque chargée avec succès');
end;
```

---

## 10. Interface Graphique et Apparence

### Thèmes et Styles

**Windows :** Utilise les styles natifs Windows (XP, Vista, 7, 10, 11...)

**Linux :** Utilise GTK2, GTK3 ou Qt selon la configuration. L'apparence dépend du gestionnaire de bureau (GNOME, KDE, XFCE...).

### Dans Lazarus

Lazarus s'adapte automatiquement à l'apparence native de chaque système grâce à la LCL (Lazarus Component Library). Votre application aura l'apparence native sans code supplémentaire !

**Conseil :** Testez toujours votre interface sur les deux systèmes, car les polices et espacements peuvent varier légèrement.

---

## 11. Processus et Commandes Système

### Exécution de Commandes

**Windows :** Utilise `cmd.exe`
```pascal
ExecuteProcess('cmd.exe', ['/c', 'dir']);
```

**Linux :** Utilise le shell (généralement `bash`)
```pascal
ExecuteProcess('/bin/bash', ['-c', 'ls -la']);
```

### Solution Portable

```pascal
uses
  Process;

procedure ExecuterCommande(Commande: string);  
var  
  Proc: TProcess;
begin
  Proc := TProcess.Create(nil);
  try
    {$IFDEF WINDOWS}
    Proc.Executable := 'cmd.exe';
    Proc.Parameters.Add('/c');
    {$ENDIF}

    {$IFDEF LINUX}
    Proc.Executable := '/bin/bash';
    Proc.Parameters.Add('-c');
    {$ENDIF}

    Proc.Parameters.Add(Commande);
    Proc.Execute;
  finally
    Proc.Free;
  end;
end;
```

---

## 12. Gestion des Signaux et Interruptions

### Terminaison des Programmes

**Windows :** Utilise des messages Windows (WM_QUIT, etc.)

**Linux :** Utilise des signaux POSIX (SIGTERM, SIGINT, SIGKILL...)

**En pratique :** Lazarus gère cela pour vous dans les applications graphiques. Pour les applications console, vous pouvez intercepter Ctrl+C :

```pascal
uses
  SysUtils;

var
  Interrompu: Boolean = False;

procedure GestionnaireInterruption(Signal: Integer); cdecl;  
begin  
  Interrompu := True;
  WriteLn('Interruption reçue, arrêt en cours...');
end;

begin
  // Sous Linux, on peut intercepter les signaux
  {$IFDEF LINUX}
  Signal(SIGINT, @GestionnaireInterruption);
  {$ENDIF}

  while not Interrompu do
  begin
    // Votre traitement
    Sleep(100);
  end;
end.
```

---

## 13. Encodage des Caractères

### Par Défaut

**Windows :** Utilise historiquement des encodages régionaux (Windows-1252, etc.) mais supporte UTF-8

**Linux :** Utilise généralement UTF-8 par défaut

### Recommandation

**Utilisez toujours UTF-8 !** C'est le standard universel qui fonctionne partout.

```pascal
uses
  LazUTF8;

var
  Texte: string;
begin
  // FreePascal moderne utilise UTF-8 par défaut
  Texte := 'Bonjour, こんにちは, مرحبا';

  // Pour la lecture/écriture de fichiers :
  // Lazarus gère UTF-8 nativement avec TStringList
end;
```

---

## 14. Chemins Maximums

**Windows :**
- Limite traditionnelle : 260 caractères (MAX_PATH)
- Windows 10/11 moderne : peut être étendu

**Linux :**
- Limite : 4096 caractères généralement (PATH_MAX)

**Bonne pratique :** Gardez vos chemins raisonnablement courts (< 200 caractères) pour éviter tout problème.

---

## Résumé des Bonnes Pratiques

1. **Utilisez toujours les constantes portables** : `PathDelim`, `LineEnding`
2. **Utilisez les fonctions FreePascal** : `GetTempDir`, `GetAppConfigDir`
3. **Évitez les chemins codés en dur** : construisez-les dynamiquement
4. **Testez sur les deux plateformes** régulièrement
5. **Utilisez UTF-8** pour tous vos fichiers texte
6. **Soyez cohérent** dans votre usage des majuscules/minuscules
7. **Documentez les spécificités** si votre application en a

---

## Conclusion

La plupart du temps, FreePascal et Lazarus masquent ces différences pour vous. En suivant les bonnes pratiques et en utilisant les fonctions fournies par la bibliothèque standard, vous écrirez naturellement du code portable.

Le chapitre suivant (19.2) vous montrera concrètement comment utiliser ces connaissances avec les directives de compilation conditionnelle pour gérer les cas particuliers.

**N'oubliez pas :** La meilleure façon d'apprendre est de tester ! Compilez et exécutez vos programmes sur Windows ET sur Linux dès le début du développement, pas à la fin.

⏭️ [Gestion portable des chemins (PathDelim, DirectorySeparator)](/19-developpement-multi-plateforme-pratique/02-gestion-portable-chemins.md)
