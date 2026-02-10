# Compiler et exécuter les exemples

## Prérequis

- **FreePascal (fpc)** installé sur votre système

**Installation :**

```bash
# Ubuntu/Debian
sudo apt update && sudo apt install fpc

# Fedora
sudo dnf install fpc

# Windows : télécharger depuis https://www.freepascal.org/download.html
```

## Compiler un exemple

```bash
# Syntaxe générale
fpc nom-du-fichier.pas

# Exemple
fpc 10-hello-world.pas
```

**Résultat :** Un exécutable est créé dans le même dossier (sans extension sous Linux, `.exe` sous Windows).

## Exécuter un exemple

```bash
# Linux
./10-hello-world

# Windows
10-hello-world.exe
```

## Compiler tous les exemples d'un coup

```bash
# Linux/macOS
for f in *.pas; do echo "=== $f ===" && fpc "$f"; done

# Windows (PowerShell)
Get-ChildItem *.pas | ForEach-Object { Write-Host "=== $($_.Name) ===" ; fpc $_.Name }
```

## Recompiler (rebuild) un exemple

FreePascal recompile automatiquement si le source a changé :

```bash
fpc 10-hello-world.pas
```

Pour forcer une recompilation complète :

```bash
fpc -B 10-hello-world.pas
```

## Nettoyer les fichiers compilés

```bash
# Linux/macOS
rm -f *.o  
find . -maxdepth 1 -type f -executable -delete  

# Windows (PowerShell)
Remove-Item *.o, *.exe -ErrorAction SilentlyContinue
```

## Liste des exemples

| Fichier | Section | Description | Interactif |
|---------|---------|-------------|------------|
| `02-bonjour.pas` | 1.2 | Programme minimal "Bonjour le monde" | Non |
| `05-aire-rectangle.pas` | 1.5 | Calcul d'aire avec saisie utilisateur | Oui |
| `05-pair-ou-impair.pas` | 1.5 | Vérifie si un nombre est pair ou impair | Oui |
| `05-factorielle.pas` | 1.5 | Calcul de factorielle (récursivité) | Oui |
| `10-hello-world.pas` | 1.10 | Le classique Hello World | Non |
| `10-hello-world-pause.pas` | 1.10 | Hello World avec pause (ReadLn) | Oui (Entrée) |
| `10-hello-multiline.pas` | 1.10 | Affichage multi-lignes | Non |
| `10-hello-write.pas` | 1.10 | Différence entre Write et WriteLn | Non |
| `10-hello-special.pas` | 1.10 | Caractères spéciaux et décorations | Non |
| `10-hello-escape.pas` | 1.10 | Apostrophe échappée avec '' | Non |
| `10-hello-commented.pas` | 1.10 | Commentaires // et { } | Non |

## Sorties attendues

### 02-bonjour.pas
```
Bonjour le monde !
```

### 05-aire-rectangle.pas (avec 5 et 3 en entrée)
```
Entrez la longueur :  
Entrez la largeur :  
L'aire du rectangle est : 15.00  
```

### 05-pair-ou-impair.pas (avec 4 en entrée)
```
Entrez un nombre entier :
4 est pair
```

### 05-factorielle.pas (avec 5 en entrée)
```
Entrez un nombre :  
Factorielle de 5 = 120  
```

### 10-hello-world.pas
```
Hello, World!
```

### 10-hello-world-pause.pas
```
Hello, World!  
Appuyez sur Entrée pour continuer...  
```

### 10-hello-multiline.pas
```
Bonjour !  
Je suis votre premier programme.  
Félicitations !  
```

### 10-hello-write.pas
```
Hello, World!
```

### 10-hello-special.pas
```
Hello, World!
-------------
* Bienvenue *
-------------
```

### 10-hello-escape.pas
```
Ligne 1  
Ligne 2  

Guillemets : '
```

### 10-hello-commented.pas
```
Hello, World!  
Programme terminé.  
```
