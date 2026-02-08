🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.8 Tests sur différentes plateformes

## Introduction

Imaginez que vous écrivez un livre qui doit être traduit en plusieurs langues. Vous ne pouvez pas simplement traduire mot à mot et supposer que tout sera parfait. Vous devez faire relire par des natifs de chaque langue pour vérifier que le sens, le ton et les nuances sont préservés.

C'est exactement la même chose avec les applications multi-plateformes ! Même si votre code compile sur toutes les plateformes, cela ne garantit pas que l'application **fonctionne** correctement partout. Les tests sur différentes plateformes sont **indispensables**.

Dans ce chapitre, nous allons apprendre à tester efficacement nos applications FreePascal/Lazarus sur Windows, Linux et macOS.

---

## 1. Pourquoi Tester sur Chaque Plateforme ?

### La Compilation ne Suffit Pas

**Idée fausse courante :**
> "Mon code compile sans erreur sur Linux, donc il fonctionnera sur Windows aussi."

**Réalité :**
La compilation vérifie la syntaxe, pas le comportement réel de l'application.

### Problèmes Qui N'Apparaissent Que Sur Certaines Plateformes

**Exemples réels :**

**1. Chemins de fichiers :**
```pascal
// Fonctionne sous Windows, plante sous Linux
Fichier := 'C:\Data\config.ini';  // Erreur : disque C: n'existe pas sous Linux
```

**2. Permissions :**
```pascal
// Fonctionne sous Windows, échoue sous Linux
AssignFile(F, '/etc/myapp/config.ini');
Rewrite(F);  // Erreur : Permission denied sous Linux
```

**3. Interface graphique :**
```pascal
// Sous Windows : Police Segoe UI 10pt
// Sous Linux : Police différente, le texte déborde du bouton !
Button1.Caption := 'Enregistrer les modifications';
```

**4. Comportement système :**
```pascal
// Fonctionne différemment selon l'OS
ExecuteProcess('notepad.exe', ['fichier.txt']);  // Windows seulement
```

**5. Encodage de caractères :**
```pascal
// Fonctionne sous Linux (UTF-8), problème sous Windows (encodage régional)
WriteLn('Caractères accentués : éàù');
```

### Les Trois Types de Bugs Multi-Plateformes

**1. Bugs de compilation :**
- Code qui ne compile que sur une plateforme
- Généralement facile à détecter et corriger

**2. Bugs d'exécution :**
- L'application se lance mais crash sur certaines plateformes
- Peut être difficile à déboguer à distance

**3. Bugs de comportement :**
- L'application fonctionne mais mal
- Interface déformée, fonctionnalités manquantes
- **Les plus insidieux** car pas toujours évidents

---

## 2. Environnements de Test

### Option 1 : Machines Physiques Réelles (Idéal)

**Configuration idéale :**
- Un PC Windows
- Un PC Linux (ou dual-boot)
- Un Mac (optionnel si vous ciblez macOS)

**Avantages :**
- ✅ Performances réelles
- ✅ Pas de problèmes de compatibilité
- ✅ Test de tous les aspects matériels
- ✅ Expérience utilisateur authentique

**Inconvénients :**
- ❌ Coût élevé (plusieurs ordinateurs)
- ❌ Espace physique nécessaire
- ❌ Maintenance de plusieurs systèmes

**Quand l'utiliser :**
- Projets professionnels
- Tests finaux avant release
- Tests de performance

### Option 2 : Machines Virtuelles (Recommandé)

**Logiciels de virtualisation :**
- **VirtualBox** (gratuit, multi-plateforme)
- **VMware Workstation** (payant, performant)
- **VMware Fusion** (macOS)
- **Parallels Desktop** (macOS)
- **QEMU/KVM** (Linux)

**Configuration typique :**
```
PC Hôte Windows 11 (16 GB RAM)
├── VM Ubuntu 22.04 (4 GB RAM)
├── VM Windows 10 (4 GB RAM)
└── VM Fedora 38 (4 GB RAM)
```

**Avantages :**
- ✅ Un seul ordinateur physique nécessaire
- ✅ Snapshots (retour en arrière facile)
- ✅ Isolation complète
- ✅ Plusieurs versions d'OS possibles

**Inconvénients :**
- ❌ Performances réduites
- ❌ Nécessite beaucoup de RAM
- ❌ Peut ne pas supporter certains aspects 3D/GPU

**Configuration recommandée pour l'hôte :**
- CPU : Quad-core minimum
- RAM : 16 GB minimum (32 GB idéal)
- Disque : SSD 500 GB minimum
- Virtualisation activée dans le BIOS

### Option 3 : Containers Docker (Pour Linux)

**Docker** permet de tester différentes distributions Linux sans machine virtuelle complète.

**Avantages :**
- ✅ Très léger (pas d'OS complet)
- ✅ Démarrage rapide
- ✅ Facilement scriptable

**Inconvénients :**
- ❌ Linux uniquement (pas de GUI facilement)
- ❌ Mieux pour applications console
- ❌ Courbe d'apprentissage

**Exemple de Dockerfile :**
```dockerfile
FROM ubuntu:22.04

# Installer dépendances
RUN apt-get update && apt-get install -y \
    libgtk2.0-0 \
    libsqlite3-0 \
    libssl1.1

# Copier l'application
COPY MonApp /app/MonApp
RUN chmod +x /app/MonApp

WORKDIR /app
CMD ["./MonApp"]
```

**Utilisation :**
```bash
# Construire l'image
docker build -t monapp-test .

# Lancer le test
docker run --rm monapp-test
```

### Option 4 : Services Cloud (CI/CD)

**GitHub Actions, GitLab CI, etc.**

**Exemple de workflow GitHub Actions :**
```yaml
name: Multi-Platform Tests

on: [push, pull_request]

jobs:
  test-windows:
    runs-on: windows-latest
    steps:
      - uses: actions/checkout@v2
      - name: Build
        run: lazbuild MonProjet.lpi
      - name: Test
        run: ./bin/MonApp.exe --test

  test-linux:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install dependencies
        run: sudo apt install lazarus
      - name: Build
        run: lazbuild MonProjet.lpi
      - name: Test
        run: ./bin/MonApp --test

  test-macos:
    runs-on: macos-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install dependencies
        run: brew install lazarus
      - name: Build
        run: lazbuild MonProjet.lpi
      - name: Test
        run: ./bin/MonApp --test
```

**Avantages :**
- ✅ Gratuit pour projets open source
- ✅ Automatique à chaque commit
- ✅ Teste sur les 3 plateformes
- ✅ Rapports détaillés

**Inconvénients :**
- ❌ Pas d'interface graphique pour tests manuels
- ❌ Limité en durée d'exécution
- ❌ Difficile de déboguer interactivement

---

## 3. Types de Tests à Effectuer

### Tests Fonctionnels

**Objectif :** Vérifier que toutes les fonctionnalités marchent.

**Checklist type :**

**Pour une application de gestion de fichiers :**
```
Windows :
[ ] Ouverture de fichier
[ ] Sauvegarde de fichier
[ ] Création de dossier
[ ] Suppression de fichier
[ ] Copier/Coller
[ ] Glisser-déposer

Linux :
[ ] Ouverture de fichier
[ ] Sauvegarde de fichier
[ ] Création de dossier
[ ] Suppression de fichier
[ ] Copier/Coller
[ ] Glisser-déposer

macOS :
[ ] Ouverture de fichier
[ ] (etc.)
```

**Script de test fonctionnel :**

```pascal
program TestFonctionnel;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  TestsPasses, TestsEchoues: Integer;

procedure Test(const Nom: string; Condition: Boolean);
begin
  Write(Nom, '... ');
  if Condition then
  begin
    WriteLn('OK');
    Inc(TestsPasses);
  end
  else
  begin
    WriteLn('ÉCHEC');
    Inc(TestsEchoues);
  end;
end;

procedure TestsCreationFichier;
var
  F: TextFile;
  TestFile: string;
begin
  TestFile := GetTempDir + 'test.txt';

  try
    AssignFile(F, TestFile);
    Rewrite(F);
    WriteLn(F, 'Test');
    CloseFile(F);

    Test('Création de fichier', FileExists(TestFile));

    DeleteFile(TestFile);
    Test('Suppression de fichier', not FileExists(TestFile));
  except
    Test('Création/Suppression de fichier', False);
  end;
end;

procedure TestsRepertoires;
var
  TestDir: string;
begin
  TestDir := GetTempDir + 'test_dir';

  Test('Création de répertoire', CreateDir(TestDir));
  Test('Répertoire existe', DirectoryExists(TestDir));
  Test('Suppression de répertoire', RemoveDir(TestDir));
end;

begin
  WriteLn('========================================');
  WriteLn('Tests Fonctionnels Multi-Plateformes');
  WriteLn('Plateforme : ', {$I %FPCTARGETOS%});
  WriteLn('========================================');
  WriteLn;

  TestsPasses := 0;
  TestsEchoues := 0;

  WriteLn('--- Tests Fichiers ---');
  TestsCreationFichier;

  WriteLn;
  WriteLn('--- Tests Répertoires ---');
  TestsRepertoires;

  WriteLn;
  WriteLn('========================================');
  WriteLn('Résultats :');
  WriteLn('  Réussis : ', TestsPasses);
  WriteLn('  Échoués : ', TestsEchoues);
  WriteLn('========================================');

  if TestsEchoues > 0 then
    Halt(1);
end.
```

### Tests d'Interface (GUI)

**Aspects à vérifier :**

**1. Disposition (Layout) :**
- Les boutons sont-ils tous visibles ?
- Le texte déborde-t-il ?
- Les contrôles se chevauchent-ils ?

**2. Polices et Tailles :**
- Les polices sont-elles lisibles ?
- Les tailles sont-elles cohérentes ?

**3. Couleurs :**
- Les couleurs sont-elles correctes ?
- Le contraste est-il suffisant ?
- Respect des thèmes système ?

**4. Icônes :**
- Les icônes s'affichent-elles ?
- Sont-elles de la bonne taille ?

**Méthode de test :**

1. **Screenshots comparatifs :**
   ```pascal
   // Capturer l'écran principal
   Form1.SaveToFile('screenshot_' + {$I %FPCTARGETOS%} + '.png');
   ```

2. **Test visuel manuel :**
   - Ouvrir chaque fenêtre
   - Vérifier tous les onglets
   - Tester le redimensionnement
   - Tester maximisation/minimisation

3. **Checklist GUI :**
   ```
   [ ] Fenêtre principale correcte
   [ ] Menus accessibles
   [ ] Barres d'outils visibles
   [ ] Boîtes de dialogue centrées
   [ ] Polices lisibles
   [ ] Icônes correctes
   [ ] Thème système respecté
   [ ] Redimensionnement fonctionne
   [ ] Raccourcis clavier fonctionnent
   ```

### Tests de Performance

**Mesurer et comparer les performances entre plateformes.**

**Programme de benchmark :**

```pascal
program BenchmarkMultiPlateforme;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, Classes;

procedure BenchmarkCalculs;
var
  Start: TDateTime;
  i, Sum: Integer;
begin
  WriteLn('Benchmark : Calculs intensifs...');
  Start := Now;

  Sum := 0;
  for i := 1 to 10000000 do
    Sum := Sum + i mod 13;

  WriteLn('  Temps : ', MilliSecondsBetween(Now, Start), ' ms');
  WriteLn('  Résultat : ', Sum);
end;

procedure BenchmarkFichiers;
var
  Start: TDateTime;
  F: TextFile;
  i: Integer;
  TestFile: string;
begin
  WriteLn('Benchmark : Écriture fichier...');
  TestFile := GetTempDir + 'bench.txt';
  Start := Now;

  AssignFile(F, TestFile);
  Rewrite(F);
  for i := 1 to 100000 do
    WriteLn(F, 'Ligne de test ', i);
  CloseFile(F);

  WriteLn('  Temps : ', MilliSecondsBetween(Now, Start), ' ms');

  DeleteFile(TestFile);
end;

procedure BenchmarkMemoire;
var
  Start: TDateTime;
  List: TStringList;
  i: Integer;
begin
  WriteLn('Benchmark : Allocation mémoire...');
  Start := Now;

  List := TStringList.Create;
  try
    for i := 1 to 100000 do
      List.Add('Item ' + IntToStr(i));
  finally
    List.Free;
  end;

  WriteLn('  Temps : ', MilliSecondsBetween(Now, Start), ' ms');
end;

begin
  WriteLn('========================================');
  WriteLn('Benchmark Multi-Plateformes');
  WriteLn('Plateforme : ', {$I %FPCTARGETOS%});
  WriteLn('CPU : ', {$I %FPCTARGETCPU%});
  WriteLn('========================================');
  WriteLn;

  BenchmarkCalculs;
  WriteLn;
  BenchmarkFichiers;
  WriteLn;
  BenchmarkMemoire;

  WriteLn;
  WriteLn('========================================');
  WriteLn('Benchmark terminé');
  WriteLn('========================================');
end.
```

**Comparer les résultats :**
```
Windows 11 (x86_64)
- Calculs : 245 ms
- Fichiers : 892 ms
- Mémoire : 156 ms

Ubuntu 22.04 (x86_64)
- Calculs : 238 ms
- Fichiers : 654 ms  ← Plus rapide !
- Mémoire : 142 ms

macOS 13 (x86_64)
- Calculs : 251 ms
- Fichiers : 723 ms
- Mémoire : 148 ms
```

### Tests de Compatibilité

**Vérifier la compatibilité avec différentes versions d'OS.**

**Matrice de test recommandée :**

| Application | Windows 10 | Windows 11 | Ubuntu 20.04 | Ubuntu 22.04 | macOS 12 | macOS 13 |
|-------------|-----------|-----------|--------------|--------------|----------|----------|
| Version 1.0 | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ Bug #123 |
| Version 1.1 | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |

---

## 4. Processus de Test Recommandé

### Étape 1 : Tests Locaux (Développement)

**Sur votre machine de développement :**

1. **Compiler** en mode Debug
2. **Tester** les nouvelles fonctionnalités
3. **Déboguer** les problèmes
4. **Valider** localement

**Fréquence :** Après chaque modification significative

### Étape 2 : Tests Cross-Platform (Hebdomadaire)

**Sur les autres plateformes cibles :**

1. **Cross-compiler** pour toutes les plateformes
2. **Transférer** les exécutables sur les VMs/machines de test
3. **Exécuter** les tests fonctionnels
4. **Noter** les problèmes découverts
5. **Corriger** et recommencer

**Fréquence :** Au moins une fois par semaine

### Étape 3 : Tests Complets (Avant Release)

**Avant chaque version publique :**

1. **Tests fonctionnels** complets sur toutes les plateformes
2. **Tests d'interface** visuels
3. **Tests de performance** et benchmarks
4. **Tests d'installation** (installeurs)
5. **Tests utilisateur** (beta-testeurs)
6. **Validation finale**

**Fréquence :** Avant chaque release

### Workflow de Test Typique

```
[Développement Windows]
         ↓
[Tests locaux OK ?] → NON → [Déboguer]
         ↓ OUI
[Cross-compilation]
         ↓
[Transfert vers VMs]
         ↓
[Tests sur Linux] → Problèmes ? → [Noter bugs]
         ↓ Non                            ↓
[Tests sur macOS] → Problèmes ? → [Noter bugs]
         ↓ Non                            ↓
[Tous tests OK ?] → NON → [Corriger] → [Retour début]
         ↓ OUI
[Prêt pour release]
```

---

## 5. Outils et Scripts de Test

### Script de Test Automatisé

**test-all.sh (Linux/macOS) :**

```bash
#!/bin/bash

# Couleurs pour l'affichage
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

PASSED=0
FAILED=0

# Fonction de test
run_test() {
    local test_name=$1
    local test_command=$2

    echo -n "Test : $test_name... "

    if eval "$test_command" > /dev/null 2>&1; then
        echo -e "${GREEN}OK${NC}"
        ((PASSED++))
        return 0
    else
        echo -e "${RED}ÉCHEC${NC}"
        ((FAILED++))
        return 1
    fi
}

echo "=========================================="
echo "Suite de Tests Multi-Plateformes"
echo "Plateforme : $(uname -s)"
echo "=========================================="
echo

# Tests de base
run_test "Application démarre" "./MonApp --version"
run_test "Fichier config créé" "./MonApp --create-config && test -f config.ini"
run_test "Import données" "./MonApp --import test-data.csv"
run_test "Export données" "./MonApp --export output.csv && test -f output.csv"

echo
echo "=========================================="
echo "Résultats :"
echo "  Réussis : $PASSED"
echo "  Échoués : $FAILED"
echo "=========================================="

if [ $FAILED -gt 0 ]; then
    exit 1
fi
```

**test-all.bat (Windows) :**

```batch
@echo off
setlocal EnableDelayedExpansion

set PASSED=0
set FAILED=0

echo ==========================================
echo Suite de Tests Multi-Plateformes
echo Plateforme : Windows
echo ==========================================
echo.

call :run_test "Application démarre" "MonApp.exe --version"
call :run_test "Fichier config créé" "MonApp.exe --create-config"
call :run_test "Import données" "MonApp.exe --import test-data.csv"
call :run_test "Export données" "MonApp.exe --export output.csv"

echo.
echo ==========================================
echo Résultats :
echo   Réussis : %PASSED%
echo   Échoués : %FAILED%
echo ==========================================

if %FAILED% gtr 0 exit /b 1
goto :eof

:run_test
echo | set /p="Test : %~1... "
%~2 >nul 2>&1
if %errorlevel% equ 0 (
    echo OK
    set /a PASSED+=1
) else (
    echo ÉCHEC
    set /a FAILED+=1
)
goto :eof
```

### Framework de Tests Unitaires : FPCUnit

**Installation :**
FPCUnit est inclus avec FreePascal.

**Exemple de test unitaire :**

```pascal
unit TestsCalculatrice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  UCalculatrice;

type
  TTestCalculatrice = class(TTestCase)
  published
    procedure TestAddition;
    procedure TestSoustraction;
    procedure TestMultiplication;
    procedure TestDivision;
    procedure TestDivisionParZero;
  end;

implementation

procedure TTestCalculatrice.TestAddition;
begin
  AssertEquals('Addition simple', 5, Additionner(2, 3));
  AssertEquals('Addition négatifs', -5, Additionner(-2, -3));
  AssertEquals('Addition zéro', 42, Additionner(42, 0));
end;

procedure TTestCalculatrice.TestSoustraction;
begin
  AssertEquals('Soustraction simple', 2, Soustraire(5, 3));
  AssertEquals('Soustraction négatif', -1, Soustraire(3, 4));
end;

procedure TTestCalculatrice.TestMultiplication;
begin
  AssertEquals('Multiplication simple', 15, Multiplier(3, 5));
  AssertEquals('Multiplication par zéro', 0, Multiplier(42, 0));
end;

procedure TTestCalculatrice.TestDivision;
begin
  AssertEquals('Division simple', 5.0, Diviser(15, 3), 0.001);
  AssertEquals('Division décimale', 2.5, Diviser(5, 2), 0.001);
end;

procedure TTestCalculatrice.TestDivisionParZero;
begin
  try
    Diviser(10, 0);
    Fail('Exception attendue pour division par zéro');
  except
    on E: EDivByZero do
      ; // OK, exception attendue
  end;
end;

initialization
  RegisterTest(TTestCalculatrice);

end.
```

**Exécution des tests :**

```pascal
program RunTests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner,
  TestsCalculatrice;

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.
```

---

## 6. Gestion des Différences de Comportement

### Identifier les Différences

**Journal des différences observées :**

```markdown
# Différences de Comportement Multi-Plateformes

## Interface Graphique

### Police par défaut
- Windows : Segoe UI 9pt
- Linux : DejaVu Sans 10pt
- macOS : SF Pro 13pt
→ Solution : Définir explicitement Font.Name et Font.Size

### Position des boutons de dialogue
- Windows : OK à gauche, Annuler à droite
- Linux : OK à droite, Annuler à gauche
- macOS : Annuler à gauche, OK à droite
→ Solution : Utiliser TButtonPanel (s'adapte automatiquement)

## Système de Fichiers

### Sensibilité à la casse
- Windows : Insensible (test.txt = TEST.TXT)
- Linux : Sensible (test.txt ≠ TEST.TXT)
→ Solution : Toujours utiliser la même casse

### Permissions
- Windows : Moins strict
- Linux/macOS : Permissions Unix strictes
→ Solution : Gérer les erreurs de permission
```

### Adapter le Code

**Exemple : Boutons de dialogue adaptatifs**

```pascal
procedure TForm1.CreerDialogue;
var
  Panel: TButtonPanel;
begin
  Panel := TButtonPanel.Create(Self);
  Panel.Parent := Self;
  Panel.ShowButtons := [pbOK, pbCancel];

  // TButtonPanel s'adapte automatiquement à chaque plateforme !
  // Windows : OK | Cancel
  // Linux : Cancel | OK
  // macOS : Cancel | OK
end;
```

**Exemple : Gestion des permissions**

```pascal
function CreerFichierConfig: Boolean;
var
  CheminConfig: string;
  F: TextFile;
begin
  {$IFDEF WINDOWS}
  // Windows : Dans le dossier du programme
  CheminConfig := ExtractFilePath(ParamStr(0)) + 'config.ini';
  {$ELSE}
  // Linux/macOS : Dans le dossier utilisateur
  CheminConfig := GetAppConfigDir(False) + 'config.ini';
  {$ENDIF}

  try
    AssignFile(F, CheminConfig);
    Rewrite(F);
    WriteLn(F, '[Config]');
    CloseFile(F);
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn('Erreur création config : ', E.Message);
      Result := False;
    end;
  end;
end;
```

---

## 7. Documentation des Tests

### Rapport de Test Type

**TEST-REPORT.md :**

```markdown
# Rapport de Test Multi-Plateformes
## Version : 1.2.0
## Date : 2024-10-15

---

## Windows 11 (x64)

### Configuration
- OS : Windows 11 Pro 22H2
- RAM : 16 GB
- Testeur : Pierre

### Tests Fonctionnels
- [x] Démarrage application : OK
- [x] Ouverture fichier : OK
- [x] Sauvegarde fichier : OK
- [x] Import CSV : OK
- [x] Export PDF : OK
- [x] Connexion base de données : OK

### Tests Interface
- [x] Fenêtre principale : OK
- [x] Menus : OK
- [x] Dialogues : OK
- [ ] Toolbar : PROBLÈME - Icônes floues

### Bugs Découverts
1. **Bug #45** : Icônes de toolbar floues
   - Gravité : Mineure
   - Cause : DPI scaling
   - Status : En cours

### Performance
- Démarrage : 1.2s
- Import 1000 lignes : 0.8s
- Export PDF : 2.1s

---

## Ubuntu 22.04 (x64)

### Configuration
- OS : Ubuntu 22.04 LTS
- RAM : 8 GB
- Testeur : Marie

### Tests Fonctionnels
- [x] Démarrage application : OK
- [x] Ouverture fichier : OK
- [x] Sauvegarde fichier : OK
- [x] Import CSV : OK
- [ ] Export PDF : ÉCHEC
- [x] Connexion base de données : OK

### Tests Interface
- [x] Fenêtre principale : OK
- [x] Menus : OK
- [x] Dialogues : OK
- [x] Toolbar : OK

### Bugs Découverts
1. **Bug #46** : Export PDF échoue
   - Gravité : Majeure
   - Cause : Dépendance libpoppler manquante
   - Solution : Ajouter dépendance au README
   - Status : Résolu

### Performance
- Démarrage : 0.9s (plus rapide que Windows!)
- Import 1000 lignes : 0.6s
- Export PDF : N/A (bug #46)

---

## Synthèse

### Plateformes Validées
- Windows 11 : ✅ OK (1 bug mineur)
- Ubuntu 22.04 : ⚠️ OK après correction

### Prêt pour Release ?
- [ ] Non - Corriger bug #46 d'abord
- [ ] Investiguer bug #45

### Actions Requises
1. Corriger export PDF sous Linux
2. Documenter dépendance libpoppler
3. Améliorer gestion DPI Windows
4. Re-tester après corrections
```

---

## 8. Automatisation des Tests

### Tests Continus avec GitHub Actions

**Fichier .github/workflows/test.yml :**

```yaml
name: Tests Multi-Plateformes

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  test-windows:
    name: Tests Windows
    runs-on: windows-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Lazarus
        uses: gcarreno/setup-lazarus@v3
        with:
          lazarus-version: stable

      - name: Compilation
        run: lazbuild --build-mode=Release-Windows-64 MonProjet.lpi

      - name: Tests Fonctionnels
        run: |
          cd bin/Release-Windows-64
          ./MonApp.exe --run-tests

      - name: Upload Artifact
        uses: actions/upload-artifact@v3
        with:
          name: windows-build
          path: bin/Release-Windows-64/

  test-linux:
    name: Tests Linux
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install Dependencies
        run: |
          sudo apt-get update
          sudo apt-get install -y lazarus lcl libgtk2.0-dev

      - name: Compilation
        run: lazbuild --build-mode=Release-Linux-64 MonProjet.lpi

      - name: Tests Fonctionnels
        run: |
          cd bin/Release-Linux-64
          chmod +x MonApp
          ./MonApp --run-tests

      - name: Upload Artifact
        uses: actions/upload-artifact@v3
        with:
          name: linux-build
          path: bin/Release-Linux-64/

  test-macos:
    name: Tests macOS
    runs-on: macos-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install Dependencies
        run: brew install lazarus

      - name: Compilation
        run: lazbuild --build-mode=Release-Darwin-64 MonProjet.lpi

      - name: Tests Fonctionnels
        run: |
          cd bin/Release-Darwin-64
          chmod +x MonApp
          ./MonApp --run-tests

      - name: Upload Artifact
        uses: actions/upload-artifact@v3
        with:
          name: macos-build
          path: bin/Release-Darwin-64/
```

**Résultat :** À chaque push, les tests tournent automatiquement sur les 3 plateformes !

---

## 9. Checklist Complète de Test

### Avant Chaque Release

```markdown
## Checklist de Test Multi-Plateformes

### Préparation
- [ ] Code committé et versionné
- [ ] CHANGELOG.md mis à jour
- [ ] Version incrémentée

### Compilation
- [ ] Windows 64 bits : Compilé sans erreur
- [ ] Linux 64 bits : Compilé sans erreur
- [ ] macOS 64 bits : Compilé sans erreur
- [ ] Windows 32 bits (optionnel) : Compilé sans erreur

### Tests Fonctionnels - Windows
- [ ] Installation
- [ ] Démarrage
- [ ] Toutes fonctionnalités testées
- [ ] Fermeture propre
- [ ] Désinstallation

### Tests Fonctionnels - Linux
- [ ] Installation des dépendances
- [ ] Démarrage
- [ ] Toutes fonctionnalités testées
- [ ] Fermeture propre
- [ ] Nettoyage

### Tests Fonctionnels - macOS
- [ ] Installation
- [ ] Démarrage
- [ ] Toutes fonctionnalités testées
- [ ] Fermeture propre
- [ ] Désinstallation

### Tests Interface
- [ ] Windows : Layout correct
- [ ] Linux : Layout correct
- [ ] macOS : Layout correct
- [ ] Polices lisibles partout
- [ ] Icônes correctes partout

### Tests Performance
- [ ] Windows : Performance acceptable
- [ ] Linux : Performance acceptable
- [ ] macOS : Performance acceptable
- [ ] Pas de fuites mémoire

### Tests Compatibilité
- [ ] Windows 10 : Fonctionne
- [ ] Windows 11 : Fonctionne
- [ ] Ubuntu 20.04 : Fonctionne
- [ ] Ubuntu 22.04 : Fonctionne
- [ ] macOS 12+ : Fonctionne

### Documentation
- [ ] README à jour
- [ ] DEPENDENCIES documentées
- [ ] CHANGELOG complet
- [ ] Captures d'écran à jour

### Distribution
- [ ] Archives créées
- [ ] Installeurs testés
- [ ] Checksums générés
- [ ] Release notes rédigées

### Validation Finale
- [ ] Tous tests passés
- [ ] Aucun bug bloquant
- [ ] Approbation équipe
- [ ] Prêt pour publication
```

---

## 10. Bonnes Pratiques

### ✅ 1. Tester Tôt et Souvent

**Ne pas attendre la fin du développement !**

```
Mauvaise approche :
  Développer 6 mois → Tester toutes plateformes
  → Découvrir 50 bugs → 2 mois de corrections

Bonne approche :
  Développer 1 semaine → Tester toutes plateformes
  → Découvrir 2-3 bugs → 1 jour de corrections
  (Répéter chaque semaine)
```

### ✅ 2. Automatiser au Maximum

**Ce qui peut être automatisé doit l'être :**
- Compilation
- Tests fonctionnels de base
- Tests unitaires
- Génération de rapports

**Ce qui doit rester manuel :**
- Tests d'interface visuels
- Tests d'expérience utilisateur
- Validation finale

### ✅ 3. Documenter Tous les Bugs

**Template de bug report :**

```markdown
## Bug #XX : Titre court

### Plateforme
- OS : Ubuntu 22.04
- Version app : 1.2.0

### Description
Description détaillée du problème

### Reproduction
1. Ouvrir l'application
2. Cliquer sur Fichier > Ouvrir
3. Sélectionner un fichier .csv
4. → Erreur apparaît

### Comportement attendu
Le fichier devrait s'ouvrir

### Comportement observé
Message d'erreur : "Access denied"

### Logs/Screenshots
[Capture d'écran]

### Gravité
- [ ] Bloquante
- [x] Majeure
- [ ] Mineure
- [ ] Cosmétique

### Status
En cours d'investigation
```

### ✅ 4. Utiliser des Snapshots de VM

**Avant chaque session de test :**
1. Créer un snapshot de votre VM
2. Effectuer les tests
3. Si VM corrompue : restaurer le snapshot
4. Répéter

**Avantage :** Environnement de test toujours propre.

### ✅ 5. Tester avec des Données Réelles

**Ne pas tester qu'avec des données de test !**

Demandez à des utilisateurs réels de tester avec :
- Leurs vrais fichiers
- Leurs vraies bases de données
- Leurs vrais usages

**Vous découvrirez des cas d'usage inattendus.**

### ✅ 6. Tester les Cas Limites

**Exemples :**
- Fichier vide
- Fichier très gros (100 MB+)
- Fichier corrompu
- Nom de fichier avec caractères spéciaux
- Chemins très longs
- Pas de connexion internet
- Espace disque insuffisant
- Permissions insuffisantes

### ✅ 7. Maintenir une Matrice de Compatibilité

**Tableau à jour :**

| Version App | Win 10 | Win 11 | Ubuntu 20 | Ubuntu 22 | macOS 12 | macOS 13 |
|-------------|--------|--------|-----------|-----------|----------|----------|
| 1.0 | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ |
| 1.1 | ✅ | ✅ | ✅ | ✅ | ✅ | ⚠️ |
| 1.2 | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |

**Légende :**
- ✅ : Entièrement supporté et testé
- ⚠️ : Fonctionne avec limitations
- ❌ : Non supporté

---

## 11. Cas Pratique : Session de Test Complète

### Scénario : Test de l'application "TaskManager v2.0"

**Contexte :**
- Application de gestion de tâches
- Plateformes : Windows, Linux, macOS
- Base de données SQLite
- Interface graphique LCL

**Session de test typique :**

**Lundi : Développement Windows**
```
09:00 - Développement nouvelles fonctionnalités
12:00 - Tests locaux Windows
14:00 - Commit et push
```

**Mercredi : Tests Cross-Platform**
```
09:00 - Cross-compilation pour Linux et macOS
09:30 - Transfert vers VMs
10:00 - Tests Linux (Ubuntu 22.04 VM)
       → Bug découvert : Export PDF échoue
       → Note dans bug tracker
11:00 - Tests macOS (macOS VM)
       → Interface OK
       → Performance acceptable
12:00 - Retour sur Windows
13:00 - Correction bug Linux
14:00 - Re-test Linux
       → Export PDF maintenant OK
15:00 - Mise à jour documentation
```

**Vendredi : Validation Finale**
```
09:00 - Tests complets Windows
10:00 - Tests complets Linux
11:00 - Tests complets macOS
14:00 - Génération rapport de test
15:00 - Réunion équipe : GO pour release
16:00 - Création des archives de distribution
```

---

## 12. Résumé

Les tests multi-plateformes sont **indispensables** pour garantir la qualité de vos applications. Sans tests rigoureux, vous risquez de livrer des applications qui ne fonctionnent que sur votre machine.

**Points clés à retenir :**

1. **Compiler ≠ Fonctionner** : La compilation réussie ne garantit pas le bon fonctionnement
2. **Tester tôt et souvent** : Plus vous attendez, plus c'est difficile de corriger
3. **Automatiser** ce qui peut l'être : Tests unitaires, compilation, CI/CD
4. **Documenter** tous les résultats : Rapports de test, bugs découverts
5. **Utiliser des VMs** : C'est la solution la plus pratique et économique
6. **Tester sur versions réelles** : Pas uniquement sur la dernière version
7. **Prévoir du temps** : Les tests prennent du temps mais évitent les catastrophes

**Règle d'or :** Si vous ne pouvez pas tester sur une plateforme, ne prétendez pas la supporter !

**Investissement :**
- Temps : 20-30% du temps de développement
- Retour : Application fiable, utilisateurs satisfaits, réputation préservée

Dans le prochain chapitre, nous verrons comment créer des packages de distribution professionnels pour chaque plateforme, facilitant l'installation et la mise à jour pour vos utilisateurs.

⏭️ [Empaquetage et distribution](/19-developpement-multi-plateforme-pratique/09-empaquetage-distribution.md)
