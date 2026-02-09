🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 20.9 Tests Unitaires avec FPCUnit (Introduction)

## Introduction

Les tests unitaires sont une pratique fondamentale du développement professionnel moderne. Ils permettent de vérifier automatiquement que votre code fonctionne correctement et continuera de fonctionner après des modifications. C'est comme avoir un filet de sécurité pour votre code.

**Analogie simple :** Imaginez que vous construisez une maison. Les tests unitaires, c'est comme vérifier que chaque brique, chaque poutre, chaque fil électrique fonctionne correctement AVANT d'assembler le tout. Si une brique est défectueuse, vous le savez immédiatement, pas quand la maison s'effondre.

**Dans cette section, vous apprendrez à :**
- Comprendre les concepts de tests unitaires
- Installer et configurer FPCUnit
- Écrire vos premiers tests unitaires
- Organiser vos tests efficacement
- Automatiser l'exécution des tests
- Adopter les bonnes pratiques

---

## 1. Qu'est-ce qu'un Test Unitaire ?

### 1.1 Définition

**Test Unitaire :** Un petit morceau de code qui vérifie qu'une fonction ou méthode spécifique (une "unité") fonctionne correctement.

**Exemple concret :**

```pascal
// Fonction à tester
function Additionner(a, b: Integer): Integer;
begin
  Result := a + b;
end;

// Test unitaire correspondant
procedure TestAdditionner;
begin
  Assert(Additionner(2, 3) = 5, 'Erreur: 2 + 3 devrait donner 5');
  Assert(Additionner(0, 0) = 0, 'Erreur: 0 + 0 devrait donner 0');
  Assert(Additionner(-5, 5) = 0, 'Erreur: -5 + 5 devrait donner 0');
end;
```

### 1.2 Pourquoi Tester ?

**❌ Sans tests :**
```
1. Vous écrivez du code
2. Vous testez manuellement
3. Vous modifiez le code
4. Vous devez RE-tester TOUT manuellement
5. Vous oubliez un cas → Bug en production
```

**✅ Avec tests :**
```
1. Vous écrivez du code
2. Vous écrivez des tests automatiques
3. Vous modifiez le code
4. Vous relancez TOUS les tests en 1 clic
5. Confiance que rien n'est cassé
```

**Avantages des tests unitaires :**

| Avantage | Explication |
|----------|-------------|
| 🛡️ **Filet de sécurité** | Détecte les régressions immédiatement |
| 📝 **Documentation** | Les tests montrent comment utiliser le code |
| 🏗️ **Meilleur design** | Code testable = code bien structuré |
| 😌 **Confiance** | Modifier le code sans peur de tout casser |
| ⚡ **Rapidité** | Tests automatiques > tests manuels |
| 🐛 **Débogage** | Isole précisément où est le problème |

### 1.3 Tests Unitaires vs Autres Types de Tests

```
┌─────────────────────────────────────────┐
│  APPLICATION COMPLÈTE                   │
│  ┌───────────────────────────────────┐  │
│  │  Tests End-to-End (E2E)          │  │ ← Teste toute l'application
│  │  (Interface → BDD → Résultat)    │  │
│  └───────────────────────────────────┘  │
│       ▲                                 │
│       │                                 │
│  ┌───────────────────────────────────┐  │
│  │  Tests d'Intégration              │  │ ← Teste plusieurs composants
│  │  (Module A + Module B)            │  │   ensemble
│  └───────────────────────────────────┘  │
│       ▲                                 │
│       │                                 │
│  ┌───────────────────────────────────┐  │
│  │  Tests Unitaires                  │  │ ← Teste chaque fonction
│  │  (Fonction isolée)                │  │   individuellement (NOTRE FOCUS)
│  └───────────────────────────────────┘  │
└─────────────────────────────────────────┘
```

**Focus de cette section :** Tests unitaires uniquement.

---

## 2. Présentation de FPCUnit

### 2.1 Qu'est-ce que FPCUnit ?

**FPCUnit** est le framework de tests unitaires officiel pour FreePascal. C'est l'équivalent de :
- JUnit (Java)
- NUnit (C#)
- PyTest (Python)

**Caractéristiques :**
- ✅ Intégré à FreePascal
- ✅ Multi-plateforme (Windows, Linux, macOS)
- ✅ Interface console ou GUI
- ✅ Rapports détaillés
- ✅ Facile à utiliser

### 2.2 Architecture FPCUnit

```
┌──────────────────────────────────────┐
│  Votre Programme                     │
│  ┌────────────┐  ┌────────────┐     │
│  │ Fonction A │  │ Fonction B │     │
│  └────────────┘  └────────────┘     │
└───────────▲──────────────▲───────────┘
            │              │
            │ Teste        │ Teste
            │              │
┌───────────┴──────────────┴───────────┐
│  Tests FPCUnit                       │
│  ┌────────────┐  ┌────────────┐     │
│  │  Test A1   │  │  Test B1   │     │
│  │  Test A2   │  │  Test B2   │     │
│  └────────────┘  └────────────┘     │
└──────────────────────────────────────┘
            │
            ▼
    ┌──────────────┐
    │   Résultats  │
    │   ✓ ou ✗     │
    └──────────────┘
```

### 2.3 Concepts Clés

**Test Case (Cas de Test) :**
- Une classe qui contient plusieurs tests
- Hérite de `TTestCase`
- Regroupe les tests liés

**Test Method (Méthode de Test) :**
- Une procédure qui effectue un test spécifique
- Commence généralement par `Test`
- Utilise des assertions

**Assertion :**
- Vérification d'une condition
- Si faux → test échoue
- Exemples : `AssertEquals`, `AssertTrue`, `AssertNotNull`

**Test Suite :**
- Collection de tests
- Peut contenir plusieurs TestCases
- Exécutée en bloc

---

## 3. Installation et Configuration

### 3.1 Vérifier que FPCUnit est Disponible

FPCUnit est normalement inclus avec FreePascal.

**Vérification :**

```bash
# Rechercher les fichiers FPCUnit
find /usr/lib/fpc -name "*fpcunit*"
# ou sous Windows
dir /s C:\fpc\*fpcunit*
```

**Si installé, vous devriez voir :**
```
fpcunit.ppu
fpcunit.o
testutils.ppu
...
```

### 3.2 Premier Projet de Test dans Lazarus

**Méthode 1 : Projet Console Manuel**

1. **Créer un nouveau projet**
   - **Fichier** → **Nouveau** → **Projet**
   - Choisir **Programme** (console)

2. **Ajouter FPCUnit dans les uses**

```pascal
program MesTests;

{$mode objfpc}{$h+}

uses
  Classes, SysUtils,
  fpcunit, testregistry, consoletestrunner;

begin
  // Configuration et exécution des tests
  RunRegisteredTests;
end.
```

**Méthode 2 : Utiliser l'Assistant Lazarus**

1. **Fichier** → **Nouveau** → **Projet**
2. Choisir **FPCUnit Test Application**
3. Suivre l'assistant

**Avantage :** Crée automatiquement la structure de base.

### 3.3 Structure de Projet Recommandée

```
MonProjet/
├── src/                    ← Code source de l'application
│   ├── calculatrice.pas
│   └── utils.pas
├── tests/                  ← Tests unitaires
│   ├── TestCalculatrice.pas
│   ├── TestUtils.pas
│   └── AllTests.lpr        ← Programme principal des tests
└── MonProjet.lpi          ← Projet principal
```

---

## 4. Premier Test Simple

### 4.1 Code à Tester

**Fichier : `calculatrice.pas`**

```pascal
unit Calculatrice;

{$mode objfpc}{$H+}

interface

function Additionner(a, b: Integer): Integer;
function Soustraire(a, b: Integer): Integer;
function Multiplier(a, b: Integer): Integer;
function Diviser(a, b: Double): Double;

implementation

uses SysUtils;

function Additionner(a, b: Integer): Integer;
begin
  Result := a + b;
end;

function Soustraire(a, b: Integer): Integer;
begin
  Result := a - b;
end;

function Multiplier(a, b: Integer): Integer;
begin
  Result := a * b;
end;

function Diviser(a, b: Double): Double;
begin
  if b = 0 then
    raise Exception.Create('Division par zéro');
  Result := a / b;
end;

end.
```

### 4.2 Créer le Test

**Fichier : `TestCalculatrice.pas`**

```pascal
unit TestCalculatrice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Calculatrice;

type
  TTestCalculatrice = class(TTestCase)
  published  // Les méthodes published sont auto-détectées comme tests
    procedure TestAdditionner;
    procedure TestSoustraire;
    procedure TestMultiplier;
    procedure TestDiviser;
    procedure TestDivisionParZero;
  end;

implementation

procedure TTestCalculatrice.TestAdditionner;
begin
  AssertEquals('2 + 3 devrait être 5', 5, Additionner(2, 3));
  AssertEquals('0 + 0 devrait être 0', 0, Additionner(0, 0));
  AssertEquals('-5 + 5 devrait être 0', 0, Additionner(-5, 5));
  AssertEquals('-10 + -5 devrait être -15', -15, Additionner(-10, -5));
end;

procedure TTestCalculatrice.TestSoustraire;
begin
  AssertEquals('5 - 3 devrait être 2', 2, Soustraire(5, 3));
  AssertEquals('0 - 0 devrait être 0', 0, Soustraire(0, 0));
  AssertEquals('10 - (-5) devrait être 15', 15, Soustraire(10, -5));
end;

procedure TTestCalculatrice.TestMultiplier;
begin
  AssertEquals('3 * 4 devrait être 12', 12, Multiplier(3, 4));
  AssertEquals('0 * 100 devrait être 0', 0, Multiplier(0, 100));
  AssertEquals('-5 * 3 devrait être -15', -15, Multiplier(-5, 3));
end;

procedure TTestCalculatrice.TestDiviser;
begin
  AssertEquals('10 / 2 devrait être 5', 5.0, Diviser(10, 2), 0.001);  // 0.001 = tolérance pour comparaison de réels
  AssertEquals('7 / 2 devrait être 3.5', 3.5, Diviser(7, 2), 0.001);
end;

procedure TTestCalculatrice.TestDivisionParZero;
begin
  try
    Diviser(10, 0);
    Fail('Une exception devrait être levée');
  except
    on E: Exception do
      AssertEquals('Division par zéro', E.Message);
  end;
end;

initialization
  RegisterTest(TTestCalculatrice);  // Enregistre la classe de tests auprès du runner

end.
```

### 4.3 Programme Principal des Tests

**Fichier : `AllTests.lpr`**

```pascal
program AllTests;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  fpcunit, testregistry, consoletestrunner,  // consoletestrunner fournit TTestRunner
  TestCalculatrice;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  try
    Application.Initialize;
    Application.Title := 'Tests Unitaires - Calculatrice';
    Application.Run;
  finally
    Application.Free;
  end;
end.
```

### 4.4 Exécution des Tests

**Compilation et exécution :**

```bash
# Compiler
fpc AllTests.lpr

# Exécuter
./AllTests
```

**Sortie attendue :**

```
Tests Unitaires - Calculatrice
Time: 0.001

OK: 5 tests
```

**Si un test échoue :**

```
Tests Unitaires - Calculatrice
Time: 0.002

FAILURES:
  Test "TestAdditionner" failed:
    Expected: 5 but was: 6

ERRORS:
  0

Ran: 5 tests
Failures: 1
Errors: 0
```

---

## 5. Assertions Disponibles

### 5.1 Assertions de Base

**AssertEquals :**

```pascal
// Pour entiers
AssertEquals('Message', Attendu, Obtenu);
AssertEquals('2 + 2 = 4', 4, Additionner(2, 2));

// Pour réels (avec tolérance)
AssertEquals('Message', Attendu, Obtenu, Delta);
AssertEquals('Division', 3.5, Diviser(7, 2), 0.001);

// Pour chaînes
AssertEquals('Noms identiques', 'Jean', Nom);
```

**AssertTrue / AssertFalse :**

```pascal
AssertTrue('Devrait être vrai', Condition);
AssertTrue('x > 0', x > 0);

AssertFalse('Devrait être faux', Condition);
AssertFalse('Liste vide', Liste.Count > 0);
```

**AssertNull / AssertNotNull :**

```pascal
AssertNull('Devrait être nil', Pointeur);
AssertNull('Pas encore créé', MonObjet);

AssertNotNull('Ne devrait pas être nil', Pointeur);
AssertNotNull('Objet créé', MonObjet);
```

### 5.2 Assertions pour Exceptions

**Vérifier qu'une exception est levée :**

```pascal
procedure TestExceptionAttendue;
begin
  AssertException('Division par zéro devrait lever exception',
                  EDivByZero,
                  @FonctionQuiDivise);
end;

// Ou manuellement
procedure TestExceptionManuelle;
begin
  try
    FonctionDangereuse;
    Fail('Une exception devrait être levée');
  except
    on E: EMonException do
      ; // OK, attendu
  end;
end;
```

### 5.3 Autres Assertions Utiles

**Fail :**

```pascal
procedure TestLogique;
begin
  if not ConditionComplexe then
    Fail('La condition devrait être vraie');
end;
```

**AssertSame / AssertNotSame :**

```pascal
AssertSame('Même objet', Objet1, Objet2);
AssertNotSame('Objets différents', Objet1, Objet2);
```

---

## 6. Cycle de Vie d'un Test

### 6.1 Méthodes Setup et TearDown

**Problème :** Préparer/nettoyer l'environnement pour chaque test.

**Solution :**

```pascal
type
  TTestAvecSetup = class(TTestCase)
  private
    FListe: TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAjout;
    procedure TestSuppression;
  end;

procedure TTestAvecSetup.SetUp;
begin
  // Appelé AVANT chaque test
  FListe := TStringList.Create;
  FListe.Add('Element 1');
  FListe.Add('Element 2');
end;

procedure TTestAvecSetup.TearDown;
begin
  // Appelé APRÈS chaque test
  FreeAndNil(FListe);
end;

procedure TTestAvecSetup.TestAjout;
begin
  FListe.Add('Element 3');
  AssertEquals('3 éléments', 3, FListe.Count);
end;

procedure TTestAvecSetup.TestSuppression;
begin
  FListe.Delete(0);
  AssertEquals('1 élément restant', 1, FListe.Count);
end;
```

**Séquence d'exécution :**

```
Pour chaque test:
1. SetUp           ← Prépare l'environnement
2. Test            ← Exécute le test
3. TearDown        ← Nettoie
```

**Exemple complet :**

```
SetUp → TestAjout → TearDown
SetUp → TestSuppression → TearDown
```

### 6.2 Initialisation Unique pour Toute la Classe

**FPCUnit ne fournit pas de mécanisme `SetUpClass`/`TearDownClass` intégré** (contrairement à JUnit/NUnit). Pour initialiser une ressource UNE FOIS pour toute une suite de tests, utilisez la section `initialization`/`finalization` de l'unité de test :

```pascal
unit TestAvecBDD;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type
  TTestAvecInitGlobale = class(TTestCase)
  published
    procedure Test1;
    procedure Test2;
  end;

implementation

var
  BaseDeDonnees: TDatabase;

procedure TTestAvecInitGlobale.Test1;
begin
  // Utilise BaseDeDonnees, initialisée une seule fois
  AssertNotNull('Base de données initialisée', BaseDeDonnees);
end;

procedure TTestAvecInitGlobale.Test2;
begin
  AssertTrue('Base de données connectée', BaseDeDonnees.Connected);
end;

initialization
  // Appelé UNE FOIS au chargement de l'unité
  BaseDeDonnees := TDatabase.Create;
  BaseDeDonnees.Connect('test.db');
  RegisterTest(TTestAvecInitGlobale);

finalization
  // Appelé UNE FOIS à la fin du programme
  BaseDeDonnees.Disconnect;
  FreeAndNil(BaseDeDonnees);

end.
```

**Usage :** Pour ressources coûteuses (connexions BDD, fichiers volumineux).

---

## 7. Organisation des Tests

### 7.1 Un TestCase par Classe/Module

**Structure recommandée :**

```pascal
// Pour la classe TClient
unit TestClient;
type
  TTestClient = class(TTestCase)
  published
    procedure TestCreation;
    procedure TestValidation;
    procedure TestCalculAge;
  end;

// Pour la classe TCommande
unit TestCommande;
type
  TTestCommande = class(TTestCase)
  published
    procedure TestAjouterArticle;
    procedure TestCalculerTotal;
    procedure TestAnnuler;
  end;
```

### 7.2 Nommer les Tests Clairement

**❌ Mauvais noms :**

```pascal
procedure Test1;
procedure Test2;
procedure TestFonction;
```

**✅ Bons noms :**

```pascal
procedure TestAdditionnerDeuxNombresPositifs;
procedure TestDivisionParZeroLeveException;
procedure TestListeVideRetourneTailleDeux;
```

**Convention de nommage :**
```
Test[Fonction]_[Scenario]_[ResultatAttendu]
```

**Exemples :**
```pascal
TestCalculerTotal_PanierVide_RetourneZero
TestAjouterClient_NomVide_LeveException
TestConnexion_MauvauxMotDePasse_RetourneFalse
```

### 7.3 Organiser en Suites

**Créer une suite de tests :**

```pascal
uses
  testregistry,
  TestCalculatrice, TestClient, TestCommande;

initialization
  // Suite principale
  RegisterTest('Mathématiques', TTestCalculatrice.Suite);
  RegisterTest('Gestion', TTestClient.Suite);
  RegisterTest('Gestion', TTestCommande.Suite);
```

**Résultat dans le runner :**
```
└─ Mathématiques
   └─ TTestCalculatrice
      ├─ TestAdditionner
      └─ TestSoustraire
└─ Gestion
   ├─ TTestClient
   │  └─ TestCreation
   └─ TTestCommande
      └─ TestAjouterArticle
```

---

## 8. Tests avec Dépendances

### 8.1 Problème des Dépendances Externes

**Exemple problématique :**

```pascal
function EnvoyerEmail(const destinataire, sujet, corps: String): Boolean;
begin
  // Envoie VRAIMENT un email !
  Result := SMTPClient.Send(destinataire, sujet, corps);
end;
```

**Problème :** Comment tester sans envoyer des milliers d'emails réels ?

### 8.2 Solution : Mock Objects (Objets Simulés)

**Version testable :**

```pascal
type
  IEmailSender = interface
    function Send(const dest, subj, body: String): Boolean;
  end;

  // Vraie implémentation
  TRealEmailSender = class(TInterfacedObject, IEmailSender)
    function Send(const dest, subj, body: String): Boolean;
  end;

  // Mock pour tests
  TMockEmailSender = class(TInterfacedObject, IEmailSender)
  private
    FEmailsEnvoyes: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function Send(const dest, subj, body: String): Boolean;
    function GetNombreEmails: Integer;
  end;

constructor TMockEmailSender.Create;
begin
  inherited;
  FEmailsEnvoyes := TStringList.Create;
end;

destructor TMockEmailSender.Destroy;
begin
  FEmailsEnvoyes.Free;
  inherited;
end;

function TMockEmailSender.Send(const dest, subj, body: String): Boolean;
begin
  // Ne fait QUE enregistrer
  FEmailsEnvoyes.Add(Format('%s|%s|%s', [dest, subj, body]));
  Result := True;
end;

function TMockEmailSender.GetNombreEmails: Integer;
begin
  Result := FEmailsEnvoyes.Count;
end;

// Test
procedure TTestEmail.TestEnvoiNotification;
var
  mockSender: TMockEmailSender;
begin
  mockSender := TMockEmailSender.Create;
  try
    // Utiliser le mock
    EnvoyerNotification('jean@example.com', mockSender);

    // Vérifier
    AssertEquals('Un email envoyé', 1, mockSender.GetNombreEmails);
  finally
    mockSender.Free;
  end;
end;
```

---

## 9. Exécution et Rapports

### 9.1 Runner Console

**Programme simple :**

```pascal
program RunTests;
uses
  fpcunit, consoletestrunner, testregistry,
  TestCalculatrice;

begin
  RunRegisteredTests;
end.
```

**Sortie :**
```
Time: 0.002

OK: 5 tests
```

### 9.2 Runner avec Options

**Sortie détaillée :**

```pascal
var
  App: TTestRunner;
begin
  App := TTestRunner.Create(nil);
  try
    App.Initialize;

    // Options
    DefaultFormat := fPlain;  // Format texte
    ShowProgress := True;     // Afficher progression

    App.Run;
  finally
    App.Free;
  end;
end.
```

**Sortie détaillée :**
```
Running tests...
.....

Time: 0.002

OK: 5 tests
Successes: 5
Failures: 0
Errors: 0
```

### 9.3 Sortie XML pour CI/CD

**Générer rapport XML :**

```pascal
uses
  fpcunit, consoletestrunner, xmltestreport;

var
  App: TTestRunner;
begin
  App := TTestRunner.Create(nil);
  try
    // Exécuter avec --format=xml en ligne de commande
    // ou configurer programmatiquement :
    App.Initialize;
    App.Run;
  finally
    App.Free;
  end;
end.
```

**Exécution en ligne de commande :**
```bash
./MesTests --format=xml --file=test-results.xml
```

**Fichier `test-results.xml` généré :**

```xml
<?xml version="1.0"?>
<TestResults>
  <TestSuite Name="AllTests" Tests="5" Failures="0" Errors="0" Time="0.002">
    <TestCase Name="TestAdditionner" Status="Passed" Time="0.001"/>
    <TestCase Name="TestSoustraire" Status="Passed" Time="0.000"/>
    <!-- ... -->
  </TestSuite>
</TestResults>
```

---

## 10. Test-Driven Development (TDD)

### 10.1 Principe du TDD

**Cycle TDD (Red-Green-Refactor) :**

```
1. RED (Rouge)
   └─ Écrire un test qui ÉCHOUE

2. GREEN (Vert)
   └─ Écrire le code MINIMAL pour passer le test

3. REFACTOR (Refactoriser)
   └─ Améliorer le code sans changer le comportement

4. Répéter
```

### 10.2 Exemple TDD Complet

**Étape 1 : Test d'abord (RED)**

```pascal
// Test écrit AVANT le code
procedure TTestPanier.TestAjouterArticle;
var
  panier: TPanier;
begin
  panier := TPanier.Create;
  try
    panier.Ajouter('Pomme', 1.5);
    AssertEquals('Un article', 1, panier.NombreArticles);
  finally
    panier.Free;
  end;
end;
```

**Compilation échoue :** TPanier n'existe pas encore !

**Étape 2 : Code minimal (GREEN)**

```pascal
type
  TPanier = class
  private
    FArticles: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Ajouter(const nom: String; prix: Double);
    function NombreArticles: Integer;
  end;

constructor TPanier.Create;
begin
  FArticles := TList.Create;
end;

destructor TPanier.Destroy;
begin
  FArticles.Free;
  inherited;
end;

procedure TPanier.Ajouter(const nom: String; prix: Double);
begin
  FArticles.Add(nil);  // Version minimale !
end;

function TPanier.NombreArticles: Integer;
begin
  Result := FArticles.Count;
end;
```

**Test passe :** ✓ Vert

**Étape 3 : Refactoriser**

```pascal
// Améliorer sans changer le comportement
type
  TArticle = class
    Nom: String;
    Prix: Double;
  end;

procedure TPanier.Ajouter(const nom: String; prix: Double);
var
  article: TArticle;
begin
  article := TArticle.Create;
  article.Nom := nom;
  article.Prix := prix;
  FArticles.Add(article);
end;
```

**Test passe toujours :** ✓ Vert

**Étape 4 : Nouveau test**

```pascal
procedure TTestPanier.TestCalculerTotal;
var
  panier: TPanier;
begin
  panier := TPanier.Create;
  try
    panier.Ajouter('Pomme', 1.5);
    panier.Ajouter('Pain', 2.0);
    AssertEquals('Total 3.5', 3.5, panier.CalculerTotal, 0.01);
  finally
    panier.Free;
  end;
end;
```

**Retour à RED** → Implémenter `CalculerTotal` → GREEN → REFACTOR...

### 10.3 Avantages du TDD

| Avantage | Explication |
|----------|-------------|
| 🎯 **Clarté des specs** | Le test définit exactement ce qui est attendu |
| 🏗️ **Meilleur design** | Force à penser à l'API avant l'implémentation |
| 📝 **Documentation vivante** | Les tests montrent comment utiliser le code |
| 🛡️ **Couverture 100%** | Tout le code est testé par construction |
| 🐛 **Moins de bugs** | Les bugs sont détectés immédiatement |

---

## 11. Bonnes Pratiques

### 11.1 Principes FIRST

Les bons tests sont **FIRST** :

**F - Fast (Rapides)**
- Les tests doivent s'exécuter rapidement
- Éviter les dépendances lentes (BDD, réseau)
- Objectif : < 1 seconde pour toute la suite

**I - Independent (Indépendants)**
- Chaque test doit fonctionner seul
- L'ordre d'exécution ne doit pas importer
- Pas de dépendance entre tests

**R - Repeatable (Répétables)**
- Même résultat à chaque exécution
- Pas de dépendance à l'heure, dates aléatoires
- Environnement contrôlé

**S - Self-Validating (Auto-validants)**
- Le test doit passer ou échouer clairement
- Pas d'inspection manuelle nécessaire
- Résultat booléen : succès ou échec

**T - Timely (Au bon moment)**
- Écrits au bon moment (idéalement avant le code avec TDD)
- Pas après coup quand tout est fini

### 11.2 Ce qu'il Faut Tester

**✅ À tester :**
- Logique métier
- Calculs
- Validations
- Transformations de données
- Gestion des cas limites
- Gestion des erreurs

**❌ Généralement pas nécessaire :**
- Getters/setters simples
- Constructeurs triviaux
- Code de framework/bibliothèque
- Code généré automatiquement

### 11.3 Arrange-Act-Assert (AAA)

**Structure de test recommandée :**

```pascal
procedure TestExemple;
var
  // ARRANGE (Préparer)
  calculatrice: TCalculatrice;
  resultat: Integer;
begin
  // ARRANGE
  calculatrice := TCalculatrice.Create;
  try
    // ACT (Agir)
    resultat := calculatrice.Additionner(2, 3);

    // ASSERT (Vérifier)
    AssertEquals('2 + 3 = 5', 5, resultat);
  finally
    calculatrice.Free;
  end;
end;
```

**Bénéfice :** Structure claire et lisible.

### 11.4 Un Concept par Test

**❌ Mauvais : Teste trop de choses**

```pascal
procedure TestToutEnUn;
begin
  AssertEquals(5, Additionner(2, 3));
  AssertEquals(1, Soustraire(3, 2));
  AssertEquals(6, Multiplier(2, 3));
  AssertEquals(2.0, Diviser(6, 3), 0.01);
end;
```

**Problème :** Si ça échoue, on ne sait pas quelle opération pose problème.

**✅ Bon : Un test par concept**

```pascal
procedure TestAdditionner;
begin
  AssertEquals(5, Additionner(2, 3));
end;

procedure TestSoustraire;
begin
  AssertEquals(1, Soustraire(3, 2));
end;

procedure TestMultiplier;
begin
  AssertEquals(6, Multiplier(2, 3));
end;

procedure TestDiviser;
begin
  AssertEquals(2.0, Diviser(6, 3), 0.01);
end;
```

---

## 12. Intégration CI/CD

### 12.1 Script d'Automatisation

**Bash (Linux) :**

```bash
#!/bin/bash
# run_tests.sh

echo "=== Exécution des tests unitaires ==="

# Compiler les tests
fpc -B tests/AllTests.lpr

if [ $? -ne 0 ]; then
    echo "ERREUR: Compilation échouée"
    exit 1
fi

# Exécuter les tests
./tests/AllTests --format=xml --output=test-results.xml

if [ $? -ne 0 ]; then
    echo "ÉCHEC: Des tests ont échoué"
    cat test-results.xml
    exit 1
fi

echo "SUCCÈS: Tous les tests passent"
exit 0
```

**Batch (Windows) :**

```batch
@echo off
REM run_tests.bat

echo === Execution des tests unitaires ===

REM Compiler
fpc -B tests\AllTests.lpr

if errorlevel 1 (
    echo ERREUR: Compilation echouee
    exit /b 1
)

REM Executer
tests\AllTests.exe --format=xml --output=test-results.xml

if errorlevel 1 (
    echo ECHEC: Des tests ont echoue
    type test-results.xml
    exit /b 1
)

echo SUCCES: Tous les tests passent
exit /b 0
```

### 12.2 GitLab CI

**.gitlab-ci.yml :**

```yaml
stages:
  - test

unit_tests:
  stage: test
  image: registry.gitlab.com/freepascal/fpc:latest
  script:
    - fpc -B tests/AllTests.lpr
    - ./tests/AllTests --format=xml --output=test-results.xml
  artifacts:
    when: always
    reports:
      junit: test-results.xml
```

### 12.3 GitHub Actions

**.github/workflows/tests.yml :**

```yaml
name: Tests Unitaires

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest

    steps:
    - uses: actions/checkout@v3

    - name: Installer FreePascal
      run: |
        sudo apt-get update
        sudo apt-get install -y fpc

    - name: Compiler les tests
      run: fpc -B tests/AllTests.lpr

    - name: Exécuter les tests
      run: ./tests/AllTests --format=xml --output=test-results.xml

    - name: Publier les résultats
      uses: EnricoMi/publish-unit-test-result-action@v2
      if: always()
      with:
        files: test-results.xml
```

---

## 13. Multi-plateforme : Windows vs Linux

### 13.1 Chemins de Fichiers

**Code portable :**

```pascal
procedure TTestFichier.SetUp;
begin
  FCheminTest := GetTempDir + 'test.txt';
  // Fonctionne sur Windows ET Linux
end;
```

### 13.2 Fins de Ligne

**Test portable :**

```pascal
procedure TestLireFichier;
var
  contenu: String;
begin
  contenu := LireFichier('test.txt');

  // Normaliser les fins de ligne
  contenu := StringReplace(contenu, #13#10, #10, [rfReplaceAll]);
  contenu := StringReplace(contenu, #13, #10, [rfReplaceAll]);

  AssertEquals('Contenu', 'Ligne1'#10'Ligne2', contenu);
end;
```

### 13.3 Tests Spécifiques à une Plateforme

```pascal
procedure TTestPlateforme.TestFonctionWindows;
begin
  {$IFDEF WINDOWS}
  // Test spécifique Windows
  AssertTrue('Fonctionnalité Windows', FonctionWindows);
  {$ELSE}
  // Sauter sous Linux
  Ignore('Test Windows uniquement');
  {$ENDIF}
end;
```

---

## 14. Récapitulatif

### 14.1 Checklist des Tests Unitaires

**Pour chaque fonction importante :**
- [ ] Au moins un test du cas nominal
- [ ] Tests des cas limites (0, valeurs extrêmes)
- [ ] Tests des cas d'erreur (exceptions)
- [ ] Tests avec valeurs négatives si applicable
- [ ] Tests avec null/nil si applicable

**Organisation :**
- [ ] Un TestCase par classe/module
- [ ] Noms de tests explicites
- [ ] Structure AAA (Arrange-Act-Assert)
- [ ] Tests indépendants

**Automatisation :**
- [ ] Script d'exécution des tests
- [ ] Intégration CI/CD
- [ ] Rapport de couverture

### 14.2 Commandes Essentielles

**Compiler et exécuter :**
```bash
fpc AllTests.lpr
./AllTests
```

**Avec options :**
```bash
./AllTests --format=plain --progress
./AllTests --format=xml --output=results.xml
```

**Dans un script :**
```bash
./AllTests && echo "Tests OK" || echo "Tests ÉCHOUÉS"
```

---

## Conclusion

Les tests unitaires sont un investissement initial qui se rentabilise rapidement. Ils vous permettent de :
- ✅ Modifier le code avec confiance
- ✅ Détecter les bugs immédiatement
- ✅ Documenter comment utiliser le code
- ✅ Maintenir la qualité dans le temps

**Points clés à retenir :**

1. **Commencez simple** - Même quelques tests valent mieux que rien
2. **Testez en écrivant le code** - Pas après coup
3. **Gardez les tests rapides** - Pour les exécuter souvent
4. **Un test = un concept** - Tests simples et clairs
5. **Automatisez** - Intégrez dans votre workflow

**Citation finale :**
> "Code without tests is broken by design." - Jacob Kaplan-Moss

Avec FPCUnit, vous avez tous les outils pour écrire des tests professionnels. Commencez dès aujourd'hui à tester vos nouvelles fonctions, et progressivement ajoutez des tests à votre code existant.

**Bon courage et bons tests !** ✅

---

**Multi-plateforme Windows/Ubuntu** ✓  
**Formation FreePascal/Lazarus - Niveau Débutant à Intermédiaire**  

⏭️ [Sommaire](/SOMMAIRE.md)
