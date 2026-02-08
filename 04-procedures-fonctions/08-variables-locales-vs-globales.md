🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.8 Variables locales vs globales

## Introduction

Lorsqu'on programme avec des procédures et fonctions, il est essentiel de comprendre où déclarer ses variables. Il existe deux grands types de variables : les **variables globales** et les **variables locales**. Chacune a ses avantages, ses inconvénients et ses règles d'utilisation.

## Qu'est-ce qu'une variable globale ?

Une **variable globale** est déclarée dans la section principale du programme, **avant** toutes les procédures et fonctions. Elle est accessible depuis n'importe quelle partie du programme.

### Syntaxe

```pascal
{$mode objfpc}{$H+}
program Exemple;

var
  variableGlobale: Integer;  // Variable globale

procedure MaProcedure;
begin
  variableGlobale := 10;  // Accessible ici
end;

function MaFonction: Integer;
begin
  Result := variableGlobale + 5;  // Accessible ici aussi
end;

begin
  variableGlobale := 100;  // Accessible dans le programme principal
  MaProcedure;
  WriteLn(MaFonction);
end.
```

### Caractéristiques des variables globales

- **Portée** : tout le programme (procédures, fonctions, programme principal)
- **Durée de vie** : du début à la fin du programme
- **Visibilité** : accessible partout après sa déclaration
- **Initialisation** : automatiquement initialisée à 0 (ou valeur par défaut)

## Qu'est-ce qu'une variable locale ?

Une **variable locale** est déclarée à l'intérieur d'une procédure ou fonction. Elle n'existe que dans cette procédure/fonction.

### Syntaxe

```pascal
procedure MaProcedure;
var
  variableLocale: Integer;  // Variable locale
begin
  variableLocale := 10;
  WriteLn(variableLocale);
end;

begin
  MaProcedure;
  // WriteLn(variableLocale);  // ❌ ERREUR : variable inconnue ici
end.
```

### Caractéristiques des variables locales

- **Portée** : uniquement dans la procédure/fonction où elle est déclarée
- **Durée de vie** : créée à l'appel, détruite à la sortie
- **Visibilité** : invisible en dehors de sa procédure/fonction
- **Initialisation** : non initialisée automatiquement (contient n'importe quoi)

## Comparaison visuelle

```pascal
program Demonstration;

var
  globale: Integer;

procedure Proc1;
var
  locale1: Integer;
begin
  globale := 5;
  locale1 := 10;
end;

procedure Proc2;
var
  locale2: Integer;
begin
  globale := 15;
  // locale1 := 20;  // Erreur : locale1 n'existe pas ici
  locale2 := 25;
end;

begin
  globale := 100;
  // locale1 := 50;  // Erreur : locale1 n'existe pas ici
  // locale2 := 75;  // Erreur : locale2 n'existe pas ici
end.
```

## Exemples pratiques

### 1. Variable globale : compteur partagé

```pascal
program CompteurGlobal;

var
  compteur: Integer;  // Variable globale

procedure Incrementer;
begin
  compteur := compteur + 1;
end;

procedure Afficher;
begin
  WriteLn('Compteur : ', compteur);
end;

begin
  compteur := 0;
  Afficher;       // Compteur : 0

  Incrementer;
  Afficher;       // Compteur : 1

  Incrementer;
  Incrementer;
  Afficher;       // Compteur : 3
end.
```

### 2. Variables locales : calculs indépendants

```pascal
procedure CalculerCarre;
var
  nombre, resultat: Integer;  // Variables locales
begin
  nombre := 5;
  resultat := nombre * nombre;
  WriteLn('Carré de ', nombre, ' = ', resultat);
end;

procedure CalculerCube;
var
  nombre, resultat: Integer;  // Variables locales différentes
begin
  nombre := 3;
  resultat := nombre * nombre * nombre;
  WriteLn('Cube de ', nombre, ' = ', resultat);
end;

begin
  CalculerCarre;  // Carré de 5 = 25
  CalculerCube;   // Cube de 3 = 27
end;
```

**Note :** Les variables `nombre` et `resultat` dans les deux procédures sont **complètement indépendantes** car elles sont locales.

### 3. Mélange de variables locales et globales

```pascal
program MelangeVariables;

var
  total: Integer;  // Globale

procedure AjouterDix;
var
  valeur: Integer;  // Locale
begin
  valeur := 10;
  total := total + valeur;
end;

procedure AjouterVingt;
var
  valeur: Integer;  // Locale (différente de celle de AjouterDix)
begin
  valeur := 20;
  total := total + valeur;
end;

begin
  total := 0;
  WriteLn('Total initial : ', total);      // 0

  AjouterDix;
  WriteLn('Après AjouterDix : ', total);   // 10

  AjouterVingt;
  WriteLn('Après AjouterVingt : ', total); // 30
end.
```

## Le masquage (shadowing)

Quand une variable locale a le **même nom** qu'une variable globale, la variable locale **masque** (cache) la variable globale dans sa portée.

### Exemple de masquage

```pascal
program Masquage;

var
  nombre: Integer;  // Variable globale

procedure Test;
var
  nombre: Integer;  // Variable locale (même nom)
begin
  nombre := 50;  // Modifie la variable LOCALE
  WriteLn('Dans Test : ', nombre);  // 50
end;

begin
  nombre := 100;  // Modifie la variable GLOBALE
  WriteLn('Avant Test : ', nombre);  // 100

  Test;

  WriteLn('Après Test : ', nombre);  // 100 (inchangé !)
end.
```

**Résultat :**
```
Avant Test : 100
Dans Test : 50
Après Test : 100
```

**Explication :** Dans la procédure `Test`, quand on écrit `nombre`, on fait référence à la variable locale, pas à la globale. Les deux variables sont complètement indépendantes.

### Accéder à la variable globale masquée

En Pascal standard, il n'existe pas de moyen direct d'accéder à une variable globale masquée. **Solution : éviter le masquage** en utilisant des noms différents.

```pascal
var
  nombreGlobal: Integer;  // Nom explicite

procedure Test;
var
  nombreLocal: Integer;  // Nom différent
begin
  nombreLocal := 50;
  nombreGlobal := 75;  // Maintenant on peut accéder aux deux
  WriteLn('Local : ', nombreLocal, ', Global : ', nombreGlobal);
end;
```

## Durée de vie des variables

### Variables globales

Les variables globales existent pendant **toute la durée d'exécution** du programme.

```pascal
var
  compteur: Integer;

procedure Incrementer;
begin
  compteur := compteur + 1;  // La valeur persiste entre les appels
end;

begin
  compteur := 0;
  Incrementer;  // compteur = 1
  Incrementer;  // compteur = 2
  Incrementer;  // compteur = 3
  WriteLn(compteur);  // 3
end.
```

### Variables locales

Les variables locales sont **créées** à chaque appel et **détruites** à la sortie.

```pascal
procedure Test;
var
  compteur: Integer;
begin
  compteur := compteur + 1;  // ⚠️ DANGER : variable non initialisée !
  WriteLn(compteur);
end;

begin
  Test;  // Affiche une valeur aléatoire
  Test;  // Affiche une autre valeur aléatoire
  Test;  // Affiche encore une autre valeur
end.
```

**Important :** Les variables locales ne sont **pas initialisées automatiquement**. Il faut toujours les initialiser avant de les utiliser :

```pascal
procedure TestCorrect;
var
  compteur: Integer;
begin
  compteur := 0;  // ✅ Initialisation
  compteur := compteur + 1;
  WriteLn(compteur);  // Affiche toujours 1
end;
```

## Tableau comparatif

| Critère | Variable globale | Variable locale |
|---------|-----------------|-----------------|
| **Déclaration** | Dans la section `var` principale | Dans le `var` de la procédure/fonction |
| **Portée** | Tout le programme | Uniquement sa procédure/fonction |
| **Durée de vie** | Du début à la fin du programme | Création à l'appel, destruction à la sortie |
| **Initialisation** | Automatique (0 ou valeur par défaut) | Manuelle (obligatoire) |
| **Persistance** | Les valeurs persistent | Les valeurs sont perdues à chaque sortie |
| **Mémoire** | Occupe la mémoire en permanence | Occupe la mémoire seulement durant l'appel |
| **Visibilité** | Visible partout | Invisible en dehors |

## Avantages et inconvénients

### Variables globales

#### ✅ Avantages
- Partagées entre toutes les procédures/fonctions
- Persistent pendant tout le programme
- Pratiques pour des valeurs partagées (configuration, compteurs, etc.)

#### ❌ Inconvénients
- Difficiles à déboguer (n'importe quelle fonction peut les modifier)
- Risque de modifications accidentelles
- Couplage fort entre les fonctions
- Occupe la mémoire en permanence
- Mauvaise pratique en programmation moderne

### Variables locales

#### ✅ Avantages
- Isolées et sûres (pas de modification accidentelle)
- Faciles à comprendre et déboguer
- Réutilisation de noms sans conflit
- Mémoire libérée automatiquement
- Bonne pratique de programmation

#### ❌ Inconvénients
- Ne persistent pas entre les appels
- Ne peuvent pas être partagées entre procédures
- Doivent être initialisées manuellement

## Bonnes pratiques

### 1. Privilégiez les variables locales

```pascal
// ❌ Mauvaise pratique
var
  a, b, resultat: Integer;  // Globales

procedure Additionner;
begin
  resultat := a + b;
end;

// ✅ Bonne pratique
function Additionner(a, b: Integer): Integer;
var
  resultat: Integer;  // Locale
begin
  resultat := a + b;
  Result := resultat;
end;
```

### 2. Utilisez des paramètres plutôt que des globales

```pascal
// ❌ Mauvaise pratique : utilise des globales
var
  largeur, hauteur: Integer;

procedure CalculerAire;
var
  aire: Integer;
begin
  aire := largeur * hauteur;
  WriteLn(aire);
end;

// ✅ Bonne pratique : utilise des paramètres
function CalculerAire(largeur, hauteur: Integer): Integer;
begin
  Result := largeur * hauteur;
end;
```

### 3. Initialisez toujours les variables locales

```pascal
// ❌ Mauvaise pratique
procedure Test;
var
  x: Integer;
begin
  WriteLn(x);  // Valeur aléatoire !
end;

// ✅ Bonne pratique
procedure Test;
var
  x: Integer;
begin
  x := 0;  // Initialisation
  WriteLn(x);
end;
```

### 4. Évitez le masquage

```pascal
// ⚠️ À éviter : masquage
var
  valeur: Integer;

procedure Test;
var
  valeur: Integer;  // Masque la globale
begin
  // Confusion possible
end;

// ✅ Mieux : noms différents
var
  valeurGlobale: Integer;

procedure Test;
var
  valeurLocale: Integer;
begin
  // Clair et sans confusion
end;
```

### 5. Limitez l'usage des variables globales

Les variables globales ne devraient être utilisées que pour :
- **Constantes de configuration**
- **État global nécessaire** (rarement)
- **Ressources partagées** (fichiers, connexions, etc.)

## Exemple complet : gestion d'un compte bancaire

```pascal
{$mode objfpc}{$H+}
program GestionCompte;

// Variables globales (état du compte)
var
  solde: Real;
  nomTitulaire: String;
  numeroCompte: String;

// Initialiser le compte
procedure InitialiserCompte(nom, numero: String);
begin
  nomTitulaire := nom;
  numeroCompte := numero;
  solde := 0.0;
end;

// Déposer de l'argent
procedure Deposer(montant: Real);
var
  frais: Real;  // Variable locale
begin
  frais := 0.0;  // Pas de frais pour un dépôt
  solde := solde + montant - frais;
  WriteLn('Dépôt de ', montant:0:2, ' €. Nouveau solde : ', solde:0:2, ' €');
end;

// Retirer de l'argent
function Retirer(montant: Real): Boolean;
var
  frais: Real;  // Variable locale
begin
  frais := 1.50;  // Frais de retrait

  if (montant + frais) <= solde then
  begin
    solde := solde - montant - frais;
    WriteLn('Retrait de ', montant:0:2, ' €. Nouveau solde : ', solde:0:2, ' €');
    Result := True;
  end
  else
  begin
    WriteLn('Solde insuffisant !');
    Result := False;
  end;
end;

// Afficher les informations du compte
procedure AfficherInfos;
begin
  WriteLn('=== Informations du compte ===');
  WriteLn('Titulaire : ', nomTitulaire);
  WriteLn('Numéro : ', numeroCompte);
  WriteLn('Solde : ', solde:0:2, ' €');
  WriteLn('==============================');
end;

// Programme principal
begin
  InitialiserCompte('Dupont Jean', 'FR76 1234 5678 9012');
  AfficherInfos;

  Deposer(1000.00);
  Deposer(500.00);

  Retirer(200.00);
  Retirer(2000.00);  // Échouera

  AfficherInfos;
end.
```

**Résultat :**
```
=== Informations du compte ===
Titulaire : Dupont Jean
Numéro : FR76 1234 5678 9012
Solde : 0.00 €
==============================
Dépôt de 1000.00 €. Nouveau solde : 1000.00 €
Dépôt de 500.00 €. Nouveau solde : 1500.00 €
Retrait de 200.00 €. Nouveau solde : 1298.50 €
Solde insuffisant !
=== Informations du compte ===
Titulaire : Dupont Jean
Numéro : FR76 1234 5678 9012
Solde : 1298.50 €
==============================
```

**Analyse :**
- Les variables `solde`, `nomTitulaire`, `numeroCompte` sont **globales** car elles représentent l'état du compte
- Les variables `frais` dans `Deposer` et `Retirer` sont **locales** car elles ne servent que dans le calcul

## Cas particulier : variables locales statiques

En Pascal, il n'existe pas de variables locales "statiques" comme dans certains autres langages. Si vous avez besoin qu'une variable locale persiste entre les appels, utilisez soit :

1. **Une variable globale** (si approprié)
2. **Un paramètre var** pour conserver l'état
3. **La programmation orientée objet** (attributs de classe - vu plus tard)

## Erreurs courantes à éviter

### 1. Utiliser une variable locale non initialisée

```pascal
procedure Test;
var
  x: Integer;
begin
  WriteLn(x);  // ❌ x contient n'importe quoi

  x := 0;      // ✅ Toujours initialiser
  WriteLn(x);
end;
```

### 2. Espérer qu'une variable locale persiste

```pascal
procedure Compter;
var
  compteur: Integer;
begin
  compteur := compteur + 1;  // ❌ Valeur aléatoire
  WriteLn(compteur);
end;

begin
  Compter;  // Affiche n'importe quoi
  Compter;  // Affiche autre chose
end;
```

### 3. Abuser des variables globales

```pascal
// ❌ Trop de globales
var
  a, b, c, d, e, f, g, h: Integer;
  x, y, z: Real;
  nom, prenom, ville: String;

// ✅ Mieux : utiliser des paramètres et variables locales
```

### 4. Créer des dépendances cachées

```pascal
var
  resultat: Integer;  // Globale

procedure Calculer;
begin
  resultat := 42;  // Modifie une globale sans que ce soit évident
end;

begin
  Calculer;
  WriteLn(resultat);  // D'où vient cette valeur ?
end;
```

## Points clés à retenir

1. **Variables globales** : déclarées dans le programme principal, accessibles partout
2. **Variables locales** : déclarées dans une procédure/fonction, accessibles uniquement là
3. Les variables locales doivent être **initialisées manuellement**
4. Les variables locales sont **détruites** à la sortie de la procédure/fonction
5. Le **masquage** : une locale peut cacher une globale du même nom
6. **Bonne pratique** : privilégier les variables locales et les paramètres
7. **Limiter** l'usage des variables globales au strict nécessaire
8. Les variables locales favorisent un code **plus sûr et maintenable**

---

**Prochaine étape :** Dans la section 4.9, nous explorerons la **récursivité**, une technique puissante où une fonction s'appelle elle-même pour résoudre certains types de problèmes.

⏭️ [Récursivité](/04-procedures-fonctions/09-recursivite.md)
