🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.5 Paramètres constants (const)

## Introduction

Nous avons vu deux façons de passer des paramètres :
- **Par valeur** : copie de la variable, pas de modification possible
- **Par référence (var)** : référence à la variable, modification possible

Il existe une troisième façon : les **paramètres constants (const)** qui combinent les avantages des deux.

## Le problème à résoudre

Imaginez que vous avez une longue chaîne de caractères ou une grande structure de données :

```pascal
procedure AfficherTexte(texte: String);  // Paramètre par valeur
begin
  WriteLn(texte);
end;

var
  longTexte: String;
begin
  longTexte := 'Un texte très très long avec des milliers de caractères...';
  AfficherTexte(longTexte);  // Copie toute la chaîne → lent !
end;
```

**Problème :** Passer par valeur **copie** toute la chaîne, ce qui peut être lent et consommer beaucoup de mémoire.

**Solution naïve avec var :**
```pascal
procedure AfficherTexte(var texte: String);  // Pas de copie
begin
  WriteLn(texte);
end;
```

**Nouveau problème :** Avec `var`, on pourrait **accidentellement modifier** le texte dans la procédure, ce qui n'est pas souhaitable.

## La solution : paramètres constants (const)

Le mot-clé `const` combine le meilleur des deux mondes :
- ✅ **Pas de copie** (comme `var`) → performant
- ✅ **Pas de modification possible** (comme par valeur) → sécurisé

```pascal
procedure AfficherTexte(const texte: String);
begin
  WriteLn(texte);
  // texte := 'autre chose';  // ❌ ERREUR : modification interdite !
end;
```

## Syntaxe

### Pour une procédure

```pascal
procedure NomProcedure(const nomParametre: Type);
begin
  // Lecture seule : on ne peut pas modifier nomParametre
end;
```

### Pour une fonction

```pascal
function NomFonction(const nomParametre: Type): TypeRetour;
begin
  // Lecture seule : on ne peut pas modifier nomParametre
  Result := ...;
end;
```

## Comparaison des trois méthodes

### Exemple comparatif

```pascal
program ComparaisonParametres;

// 1. Par valeur (copie)
procedure ParValeur(texte: String);
begin
  WriteLn('Par valeur : ', texte);
  texte := 'Modifié';  // Possible mais n'affecte pas l'original
end;

// 2. Par référence (var)
procedure ParReference(var texte: String);
begin
  WriteLn('Par référence : ', texte);
  texte := 'Modifié';  // Possible et affecte l'original
end;

// 3. Constant (const)
procedure ParConstant(const texte: String);
begin
  WriteLn('Par constant : ', texte);
  // texte := 'Modifié';  // ERREUR de compilation !
end;

var
  message: String;
begin
  message := 'Original';

  ParValeur(message);
  WriteLn('Après ParValeur : ', message);      // Original

  ParReference(message);
  WriteLn('Après ParReference : ', message);   // Modifié

  message := 'Original';  // Réinitialisation
  ParConstant(message);
  WriteLn('Après ParConstant : ', message);    // Original
end.
```

## Tableau comparatif complet

| Critère | Par valeur | `var` | `const` |
|---------|------------|-------|---------|
| **Copie des données** | ✅ Oui | ❌ Non | ❌ Non |
| **Modification possible** | ✅ Oui (copie) | ✅ Oui (original) | ❌ Non |
| **Variable modifiée** | ❌ Non | ✅ Oui | ❌ Non |
| **Performance** | Lent (grandes structures) | Rapide | Rapide |
| **Sécurité** | ✅ Élevée | ⚠️ Faible | ✅ Élevée |
| **Passage** | Variables, constantes, expressions | Variables uniquement | Variables, constantes, expressions |

## Cas d'usage typiques

### 1. Chaînes de caractères longues

```pascal
procedure AfficherRapport(const rapport: String);
begin
  WriteLn('=== RAPPORT ===');
  WriteLn(rapport);
  WriteLn('===============');
end;

var
  contenu: String;
begin
  contenu := 'Un rapport très long avec beaucoup de données...';
  AfficherRapport(contenu);  // Pas de copie, pas de modification
end.
```

### 2. Calculs sur de grandes valeurs

```pascal
function CalculerLongueur(const texte: String): Integer;
begin
  Result := Length(texte);
  // On lit texte mais on ne le modifie pas
end;

function ContientMot(const phrase: String; const mot: String): Boolean;
begin
  Result := Pos(mot, phrase) > 0;
end;
```

### 3. Tableaux (que nous verrons plus tard)

```pascal
procedure AfficherTableau(const tableau: array of Integer);
var
  i: Integer;
begin
  for i := Low(tableau) to High(tableau) do
    Write(tableau[i], ' ');
  WriteLn;
  // Lecture seule, pas de copie du tableau
end;
```

### 4. Enregistrements (structures)

```pascal
type
  TPersonne = record
    Nom: String;
    Age: Integer;
    Ville: String;
  end;

procedure AfficherPersonne(const personne: TPersonne);
begin
  WriteLn('Nom : ', personne.Nom);
  WriteLn('Âge : ', personne.Age);
  WriteLn('Ville : ', personne.Ville);
  // personne.Age := 30;  // ❌ ERREUR : modification interdite
end;

var
  p: TPersonne;
begin
  p.Nom := 'Dupont';
  p.Age := 25;
  p.Ville := 'Paris';
  AfficherPersonne(p);  // Pas de copie de toute la structure
end.
```

## Exemples pratiques

### 1. Validation de données

```pascal
function EstEmailValide(const email: String): Boolean;
begin
  Result := (Pos('@', email) > 0) and (Pos('.', email) > 0);
end;

var
  monEmail: String;
begin
  monEmail := 'user@example.com';
  if EstEmailValide(monEmail) then
    WriteLn('Email valide')
  else
    WriteLn('Email invalide');
end.
```

### 2. Recherche dans du texte

```pascal
uses StrUtils;

function CompterOccurrences(const texte: String; const motif: String): Integer;
var
  position, compteur: Integer;
begin
  compteur := 0;
  position := 1;

  while position <= Length(texte) do
  begin
    position := PosEx(motif, texte, position);
    if position > 0 then
    begin
      Inc(compteur);
      Inc(position);
    end
    else
      break;
  end;

  Result := compteur;
end;

begin
  WriteLn('Nombre de "le" : ', CompterOccurrences('le chat et le chien', 'le'));
end.
```

### 3. Formatage d'affichage

```pascal
procedure AfficherTitre(const titre: String);
var
  longueur, i: Integer;
begin
  longueur := Length(titre);

  // Ligne supérieure
  for i := 1 to longueur + 4 do
    Write('=');
  WriteLn;

  // Titre
  WriteLn('| ', titre, ' |');

  // Ligne inférieure
  for i := 1 to longueur + 4 do
    Write('=');
  WriteLn;
end;

begin
  AfficherTitre('Bienvenue');
  AfficherTitre('Menu Principal');
end.
```

**Résultat :**
```
=============
| Bienvenue |
=============
===================
| Menu Principal |
===================
```

### 4. Comparaison de valeurs

```pascal
function Minimum(const a, b: Integer): Integer;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function Maximum(const a, b, c: Real): Real;
var
  max: Real;
begin
  max := a;
  if b > max then
    max := b;
  if c > max then
    max := c;
  Result := max;
end;

begin
  WriteLn('Min(5, 3) = ', Minimum(5, 3));
  WriteLn('Max(2.5, 7.1, 4.3) = ', Maximum(2.5, 7.1, 4.3):0:1);
end.
```

## Quand utiliser const ?

### ✅ Utilisez `const` quand :

1. **Vous lisez le paramètre sans le modifier**
   ```pascal
   function CalculerSomme(const a, b: Integer): Integer;
   ```

2. **Le paramètre est une structure volumineuse** (String, tableau, enregistrement)
   ```pascal
   procedure TraiterDonnees(const grosseStructure: TGrosseStructure);
   ```

3. **Vous voulez garantir que le paramètre ne sera pas modifié**
   ```pascal
   procedure Afficher(const valeur: String);
   ```

4. **Vous passez des constantes ou expressions**
   ```pascal
   begin
     Afficher('Message');        // ✅ OK avec const
     Afficher(IntToStr(100));    // ✅ OK avec const
   end;
   ```

### ❌ N'utilisez PAS `const` quand :

1. **Vous devez modifier le paramètre** → utilisez `var`
   ```pascal
   procedure Incrementer(var n: Integer);  // Pas const !
   ```

2. **Le paramètre est un type simple et petit** (Integer, Boolean, Char)
   ```pascal
   function Carre(n: Integer): Integer;  // Par valeur suffit
   ```
   *Note : Pour les types simples, const n'apporte pas d'avantage significatif*

3. **Vous avez besoin d'une variable de travail locale**
   ```pascal
   procedure Traiter(n: Integer);  // Par valeur pour pouvoir modifier n localement
   begin
     n := n * 2;  // On travaille avec la copie
     WriteLn(n);
   end;
   ```

## Passage de constantes et expressions

Contrairement à `var`, on peut passer des constantes et expressions avec `const` :

```pascal
procedure Afficher(const texte: String);
begin
  WriteLn(texte);
end;

function Additionner(const a, b: Integer): Integer;
begin
  Result := a + b;
end;

var
  x: Integer;
begin
  Afficher('Message direct');        // ✅ Constante
  Afficher('A' + 'B');               // ✅ Expression

  x := 5;
  WriteLn(Additionner(x, 10));       // ✅ Variable + constante
  WriteLn(Additionner(3 + 2, 7));    // ✅ Expressions
end.
```

## Limites et comportement spécial

### 1. Const avec types simples

Pour les types simples (Integer, Boolean, Real, Char), `const` se comporte comme un passage par valeur :

```pascal
procedure Test(const n: Integer);
begin
  // En interne, n est passé par valeur pour les types simples
  WriteLn(n);
end;
```

**Pourquoi ?** Les types simples sont petits, donc la copie est rapide. Le compilateur optimise automatiquement.

### 2. Const avec types complexes

Pour les types complexes (String, tableaux, enregistrements), `const` passe une référence (comme `var`) mais en lecture seule :

```pascal
procedure Test(const s: String);
begin
  // s est passé par référence (pas de copie)
  // mais on ne peut pas le modifier
  WriteLn(s);
end;
```

## Exemple complet : système de gestion de messages

```pascal
{$mode objfpc}{$H+}
program GestionMessages;

type
  TMessage = record
    Expediteur: String;
    Destinataire: String;
    Contenu: String;
    DateHeure: String;
  end;

// Affichage (lecture seule, pas de modification)
procedure AfficherMessage(const msg: TMessage);
begin
  WriteLn('=== MESSAGE ===');
  WriteLn('De : ', msg.Expediteur);
  WriteLn('À : ', msg.Destinataire);
  WriteLn('Date : ', msg.DateHeure);
  WriteLn('---');
  WriteLn(msg.Contenu);
  WriteLn('===============');
end;

// Vérification (lecture seule)
function EstMessageVide(const msg: TMessage): Boolean;
begin
  Result := (Length(msg.Contenu) = 0);
end;

// Création (modification nécessaire)
procedure CreerMessage(var msg: TMessage;
                      const exp, dest, contenu: String);
begin
  msg.Expediteur := exp;
  msg.Destinataire := dest;
  msg.Contenu := contenu;
  msg.DateHeure := '2025-10-12 14:30';  // Simplifié
end;

var
  monMessage: TMessage;
begin
  CreerMessage(monMessage, 'Alice', 'Bob', 'Bonjour Bob !');

  if not EstMessageVide(monMessage) then
    AfficherMessage(monMessage);
end.
```

## Erreurs courantes à éviter

### 1. Essayer de modifier un paramètre const

```pascal
procedure Test(const n: Integer);
begin
  n := 10;  // ❌ ERREUR : n est constant
end;
```

### 2. Utiliser const quand var est nécessaire

```pascal
procedure Doubler(const n: Integer);  // ❌ Erreur de conception
begin
  n := n * 2;  // ❌ Ne compile pas
end;

// ✅ Correction : utiliser var
procedure Doubler(var n: Integer);
begin
  n := n * 2;
end;
```

### 3. Surprotéger avec const partout

```pascal
// ❌ Inutile pour les types simples
function Carre(const n: Integer): Integer;
begin
  Result := n * n;
end;

// ✅ Mieux : par valeur suffit
function Carre(n: Integer): Integer;
begin
  Result := n * n;
end;
```

## Bonnes pratiques

1. **Utilisez const par défaut** pour les String et structures quand vous ne modifiez pas
2. **Privilégiez la lisibilité** : const indique clairement "je ne modifie pas ce paramètre"
3. **Ne sur-optimisez pas** : pour les types simples, par valeur suffit
4. **Cohérence** : si plusieurs fonctions traitent le même type, utilisez const de façon cohérente

## Points clés à retenir

1. `const` = **lecture seule** + **pas de copie** (pour types complexes)
2. **Avantages** : performance + sécurité
3. On peut passer **variables, constantes et expressions** avec const
4. **Différence avec var** : on ne peut pas modifier le paramètre
5. **Différence avec par valeur** : pas de copie pour les grandes structures
6. **Usage principal** : paramètres en lecture seule (String, tableaux, enregistrements)
7. Pour les **types simples** (Integer, etc.), l'impact est minimal
8. **Règle pratique** : "Si je ne modifie pas une String ou structure, j'utilise const"

---

**Prochaine étape :** Dans la section 4.6, nous découvrirons les **paramètres par défaut** qui permettent de rendre certains paramètres optionnels lors de l'appel.

⏭️ [Paramètres par défaut](/04-procedures-fonctions/06-parametres-par-defaut.md)
