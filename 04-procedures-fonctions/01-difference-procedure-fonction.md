🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.1 Différence entre procédure et fonction

## Introduction

En programmation Pascal, il existe deux façons principales de regrouper du code réutilisable : les **procédures** et les **fonctions**. Bien qu'elles se ressemblent beaucoup, elles ont une différence fondamentale qui détermine quand utiliser l'une ou l'autre.

## Qu'est-ce qu'une procédure ?

Une **procédure** est un bloc de code qui effectue une ou plusieurs actions, mais **ne retourne pas de valeur**.

### Syntaxe de base

```pascal
procedure NomDeLaProcedure;
begin
  // Instructions à exécuter
end;
```

### Exemple concret

```pascal
procedure AfficherMessage;
begin
  WriteLn('Bonjour ! Bienvenue dans le programme.');
end;
```

### Utilisation

```pascal
program ExempleProcedure;

procedure AfficherMessage;
begin
  WriteLn('Bonjour ! Bienvenue dans le programme.');
end;

begin
  AfficherMessage;  // Appel de la procédure
  WriteLn('Fin du programme.');
end.
```

**Ce que fait cette procédure :** Elle affiche un message à l'écran, c'est tout. Elle effectue une **action**.

## Qu'est-ce qu'une fonction ?

Une **fonction** est un bloc de code qui effectue des calculs ou des opérations et **retourne obligatoirement une valeur**.

### Syntaxe de base

```pascal
function NomDeLaFonction: TypeDeRetour;
begin
  // Instructions à exécuter
  NomDeLaFonction := valeur;  // Affectation du résultat
end;
```

### Exemple concret

```pascal
function CalculerCarre(nombre: Integer): Integer;
begin
  CalculerCarre := nombre * nombre;
end;
```

### Utilisation

```pascal
program ExempleFonction;

function CalculerCarre(nombre: Integer): Integer;
begin
  CalculerCarre := nombre * nombre;  // En Pascal, on retourne une valeur en assignant au nom de la fonction
end;

var
  resultat: Integer;
begin
  resultat := CalculerCarre(5);  // La fonction retourne 25
  WriteLn('Le carré de 5 est : ', resultat);
end.
```

**Ce que fait cette fonction :** Elle calcule le carré d'un nombre et **retourne le résultat** que l'on peut ensuite utiliser.

## Différence fondamentale

| Aspect | Procédure | Fonction |
|--------|-----------|----------|
| **Retourne une valeur** | ❌ Non | ✅ Oui |
| **Type de retour** | Aucun | Obligatoire (Integer, String, Boolean, etc.) |
| **Utilisation** | Appelée comme une instruction | Utilisée dans une expression ou affectation |
| **Objectif principal** | Effectuer des actions | Calculer et retourner une valeur |

## Analogie pour mieux comprendre

Imaginez que vous demandez quelque chose à un ami :

- **Procédure** : "Va fermer la porte." → Votre ami effectue l'action, mais ne vous rapporte rien.
- **Fonction** : "Dis-moi combien font 5 fois 3." → Votre ami calcule et vous **répond "15"**.

## Exemples comparatifs

### Avec procédure

```pascal
procedure AfficherDouble(nombre: Integer);
begin
  WriteLn('Le double de ', nombre, ' est ', nombre * 2);
end;

// Utilisation
AfficherDouble(7);  // Affiche : Le double de 7 est 14
```

La procédure affiche directement le résultat. On ne peut pas récupérer la valeur calculée.

### Avec fonction

```pascal
function CalculerDouble(nombre: Integer): Integer;
begin
  CalculerDouble := nombre * 2;
end;

// Utilisation
var
  resultat: Integer;
begin
  resultat := CalculerDouble(7);  // resultat vaut 14
  WriteLn('Résultat : ', resultat);

  // On peut réutiliser la valeur
  resultat := resultat + 10;
  WriteLn('Résultat + 10 : ', resultat);  // 24
end;
```

La fonction retourne la valeur, ce qui permet de la stocker et de la réutiliser.

## Quand utiliser l'une ou l'autre ?

### Utilisez une **procédure** quand :
- Vous voulez effectuer une **action** (afficher, modifier, enregistrer, etc.)
- Vous n'avez pas besoin de récupérer une valeur
- Vous voulez modifier plusieurs variables passées en paramètres

**Exemples typiques :**
- Afficher un menu
- Dessiner une forme à l'écran
- Enregistrer des données dans un fichier
- Initialiser des variables

### Utilisez une **fonction** quand :
- Vous devez **calculer et retourner** un résultat
- La valeur retournée sera utilisée dans des calculs ou affectations
- Vous implémentez une formule mathématique
- Vous transformez des données

**Exemples typiques :**
- Calculer une somme, moyenne, maximum
- Vérifier si un nombre est pair (retourner Boolean)
- Convertir des températures (Celsius vers Fahrenheit)
- Chercher un élément dans un tableau (retourner sa position)

## Exemple complet combinant les deux

```pascal
program ProcedureVsFonction;

function EstPair(nombre: Integer): Boolean;
begin
  EstPair := (nombre mod 2 = 0);
end;

procedure AfficherParite(nombre: Integer);
begin
  if EstPair(nombre) then
    WriteLn(nombre, ' est pair.')
  else
    WriteLn(nombre, ' est impair.');
end;

var
  valeur: Integer;
begin
  Write('Entrez un nombre : ');
  ReadLn(valeur);

  // La fonction retourne une valeur (Boolean)
  if EstPair(valeur) then
    WriteLn('C''est un nombre pair !');

  // La procédure effectue une action (affichage)
  AfficherParite(valeur);
end.
```

## Points clés à retenir

1. **Procédure** = action sans valeur de retour
2. **Fonction** = calcul avec valeur de retour obligatoire
3. Les fonctions peuvent être utilisées dans des expressions : `x := MaFonction(5) + 10;`
4. Les procédures sont appelées seules : `MaProcedure(5);`
5. Une fonction peut effectuer des actions (WriteLn, etc.), mais son rôle principal est de retourner une valeur
6. En Pascal, on affecte la valeur de retour en utilisant le nom de la fonction : `MaFonction := resultat;`

## Remarque importante

En Pascal moderne (FreePascal), on peut aussi utiliser le mot-clé `Result` pour affecter la valeur de retour d'une fonction :

```pascal
function CalculerCarre(nombre: Integer): Integer;
begin
  Result := nombre * nombre;  // Équivalent à : CalculerCarre := nombre * nombre;
end;
```

Cette syntaxe avec `Result` est souvent préférée car elle est plus claire et évite les confusions.

---

**Prochaine étape :** Dans la section 4.2, nous verrons comment déclarer et appeler ces procédures et fonctions de manière plus détaillée, notamment avec des paramètres.

⏭️ [Déclaration et appel](/04-procedures-fonctions/02-declaration-appel.md)
