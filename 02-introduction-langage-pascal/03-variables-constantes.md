🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.3 Variables et constantes

## Comprendre les variables

### Qu'est-ce qu'une variable ?

Imaginez votre ordinateur comme une immense armoire avec des milliers de tiroirs. Une **variable** est comme une étiquette que vous collez sur un tiroir pour y ranger une information. Vous pouvez :
- Mettre quelque chose dans le tiroir
- Regarder ce qu'il contient
- Remplacer son contenu par autre chose

En programmation, une variable est un **espace mémoire nommé** qui peut stocker une valeur. Cette valeur peut changer pendant l'exécution du programme (d'où le nom "variable").

### Pourquoi utiliser des variables ?

Sans variables, nous serions obligés d'écrire directement les valeurs dans notre code :

```pascal
writeln('Bonjour Alice');  
writeln('Alice a 25 ans');  
```

Si nous voulons changer le nom, nous devons modifier le code à plusieurs endroits. Avec des variables :

```pascal
var
  nom: string;
  age: integer;
begin
  nom := 'Alice';
  age := 25;
  writeln('Bonjour ', nom);
  writeln(nom, ' a ', age, ' ans');
end.
```

Maintenant, pour changer le nom, il suffit de modifier une seule ligne !

## Déclarer une variable

### La syntaxe de base

En Pascal, toutes les variables doivent être **déclarées** avant d'être utilisées. La déclaration se fait dans la section `var` :

```pascal
var
  nomDeVariable: TypeDeDonnées;
```

**Anatomie d'une déclaration :**
- `var` : mot-clé qui introduit la section des variables
- `nomDeVariable` : le nom que vous donnez à votre variable
- `:` : deux-points pour séparer le nom du type
- `TypeDeDonnées` : le type d'information que la variable peut contenir
- `;` : point-virgule pour terminer la déclaration

### Exemples de déclarations simples

```pascal
var
  age: integer;           // Un nombre entier
  prenom: string;         // Du texte
  prix: real;             // Un nombre décimal
  estMajeur: boolean;     // Vrai ou faux
```

### Déclarer plusieurs variables du même type

Vous pouvez déclarer plusieurs variables du même type sur une seule ligne :

```pascal
var
  longueur, largeur, hauteur: real;
  nom, prenom, ville: string;
```

C'est équivalent à :

```pascal
var
  longueur: real;
  largeur: real;
  hauteur: real;
  nom: string;
  prenom: string;
  ville: string;
```

## Les règles de nommage des variables

### Règles obligatoires

Un nom de variable en Pascal doit respecter ces règles :

1. **Commencer par une lettre** (a-z, A-Z) ou un souligné (_)
2. **Contenir uniquement** des lettres, chiffres et soulignés
3. **Ne pas être un mot-clé réservé** (begin, end, var, etc.)
4. **Ne pas contenir d'espaces** ni de caractères spéciaux

**Noms valides :**
```pascal
age  
prenom  
prix_total  
montantHT  
temperature_moyenne  
compteur1  
_temporaire
```

**Noms invalides :**
```pascal
1nombre        // Commence par un chiffre
mon age        // Contient un espace  
prix-total     // Contient un tiret  
montant€       // Contient un caractère spécial  
begin          // Mot-clé réservé  
```

### Conventions recommandées (bonnes pratiques)

Même si Pascal accepte différents styles, voici les conventions courantes :

**Style camelCase (recommandé pour Pascal) :**
```pascal
var
  nombreDEleves: integer;
  prixTotalTTC: real;
  estConnecte: boolean;
```

**Style snake_case (accepté) :**
```pascal
var
  nombre_eleves: integer;
  prix_total_ttc: real;
  est_connecte: boolean;
```

**Conseils importants :**
- Utilisez des noms **explicites** et **significatifs**
- Évitez les abréviations obscures
- Restez cohérent dans votre style

**Mauvais exemples :**
```pascal
var
  x: integer;        // Trop vague
  nb: integer;       // Abréviation peu claire
  temp: real;        // Que représente "temp" ?
```

**Bons exemples :**
```pascal
var
  nombreDEtudiants: integer;
  temperatureCelsius: real;
  nomUtilisateur: string;
```

## Affecter des valeurs aux variables

### L'opérateur d'affectation :=

Pour mettre une valeur dans une variable, on utilise l'opérateur `:=` (deux-points suivi du signe égal) :

```pascal
var
  age: integer;
begin
  age := 25;    // On affecte la valeur 25 à la variable age
end.
```

**Attention :** Ne confondez pas `=` et `:=` !
- `:=` sert à **affecter** une valeur (mettre dans la variable)
- `=` sert à **comparer** des valeurs (nous verrons cela plus tard)

### Exemples d'affectations

```pascal
program ExemplesAffectation;  
var  
  prenom: string;
  age: integer;
  taille: real;
  estAdulte: boolean;
begin
  // Affectation de valeurs
  prenom := 'Sophie';
  age := 30;
  taille := 1.68;
  estAdulte := true;

  // Affichage
  writeln('Prénom : ', prenom);
  writeln('Âge : ', age, ' ans');
  // :0:2 = largeur minimale 0, 2 decimales (formate le nombre reel)
  writeln('Taille : ', taille:0:2, ' m');
  writeln('Adulte : ', estAdulte);
end.
```

### Valeur initiale des variables

**Important :** En Pascal, les variables déclarées n'ont pas de valeur garantie avant d'être initialisées. Elles peuvent contenir n'importe quelle valeur "résiduelle" de la mémoire.

**Mauvaise pratique :**
```pascal
var
  compteur: integer;
begin
  writeln(compteur);  // Valeur imprévisible !
end.
```

**Bonne pratique :**
```pascal
var
  compteur: integer;
begin
  compteur := 0;      // Toujours initialiser avant d'utiliser
  writeln(compteur);  // Affiche 0
end.
```

### Modifier une variable

Une fois qu'une variable a une valeur, vous pouvez la changer autant de fois que nécessaire :

```pascal
var
  nombre: integer;
begin
  nombre := 10;
  writeln('Nombre vaut : ', nombre);  // Affiche 10

  nombre := 20;
  writeln('Nombre vaut : ', nombre);  // Affiche 20

  nombre := nombre + 5;
  writeln('Nombre vaut : ', nombre);  // Affiche 25
end.
```

## Les constantes

### Qu'est-ce qu'une constante ?

Une **constante** est comme une variable, mais sa valeur **ne peut jamais changer** une fois définie. C'est comme écrire au stylo indélébile plutôt qu'au crayon.

**Quand utiliser une constante ?**
- Pour des valeurs qui ne changeront jamais (PI, nombre de jours dans une semaine, etc.)
- Pour éviter les "nombres magiques" dans le code
- Pour faciliter la maintenance (changer la valeur à un seul endroit)

### Déclarer une constante

Les constantes se déclarent dans la section `const` :

```pascal
const
  NomConstante = valeur;
```

**Notez la différence avec les variables :**
- On utilise `=` au lieu de `:`
- On n'indique pas de type (il est déduit automatiquement)
- Pas de `:` entre le nom et la valeur

### Exemples de constantes

```pascal
program ExemplesConstantes;

// Les constantes utilisent = (pas :=) et le type est deduit automatiquement
const
  PI = 3.14159;            // -> real
  TauxTVA = 20;            // -> integer
  NomEntreprise = 'Ma Société';  // -> string
  JoursParSemaine = 7;
  EstDebugMode = false;    // -> boolean

var
  rayon: real;
  surface: real;
begin
  rayon := 5.0;
  surface := PI * rayon * rayon;
  writeln('Surface du cercle : ', surface:0:2);
end.
```

### Tentative de modification (erreur)

```pascal
const
  MaxPoints = 100;
begin
  MaxPoints := 200;  // ERREUR DE COMPILATION !
                     // Une constante ne peut pas être modifiée
end.
```

Le compilateur refusera ce code avec un message d'erreur.

## Variables vs Constantes : Tableau comparatif

| Aspect | Variable | Constante |
|--------|----------|-----------|
| Déclaration | Section `var` | Section `const` |
| Syntaxe | `nom: type;` | `nom = valeur;` |
| Affectation | Avec `:=` | À la déclaration avec `=` |
| Modification | Peut changer | Ne peut jamais changer |
| Initialisation | Optionnelle (mais recommandée) | Obligatoire |
| Usage | Données qui évoluent | Valeurs fixes |

## Exemples pratiques complets

### Exemple 1 : Calcul de surface

```pascal
program CalculSurface;

const
  PI = 3.14159;

var
  rayon: real;
  circonference: real;
  surface: real;

begin
  // Initialisation
  rayon := 7.5;

  // Calculs
  circonference := 2 * PI * rayon;
  surface := PI * rayon * rayon;

  // Affichage
  writeln('=== Cercle ===');
  writeln('Rayon : ', rayon:0:2, ' cm');
  writeln('Circonférence : ', circonference:0:2, ' cm');
  writeln('Surface : ', surface:0:2, ' cm²');
end.
```

### Exemple 2 : Calcul de prix TTC

```pascal
program CalculPrixTTC;

const
  TauxTVA = 20.0;  // En pourcentage

var
  prixHT: real;
  montantTVA: real;
  prixTTC: real;

begin
  // Saisie du prix HT
  prixHT := 100.0;

  // Calculs
  montantTVA := prixHT * TauxTVA / 100;
  prixTTC := prixHT + montantTVA;

  // Affichage des résultats
  writeln('Prix HT : ', prixHT:0:2, ' €');
  // :0:0 = 0 decimales (affiche le reel comme un entier)
  writeln('TVA (', TauxTVA:0:0, '%) : ', montantTVA:0:2, ' €');
  writeln('Prix TTC : ', prixTTC:0:2, ' €');
end.
```

### Exemple 3 : Informations personnelles

```pascal
program FichePersonnelle;

const
  AnneeEnCours = 2024;

var
  prenom: string;
  nom: string;
  anneeNaissance: integer;
  age: integer;
  ville: string;

begin
  // Affectation des valeurs
  prenom := 'Marie';
  nom := 'Dupont';
  anneeNaissance := 1990;
  ville := 'Paris';

  // Calcul de l'âge
  age := AnneeEnCours - anneeNaissance;

  // Affichage de la fiche
  writeln('=== FICHE PERSONNELLE ===');
  writeln('Nom : ', nom);
  writeln('Prénom : ', prenom);
  writeln('Année de naissance : ', anneeNaissance);
  writeln('Âge : ', age, ' ans');
  writeln('Ville : ', ville);
end.
```

### Exemple 4 : Conversion de températures

```pascal
program ConversionTemperature;

const
  MessageBienvenue = 'Conversion Celsius vers Fahrenheit';

var
  celsius: real;
  fahrenheit: real;

begin
  writeln(MessageBienvenue);
  writeln('========================');
  writeln;

  // Température en Celsius
  celsius := 25.0;

  // Formule de conversion : F = (C × 9/5) + 32
  fahrenheit := (celsius * 9 / 5) + 32;

  // Affichage
  writeln(celsius:0:1, ' °C = ', fahrenheit:0:1, ' °F');
end.
```

## Portée des variables et constantes

Les variables et constantes déclarées au niveau du programme (entre `program` et `begin`) sont **globales** : elles sont accessibles partout dans le programme.

```pascal
program Portee;

const
  Message = 'Je suis une constante globale';

var
  compteur: integer;  // Variable globale

begin
  compteur := 0;
  writeln(Message);
  writeln('Compteur : ', compteur);
end.
```

**Note :** Nous verrons plus tard qu'il existe aussi des variables **locales** (dans les procédures et fonctions).

## Erreurs courantes à éviter

### 1. Utiliser une variable avant de la déclarer

```pascal
begin
  age := 25;     // ERREUR : age n'est pas déclaré
end.
```

**Correction :**
```pascal
var
  age: integer;
begin
  age := 25;     // OK
end.
```

### 2. Confondre = et :=

```pascal
var
  x: integer;
begin
  x = 10;        // ERREUR : utiliser :=
end.
```

### 3. Oublier d'initialiser une variable

```pascal
var
  total: integer;
begin
  total := total + 10;  // Risqué : total n'a pas de valeur initiale
end.
```

**Correction :**
```pascal
var
  total: integer;
begin
  total := 0;           // Initialisation
  total := total + 10;  // Maintenant c'est sûr
end.
```

### 4. Essayer de modifier une constante

```pascal
const
  MaxValeur = 100;
begin
  MaxValeur := 200;     // ERREUR : impossible !
end.
```

### 5. Utiliser des noms de variables non descriptifs

```pascal
var
  x: integer;     // Que représente x ?
  t: real;        // Et t ?
begin
  x := 25;
  t := 1.75;
end.
```

**Mieux :**
```pascal
var
  age: integer;
  taille: real;
begin
  age := 25;
  taille := 1.75;
end.
```

## Récapitulatif

**Variables :**
- Espaces mémoire nommés pour stocker des données
- Déclarées avec `var nomVariable: type;`
- Modifiables avec l'opérateur `:=`
- Doivent être initialisées avant utilisation

**Constantes :**
- Valeurs fixes qui ne changent jamais
- Déclarées avec `const NomConstante = valeur;`
- Ne peuvent pas être modifiées après déclaration
- Utiles pour éviter les "nombres magiques"

**Règles de nommage :**
- Commencer par une lettre
- Contenir lettres, chiffres, soulignés uniquement
- Être descriptif et significatif
- Respecter une convention cohérente

---

**Point clé :** Les variables et constantes sont les briques de base de tout programme. Bien les nommer et les utiliser correctement est essentiel pour écrire du code clair et maintenable. Prenez le temps de choisir de bons noms : votre futur vous (et vos collègues) vous remercieront !

⏭️ [Types de données primitifs (Integer, Real, Boolean, Char)](/02-introduction-langage-pascal/04-types-donnees-primitifs.md)
