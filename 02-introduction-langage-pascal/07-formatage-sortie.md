🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.7 Formatage de sortie

## Introduction au formatage

Le **formatage** consiste à contrôler précisément comment les données sont affichées à l'écran. Un bon formatage rend vos programmes :
- Plus **lisibles** et professionnels
- Plus **faciles à comprendre** pour l'utilisateur
- Plus **agréables visuellement**

Imaginez la différence entre un reçu de caisse bien aligné et un simple tas de chiffres ! C'est exactement ce que le formatage apporte à vos programmes.

## Formatage des nombres réels

### Syntaxe générale

Pour les nombres de type `real`, la syntaxe complète est :

```pascal
WriteLn(variable:largeurTotale:nombreDecimales);
```

- **largeurTotale** : nombre minimum de caractères pour l'affichage
- **nombreDecimales** : nombre de chiffres après la virgule

### Contrôler les décimales

```pascal
program FormateDecimales;  
var  
  pi: real;
begin
  pi := 3.14159265358979;

  WriteLn('Sans formatage    : ', pi);
  WriteLn('0 décimale        : ', pi:0:0);    // 3
  WriteLn('1 décimale        : ', pi:0:1);    // 3.1
  WriteLn('2 décimales       : ', pi:0:2);    // 3.14
  WriteLn('4 décimales       : ', pi:0:4);    // 3.1416 (arrondi)
  WriteLn('10 décimales      : ', pi:0:10);   // 3.1415926536
end.
```

**Résultat :**
```
Sans formatage    :  3.1415926536E+00
0 décimale        : 3
1 décimale        : 3.1
2 décimales       : 3.14
4 décimales       : 3.1416
10 décimales      : 3.1415926536
```

### Contrôler la largeur totale

La largeur totale inclut tous les caractères : le signe, les chiffres avant et après la virgule, et la virgule elle-même.

```pascal
program FormateLargeur;  
var  
  prix: real;
begin
  prix := 19.99;

  WriteLn('|', prix:0:2, '|');      // |19.99|
  WriteLn('|', prix:8:2, '|');      // |   19.99|
  WriteLn('|', prix:10:2, '|');     // |     19.99|
  WriteLn('|', prix:15:2, '|');     // |          19.99|
end.
```

**Résultat :**
```
|19.99|
|   19.99|
|     19.99|
|          19.99|
```

Les espaces sont ajoutés **à gauche** pour atteindre la largeur demandée (alignement à droite).

### Nombres négatifs

```pascal
var
  temperature: real;
begin
  temperature := -15.5;

  WriteLn(temperature:0:1);      // -15.5
  WriteLn(temperature:8:1);      // -15.5 (le signe compte dans la largeur)
end.
```

### Zéros à gauche vs espaces

Par défaut, Pascal utilise des **espaces** pour remplir la largeur. Si vous voulez des zéros, vous devrez les gérer autrement (avec des fonctions de chaînes que nous verrons plus tard).

## Formatage des nombres entiers

### Syntaxe pour les integers

Pour les entiers, seule la largeur peut être spécifiée :

```pascal
WriteLn(variable:largeur);
```

### Exemples de formatage d'entiers

```pascal
program FormateEntiers;  
var  
  nombre: integer;
begin
  nombre := 42;

  WriteLn('|', nombre, '|');        // |42|
  WriteLn('|', nombre:5, '|');      // |   42|
  WriteLn('|', nombre:10, '|');     // |        42|
  WriteLn('|', nombre:2, '|');      // |42| (largeur minimum respectée)
  WriteLn('|', nombre:1, '|');      // |42| (la largeur s'adapte)
end.
```

**Résultat :**
```
|42|
|   42|
|        42|
|42|
|42|
```

### Nombres avec plusieurs chiffres

```pascal
var
  population: integer;
begin
  population := 1234567;

  WriteLn(population);           // 1234567
  WriteLn(population:10);        //    1234567
  WriteLn(population:15);        //        1234567
end.
```

### Astuce : alignement de colonnes

Le formatage d'entiers est particulièrement utile pour créer des tableaux bien alignés :

```pascal
program TableauAligneNum;  
begin  
  WriteLn('Numéro':8, 'Quantité':12, 'Prix':10);
  WriteLn('------':8, '--------':12, '----':10);
  WriteLn(1:8, 150:12, 2500:10);
  WriteLn(2:8, 75:12, 1200:10);
  WriteLn(3:8, 230:12, 3800:10);
end.
```

**Résultat :**
```
  Numéro    Quantité      Prix
  ------    --------      ----
       1         150      2500
       2          75      1200
       3         230      3800
```

## Formatage des chaînes de caractères

### Syntaxe pour les strings

```pascal
WriteLn(chaine:largeur);
```

- **Largeur positive** : alignement à droite (espaces à gauche)
- **Largeur négative** : alignement à gauche (espaces à droite)

### Alignement à droite (largeur positive)

```pascal
program AlignementDroite;  
var  
  nom: string;
begin
  nom := 'Alice';

  WriteLn('|', nom, '|');          // |Alice|
  WriteLn('|', nom:10, '|');       // |     Alice|
  WriteLn('|', nom:15, '|');       // |          Alice|
  WriteLn('|', nom:20, '|');       // |               Alice|
end.
```

**Résultat :**
```
|Alice|
|     Alice|
|          Alice|
|               Alice|
```

### Alignement à gauche (largeur négative)

```pascal
program AlignementGauche;  
var  
  nom: string;
begin
  nom := 'Alice';

  WriteLn('|', nom:-10, '|');      // |Alice     |
  WriteLn('|', nom:-15, '|');      // |Alice          |
  WriteLn('|', nom:-20, '|');      // |Alice               |
end.
```

**Résultat :**
```
|Alice     |
|Alice          |
|Alice               |
```

### Application pratique : tableau de noms

```pascal
program TableauNoms;  
begin  
  WriteLn('Prénom':15, 'Nom':15, 'Ville':20);
  WriteLn('--------------':15, '--------------':15, '-------------------':20);
  WriteLn('Alice':15, 'Dupont':15, 'Paris':20);
  WriteLn('Bob':15, 'Martin':15, 'Lyon':20);
  WriteLn('Charlie':15, 'Durand':15, 'Marseille':20);
end.
```

## Créer des tableaux professionnels

### Tableau simple avec bordures

```pascal
program TableauBordures;  
begin  
  WriteLn('+', '-----------------':17, '+', '---------':9, '+', '---------':9, '+');
  WriteLn('|', 'Article':17, '|', 'Prix':9, '|', 'Qté':9, '|');
  WriteLn('+', '-----------------':17, '+', '---------':9, '+', '---------':9, '+');
  WriteLn('|', 'Clavier':17, '|', 25.99:9:2, '|', 5:9, '|');
  WriteLn('|', 'Souris':17, '|', 15.50:9:2, '|', 10:9, '|');
  WriteLn('|', 'Écran':17, '|', 199.99:9:2, '|', 2:9, '|');
  WriteLn('+', '-----------------':17, '+', '---------':9, '+', '---------':9, '+');
end.
```

**Résultat :**
```
+-----------------+---------+---------+
|          Article|     Prix|      Qté|
+-----------------+---------+---------+
|          Clavier|    25.99|        5|
|           Souris|    15.50|       10|
|            Écran|   199.99|        2|
+-----------------+---------+---------+
```

### Tableau avec totaux

```pascal
program TableauTotaux;  
const  
  Separateur = '================================';
var
  prix1, prix2, prix3, total: real;
begin
  prix1 := 25.99;
  prix2 := 15.50;
  prix3 := 199.99;
  total := prix1 + prix2 + prix3;

  WriteLn(Separateur);
  WriteLn('        FACTURE');
  WriteLn(Separateur);
  WriteLn('Article':20, 'Prix':12);
  WriteLn('-------':20, '----':12);
  WriteLn('Clavier':20, prix1:12:2);
  WriteLn('Souris':20, prix2:12:2);
  WriteLn('Écran':20, prix3:12:2);
  WriteLn('-------':20, '--------':12);
  WriteLn('TOTAL':20, total:12:2);
  WriteLn(Separateur);
end.
```

**Résultat :**
```
================================
        FACTURE
================================
             Article        Prix
             -------        ----
             Clavier       25.99
              Souris       15.50
               Écran      199.99
             -------    --------
               TOTAL      241.48
================================
```

## Formatage de données monétaires

### Afficher des prix

Pour les prix, utilisez toujours **2 décimales** :

```pascal
program FormatPrix;  
var  
  prixHT, tva, prixTTC: real;
begin
  prixHT := 100.0;
  tva := prixHT * 0.20;
  prixTTC := prixHT + tva;

  WriteLn('Prix HT    : ', prixHT:8:2, ' €');
  WriteLn('TVA (20%)  : ', tva:8:2, ' €');
  WriteLn('Prix TTC   : ', prixTTC:8:2, ' €');
end.
```

**Résultat :**
```
Prix HT    :   100.00 €  
TVA (20%)  :    20.00 €  
Prix TTC   :   120.00 €  
```

### Facture complète avec formatage

```pascal
program FactureComplete;  
const  
  TauxTVA = 20.0;
  LargeurNom = 25;
  LargeurQte = 8;
  LargeurPrix = 12;
var
  article1, article2, article3: string;
  prix1, prix2, prix3: real;
  qte1, qte2, qte3: integer;
  total1, total2, total3: real;
  sousTotal, montantTVA, totalTTC: real;
begin
  // Données
  article1 := 'Clavier mécanique';
  prix1 := 89.99;
  qte1 := 2;

  article2 := 'Souris ergonomique';
  prix2 := 45.50;
  qte2 := 1;

  article3 := 'Tapis de souris';
  prix3 := 12.99;
  qte3 := 3;

  // Calculs
  total1 := prix1 * qte1;
  total2 := prix2 * qte2;
  total3 := prix3 * qte3;

  sousTotal := total1 + total2 + total3;
  montantTVA := sousTotal * TauxTVA / 100;
  totalTTC := sousTotal + montantTVA;

  // Affichage
  WriteLn;
  WriteLn('==============================================');
  WriteLn('              FACTURE N° 2024-001');
  WriteLn('==============================================');
  WriteLn;
  // :-LargeurNom = largeur negative via constante, donc alignement a gauche
  WriteLn('Article':-LargeurNom, 'Qté':LargeurQte, 'P.U.':LargeurPrix, 'Total':LargeurPrix);
  WriteLn('------------------------':LargeurNom, '-------':LargeurQte,
          '-----------':LargeurPrix, '-----------':LargeurPrix);

  WriteLn(article1:-LargeurNom, qte1:LargeurQte, prix1:LargeurPrix:2, total1:LargeurPrix:2);
  WriteLn(article2:-LargeurNom, qte2:LargeurQte, prix2:LargeurPrix:2, total2:LargeurPrix:2);
  WriteLn(article3:-LargeurNom, qte3:LargeurQte, prix3:LargeurPrix:2, total3:LargeurPrix:2);

  WriteLn;
  WriteLn('Sous-total HT':LargeurNom + LargeurQte + LargeurPrix, sousTotal:LargeurPrix:2);
  WriteLn('TVA (20%)':LargeurNom + LargeurQte + LargeurPrix, montantTVA:LargeurPrix:2);
  WriteLn('============':LargeurNom + LargeurQte + LargeurPrix, '===========':LargeurPrix);
  WriteLn('TOTAL TTC':LargeurNom + LargeurQte + LargeurPrix, totalTTC:LargeurPrix:2, ' €');
  WriteLn;
  WriteLn('==============================================');
end.
```

## Formatage de rapports et statistiques

### Rapport avec pourcentages

```pascal
program RapportPourcentages;  
var  
  totalVentes, ventes1, ventes2, ventes3: real;
  pct1, pct2, pct3: real;
begin
  ventes1 := 15000;
  ventes2 := 25000;
  ventes3 := 10000;
  totalVentes := ventes1 + ventes2 + ventes3;

  pct1 := (ventes1 / totalVentes) * 100;
  pct2 := (ventes2 / totalVentes) * 100;
  pct3 := (ventes3 / totalVentes) * 100;

  WriteLn;
  WriteLn('========================================');
  WriteLn('        RAPPORT DES VENTES');
  WriteLn('========================================');
  WriteLn;
  WriteLn('Région':-15, 'Ventes':15, 'Part':10);
  WriteLn('--------------':-15, '--------------':15, '---------':10);
  WriteLn('Nord':-15, ventes1:15:2, pct1:9:1, '%');
  WriteLn('Sud':-15, ventes2:15:2, pct2:9:1, '%');
  WriteLn('Est':-15, ventes3:15:2, pct3:9:1, '%');
  WriteLn('--------------':-15, '--------------':15, '---------':10);
  WriteLn('TOTAL':-15, totalVentes:15:2, 100.0:9:1, '%');
  WriteLn('========================================');
end.
```

**Résultat :**
```
========================================
        RAPPORT DES VENTES
========================================

Région               Ventes      Part
--------------    --------------  ---------
Nord                    15000.00      30.0%  
Sud                     25000.00      50.0%  
Est                     10000.00      20.0%  
--------------    --------------  ---------
TOTAL                   50000.00     100.0%
========================================
```

## Formatage de menus interactifs

### Menu simple bien formaté

```pascal
program MenuFormate;  
begin  
  WriteLn;
  WriteLn('╔════════════════════════════════╗');
  WriteLn('║      MENU PRINCIPAL            ║');
  WriteLn('╠════════════════════════════════╣');
  WriteLn('║                                ║');
  WriteLn('║  1. Nouvelle partie            ║');
  WriteLn('║  2. Charger une partie         ║');
  WriteLn('║  3. Options                    ║');
  WriteLn('║  4. Crédits                    ║');
  WriteLn('║  0. Quitter                    ║');
  WriteLn('║                                ║');
  WriteLn('╚════════════════════════════════╝');
  WriteLn;
  Write('Votre choix : ');
end.
```

**Note :** Les caractères spéciaux (╔, ═, ║, etc.) peuvent ne pas s'afficher correctement sur tous les systèmes. Utilisez plutôt des caractères standards :

```pascal
program MenuFormate;  
begin  
  WriteLn;
  WriteLn('+================================+');
  WriteLn('|      MENU PRINCIPAL            |');
  WriteLn('+================================+');
  WriteLn('|                                |');
  WriteLn('|  1. Nouvelle partie            |');
  WriteLn('|  2. Charger une partie         |');
  WriteLn('|  3. Options                    |');
  WriteLn('|  4. Crédits                    |');
  WriteLn('|  0. Quitter                    |');
  WriteLn('|                                |');
  WriteLn('+================================+');
  WriteLn;
  Write('Votre choix : ');
end.
```

### Menu avec numérotation alignée

```pascal
program MenuNumerate;  
var  
  i: integer;
begin
  WriteLn;
  WriteLn('=== LISTE DES OPTIONS ===');
  WriteLn;

  for i := 1 to 10 do
    WriteLn(i:3, '. Option numéro ', i);

  WriteLn;
  WriteLn('=========================');
end.
```

**Résultat :**
```
=== LISTE DES OPTIONS ===

  1. Option numéro 1
  2. Option numéro 2
  3. Option numéro 3
  4. Option numéro 4
  5. Option numéro 5
  6. Option numéro 6
  7. Option numéro 7
  8. Option numéro 8
  9. Option numéro 9
 10. Option numéro 10

=========================
```

## Techniques avancées de formatage

### Centrer du texte

Pour centrer du texte, calculez le nombre d'espaces nécessaires :

```pascal
program CentrerTexte;  
const  
  LargeurEcran = 50;
var
  texte: string;
  espaces: integer;
begin
  texte := 'TITRE CENTRÉ';
  espaces := (LargeurEcran - Length(texte)) div 2;

  // '':espaces = chaine vide formatee sur N caracteres, ce qui produit N espaces
  WriteLn('':espaces, texte);
end.
```

### Créer des barres de progression textuelles

```pascal
program BarreProgression;  
var  
  pourcentage: integer;
  i: integer;
begin
  pourcentage := 65;

  Write('Progression : [');

  // Afficher les caractères remplis
  for i := 1 to pourcentage div 5 do
    Write('=');

  // Afficher les caractères vides
  for i := 1 to 20 - (pourcentage div 5) do
    Write(' ');

  WriteLn('] ', pourcentage, '%');
end.
```

**Résultat :**
```
Progression : [=============       ] 65%
```

### Formatage de dates et heures

```pascal
program FormatDateTime;  
var  
  jour, mois, annee: integer;
  heure, minute, seconde: integer;
begin
  jour := 15;
  mois := 3;
  annee := 2024;
  heure := 14;
  minute := 5;
  seconde := 7;

  // Format français : JJ/MM/AAAA
  WriteLn('Date : ', jour:2, '/', mois:2, '/', annee:4);

  // Format avec zéros : utiliser Write multiple
  Write('Date : ');
  if jour < 10 then Write('0');
  Write(jour, '/');
  if mois < 10 then Write('0');
  Write(mois, '/', annee);
  WriteLn;

  // Heure : HH:MM:SS
  Write('Heure : ');
  if heure < 10 then Write('0');
  Write(heure, ':');
  if minute < 10 then Write('0');
  Write(minute, ':');
  if seconde < 10 then Write('0');
  WriteLn(seconde);
end.
```

**Résultat :**
```
Date : 15/ 3/2024  
Date : 15/03/2024  
Heure : 14:05:07  
```

## Formatage conditionnel

### Afficher des valeurs avec couleur textuelle

Bien que la console standard ne supporte pas les couleurs facilement, vous pouvez utiliser des symboles :

```pascal
program FormatConditionnel;  
var  
  solde: real;
  symbole: string;
begin
  solde := -150.50;

  if solde >= 0 then
    symbole := '+'
  else
    symbole := '-';

  // abs() retourne la valeur absolue (sans le signe negatif)
  WriteLn('Solde : ', symbole, ' ', abs(solde):0:2, ' €');

  if solde < 0 then
    WriteLn('*** ATTENTION : Solde négatif ! ***');
end.
```

### Afficher des notes avec appréciation

```pascal
program NotesFormatees;  
var  
  note: real;
  appreciation: string;
  etoiles: string;
begin
  note := 15.5;

  if note >= 18 then
  begin
    appreciation := 'Excellent';
    etoiles := '*****';
  end
  else if note >= 16 then
  begin
    appreciation := 'Très bien';
    etoiles := '****';
  end
  else if note >= 14 then
  begin
    appreciation := 'Bien';
    etoiles := '***';
  end
  else if note >= 12 then
  begin
    appreciation := 'Assez bien';
    etoiles := '**';
  end
  else if note >= 10 then
  begin
    appreciation := 'Passable';
    etoiles := '*';
  end
  else
  begin
    appreciation := 'Insuffisant';
    etoiles := '';
  end;

  WriteLn('Note : ', note:4:1, '/20');
  WriteLn('Appréciation : ', appreciation:-15, ' ', etoiles);
end.
```

## Bonnes pratiques de formatage

### 1. Utiliser des constantes pour les largeurs

```pascal
const
  LargeurNom = 20;
  LargeurAge = 5;
  LargeurVille = 15;
begin
  WriteLn('Nom':-LargeurNom, 'Age':LargeurAge, 'Ville':-LargeurVille);
  WriteLn('Alice':-LargeurNom, 25:LargeurAge, 'Paris':-LargeurVille);
  WriteLn('Bob':-LargeurNom, 30:LargeurAge, 'Lyon':-LargeurVille);
end.
```

Avantage : modifier une seule constante met à jour tout le formatage.

### 2. Créer des procédures de formatage

```pascal
procedure AfficherLigne(nom: string; age: integer; ville: string);  
const  
  L1 = 20;
  L2 = 5;
  L3 = 15;
begin
  WriteLn(nom:-L1, age:L2, ville:-L3);
end;

begin
  AfficherLigne('Nom', 0, 'Ville');  // En-tête
  AfficherLigne('Alice', 25, 'Paris');
  AfficherLigne('Bob', 30, 'Lyon');
end.
```

### 3. Séparer visuellement les sections

```pascal
procedure AfficherSeparateur;  
begin  
  WriteLn('================================================');
end;

procedure AfficherTitre(titre: string);  
begin  
  AfficherSeparateur;
  WriteLn('  ', titre);
  AfficherSeparateur;
end;

begin
  AfficherTitre('MON APPLICATION');
  WriteLn('Contenu...');
  AfficherSeparateur;
end.
```

### 4. Prévoir la largeur maximale

Calculez toujours la largeur nécessaire pour les valeurs les plus longues :

```pascal
// Si le montant max est 999999.99
const
  LargeurMontant = 10;  // 10 caractères pour "999999.99"
```

### 5. Rester cohérent

Utilisez le même style de formatage dans tout votre programme :
- Même nombre de décimales pour les prix
- Même alignement pour les colonnes similaires
- Même style de séparateurs

## Erreurs courantes à éviter

### 1. Largeur insuffisante

```pascal
var
  nombre: integer;
begin
  nombre := 12345;
  WriteLn(nombre:3);    // Affiche quand même 12345 (pas tronqué)
end.
```

La largeur est un **minimum**, pas un maximum. Si le nombre est plus grand, il s'affiche quand même.

### 2. Oublier les décimales pour les prix

```pascal
var
  prix: real;
begin
  prix := 19.99;
  WriteLn(prix);        // Notation scientifique !
  WriteLn(prix:0:2);    // Mieux : 19.99
end.
```

### 3. Mélanger alignements dans un tableau

```pascal
// Mauvais : mélange d'alignements
WriteLn('Nom':20, 'Prix':-10);      // Incohérent !

// Bon : cohérent
WriteLn('Nom':-20, 'Prix':10);      // Tout aligné à droite/gauche
```

### 4. Espaces en dur dans les chaînes

```pascal
// Mauvais : espaces codés en dur
WriteLn('Nom :     ', nom);

// Bon : utiliser le formatage
WriteLn('Nom :':-10, nom);
```

### 5. Ne pas tester avec de vraies données

Testez toujours votre formatage avec :
- Des valeurs courtes et longues
- Des nombres négatifs
- Des valeurs à zéro
- Des cas extrêmes

## Récapitulatif

**Formatage des réels :**
- Syntaxe : `variable:largeur:decimales`
- Exemple : `prix:10:2` (largeur 10, 2 décimales)

**Formatage des entiers :**
- Syntaxe : `variable:largeur`
- Exemple : `age:5` (largeur 5)

**Formatage des strings :**
- Largeur positive : alignement à droite
- Largeur négative : alignement à gauche
- Exemple : `nom:-20` (gauche, largeur 20)

**Bonnes pratiques :**
- Utiliser des constantes pour les largeurs
- Rester cohérent dans tout le programme
- Tester avec des données réelles variées
- Créer des procédures réutilisables
- Prévoir la largeur maximale nécessaire

**Pour les débutants :**
- Commencez simple : `prix:0:2` pour les prix
- Ajoutez progressivement le formatage
- Testez régulièrement l'affichage
- N'hésitez pas à ajouter des espaces et des séparateurs

---

**Point clé :** Un bon formatage transforme un programme amateur en application professionnelle. Prenez l'habitude de soigner vos sorties dès le début : c'est ce que voient vos utilisateurs, et cela fait toute la différence !

⏭️ [Commentaires et documentation du code](/02-introduction-langage-pascal/08-commentaires-documentation-code.md)
