🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 20.1 Utilisation Avancée du Débogueur Lazarus

## Introduction

Le débogueur est l'un des outils les plus puissants à la disposition d'un programmeur. Il permet d'examiner le comportement de votre programme ligne par ligne, d'inspecter les valeurs des variables en temps réel, et de comprendre pourquoi votre code ne fonctionne pas comme prévu. Dans cette section, nous allons explorer les fonctionnalités avancées du débogueur intégré à Lazarus.

**Prérequis** : Avoir configuré Lazarus correctement et savoir compiler un projet.

---

## 1. Configuration Initiale du Débogueur

### 1.1 Vérifier la Configuration

Avant d'utiliser le débogueur, assurez-vous que votre projet est compilé en mode débogage :

**Sous Windows et Linux :**

1. Allez dans le menu **Projet** → **Options du projet**
2. Dans la section **Options du compilateur**
3. Vérifiez que l'option **Informations de débogage** est activée
4. Cochez **Générer les informations de débogage pour GDB** (-g)

### 1.2 Paramètres de Compilation Recommandés

Pour un débogage optimal, configurez les options suivantes :

- **Niveau d'optimisation** : Aucune optimisation (-O-) ou niveau 1 au maximum
- **Informations de débogage** : Activées
- **Range checking** : Activé (détecte les dépassements de tableaux)
- **Overflow checking** : Activé (détecte les débordements arithmétiques)

> **Note** : Les optimisations peuvent rendre le débogage difficile car le compilateur réorganise le code.

---

## 2. Démarrage et Contrôle de l'Exécution

### 2.1 Modes de Lancement

Il existe plusieurs façons de démarrer le débogage :

**Démarrage Normal**
- Menu : **Exécuter** → **Exécuter** (F9)
- Lance le programme normalement, s'arrête aux points d'arrêt

**Démarrage avec Pause**
- Menu : **Exécuter** → **Pas à pas approfondi** (F7)
- Lance le programme et s'arrête immédiatement à la première ligne

**Démarrage jusqu'au Curseur**
- Menu : **Exécuter** → **Exécuter jusqu'au curseur** (F4)
- Lance le programme et s'arrête à la ligne où se trouve le curseur

### 2.2 Commandes de Contrôle Essentielles

Une fois en mode débogage, vous disposez de plusieurs commandes :

| Commande | Raccourci | Description |
|----------|-----------|-------------|
| **Pas à pas approfondi** | F7 | Execute la ligne courante et entre dans les fonctions/procédures |
| **Pas à pas** | F8 | Execute la ligne courante sans entrer dans les fonctions |
| **Pas à pas sortant** | Shift+F8 | Sort de la fonction courante et s'arrête après l'appel |
| **Exécuter** | F9 | Continue l'exécution jusqu'au prochain point d'arrêt |
| **Pause** | - | Interrompt l'exécution du programme |
| **Arrêter** | Ctrl+F2 | Termine la session de débogage |

**Exemple de Scénario :**

```pascal
procedure CalculerTotal(a, b: Integer);
var
  resultat: Integer;
begin
  resultat := a + b;              // Ligne 1
  WriteLn('Total: ', resultat);   // Ligne 2
end;

begin
  CalculerTotal(10, 20);           // Ligne A
  WriteLn('Fin du programme');     // Ligne B
end.
```

- **F8 sur Ligne A** : Execute l'appel complet et s'arrête à la Ligne B
- **F7 sur Ligne A** : Entre dans la procédure et s'arrête à la Ligne 1

---

## 3. Points d'Arrêt (Breakpoints)

### 3.1 Types de Points d'Arrêt

Les points d'arrêt sont des marqueurs qui indiquent au débogueur où suspendre l'exécution.

**Point d'Arrêt Simple**
- Cliquez dans la marge gauche de l'éditeur (à gauche des numéros de ligne)
- Un cercle rouge apparaît
- Le programme s'arrêtera systématiquement à cette ligne

**Point d'Arrêt Désactivé**
- Clic droit sur le point d'arrêt → **Désactiver**
- Le cercle devient gris
- Le point reste en place mais n'est plus actif

### 3.2 Gestion des Points d'Arrêt

**Fenêtre Points d'Arrêt**
- Menu : **Voir** → **Fenêtres de débogage** → **Points d'arrêt**
- Liste tous les points d'arrêt du projet
- Permet d'activer/désactiver/supprimer en masse

**Actions Disponibles :**
- **Activer/Désactiver** : Cochez ou décochez
- **Supprimer** : Bouton Supprimer ou touche Suppr
- **Supprimer tous** : Bouton correspondant
- **Activer/Désactiver tous** : Boutons correspondants

### 3.3 Points d'Arrêt Conditionnels

Les points d'arrêt conditionnels ne s'activent que si une condition est vraie. C'est extrêmement utile dans les boucles !

**Comment Créer un Point d'Arrêt Conditionnel :**

1. Placez un point d'arrêt normal
2. Clic droit sur le cercle rouge → **Propriétés du point d'arrêt**
3. Dans la fenêtre qui s'ouvre, entrez une **condition**

**Exemple Pratique :**

```pascal
for i := 1 to 1000 do
begin
  Traiter(i);  // Point d'arrêt ici
end;
```

Sans condition, le programme s'arrêterait 1000 fois ! Avec une condition comme `i = 500`, il s'arrête uniquement quand i vaut 500.

**Syntaxe des Conditions :**
- `i = 500` : S'arrête quand i égale 500
- `i > 100` : S'arrête quand i dépasse 100
- `(i mod 10 = 0)` : S'arrête tous les 10 itérations
- `s = 'erreur'` : S'arrête quand la chaîne s contient 'erreur'

**Compteur de Passages :**

Vous pouvez aussi configurer un point d'arrêt pour s'activer après N passages :

1. Propriétés du point d'arrêt
2. Champ **Nombre de passages** : entrez une valeur (ex: 50)
3. Le point s'activera au 50ème passage

---

## 4. Inspection des Variables

### 4.1 Survol de Variables (Tooltips)

La méthode la plus simple :

1. Placez le curseur de la souris sur une variable dans le code
2. Attendez une seconde
3. Une info-bulle apparaît avec la valeur actuelle

**Limitations :**
- Ne fonctionne que si le programme est en pause
- N'affiche que les types simples clairement

### 4.2 Fenêtre Variables Locales

**Accès :** Menu **Voir** → **Fenêtres de débogage** → **Variables locales**

Cette fenêtre affiche automatiquement :
- Toutes les variables de la procédure/fonction courante
- Les paramètres de la fonction
- Les valeurs en temps réel

**Avantages :**
- Mise à jour automatique à chaque pas
- Affichage structuré (records, tableaux)
- Possibilité d'explorer les structures complexes

**Exemple de Lecture :**

```pascal
procedure Exemple(param: Integer);
var
  x: Integer;
  s: String;
begin
  x := param * 2;
  s := 'Test';     // Arrêt ici
end;
```

La fenêtre affichera :
```
param = 10
x = 20
s = 'Test'
```

### 4.3 Fenêtre Inspecteur

**Accès :** Menu **Voir** → **Fenêtres de débogage** → **Inspecteur**

L'inspecteur permet d'examiner EN DÉTAIL une variable spécifique :

**Comment l'Utiliser :**
1. Sélectionnez une variable dans le code
2. Clic droit → **Inspecter**
3. Ou tapez le nom de la variable dans le champ de l'inspecteur

**Cas d'Usage :**
- **Enregistrements (records)** : Voir tous les champs
- **Tableaux** : Voir toutes les valeurs
- **Objets** : Voir les propriétés et leur état
- **Pointeurs** : Voir l'adresse et la valeur pointée

**Exemple avec un Record :**

```pascal
type
  TPersonne = record
    Nom: String;
    Age: Integer;
    Actif: Boolean;
  end;

var
  p: TPersonne;
begin
  p.Nom := 'Dupont';
  p.Age := 30;
  p.Actif := True;
  // Point d'arrêt ici
end;
```

L'inspecteur sur `p` montrera :
```
p
├─ Nom = 'Dupont'
├─ Age = 30
└─ Actif = True
```

### 4.4 Fenêtre Espions (Watches)

Les espions sont des variables que vous voulez surveiller EN PERMANENCE, même quand vous changez de fonction.

**Ajouter un Espion :**

**Méthode 1 :**
1. Sélectionnez une variable dans le code
2. Clic droit → **Ajouter un espion**

**Méthode 2 :**
1. Ouvrez la fenêtre Espions : **Voir** → **Fenêtres de débogage** → **Espions**
2. Cliquez sur le bouton **+** ou **Ajouter**
3. Entrez le nom de la variable ou une expression

**Expressions Complexes :**

Vous pouvez espionner plus que de simples variables :

- `Tableau[5]` : Une case spécifique d'un tableau
- `Record.Champ` : Un champ d'un enregistrement
- `Objet.Propriete` : Une propriété d'objet
- `i * 2` : Une expression calculée
- `Length(MaChaine)` : Résultat d'une fonction

**Mise à Jour :**
- Les espions se mettent à jour automatiquement à chaque pas
- Ils restent visibles même si vous sortez de la fonction

---

## 5. Pile d'Appels (Call Stack)

### 5.1 Comprendre la Pile d'Appels

La pile d'appels montre la séquence des appels de fonctions qui ont mené au point actuel.

**Accès :** Menu **Voir** → **Fenêtres de débogage** → **Pile d'appels**

**Exemple :**

```pascal
procedure NiveauC;
begin
  WriteLn('C');  // Arrêt ici
end;

procedure NiveauB;
begin
  NiveauC;
end;

procedure NiveauA;
begin
  NiveauB;
end;

begin
  NiveauA;
end.
```

**La pile d'appels affichera (de haut en bas) :**
```
1. NiveauC (ligne actuelle)
2. NiveauB
3. NiveauA
4. Programme principal
```

### 5.2 Navigation dans la Pile

**Double-clic sur un niveau :**
- Vous amène au code de ce niveau
- Les variables locales changent pour ce niveau
- Utile pour comprendre comment vous êtes arrivés là

**Cas d'Usage Typique :**

Vous avez une erreur dans `NiveauC`, mais vous voulez savoir QUELLES valeurs ont été passées depuis `NiveauA`. Vous double-cliquez sur `NiveauA` dans la pile pour voir ses variables locales.

---

## 6. Inspection de la Mémoire et Registres

### 6.1 Vue Assembleur

Pour les développeurs qui veulent voir le code machine généré :

**Accès :** Menu **Voir** → **Fenêtres de débogage** → **Assembleur**

**Utilisation :**
- Affiche le code assembleur correspondant au Pascal
- Utile pour l'optimisation de performances critiques
- Permet de voir exactement ce que fait le processeur

**Quand l'Utiliser :**
- Débogage de code système
- Problèmes de performances extrêmes
- Compréhension approfondie du fonctionnement interne

### 6.2 Fenêtre Registres

**Accès :** Menu **Voir** → **Fenêtres de débogage** → **Registres**

Affiche l'état des registres du processeur (EAX, EBX, ESP, etc.).

**Usage Avancé :**
- Débogage de code critique
- Interface avec du code assembleur
- Analyse de plantages système

> **Note pour Débutants :** Ces fonctionnalités sont rarement nécessaires pour le développement normal d'applications. Concentrez-vous d'abord sur les variables, points d'arrêt et pile d'appels.

---

## 7. Débogage d'Exceptions

### 7.1 Configuration des Exceptions

Par défaut, Lazarus s'arrête quand une exception non gérée se produit. Vous pouvez configurer ce comportement :

**Menu :** **Outils** → **Options du débogueur** → **Exceptions**

**Options :**
- **Ignorer cette exception** : Continue sans s'arrêter
- **Arrêter sur cette exception** : Pause le programme
- Configurable par TYPE d'exception

### 7.2 Analyser une Exception

Quand une exception se produit :

1. Le débogueur s'arrête sur la ligne problématique
2. Un message décrit l'exception
3. Utilisez la pile d'appels pour remonter à l'origine

**Exemple Typique :**

```pascal
var
  tab: array[1..10] of Integer;
  i: Integer;
begin
  i := 15;
  tab[i] := 100;  // Exception : indice hors limites !
end.
```

**Analyse :**
1. Le débogueur s'arrête sur `tab[i] := 100`
2. Inspectez la variable `i` : elle vaut 15
3. Le tableau n'a que 10 éléments (1..10)
4. L'erreur est claire : l'indice est trop grand

---

## 8. Astuces et Bonnes Pratiques

### 8.1 Stratégies de Débogage Efficaces

**Méthode Dichotomique :**
- Placez un point d'arrêt au milieu du code suspect
- Si l'erreur est avant : cherchez dans la première moitié
- Si l'erreur est après : cherchez dans la seconde moitié
- Répétez jusqu'à trouver la ligne exacte

**Principe de l'Entonnoir :**
1. Commencez large : point d'arrêt au début de la fonction
2. Examinez les variables d'entrée
3. Avancez pas à pas jusqu'à voir une valeur incorrecte
4. Vous avez trouvé la source du problème

### 8.2 Utilisation des WriteLn pour Débogage

Bien que le débogueur soit puissant, les `WriteLn` ont leur place :

**Avantages :**
- Pas besoin d'arrêter le programme
- Utile pour voir l'évolution dans les boucles
- Laisse une trace persistante

**Exemple :**

```pascal
for i := 1 to 100 do
begin
  WriteLn('Itération ', i, ' - Valeur: ', Tableau[i]);
  Traiter(Tableau[i]);
end;
```

**Astuce :** Utilisez un fichier log au lieu de WriteLn pour ne pas polluer la console :

```pascal
AssignFile(LogFile, 'debug.log');
Rewrite(LogFile);
WriteLn(LogFile, 'Debug info...');
CloseFile(LogFile);
```

### 8.3 Débogage Multi-plateforme (Windows/Linux)

**Différences à Connaître :**

| Aspect | Windows | Linux |
|--------|---------|-------|
| Débogueur | GDB inclus | GDB système (à installer) |
| Installation | Lazarus contient tout | `sudo apt install gdb` |
| Chemins | Backslash `\` | Slash `/` |
| Sensibilité casse | Non | Oui (fichiers) |

**Installation GDB sous Ubuntu :**

```bash
sudo apt update
sudo apt install gdb
sudo apt install fpc-source  # Sources FreePascal (recommandé)
```

**Configuration Lazarus Linux :**
- **Outils** → **Options** → **Débogueur**
- Vérifiez que le chemin vers GDB est correct (généralement `/usr/bin/gdb`)

### 8.4 Problèmes Courants et Solutions

**Le débogueur ne s'arrête pas aux points d'arrêt**
- ✓ Vérifiez que les informations de débogage sont activées
- ✓ Recompilez le projet complètement (**Exécuter** → **Nettoyer et compiler**)
- ✓ Vérifiez que vous êtes en mode Debug, pas Release

**Les valeurs de variables sont "optimisées"**
- ✓ Désactivez les optimisations du compilateur (-O-)
- ✓ Recompilez

**Le programme est trop lent en mode débogage**
- ✓ Normal ! Le débogage ralentit l'exécution
- ✓ Utilisez des points d'arrêt conditionnels au lieu de F8 répété
- ✓ Pour la version finale, compilez sans informations de débogage

**Erreur "Cannot find unit X used by Y"**
- ✓ Chemin de recherche des unités incorrect
- ✓ **Projet** → **Options** → **Chemins de recherche**

---

## 9. Raccourcis Clavier Essentiels

Mémoriser ces raccourcis accélère considérablement le débogage :

| Action | Windows/Linux |
|--------|---------------|
| Basculer point d'arrêt | F5 |
| Exécuter | F9 |
| Pas à pas | F8 |
| Pas à pas approfondi | F7 |
| Exécuter jusqu'au curseur | F4 |
| Évaluer/Modifier | Ctrl+F7 |
| Ajouter espion | Ctrl+F5 |
| Voir valeur | Alt+F8 (sur variable) |
| Arrêter | Ctrl+F2 |

---

## 10. Récapitulatif

Le débogueur Lazarus est un outil extrêmement puissant qui vous fait gagner énormément de temps. Les fonctionnalités essentielles à maîtriser sont :

1. **Points d'arrêt conditionnels** : Pour arrêter uniquement quand nécessaire
2. **Inspection de variables** : Espions et variables locales
3. **Pile d'appels** : Pour comprendre le cheminement du programme
4. **Pas à pas** : F7/F8 pour avancer ligne par ligne
5. **Évaluation d'expressions** : Pour tester des calculs à la volée

**Conseil Final :** Le débogage est une compétence qui s'améliore avec la pratique. N'hésitez pas à expérimenter avec le débogueur même sur du code qui fonctionne, pour vous familiariser avec les outils.

**Prochaine Étape :** Dans la section suivante (20.2 Points d'arrêt conditionnels), nous approfondirons les techniques avancées de points d'arrêt pour un débogage encore plus efficace.

---

**Multi-plateforme Windows/Ubuntu** ✓  
**Formation FreePascal/Lazarus - Niveau Débutant à Intermédiaire**  

⏭️ [Points d'arrêt conditionnels](/20-debogage-optimisation/02-points-arret-conditionnels.md)
