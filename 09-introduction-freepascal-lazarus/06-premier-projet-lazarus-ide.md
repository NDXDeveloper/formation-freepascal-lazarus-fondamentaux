🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.6 Premier projet avec Lazarus IDE

## Introduction

Maintenant que Lazarus est installé sur votre ordinateur, il est temps de créer vos premiers programmes ! Cette section vous guide pas à pas dans la découverte de l'IDE et la création de deux types de projets :
1. Un **programme console** (texte, comme dans un Terminal)
2. Une **application graphique** (avec fenêtre, boutons, etc.)

Ne vous précipitez pas : prenez le temps de bien comprendre chaque étape. À la fin de cette section, vous aurez créé vos premiers programmes avec Lazarus et vous saurez naviguer dans l'interface.

**Prérequis :** Lazarus doit être installé (sections 9.4 ou 9.5).

## Première découverte : l'interface de Lazarus

### Lancer Lazarus

**Sous Windows :**
- Double-cliquez sur l'icône Lazarus sur le bureau
- Ou : Menu Démarrer → Lazarus

**Sous Linux :**
- Menu Applications → Développement → Lazarus IDE
- Ou dans un Terminal : `lazarus-ide`

**Patience !** Le premier lancement peut prendre 20-30 secondes. C'est normal.

### Les fenêtres principales

Quand Lazarus s'ouvre, vous voyez plusieurs fenêtres. Ne soyez pas intimidé ! Voici à quoi elles servent :

#### 1. La fenêtre principale (barre de menus et outils)

C'est la fenêtre en haut avec :
- **Barre de menus** : File, Edit, Search, View, Project, Run, Package, Tools, Window, Help
- **Barres d'outils** : icônes pour les actions courantes
- **Palette de composants** : onglets avec des icônes de composants (Standard, Additional, Common Controls, etc.)

**Rôle :** C'est le "centre de contrôle" de Lazarus.

#### 2. L'éditeur de code

Une grande fenêtre blanche (ou colorée selon votre thème) avec :
- Numéros de ligne à gauche
- Zone de texte au centre pour écrire votre code
- Onglets en haut si vous avez plusieurs fichiers ouverts

**Rôle :** C'est ici que vous écrirez votre code Pascal.

#### 3. L'inspecteur d'objets (Object Inspector)

Généralement à gauche ou à droite, avec deux onglets :
- **Properties** (Propriétés) : caractéristiques de l'élément sélectionné
- **Events** (Événements) : actions possibles pour cet élément

**Rôle :** Configurer visuellement les composants sans écrire de code.

#### 4. L'explorateur de projet (Project Inspector)

Une petite fenêtre listant :
- Les fichiers de votre projet
- Les unités utilisées
- Les dépendances

**Rôle :** Vue d'ensemble de votre projet.

#### 5. La fenêtre de messages

En bas (peut être masquée) :
- Messages du compilateur
- Erreurs et avertissements
- Résultats de la compilation

**Rôle :** Feedback sur votre code.

### Organisation de l'espace de travail

**Si les fenêtres sont désordonnées :**
1. Menu **Window** → **Reset Layout**
2. Choisissez un preset (par exemple "Default IDE Layout")
3. Cliquez OK

**Astuce :** Vous pouvez déplacer, redimensionner et ancrer toutes les fenêtres selon vos préférences.

### La palette de composants

Dans la fenêtre principale, vous voyez des onglets avec des icônes :
- **Standard** : boutons, labels, zones de texte...
- **Additional** : composants supplémentaires
- **Common Controls** : listes, arbres, grilles...
- **Dialogs** : boîtes de dialogue
- **Misc** : divers
- **Data Access** : accès aux bases de données
- **Data Controls** : composants liés aux données

**Pour l'instant :** Nous utiliserons surtout l'onglet **Standard**.

## Premier projet : Programme console "Hello World"

Commençons par le programme le plus simple : afficher "Hello World" dans une console.

### Étape 1 : Créer un nouveau projet

1. Menu **File** (Fichier) → **New...** (Nouveau)
2. Une fenêtre s'ouvre avec plusieurs choix
3. Cliquez sur **Simple Program** (Programme simple)
4. Cliquez sur **OK**

**Ce qui se passe :**
- Lazarus crée un nouveau programme console minimal
- L'éditeur de code s'ouvre avec un code de départ

### Étape 2 : Examiner le code généré

Vous devriez voir :

```pascal
program Project1;

begin  
end.  
```

**Explication ligne par ligne :**

```pascal
program Project1;     // Nom du programme (sera changé à la sauvegarde)
```
→ Déclare le début d'un programme nommé "Project1"

```pascal
begin                 // Début du programme principal
```
→ Marque le début du code qui sera exécuté

```pascal
end.                  // Fin du programme (notez le point !)
```
→ Marque la fin du programme. Le point final est obligatoire !

**C'est un programme vide** : il ne fait rien du tout. Ajoutons du code !

### Étape 3 : Écrire notre premier code

Entre `begin` et `end.`, ajoutez ces lignes :

```pascal
program Project1;

begin
  WriteLn('Hello World !');
  WriteLn('Ceci est mon premier programme avec Lazarus.');
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

**Explications :**

- **WriteLn('...')** : affiche du texte et va à la ligne suivante
  - "Ln" = Line (ligne)
  - Le texte est entre apostrophes simples `'...'`
  - Le point-virgule `;` termine chaque instruction

- **ReadLn** : attend que l'utilisateur appuie sur Entrée
  - Utile pour que la fenêtre ne se ferme pas immédiatement
  - Donne le temps de lire les messages

### Étape 4 : Sauvegarder le projet

**Important :** Avant de compiler, il faut sauvegarder !

1. Menu **File** → **Save** (ou Ctrl+S)
2. Une boîte de dialogue apparaît : **"Save file as..."**
3. **Conseils pour bien sauvegarder :**
   - Créez un dossier dédié pour ce projet (ex: `HelloWorld`)
   - Naviguez dans ce dossier
   - Nom du fichier : `HelloWorld.lpr` (l'extension .lpr = Lazarus Project)
   - Cliquez **Enregistrer**

**Ce qui se passe :**
- Lazarus sauvegarde votre programme
- Le titre de la fenêtre change de "Project1" à "HelloWorld"
- Le code est maintenant sécurisé sur le disque

**Bonne pratique :** Toujours créer un dossier séparé pour chaque projet !

### Étape 5 : Compiler et exécuter

Il est temps de voir votre programme fonctionner !

**Méthode 1 : Touche F9 (recommandé)**
- Appuyez simplement sur la touche **F9** de votre clavier

**Méthode 2 : Menu**
- Menu **Run** → **Run** (ou icône de triangle vert ▶)

**Ce qui se passe :**
1. Lazarus compile le programme (quelques secondes)
2. La fenêtre de messages en bas affiche les étapes de compilation
3. Si tout va bien, vous voyez : "Project compiled successfully"
4. Le programme s'exécute automatiquement

### Étape 6 : Observer le résultat

Une fenêtre console (noire ou blanche selon votre système) s'ouvre avec :

```
Hello World !  
Ceci est mon premier programme avec Lazarus.  
Appuyez sur Entrée pour quitter...  
```

Le curseur clignote, attendant que vous appuyiez sur **Entrée**.

**Appuyez sur Entrée** → la fenêtre se ferme.

**🎉 Félicitations ! Vous venez de créer et d'exécuter votre premier programme avec Lazarus !**

### Étape 7 : Modifier et recompiler

Retournez dans Lazarus. Modifions le programme :

```pascal
program HelloWorld;

begin
  WriteLn('==================================');
  WriteLn('  Bienvenue dans mon programme !');
  WriteLn('==================================');
  WriteLn;  // Ligne vide
  WriteLn('Aujourd''hui, j''apprends Pascal !');
  WriteLn('C''est facile et amusant !');
  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

**Notes importantes :**
- **Apostrophes doubles** : Pour écrire une apostrophe dans le texte, utilisez `''` (deux fois)
  - `'aujourd''hui'` affiche : aujourd'hui
- **WriteLn;** sans texte : insère une ligne vide

Sauvegardez (**Ctrl+S**) et exécutez (**F9**).

Vous voyez ? Votre programme est maintenant plus joli !

### Comprendre la compilation

Quand vous appuyez sur F9, Lazarus fait plusieurs choses :

**1. Vérification du code**
- Vérifie la syntaxe (orthographe des mots-clés, point-virgules, etc.)
- Cherche les erreurs

**2. Compilation**
- Transforme votre code Pascal en langage machine
- Crée un fichier exécutable (.exe sous Windows, sans extension sous Linux)

**3. Édition de liens (linking)**
- Combine tous les morceaux nécessaires
- Crée le programme final

**4. Exécution**
- Lance automatiquement le programme si la compilation a réussi

**Durée totale :** Quelques secondes pour un petit programme.

## Deuxième projet : Application graphique

Passons maintenant à quelque chose de plus visuel : une vraie application avec une fenêtre !

### Étape 1 : Créer une nouvelle application

1. Menu **File** → **New...** (ou Ctrl+N)
2. Choisissez **Application**
3. Cliquez **OK**

**Ce qui apparaît :**
- Un **formulaire vide** (Form1) : c'est votre future fenêtre
- L'**inspecteur d'objets** montre les propriétés de Form1
- Du **code est généré automatiquement** en arrière-plan

### Étape 2 : Découvrir le formulaire

Le formulaire (Form1) est une fenêtre grise avec :
- Un titre : "Form1"
- Des points de grille pour faciliter l'alignement
- Des poignées de redimensionnement sur les bords

**C'est votre canevas** : vous allez y placer des composants (boutons, textes, etc.).

### Étape 3 : Personnaliser le formulaire

Dans l'**Inspecteur d'objets** (Object Inspector), cherchez la propriété **Caption** :

1. Cliquez sur **Caption** dans la liste des propriétés
2. La valeur actuelle est "Form1"
3. Changez-la en : `Ma Première Application`
4. Appuyez sur **Entrée**

**Regardez le formulaire** : le titre de la fenêtre a changé !

**Autres propriétés intéressantes à modifier :**

| Propriété | Valeur suggérée | Effet |
|-----------|-----------------|-------|
| **Width** | 400 | Largeur de la fenêtre |
| **Height** | 300 | Hauteur de la fenêtre |
| **Color** | clSkyBlue | Couleur de fond (cliquez sur "..." pour choisir) |
| **Position** | poScreenCenter | Fenêtre centrée à l'écran |

**Astuce :** Vous pouvez aussi redimensionner le formulaire directement avec la souris !

### Étape 4 : Ajouter un label (étiquette de texte)

Un **label** sert à afficher du texte.

**Méthode 1 : Palette de composants**
1. Dans la fenêtre principale, onglet **Standard**
2. Trouvez l'icône **TLabel** (un "A" majuscule)
3. Cliquez dessus une fois
4. Cliquez sur le formulaire à l'endroit où vous voulez le placer
5. Un label apparaît avec le texte "Label1"

**Méthode 2 : Double-clic (plus rapide)**
1. Dans l'onglet **Standard**
2. **Double-cliquez** sur **TLabel**
3. Il apparaît automatiquement au centre du formulaire

**Modifier le label :**
1. Cliquez sur le label pour le sélectionner
2. Dans l'Inspecteur d'objets, propriété **Caption** : `Bienvenue !`
3. Propriété **Font** → cliquez sur **...** (trois points)
4. Choisissez une taille : **14** ou **18**
5. Style : **Bold** (gras)
6. Cliquez **OK**

Le texte du label change immédiatement !

**Déplacer le label :**
- Cliquez-glissez pour le déplacer
- Utilisez les flèches du clavier pour un positionnement précis

### Étape 5 : Ajouter un bouton

Un **bouton** permet à l'utilisateur de déclencher une action.

1. Onglet **Standard** de la palette
2. Double-cliquez sur **TButton**
3. Un bouton apparaît sur le formulaire

**Modifier le bouton :**
- Propriété **Caption** : `Cliquez-moi !`
- Propriété **Width** : `120` (pour que le texte soit visible)
- Propriété **Height** : `30`

Placez le bouton au centre, sous le label.

### Étape 6 : Ajouter du code au bouton

Un bouton sans code ne fait rien. Faisons-le réagir au clic !

**Méthode 1 : Double-clic sur le bouton**
1. **Double-cliquez** sur le bouton dans le formulaire
2. L'éditeur de code s'ouvre automatiquement
3. Une procédure est créée : `TForm1.Button1Click`

**Méthode 2 : Via l'inspecteur d'objets**
1. Sélectionnez le bouton
2. Inspecteur d'objets → onglet **Events**
3. Trouvez **OnClick**
4. Double-cliquez dans la case vide à droite
5. L'éditeur s'ouvre

Vous voyez maintenant :

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  

end;
```

**Entre `begin` et `end;`, ajoutez :**

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  ShowMessage('Bonjour ! Vous avez cliqué sur le bouton !');
end;
```

**Explication :**
- **ShowMessage('...')** : affiche une boîte de dialogue avec un message
- C'est une procédure prédéfinie de Lazarus
- Elle crée automatiquement une fenêtre avec un bouton OK

### Étape 7 : Sauvegarder le projet

1. Menu **File** → **Save All** (Ctrl+Shift+S)
2. Deux boîtes de dialogue apparaissent :

**Première boîte : Sauvegarder l'unité**
- Créez un nouveau dossier : `MonAppliGraphique`
- Entrez dans ce dossier
- Nom : `unit1.pas` (ou laissez le nom proposé)
- Cliquez **Enregistrer**

**Deuxième boîte : Sauvegarder le projet**
- Même dossier
- Nom : `MonAppli.lpi`
- Cliquez **Enregistrer**

**Bonne pratique :** Un dossier = un projet complet !

### Étape 8 : Exécuter l'application

Appuyez sur **F9** (ou menu **Run** → **Run**).

**Ce qui se passe :**
1. Compilation (quelques secondes)
2. Une vraie fenêtre Windows/Linux s'ouvre avec :
   - Votre titre personnalisé
   - Votre label "Bienvenue !"
   - Votre bouton "Cliquez-moi !"

**Testez :** Cliquez sur le bouton !
→ Une boîte de dialogue apparaît avec votre message !

**🎊 Bravo ! Vous venez de créer votre première application graphique !**

### Étape 9 : Améliorer l'application

Retournez dans Lazarus. Ajoutons d'autres éléments !

**Ajouter un deuxième label :**
1. Double-cliquez sur **TLabel**
2. Propriété **Caption** : `Compteur : 0`
3. Propriété **Name** : `LabelCompteur`
4. Placez-le sous le bouton

**Modifier le code du bouton :**

Retournez dans le code (F12 pour basculer entre code et formulaire).

Dans l'onglet `unit1.pas`, à la toute fin du fichier avant le dernier `end.`, ajoutez une variable globale :

```pascal
var
  Form1: TForm1;
  Compteur: Integer = 0;  // Variable globale pour compter les clics
```

Puis modifiez le code du bouton :

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  Compteur := Compteur + 1;
  LabelCompteur.Caption := 'Compteur : ' + IntToStr(Compteur);

  if Compteur = 1 then
    ShowMessage('Premier clic !')
  else if Compteur = 5 then
    ShowMessage('Bravo ! Vous avez cliqué 5 fois !')
  else if Compteur = 10 then
    ShowMessage('Champion ! 10 clics !');
end;
```

**Explication :**
- `Compteur := Compteur + 1;` : augmente le compteur de 1
- `IntToStr(Compteur)` : convertit le nombre en texte
- `LabelCompteur.Caption := ...` : change le texte du label
- Les `if` affichent des messages à certains moments

Sauvegardez (**Ctrl+S**) et exécutez (**F9**).

Maintenant, chaque clic incrémente le compteur, et des messages spéciaux apparaissent !

## Comprendre la structure d'un projet Lazarus

### Les fichiers d'un projet console

Pour le projet **HelloWorld**, Lazarus a créé :

```
HelloWorld/
  ├── HelloWorld.lpr      # Le code source principal
  ├── HelloWorld.lps      # Paramètres de session (optionnel)
  ├── lib/                # Dossier de compilation (fichiers temporaires)
  └── HelloWorld.exe      # Le programme exécutable (Windows)
      ou HelloWorld       # (Linux)
```

**Fichiers importants :**
- **.lpr** (Lazarus Project) : votre code source
- **.exe** ou exécutable : le programme compilé, prêt à distribuer

**Fichiers temporaires :**
- Le dossier **lib/** contient des fichiers générés lors de la compilation
- Vous pouvez le supprimer sans risque (il sera recréé à la prochaine compilation)

### Les fichiers d'un projet graphique

Pour **MonAppliGraphique**, c'est plus complexe :

```
MonAppliGraphique/
  ├── MonAppli.lpi        # Fichier projet principal
  ├── MonAppli.lpr        # Code source du programme
  ├── MonAppli.lps        # Paramètres de session
  ├── unit1.pas           # Code de votre formulaire
  ├── unit1.lfm           # Description visuelle du formulaire
  ├── lib/                # Fichiers de compilation
  └── MonAppli.exe        # L'exécutable
```

**Rôle de chaque fichier :**

| Fichier | Description |
|---------|-------------|
| **.lpi** | Fichier projet (Project Information) - configuration |
| **.lpr** | Programme principal (comme program1.pas) |
| **.pas** | Unités de code (vos formulaires et fonctions) |
| **.lfm** | Formulaires (Layout Form) - structure visuelle |
| **.lps** | Session (état de l'IDE, fenêtres ouvertes...) |

**Fichiers essentiels à sauvegarder :**
- ✅ .lpi
- ✅ .lpr
- ✅ .pas
- ✅ .lfm

**Fichiers qu'on peut ignorer :**
- ❌ .lps (sera recréé)
- ❌ lib/ (sera recréé)
- ❌ .exe/.o/.ppu (seront recréés)

### Le fichier .lpi (Project Information)

C'est le fichier que Lazarus ouvre quand vous chargez un projet. Il contient :
- La liste des fichiers du projet
- Les options de compilation
- Les paramètres de l'application
- Les packages utilisés

**Pour ouvrir un projet :** File → Open Project → sélectionnez le .lpi

### Le fichier .lfm (Layout Form)

Format texte décrivant visuellement le formulaire :

```
object Form1: TForm1
  Left = 200
  Height = 300
  Top = 150
  Width = 400
  Caption = 'Ma Première Application'
  Color = clSkyBlue
  object Button1: TButton
    Left = 140
    Height = 30
    Top = 120
    Width = 120
    Caption = 'Cliquez-moi !'
    OnClick = Button1Click
  end
end
```

**N'éditez pas ce fichier manuellement !** Utilisez le concepteur visuel de Lazarus.

## Compiler vs Exécuter

Lazarus offre plusieurs options :

### Compiler seulement (Ctrl+F9)

Menu **Run** → **Compile** (ou Ctrl+F9)
- Compile le programme
- Crée l'exécutable
- **Mais ne le lance pas**

**Utile pour :**
- Vérifier qu'il n'y a pas d'erreurs
- Compiler avant de distribuer

### Compiler et Exécuter (F9)

Menu **Run** → **Run** (ou F9)
- Compile
- Puis lance automatiquement
- **C'est ce que vous utiliserez le plus souvent**

### Build (Shift+F9)

Menu **Run** → **Build** (ou Shift+F9)
- Recompile **tout** depuis zéro
- Même les parties non modifiées

**Utile quand :**
- Vous avez des erreurs bizarres
- Après avoir modifié les options du projet

### Quick Compile

Menu **Run** → **Quick Compile**
- Compilation rapide sans lancer

### Clean up

Menu **Run** → **Clean up and Build**
- Supprime tous les fichiers temporaires
- Puis recompile tout
- **Solution miracle en cas de problème de compilation !**

## Messages de compilation

### Messages normaux (verts)

```
Compiling unit1.pas  
Compiling MonAppli.lpr  
Linking MonAppli.exe  
Project "MonAppli" successfully built.  
```

✅ Tout va bien !

### Avertissements (jaunes/oranges)

```
Warning: Variable "test" is declared but never used
```

⚠️ Pas d'erreur, mais attention : code perfectible.

**Conseil :** Corrigez les avertissements, c'est une bonne pratique.

### Erreurs (rouges)

```
Error: Identifier not found "WritLn"  
Error: Illegal expression  
Error: ";" expected but "." found  
```

❌ Erreur bloquante : le programme ne compile pas.

**Double-cliquez sur l'erreur** → Lazarus positionne le curseur à l'endroit du problème.

### Erreurs courantes de débutant

**1. Point-virgule oublié**
```pascal
WriteLn('Bonjour')  // Erreur : ; manquant  
WriteLn('Au revoir');  
```
→ Solution : ajouter `;` à la fin de chaque instruction

**2. Apostrophe mal fermée**
```pascal
WriteLn('Bonjour);  // Erreur : apostrophe manquante
```
→ Solution : `WriteLn('Bonjour');`

**3. Faute de frappe dans un mot-clé**
```pascal
begn  // Erreur : begin mal écrit  
end;  
```
→ Solution : attention à l'orthographe !

**4. Oubli du point final**
```pascal
end  // Erreur : le point est obligatoire à la fin du programme !
```
→ Solution : `end.`

**5. Begin/End non appariés**
```pascal
if x > 5 then
  begin
    WriteLn('Grand');
  // Erreur : end; manquant
```
→ Solution : chaque `begin` doit avoir son `end;`

## Organisation et bonnes pratiques

### Structure des dossiers

Organisez vos projets de manière claire :

```
MesProjetsLazarus/
  ├── 01-HelloWorld/
  │   └── HelloWorld.lpr
  ├── 02-MonAppliGraphique/
  │   ├── MonAppli.lpi
  │   └── unit1.pas
  ├── 03-Calculatrice/
  │   └── ...
  └── 04-JeuMemoire/
      └── ...
```

**Règles d'or :**
- 📁 Un dossier = un projet
- 📝 Noms de dossiers sans espaces ni accents
- 🔢 Numérotez vos projets pour les retrouver facilement
- 💾 Sauvegardez régulièrement (Ctrl+S)

### Nommer ses composants

Au lieu de garder `Button1`, `Label1`, donnez des noms explicites :

**Mauvais noms :**
- Button1, Button2, Button3
- Label1, Label2
- Edit1

**Bons noms :**
- BtnCalculer, BtnQuitter, BtnNouveau
- LblTitre, LblResultat, LblInfo
- EdtNom, EdtPrenom, EdtAge

**Convention :** Préfixe de 3 lettres + nom descriptif
- Btn = Button
- Lbl = Label
- Edt = Edit
- Lst = ListBox
- Cmb = ComboBox

**Pour changer le nom :**
Propriété **Name** dans l'inspecteur d'objets.

### Commenter son code

Prenez l'habitude de commenter :

```pascal
// Ceci est un commentaire sur une ligne

{ Ceci est un commentaire
  sur plusieurs
  lignes }

(* Alternative pour les commentaires
   multi-lignes *)
```

**Exemple de code bien commenté :**

```pascal
procedure TForm1.BtnCalculerClick(Sender: TObject);  
var  
  Nombre1, Nombre2, Resultat: Integer;
begin
  // Récupération des valeurs saisies
  Nombre1 := StrToInt(EdtNombre1.Text);
  Nombre2 := StrToInt(EdtNombre2.Text);

  // Calcul de la somme
  Resultat := Nombre1 + Nombre2;

  // Affichage du résultat
  LblResultat.Caption := 'Résultat : ' + IntToStr(Resultat);
end;
```

### Sauvegardes et versioning

**Sauvegardez souvent !**
- Ctrl+S après chaque modification significative
- Ctrl+Shift+S pour tout sauvegarder

**Créez des sauvegardes :**
- Copiez votre dossier projet régulièrement
- Exemple : `MonAppli-backup-2025-01-15/`
- Ou utilisez Git (plus avancé, vous l'apprendrez plus tard)

## Raccourcis clavier essentiels

Mémorisez ces raccourcis pour être plus efficace :

| Raccourci | Action |
|-----------|--------|
| **F9** | Compiler et exécuter |
| **Ctrl+F9** | Compiler seulement |
| **Shift+F9** | Build (tout recompiler) |
| **F12** | Basculer code ↔ formulaire |
| **Ctrl+S** | Sauvegarder le fichier courant |
| **Ctrl+Shift+S** | Tout sauvegarder |
| **Ctrl+Space** | Auto-complétion |
| **Ctrl+Shift+C** | Complétion de code |
| **Ctrl+Click** | Aller à la définition |
| **Ctrl+F** | Rechercher dans le code |
| **Ctrl+H** | Rechercher et remplacer |
| **Ctrl+/** | Commenter/décommenter ligne |
| **Tab** | Indenter |
| **Shift+Tab** | Désindenter |

## Déboguer un programme simple

### Utiliser WriteLn pour déboguer

La méthode la plus simple :

```pascal
var
  x, y: Integer;
begin
  x := 10;
  WriteLn('Valeur de x : ', x);  // Affiche : Valeur de x : 10

  y := x * 2;
  WriteLn('Valeur de y : ', y);  // Affiche : Valeur de y : 20

  ReadLn;
end.
```

**Astuce :** Ajoutez des WriteLn pour voir ce qui se passe à chaque étape.

### Utiliser ShowMessage dans les applications graphiques

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Valeur: Integer;
begin
  Valeur := 42;
  ShowMessage('Valeur = ' + IntToStr(Valeur));
end;
```

### Points d'arrêt (basique)

**Placer un point d'arrêt :**
1. Cliquez dans la marge grise à gauche d'une ligne de code
2. Un rond rouge apparaît
3. Lancez le programme (F9)
4. Le programme s'arrête à cette ligne
5. Vous pouvez inspecter les variables

**Continuer l'exécution :**
- F9 : continuer jusqu'au prochain point d'arrêt
- F8 : exécuter ligne par ligne

**Nous verrons le débogage en détail au chapitre 20.**

## Distribuer votre programme

### Où se trouve l'exécutable ?

Dans votre dossier projet :
- Windows : `MonProjet.exe`
- Linux : `MonProjet` (pas d'extension)

**Sous Windows :**
Vous pouvez double-cliquer sur le .exe pour le lancer directement (sans Lazarus).

**Sous Linux :**
```bash
cd /chemin/vers/projet
./MonProjet
```

### Partager avec d'autres

**Programme console :** Copiez simplement le .exe (Windows) ou l'exécutable (Linux).

**Application graphique :**
- Windows : le .exe suffit généralement
- Linux : peut nécessiter des bibliothèques (nous verrons ça plus tard)

**Note :** Pour une distribution professionnelle, il faut créer un installateur (nous verrons ça dans les chapitres avancés).

## Conclusion

Félicitations ! Vous venez de franchir une étape majeure dans votre apprentissage de la programmation avec Lazarus.

**Ce que vous avez accompli dans cette section :**
- ✅ Découvert l'interface de Lazarus
- ✅ Créé votre premier programme console
- ✅ Créé votre première application graphique avec boutons
- ✅ Compris la structure d'un projet
- ✅ Appris à compiler et exécuter
- ✅ Découvert les fichiers d'un projet
- ✅ Appris les bases du débogage
- ✅ Connu les bonnes pratiques d'organisation

**Compétences acquises :**
- Navigation dans l'IDE Lazarus
- Création de projets console et graphiques
- Utilisation de l'inspecteur d'objets
- Placement de composants visuels
- Écriture de code événementiel
- Compilation et exécution
- Correction d'erreurs simples

**Prochaines étapes :**
Dans les sections suivantes, nous allons :
- Explorer en profondeur la structure d'un projet (section 9.7)
- Apprendre à configurer Lazarus finement (section 9.8)
- Approfondir la compilation et l'exécution (section 9.9)
- Découvrir l'aide et la documentation intégrées (section 9.10)

**N'oubliez pas :**
- Pratiquez régulièrement
- Expérimentez : changez les propriétés, ajoutez des composants
- Ne craignez pas de faire des erreurs, c'est en se trompant qu'on apprend !
- Sauvegardez souvent votre travail

**🚀 Vous êtes maintenant capable de créer vos propres programmes avec Lazarus !**

---

**Points clés à retenir :**
- L'interface Lazarus est composée de plusieurs fenêtres complémentaires
- Un projet console = fichier .lpr simple
- Un projet graphique = .lpi + .lpr + .pas + .lfm
- F9 = compiler et exécuter (raccourci le plus important)
- F12 = basculer entre code et formulaire
- L'inspecteur d'objets configure les composants visuellement
- ShowMessage() = afficher un message dans une appli graphique
- WriteLn() = afficher dans la console
- Toujours créer un dossier par projet
- Sauvegarder régulièrement avec Ctrl+S
- Les erreurs de compilation indiquent exactement où est le problème

⏭️ [Structure d'un projet Lazarus](/09-introduction-freepascal-lazarus/07-structure-projet-lazarus.md)
