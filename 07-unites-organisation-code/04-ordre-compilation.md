🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 7.4 Ordre de compilation

## Comment Pascal compile-t-il plusieurs unités ?

Quand votre programme utilise plusieurs unités, le compilateur FreePascal doit les compiler dans le **bon ordre**. C'est comme préparer un gâteau : vous devez préparer la pâte avant de la mettre au four !

La bonne nouvelle : **vous n'avez rien à faire** ! Le compilateur calcule automatiquement l'ordre correct en analysant les dépendances.

## Le principe fondamental

**Règle d'or :** Une unité doit être compilée **avant** toutes les unités qui l'utilisent.

### Analogie : La construction d'une maison

Imaginez que vous construisez une maison en plusieurs étapes :

1. **Les fondations** (aucune dépendance)
2. **Les murs** (dépendent des fondations)
3. **Le toit** (dépend des murs)
4. **La décoration** (dépend de tout le reste)

Vous ne pouvez pas construire le toit avant les murs ! C'est la même chose avec les unités Pascal.

## Exemple simple

Voici trois unités avec des dépendances :

```pascal
// UniteBase.pas - Pas de dépendances
unit UniteBase;

interface
  function Addition(a, b: Integer): Integer;

implementation
  function Addition(a, b: Integer): Integer;
  begin
    Result := a + b;
  end;
end.
```

```pascal
// UniteCalculs.pas - Dépend de UniteBase
unit UniteCalculs;

interface  
uses  
  UniteBase;  // Dépendance

function CalculComplexe(x, y: Integer): Integer;

implementation
  function CalculComplexe(x, y: Integer): Integer;
  begin
    Result := Addition(x, y) * 2;  // Utilise Addition de UniteBase
  end;
end.
```

```pascal
// Programme principal - Dépend de UniteCalculs
program MonProgramme;

uses
  UniteCalculs;  // Dépendance

begin
  WriteLn(CalculComplexe(5, 3));
end.
```

### Ordre de compilation automatique

```
1. UniteBase       (compilée en premier - pas de dépendances)
   ↓
2. UniteCalculs    (compilée ensuite - dépend de UniteBase)
   ↓
3. MonProgramme    (compilé en dernier - dépend de UniteCalculs)
```

Le compilateur FreePascal analyse automatiquement les clauses `uses` et détermine cet ordre.

## Visualisation avec un graphe de dépendances

```
            Programme Principal
                  ↓ uses
            UniteGraphique
                /        \
          uses /          \ uses
              /            \
     UniteDessin      UniteCalculs
              \            /
          uses \          / uses
                \        /
              UniteMaths
```

**Ordre de compilation :**
1. `UniteMaths` (aucune dépendance)
2. `UniteDessin` et `UniteCalculs` (dépendent seulement de UniteMaths)
3. `UniteGraphique` (dépend de UniteDessin et UniteCalculs)
4. `Programme Principal` (dépend de UniteGraphique)

## Les fichiers .ppu (Pascal Compiled Unit)

Quand une unité est compilée, FreePascal crée un fichier avec l'extension **.ppu** (Pascal Compiled Unit).

### Exemple

```
Avant compilation :
- UniteBase.pas

Après compilation :
- UniteBase.pas      (code source)
- UniteBase.ppu      (version compilée)
- UniteBase.o        (fichier objet)
```

Le fichier `.ppu` contient :
- Le code compilé de l'unité
- L'interface de l'unité
- Des informations sur les dépendances

### Pourquoi c'est important ?

Quand vous recompilez votre projet, FreePascal vérifie :
1. **Le fichier .ppu existe-t-il ?**
2. **Le fichier .pas a-t-il été modifié depuis la dernière compilation ?**
3. **Les dépendances ont-elles changé ?**

Si rien n'a changé, l'unité **n'est pas recompilée** ! Cela accélère considérablement la compilation.

## Recompilation intelligente

### Scénario 1 : Modification d'une unité de base

```
Vous modifiez : UniteMaths.pas

FreePascal recompile :
1. UniteMaths        (modifiée)
2. UniteDessin       (dépend de UniteMaths)
3. UniteCalculs      (dépend de UniteMaths)
4. UniteGraphique    (dépend de UniteDessin et UniteCalculs)
5. Programme         (dépend de UniteGraphique)
```

**Tout ce qui dépend** de l'unité modifiée est recompilé automatiquement !

### Scénario 2 : Modification d'une unité en bout de chaîne

```
Vous modifiez : UniteGraphique.pas

FreePascal recompile :
1. UniteGraphique    (modifiée)
2. Programme         (dépend de UniteGraphique)
```

Les unités dont UniteGraphique dépend (UniteMaths, UniteDessin, UniteCalculs) ne sont **pas** recompilées.

## Compilation complète vs compilation incrémentale

### Compilation incrémentale (par défaut)
Le compilateur ne recompile que ce qui est nécessaire.

```bash
# Première compilation : tout est compilé
fpc MonProgramme.pas  
Compiling MonProgramme.pas  
Compiling UniteCalculs.pas  
Compiling UniteBase.pas  
Linking MonProgramme  

# Deuxième compilation sans modifications : presque instantané !
fpc MonProgramme.pas  
Linking MonProgramme  
```

### Compilation complète (clean build)

Dans Lazarus : **Exécuter** → **Nettoyer et Compiler**

Cela supprime tous les fichiers .ppu et recompile tout depuis zéro. Utile quand :
- Vous avez des erreurs étranges
- Vous changez d'options de compilation
- Vous voulez être sûr que tout est à jour

## Que se passe-t-il en cas de dépendance circulaire ?

Si deux unités dépendent l'une de l'autre dans leur section `interface`, le compilateur **ne peut pas** déterminer l'ordre :

```
UniteA (interface) → dépend de UniteB  
UniteB (interface) → dépend de UniteA  
```

**Erreur de compilation :**
```
Error: Circular unit reference between UniteA and UniteB
```

**Solution :** Déplacer au moins une des dépendances dans la section `implementation` (voir section 7.3).

## Les messages du compilateur

Pendant la compilation, vous verrez :

```
Free Pascal Compiler version 3.2.2  
Target: x86_64-linux  
Compiling MonProgramme.pas  
Compiling UniteGraphique.pas  
Compiling UniteDessin.pas  
Compiling UniteMaths.pas  
Assembling UniteDessin  
Assembling UniteGraphique  
Linking MonProgramme  
50 lines compiled, 0.2 sec
```

Chaque ligne "Compiling..." indique qu'une unité est en cours de compilation, dans l'ordre calculé automatiquement.

## Où sont stockés les fichiers compilés ?

### Dans Lazarus
Par défaut, les fichiers .ppu sont stockés dans :
- **Windows :** `lib\x86_64-win64\` (ou i386-win32 selon l'architecture)
- **Linux :** `lib/x86_64-linux/` (ou i386-linux)

Ces dossiers sont créés automatiquement dans le répertoire de votre projet.

### Structure typique d'un projet

```
MonProjet/
├── MonProgramme.pas           (programme principal)
├── UniteBase.pas              (unité)
├── UniteCalculs.pas           (unité)
├── MonProgramme.lpi           (fichier projet Lazarus)
└── lib/
    └── x86_64-linux/
        ├── UniteBase.ppu
        ├── UniteBase.o
        ├── UniteCalculs.ppu
        └── UniteCalculs.o
```

## Options de compilation liées à l'ordre

Dans Lazarus, vous pouvez configurer certaines options :

### Menu Projet → Options du projet → Compilation et édition de liens

- **Recompiler Clean** : Force une recompilation complète
- **Construire toutes les unités** : Recompile même ce qui n'a pas changé

## Conseils pratiques

### 1. Laissez le compilateur gérer l'ordre
Ne vous préoccupez pas de l'ordre manuellement. Concentrez-vous sur la logique de vos dépendances.

### 2. Organisez vos unités logiquement
```
UniteBase        → Fonctions générales  
UniteOutils      → Utilise UniteBase  
UniteMetier      → Utilise UniteOutils  
UniteInterface   → Utilise UniteMetier  
```

Une hiérarchie claire facilite la maintenance et la compilation.

### 3. En cas de doute, recompilez tout
Si vous avez des erreurs étranges :
1. **Projet** → **Nettoyer le répertoire du projet**
2. **Exécuter** → **Compiler**

### 4. Attention aux modifications d'interface
Si vous changez l'interface d'une unité de base (signatures de fonctions, types publics), **toutes** les unités qui en dépendent seront recompilées.

## Résumé

- Le compilateur FreePascal calcule **automatiquement** l'ordre de compilation
- Une unité est compilée **avant** toutes celles qui l'utilisent
- Les fichiers **.ppu** stockent les unités compilées
- La **compilation incrémentale** ne recompile que ce qui a changé
- Les **dépendances circulaires** dans interface empêchent la compilation
- Vous pouvez forcer une **compilation complète** si nécessaire
- L'ordre est déterminé par l'analyse des clauses **uses**

Vous n'avez généralement pas besoin de vous soucier de l'ordre de compilation : le compilateur fait le travail pour vous ! Dans la prochaine section, nous verrons comment gérer la visibilité des éléments dans vos unités.

⏭️ [Variables et procédures publiques/privées](/07-unites-organisation-code/05-variables-procedures-publiques-privees.md)
