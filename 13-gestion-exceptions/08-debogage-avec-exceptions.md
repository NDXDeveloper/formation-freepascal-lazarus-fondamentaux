🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.8 Débogage avec exceptions

## Introduction

Le débogage est l'art de trouver et corriger les erreurs dans votre code. Quand il s'agit d'exceptions, le débogage peut être particulièrement délicat : l'erreur peut se produire loin de sa cause réelle, les messages peuvent être cryptiques, et le flux d'exécution est perturbé. Heureusement, Lazarus offre d'excellents outils pour vous aider.

## Comprendre les messages d'exception

### Anatomie d'un message d'exception

Quand une exception non gérée se produit, vous voyez généralement quelque chose comme :

```
Project MyApp raised exception class 'EConvertError' with message:
'123abc' is not a valid integer value

Stack trace:
  $0040A2B1  STRTOINTS,  line 45 of StrUtils.pas
  $0040B158  PROCESSDATA,  line 128 of Main.pas
  $0040C234  BUTTONCLICK,  line 87 of Main.pas
```

**Décomposition :**

1. **Classe d'exception** : `EConvertError` - indique le type d'erreur
2. **Message** : `'123abc' is not a valid integer value` - décrit le problème
3. **Stack trace** : montre le chemin d'exécution qui a mené à l'erreur

### Lire la pile d'appels (Stack Trace)

La pile d'appels se lit **de bas en haut** :

```
$0040A2B1  STRTOINTS       ← L'erreur s'est produite ICI
$0040B158  PROCESSDATA     ← Appelé par cette fonction
$0040C234  BUTTONCLICK     ← Qui a été appelée par ce handler
```

**Interprétation :**
1. L'utilisateur a cliqué sur un bouton
2. Le handler `ButtonClick` a appelé `ProcessData`
3. `ProcessData` a appelé `StrToIntS`
4. C'est dans `StrToIntS` que l'exception a été levée

## Configurer Lazarus pour le débogage d'exceptions

### Activer les informations de débogage

Pour déboguer efficacement, votre programme doit être compilé avec les informations de débogage.

**Menu : Projet > Options du projet > Compilation et édition de liens**

Cochez :
- ☑ **Générer les informations de débogage pour GDB** (`-g`)
- ☑ **Utiliser les informations des numéros de ligne** (`-gl`)
- ☑ **Utiliser Heaptrc** (détection des fuites mémoire)

**Menu : Outils > Options de débogueur**

Configurez :
- Type de débogueur : **GDB (GNU Debugger)**
- Options supplémentaires si nécessaire

### Compiler en mode débogage vs Release

```pascal
// Mode débogage : symboles inclus, optimisations désactivées
{$IFDEF DEBUG}
  {$ASSERTIONS ON}
  {$RANGECHECKS ON}
  {$OVERFLOWCHECKS ON}
{$ENDIF}

// Mode release : optimisé, symboles exclus
{$IFDEF RELEASE}
  {$OPTIMIZATION LEVEL3}
  {$ASSERTIONS OFF}
{$ENDIF}
```

## Utiliser le débogueur Lazarus

### Points d'arrêt simples

Un **point d'arrêt** (breakpoint) arrête l'exécution à une ligne spécifique.

**Comment créer un point d'arrêt :**
1. Cliquez dans la marge gauche de l'éditeur (ligne rouge apparaît)
2. Ou placez le curseur sur la ligne et appuyez sur **F5**

**Exemple :**

```pascal
procedure TraiterDonnees(const texte: String);  
var  
  nombre: Integer;
begin
  WriteLn('Début du traitement');    // ← Point d'arrêt ici
  nombre := StrToInt(texte);
  WriteLn('Résultat : ', nombre);
end;
```

Quand l'exécution atteint le point d'arrêt, le programme se met en pause et vous pouvez :
- Inspecter les variables
- Avancer pas à pas
- Voir la pile d'appels

### Naviguer pas à pas

Une fois en pause, utilisez :

| Touche | Action | Description |
|--------|--------|-------------|
| **F7** | Pas à pas détaillé (Step Into) | Entre dans les fonctions |
| **F8** | Pas à pas principal (Step Over) | Exécute la ligne, sans entrer dans les fonctions |
| **Shift+F8** | Pas à pas sortant (Step Out) | Sort de la fonction actuelle |
| **F9** | Continuer (Run) | Continue jusqu'au prochain point d'arrêt |

### Inspecter les variables

Pendant une pause, plusieurs méthodes pour voir les valeurs :

#### 1. Survol de souris
Passez la souris sur une variable → une infobulle affiche sa valeur

#### 2. Fenêtre Variables locales
**Menu : Affichage > Fenêtres de débogage > Variables locales**

Affiche toutes les variables de la fonction actuelle avec leurs valeurs.

#### 3. Fenêtre Inspecteur
**Menu : Affichage > Fenêtres de débogage > Inspecteur**

Permet d'inspecter en profondeur un objet, voir ses propriétés, etc.

#### 4. Évaluer/Modifier
**Menu : Exécuter > Évaluer/Modifier... (Ctrl+F7)**

Tapez une expression pour voir sa valeur :
```
texte + ' - longueur: ' + IntToStr(Length(texte))
```

### Fenêtre Pile d'appels

**Menu : Affichage > Fenêtres de débogage > Pile d'appels**

Cette fenêtre montre toute la chaîne d'appels :

```
#0  TraiterDonnees at Main.pas:128
#1  ProcesserLigne at Main.pas:85
#2  ButtonClick at Main.pas:42
#3  TButton.Click at buttons.pas:234
```

Vous pouvez double-cliquer sur une ligne pour voir le code à ce niveau.

## Points d'arrêt sur exceptions

Lazarus peut arrêter automatiquement quand une exception spécifique est levée.

### Configurer un point d'arrêt sur exception

**Menu : Exécuter > Ajouter un point d'arrêt > Point d'arrêt d'exception**

Ou :
**Menu : Affichage > Fenêtres de débogage > Points d'arrêt**
Puis clic droit → **Ajouter point d'arrêt d'exception**

**Configuration :**
- **Nom de l'exception** : `EConvertError` (ou laissez vide pour toutes)
- **Quand arrêter** :
  - ☐ Levée (exception levée)
  - ☑ Non gérée (exception non capturée)

### Exemple d'utilisation

```pascal
procedure TestConversion;  
var  
  nombre: Integer;
begin
  // Si vous avez configuré un point d'arrêt sur EConvertError,
  // le débogueur s'arrêtera ICI quand l'exception est levée
  nombre := StrToInt('abc');  // ← Arrêt automatique
  WriteLn(nombre);
end;
```

**Avantage :** Vous voyez exactement où l'exception est levée, avec les valeurs des variables à ce moment.

## Techniques de débogage spécifiques aux exceptions

### Technique 1 : Ajouter des messages de débogage

Ajoutez des `WriteLn` stratégiques pour tracer l'exécution :

```pascal
procedure ProcesserFichier(const nom: String);  
begin  
  WriteLn('DEBUG: Début ProcesserFichier avec fichier: ', nom);

  try
    WriteLn('DEBUG: Tentative d''ouverture du fichier');
    OuvrirFichier(nom);

    WriteLn('DEBUG: Fichier ouvert, lecture en cours');
    LireDonnees;

    WriteLn('DEBUG: Données lues, traitement');
    TraiterDonnees;

    WriteLn('DEBUG: Traitement terminé avec succès');
  except
    on E: Exception do
    begin
      WriteLn('DEBUG: Exception capturée - ', E.ClassName, ': ', E.Message);
      raise;
    end;
  end;
end;
```

**Astuce :** Utilisez une directive de compilation pour activer/désactiver :

```pascal
{$DEFINE DEBUG_MODE}

procedure ProcesserFichier(const nom: String);  
begin  
  {$IFDEF DEBUG_MODE}
  WriteLn('DEBUG: Début ProcesserFichier');
  {$ENDIF}

  // ... code
end;
```

### Technique 2 : Assert pour vérifier les suppositions

Les assertions vérifient qu'une condition est vraie. Si elle est fausse, une exception `EAssertionFailed` est levée.

```pascal
procedure TraiterTableau(const tableau: array of Integer; index: Integer);  
begin  
  // Vérifier les préconditions
  Assert(Length(tableau) > 0, 'Le tableau ne doit pas être vide');
  Assert((index >= Low(tableau)) and (index <= High(tableau)),
         'Index hors limites');

  // Si on arrive ici, les assertions sont passées
  WriteLn('Valeur : ', tableau[index]);
end;
```

**Configuration :**
```pascal
{$ASSERTIONS ON}   // Active les assertions en mode débogage
{$ASSERTIONS OFF}  // Désactive en mode release (performance)
```

### Technique 3 : Logging détaillé

Créez un système de log qui enregistre dans un fichier :

```pascal
var
  FichierLog: TextFile;

procedure InitLog;  
begin  
  AssignFile(FichierLog, 'debug.log');
  Rewrite(FichierLog);
end;

procedure Log(const msg: String);  
begin  
  WriteLn(FichierLog, FormatDateTime('hh:nn:ss.zzz', Now), ' - ', msg);
  Flush(FichierLog);  // Force l'écriture immédiate
end;

procedure FermerLog;  
begin  
  CloseFile(FichierLog);
end;

// Utilisation
try
  Log('Début du traitement');
  TraiterDonnees;
  Log('Traitement réussi');
except
  on E: Exception do
  begin
    Log('ERREUR: ' + E.ClassName + ' - ' + E.Message);
    raise;
  end;
end;
```

**Contenu de debug.log :**
```
14:23:45.123 - Début du traitement
14:23:45.156 - Ouverture fichier: donnees.txt
14:23:45.201 - ERREUR: EInOutError - File not found
```

### Technique 4 : Try-Except avec information contextuelle

Capturez et re-levez avec plus de contexte :

```pascal
procedure TraiterLigne(numeroLigne: Integer; const contenu: String);  
begin  
  try
    ProcesserContenu(contenu);
  except
    on E: Exception do
      raise Exception.CreateFmt(
        'Erreur ligne %d (%s) : %s',
        [numeroLigne, E.ClassName, E.Message]
      );
  end;
end;
```

Maintenant, si une erreur survient, vous savez exactement à quelle ligne !

### Technique 5 : Capturer la pile d'appels

FreePascal peut capturer la pile d'appels dans une exception :

```pascal
uses
  SysUtils, LineInfo;

procedure AfficherPileAppels;  
var  
  i: Integer;
  frames: PCodePointer;
begin
  WriteLn('Pile d''appels :');
  WriteLn('  Adresse exception : ', BackTraceStrFunc(ExceptAddr));
  frames := ExceptFrames;
  for i := 0 to ExceptFrameCount - 1 do
    WriteLn(Format('  #%d: %s', [i, BackTraceStrFunc(frames[i])]));
end;

// Utilisation
try
  FaireQuelqueChose;
except
  on E: Exception do
  begin
    WriteLn('Exception: ', E.Message);
    AfficherPileAppels;
  end;
end;
```

## Déboguer les exceptions courantes

### Exception : EConvertError

**Cause :** Conversion de chaîne en nombre impossible

**Débogage :**
```pascal
procedure DebugConversion(const texte: String);  
var  
  nombre, i: Integer;
begin
  WriteLn('Tentative de conversion de : "', texte, '"');
  WriteLn('Longueur : ', Length(texte));
  WriteLn('Premier caractère (ord): ', Ord(texte[1]));

  try
    nombre := StrToInt(texte);
  except
    on E: EConvertError do
    begin
      WriteLn('Conversion échouée !');
      WriteLn('La chaîne contient : ');
      for i := 1 to Length(texte) do
        WriteLn('  Position ', i, ': "', texte[i], '" (code: ', Ord(texte[i]), ')');
      raise;
    end;
  end;
end;
```

**Solutions courantes :**
- Vérifier avec `TryStrToInt` avant conversion
- Nettoyer la chaîne avec `Trim`
- Valider le format avec une expression régulière

### Exception : EDivByZero

**Cause :** Division par zéro

**Débogage :**
```pascal
procedure DebugDivision(a, b: Integer);  
begin  
  WriteLn('Division de ', a, ' par ', b);

  if b = 0 then
  begin
    WriteLn('ATTENTION: Diviseur est zéro !');
    WriteLn('Variables : a=', a, ', b=', b);
    // Mettre un point d'arrêt ici
  end;

  WriteLn('Résultat : ', a div b);
end;
```

**Solutions :**
- Toujours vérifier `if diviseur <> 0 then`
- Comprendre d'où vient le zéro (calcul précédent ?)

### Exception : EAccessViolation

**Cause :** Accès à une adresse mémoire invalide (pointeur nil, objet libéré)

**Débogage :**
```pascal
procedure DebugObjet(obj: TMonObjet);  
begin  
  WriteLn('Vérification objet...');
  WriteLn('Adresse : ', IntToHex(PtrUInt(obj), 16));

  if not Assigned(obj) then
  begin
    WriteLn('ERREUR: Objet est nil !');
    raise Exception.Create('Tentative d''utilisation d''un objet nil');
  end;

  WriteLn('Objet valide, utilisation...');
  obj.FaireTravail;
end;
```

**Solutions :**
- Toujours vérifier `if Assigned(obj) then`
- Ne jamais utiliser un objet après `Free`
- Utiliser `FreeAndNil` pour mettre à `nil` après libération

### Exception : ERangeError

**Cause :** Index hors des limites d'un tableau ou liste

**Débogage :**
```pascal
procedure DebugAccesTableau(const tableau: array of Integer; index: Integer);  
begin  
  WriteLn('Taille du tableau : ', Length(tableau));
  WriteLn('Index demandé : ', index);
  WriteLn('Limites valides : ', Low(tableau), ' à ', High(tableau));

  if (index < Low(tableau)) or (index > High(tableau)) then
  begin
    WriteLn('ERREUR: Index hors limites !');
    raise ERangeError.CreateFmt(
      'Index %d hors limites [%d..%d]',
      [index, Low(tableau), High(tableau)]
    );
  end;

  WriteLn('Valeur : ', tableau[index]);
end;
```

**Solutions :**
- Vérifier avec `if (index >= 0) and (index < Length(tableau)) then`
- Activer les vérifications de limites : `{$RANGECHECKS ON}`

## Outils avancés

### HeapTrc : Détection des fuites mémoire

HeapTrc détecte les objets non libérés.

**Activation :**
```pascal
// En début de programme
{$IFDEF DEBUG}
  SetHeapTraceOutput('heaptrc.log');
{$ENDIF}
```

**Menu : Projet > Options > Compilation**
Cochez : **Use Heaptrc unit**

**Résultat :** Un fichier `heaptrc.log` contient les fuites détectées :

```
Heap dump by heaptrc unit
123 memory blocks allocated : 4567/5000
120 memory blocks freed     : 4321/4800
3 unfreed memory blocks : 246
True heap size : 65536  
True free heap : 61290  

Call trace for block $00405678 size 128
  $00401234  TMONOBJET__CREATE,  line 45 of MonObjet.pas
  $00402345  CREEROBJETS,  line 78 of Main.pas
```

### Valgrind (Linux)

Sur Linux, Valgrind est un outil puissant pour détecter les erreurs mémoire :

```bash
valgrind --leak-check=full ./monprogramme
```

### Directives de débogage conditionnelles

Utilisez les directives pour activer du code de débogage :

```pascal
{$DEFINE DEBUG_MEMORY}
{$DEFINE DEBUG_EXCEPTIONS}

type
  TMonObjet = class
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TMonObjet.Create;  
begin  
  inherited;
  {$IFDEF DEBUG_MEMORY}
  WriteLn('TMonObjet créé à ', IntToHex(PtrUInt(Self), 16));
  {$ENDIF}
end;

destructor TMonObjet.Destroy;  
begin  
  {$IFDEF DEBUG_MEMORY}
  WriteLn('TMonObjet détruit à ', IntToHex(PtrUInt(Self), 16));
  {$ENDIF}
  inherited;
end;
```

## Méthodologie de débogage

### Approche systématique

Quand vous rencontrez une exception :

1. **Lire le message d'erreur** : Quel est le type d'exception ? Quel est le message ?

2. **Examiner la pile d'appels** : Où l'erreur s'est-elle produite ? Quel est le chemin d'appels ?

3. **Reproduire l'erreur** : Pouvez-vous la faire se produire à volonté ?

4. **Isoler le problème** :
   - Commentez du code pour trouver la ligne exacte
   - Ajoutez des points d'arrêt
   - Ajoutez des logs

5. **Inspecter les données** : Quelles sont les valeurs des variables à ce moment ?

6. **Formuler une hypothèse** : Pourquoi l'erreur se produit-elle ?

7. **Tester la correction** : Corrigez et vérifiez que l'erreur ne se reproduit pas

### Exemple de débogage complet

**Problème :** Votre application crash avec `EConvertError`

**Étape 1 - Message d'erreur :**
```
EConvertError: '123,45' is not a valid integer value
```

**Étape 2 - Pile d'appels :**
```
#0  StrToInt at SysUtils.pas:234
#1  CalculerTotal at Calculs.pas:56
#2  ButtonCalculerClick at Main.pas:89
```

**Étape 3 - Reproduire :** Entrez "123,45" dans le champ montant et cliquez sur Calculer

**Étape 4 - Isoler :** Point d'arrêt à la ligne 56 de Calculs.pas :
```pascal
function CalculerTotal(const montantTexte: String): Integer;  
begin  
  Result := StrToInt(montantTexte);  // ← Point d'arrêt ici
end;
```

**Étape 5 - Inspecter :**
```
montantTexte = "123,45"
```

**Étape 6 - Hypothèse :** L'utilisateur entre un nombre décimal, mais `StrToInt` attend un entier

**Étape 7 - Correction :**
```pascal
function CalculerTotal(const montantTexte: String): Currency;  
var  
  montant: Currency;
begin
  // Remplacer virgule par point si nécessaire
  if not TryStrToCurr(StringReplace(montantTexte, ',', '.', []), montant) then
    raise EConvertError.CreateFmt('Format invalide: "%s"', [montantTexte]);
  Result := montant;
end;
```

## Prévenir plutôt que déboguer

### Validation en amont

```pascal
function ConversionSecurisee(const texte: String): Integer;  
begin  
  // Validation avant conversion
  if Trim(texte) = '' then
    raise Exception.Create('Valeur vide');

  if not TryStrToInt(texte, Result) then
    raise EConvertError.CreateFmt('"%s" n''est pas un nombre valide', [texte]);
end;
```

### Préconditions et postconditions

```pascal
procedure TraiterIndex(const tableau: array of Integer; index: Integer);  
begin  
  // Préconditions
  Assert(Length(tableau) > 0, 'Tableau vide');
  Assert((index >= Low(tableau)) and (index <= High(tableau)), 'Index invalide');

  // Traitement
  WriteLn(tableau[index]);

  // Postconditions (si nécessaire)
  Assert(tableau[index] >= 0, 'Résultat négatif inattendu');
end;
```

### Tests unitaires

Créez des tests qui vérifient les exceptions :

```pascal
procedure TestConversionInvalide;  
var  
  exceptionLevee: Boolean;
begin
  exceptionLevee := False;
  try
    StrToInt('abc');
  except
    on E: EConvertError do
      exceptionLevee := True;
  end;

  Assert(exceptionLevee, 'EConvertError aurait dû être levée');
  WriteLn('Test réussi');
end;
```

## Checklist de débogage

Quand vous déboguez une exception :

- [ ] J'ai lu attentivement le message d'erreur complet
- [ ] J'ai examiné la pile d'appels pour comprendre le contexte
- [ ] J'ai reproduit l'erreur de manière fiable
- [ ] J'ai utilisé des points d'arrêt pour arrêter avant l'erreur
- [ ] J'ai inspecté les valeurs des variables concernées
- [ ] J'ai ajouté des logs pour tracer l'exécution
- [ ] J'ai vérifié les suppositions avec des assertions
- [ ] J'ai testé la correction pour confirmer que l'erreur est résolue
- [ ] J'ai ajouté une validation pour prévenir l'erreur à l'avenir

## Conclusion

Le débogage des exceptions est une compétence essentielle qui s'améliore avec la pratique. Les outils de Lazarus sont puissants :

- **Points d'arrêt** pour stopper l'exécution
- **Inspection de variables** pour voir les valeurs
- **Pile d'appels** pour comprendre le contexte
- **Points d'arrêt sur exceptions** pour attraper les erreurs au vol
- **Logging** pour tracer l'exécution

Combinés avec une approche méthodique et des messages d'erreur informatifs, ces outils vous permettront de résoudre rapidement même les bugs les plus tenaces.

N'oubliez pas : **le meilleur débogage est celui qu'on n'a pas à faire**. Écrivez du code défensif avec validation, assertions et tests, et vous passerez beaucoup moins de temps à déboguer !

---

**Points clés à retenir :**

- Compiler avec les informations de débogage activées (`-g -gl`)
- Utiliser les points d'arrêt pour stopper et inspecter
- La pile d'appels montre le chemin d'exécution
- Les points d'arrêt sur exceptions arrêtent quand une exception est levée
- Ajouter des logs pour tracer l'exécution
- Les assertions vérifient les suppositions
- HeapTrc détecte les fuites mémoire
- Approche systématique : lire, examiner, reproduire, isoler, inspecter, corriger
- La validation en amont prévient les exceptions
- Tester que les exceptions attendues sont bien levées

⏭️ [Logging des erreurs](/13-gestion-exceptions/09-logging-erreurs.md)
