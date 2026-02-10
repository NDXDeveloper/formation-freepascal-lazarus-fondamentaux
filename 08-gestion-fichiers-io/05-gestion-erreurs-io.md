🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.5 Gestion des erreurs I/O

## Introduction

Lorsqu'on manipule des fichiers, de nombreuses choses peuvent mal se passer : le fichier n'existe pas, le disque est plein, vous n'avez pas les permissions, etc. **La gestion des erreurs** (error handling) est essentielle pour créer des programmes robustes qui ne plantent pas face à l'imprévu.

**Analogie :**
Imaginez que vous essayez d'ouvrir un livre :
- Le livre n'est pas sur l'étagère → erreur "fichier introuvable"
- Le livre est verrouillé → erreur "accès refusé"
- Vous voulez écrire mais vous n'avez pas de stylo → erreur "disque plein"

Un bon programme doit anticiper ces situations et réagir intelligemment !

---

## Qu'est-ce qu'une erreur I/O ?

Une **erreur I/O** (Input/Output) se produit lors d'opérations sur les fichiers ou les périphériques. Au lieu de laisser le programme planter, nous devons **détecter** et **gérer** ces erreurs.

### Les erreurs courantes

| Code d'erreur | Signification | Cause typique |
|---------------|---------------|---------------|
| **2** | Fichier non trouvé | Le fichier n'existe pas |
| **3** | Chemin non trouvé | Le répertoire n'existe pas |
| **5** | Accès refusé | Pas de permissions suffisantes |
| **15** | Lecteur non valide | Le lecteur n'existe pas (ex: Z:) |
| **100** | Erreur lecture disque | Disque défectueux ou déconnecté |
| **101** | Erreur écriture disque | Disque plein ou protégé en écriture |
| **103** | Fichier non ouvert | Tentative de lecture/écriture sans ouverture |

**Note :** Le code 0 signifie "aucune erreur".

---

## Comportement par défaut de Pascal

Par défaut, Pascal gère automatiquement les erreurs I/O en **arrêtant le programme** avec un message d'erreur.

### Exemple sans gestion d'erreur

```pascal
program SansGestionErreur;

var
  F: TextFile;
  Ligne: string;

begin
  Assign(F, 'fichier_inexistant.txt');
  Reset(F);  // ERREUR : le fichier n'existe pas !

  ReadLn(F, Ligne);
  WriteLn(Ligne);
  Close(F);
end.
```

**Résultat :**
```
Runtime error 2 at $00401234
  $00401234  SANSGESTIONERREUR,  line 8 of program.pas
```

Le programme **plante** brutalement, ce qui est une très mauvaise expérience utilisateur !

---

## Gestion manuelle des erreurs avec IOResult

Pour gérer les erreurs proprement, Pascal propose un mécanisme simple basé sur :
1. Les directives de compilation `{$I-}` et `{$I+}`
2. La fonction `IOResult`

### Les directives {$I-} et {$I+}

**{$I-}** : Désactive la gestion automatique des erreurs I/O
- Les erreurs ne provoquent plus l'arrêt du programme
- Vous devez vérifier manuellement avec `IOResult`

**{$I+}** : Réactive la gestion automatique des erreurs I/O
- Retour au comportement par défaut

```pascal
{$I-}  // Désactive les erreurs automatiques
Reset(F);  // Si erreur, le programme continue
{$I+}  // Réactive les erreurs automatiques
```

### La fonction IOResult

**IOResult** retourne le code de la dernière erreur I/O survenue :
- **0** = aucune erreur
- **Autre valeur** = code d'erreur spécifique

**Important :** `IOResult` ne peut être appelé qu'**une seule fois** après chaque opération, car l'appel **réinitialise** le code d'erreur à 0.

```pascal
var
  Erreur: Integer;
begin
  {$I-}
  Reset(F);
  {$I+}

  Erreur := IOResult;  // Récupère le code d'erreur

  if Erreur <> 0 then
    WriteLn('Erreur : ', Erreur);
end;
```

---

## Exemple complet : Ouverture sécurisée

Voici comment ouvrir un fichier en gérant proprement les erreurs.

```pascal
program OuvertureSecurisee;

var
  F: TextFile;
  Ligne: string;
  CodeErreur: Integer;

begin
  Assign(F, 'donnees.txt');

  // Tentative d'ouverture avec gestion d'erreur
  {$I-}
  Reset(F);
  {$I+}

  CodeErreur := IOResult;

  if CodeErreur <> 0 then
  begin
    WriteLn('ERREUR : Impossible d''ouvrir le fichier !');
    WriteLn('Code d''erreur : ', CodeErreur);

    case CodeErreur of
      2: WriteLn('Le fichier n''existe pas.');
      3: WriteLn('Le chemin est invalide.');
      5: WriteLn('Accès refusé (permissions insuffisantes).');
    else
      WriteLn('Erreur inconnue.');
    end;

    Halt(1);  // Quitter le programme avec un code d'erreur
  end;

  // Si on arrive ici, tout s'est bien passé
  WriteLn('Fichier ouvert avec succès !');

  while not EOF(F) do
  begin
    ReadLn(F, Ligne);
    WriteLn(Ligne);
  end;

  Close(F);
  WriteLn('Traitement terminé.');
end.
```

**Avantages de cette approche :**  
✅ Le programme ne plante pas  
✅ L'utilisateur reçoit un message clair  
✅ On peut réagir différemment selon l'erreur  

---

## Fonction utilitaire : Vérifier l'existence d'un fichier

Créons une fonction réutilisable pour vérifier si un fichier existe.

```pascal
function FichierExiste(NomFichier: string): Boolean;  
var  
  F: File;
begin
  Assign(F, NomFichier);

  {$I-}
  Reset(F);
  {$I+}

  if IOResult = 0 then
  begin
    Close(F);
    FichierExiste := True;
  end
  else
    FichierExiste := False;
end;
```

**Utilisation :**

```pascal
begin
  if FichierExiste('config.ini') then
    WriteLn('Le fichier de configuration existe.')
  else
    WriteLn('Fichier de configuration manquant !');
end.
```

---

## Gestion complète : Lecture sécurisée

Voici un exemple complet qui gère toutes les erreurs possibles lors de la lecture d'un fichier.

```pascal
program LectureSecurisee;

function LireFichier(NomFichier: string): Boolean;  
var  
  F: TextFile;
  Ligne: string;
  CodeErreur: Integer;
begin
  LireFichier := False;  // Par défaut : échec

  WriteLn('Tentative d''ouverture : ', NomFichier);

  Assign(F, NomFichier);

  // Ouverture
  {$I-}
  Reset(F);
  {$I+}

  CodeErreur := IOResult;

  if CodeErreur <> 0 then
  begin
    WriteLn('ERREUR lors de l''ouverture !');
    case CodeErreur of
      2: WriteLn('→ Fichier introuvable');
      3: WriteLn('→ Chemin invalide');
      5: WriteLn('→ Accès refusé');
    else
      WriteLn('→ Erreur ', CodeErreur);
    end;
    Exit;
  end;

  WriteLn('Fichier ouvert avec succès.');
  WriteLn('Contenu :');
  WriteLn('---');

  // Lecture
  while not EOF(F) do
  begin
    {$I-}
    ReadLn(F, Ligne);
    {$I+}

    CodeErreur := IOResult;

    if CodeErreur <> 0 then
    begin
      WriteLn('ERREUR lors de la lecture ! Code : ', CodeErreur);
      Close(F);
      Exit;
    end;

    WriteLn(Ligne);
  end;

  WriteLn('---');

  // Fermeture
  {$I-}
  Close(F);
  {$I+}

  CodeErreur := IOResult;

  if CodeErreur <> 0 then
  begin
    WriteLn('ERREUR lors de la fermeture ! Code : ', CodeErreur);
    Exit;
  end;

  WriteLn('Fichier fermé correctement.');
  LireFichier := True;  // Succès !
end;

begin
  if LireFichier('test.txt') then
    WriteLn('Opération réussie !')
  else
    WriteLn('Opération échouée !');
end.
```

---

## Écriture sécurisée : Gérer le disque plein

Lors de l'écriture, il faut aussi gérer les erreurs (disque plein, protégé en écriture, etc.).

```pascal
program EcritureSecurisee;

function EcrireDansFichier(NomFichier: string; Donnees: array of string): Boolean;  
var  
  F: TextFile;
  i: Integer;
  CodeErreur: Integer;
begin
  EcrireDansFichier := False;

  Assign(F, NomFichier);

  // Ouverture en écriture
  {$I-}
  Rewrite(F);
  {$I+}

  CodeErreur := IOResult;

  if CodeErreur <> 0 then
  begin
    WriteLn('Impossible de créer le fichier !');
    case CodeErreur of
      5: WriteLn('→ Accès refusé');
      101: WriteLn('→ Disque plein ou protégé en écriture');
    else
      WriteLn('→ Erreur ', CodeErreur);
    end;
    Exit;
  end;

  // Écriture
  for i := Low(Donnees) to High(Donnees) do
  begin
    {$I-}
    WriteLn(F, Donnees[i]);
    {$I+}

    CodeErreur := IOResult;

    if CodeErreur <> 0 then
    begin
      WriteLn('Erreur lors de l''écriture !');
      case CodeErreur of
        101: WriteLn('→ Disque plein !');
      else
        WriteLn('→ Erreur ', CodeErreur);
      end;
      Close(F);
      Exit;
    end;
  end;

  // Fermeture
  {$I-}
  Close(F);
  {$I+}

  if IOResult = 0 then
    EcrireDansFichier := True;
end;

var
  Lignes: array[1..3] of string = ('Ligne 1', 'Ligne 2', 'Ligne 3');
  FClean: File;

begin
  if EcrireDansFichier('sortie.txt', Lignes) then
    WriteLn('Écriture réussie !')
  else
    WriteLn('Échec de l''écriture.');

  // Nettoyage du fichier de test
  Assign(FClean, 'sortie.txt');
  {$I-}
  Erase(FClean);
  {$I+}
  IOResult;  // Vider le code d'erreur
end.
```

---

## Gestion avancée avec Try-Except

FreePascal supporte également la gestion d'exceptions moderne avec `try-except-finally`.

### Structure de base

```pascal
try
  // Code susceptible de générer une erreur
  Reset(F);
  ReadLn(F, Ligne);
except
  // Code exécuté en cas d'erreur
  on E: Exception do
    WriteLn('Erreur : ', E.Message);
end;
```

### Exemple complet avec Try-Except

```pascal
{$mode objfpc}{$H+}
program GestionModerne;

uses
  SysUtils;  // Nécessaire pour les exceptions

var
  F: TextFile;
  Ligne: string;

begin
  Assign(F, 'donnees.txt');

  try
    Reset(F);
    WriteLn('Fichier ouvert.');

    while not EOF(F) do
    begin
      ReadLn(F, Ligne);
      WriteLn(Ligne);
    end;

  except
    on E: EInOutError do
    begin
      WriteLn('Erreur I/O : ', E.Message);
      WriteLn('Code d''erreur : ', E.ErrorCode);
    end;

    on E: Exception do
      WriteLn('Erreur inattendue : ', E.Message);
  end;

  // Fermeture sécurisée
  try
    Close(F);
  except
    // Ignorer les erreurs de fermeture
  end;

  WriteLn('Programme terminé.');
end.
```

### Try-Finally : Garantir la fermeture

Pour s'assurer qu'un fichier est toujours fermé, même en cas d'erreur :

```pascal
var
  F: TextFile;
  Ligne: string;

begin
  Assign(F, 'donnees.txt');
  Reset(F);

  try
    while not EOF(F) do
    begin
      ReadLn(F, Ligne);
      WriteLn(Ligne);
    end;
  finally
    Close(F);  // Toujours exécuté, même si erreur
  end;
end.
```

### Try-Except-Finally : Combinaison complète

```pascal
begin
  Assign(F, 'donnees.txt');

  try
    Reset(F);

    try
      while not EOF(F) do
      begin
        ReadLn(F, Ligne);
        WriteLn(Ligne);
      end;
    finally
      Close(F);  // Toujours fermé
    end;

  except
    on E: EInOutError do
      WriteLn('Erreur I/O : ', E.Message);
  end;
end.
```

---

## Créer des messages d'erreur conviviaux

Les codes d'erreur bruts ne sont pas compréhensibles pour l'utilisateur. Créons une fonction qui traduit les codes en messages clairs.

```pascal
function MessageErreurIO(Code: Integer): string;  
begin  
  case Code of
    0:   MessageErreurIO := 'Aucune erreur';
    2:   MessageErreurIO := 'Le fichier n''existe pas';
    3:   MessageErreurIO := 'Le chemin d''accès est invalide';
    4:   MessageErreurIO := 'Trop de fichiers ouverts';
    5:   MessageErreurIO := 'Accès refusé (vérifiez les permissions)';
    6:   MessageErreurIO := 'Fichier non valide';
    12:  MessageErreurIO := 'Mode d''accès invalide';
    15:  MessageErreurIO := 'Numéro de lecteur invalide';
    16:  MessageErreurIO := 'Impossible de supprimer le répertoire actuel';
    17:  MessageErreurIO := 'Impossible de renommer entre différents lecteurs';
    100: MessageErreurIO := 'Erreur de lecture du disque';
    101: MessageErreurIO := 'Erreur d''écriture (disque plein ?)';
    102: MessageErreurIO := 'Fichier non assigné';
    103: MessageErreurIO := 'Fichier non ouvert';
    104: MessageErreurIO := 'Fichier non ouvert en entrée';
    105: MessageErreurIO := 'Fichier non ouvert en sortie';
    106: MessageErreurIO := 'Format de nombre invalide';
  else
    MessageErreurIO := 'Erreur inconnue (code ' + IntToStr(Code) + ')';
  end;
end;
```

**Utilisation :**

```pascal
{$I-}
Reset(F);
{$I+}

CodeErreur := IOResult;

if CodeErreur <> 0 then
  WriteLn('ERREUR : ', MessageErreurIO(CodeErreur));
```

---

## Exemple complet : Application robuste

Créons une petite application qui lit un fichier de configuration en gérant toutes les erreurs possibles.

```pascal
{$mode objfpc}{$H+}
program ConfigRobuste;

uses
  SysUtils;

type
  TConfig = record
    NomUtilisateur: string;
    CouleurTheme: string;
    TaillePolice: Integer;
  end;

var
  Config: TConfig;

function MessageErreur(Code: Integer): string;  
begin  
  case Code of
    2: Result := 'Fichier de configuration introuvable';
    5: Result := 'Accès refusé au fichier de configuration';
    100: Result := 'Erreur de lecture du disque';
  else
    Result := 'Erreur inconnue (code ' + IntToStr(Code) + ')';
  end;
end;

function ChargerConfig(NomFichier: string; var Cfg: TConfig): Boolean;  
var  
  F: TextFile;
  Ligne, Cle, Valeur: string;
  PosEgal: Integer;
  CodeErreur: Integer;
begin
  ChargerConfig := False;

  // Valeurs par défaut
  Cfg.NomUtilisateur := 'Invité';
  Cfg.CouleurTheme := 'Bleu';
  Cfg.TaillePolice := 12;

  WriteLn('Chargement de la configuration...');

  // Vérification d'existence
  if not FileExists(NomFichier) then
  begin
    WriteLn('→ Fichier non trouvé, utilisation des valeurs par défaut');
    ChargerConfig := True;  // Pas une erreur critique
    Exit;
  end;

  Assign(F, NomFichier);

  // Ouverture
  {$I-}
  Reset(F);
  {$I+}

  CodeErreur := IOResult;

  if CodeErreur <> 0 then
  begin
    WriteLn('ERREUR : ', MessageErreur(CodeErreur));
    Exit;
  end;

  // Lecture ligne par ligne
  try
    while not EOF(F) do
    begin
      {$I-}
      ReadLn(F, Ligne);
      {$I+}

      if IOResult <> 0 then
      begin
        WriteLn('Erreur lors de la lecture du fichier');
        Close(F);
        Exit;
      end;

      // Ignorer les lignes vides et commentaires
      Ligne := Trim(Ligne);
      if (Ligne = '') or (Ligne[1] = '#') then
        Continue;

      // Parser la ligne : Clé=Valeur
      PosEgal := Pos('=', Ligne);
      if PosEgal > 0 then
      begin
        Cle := Trim(Copy(Ligne, 1, PosEgal - 1));
        Valeur := Trim(Copy(Ligne, PosEgal + 1, Length(Ligne)));

        // Affecter la configuration
        if Cle = 'NomUtilisateur' then
          Cfg.NomUtilisateur := Valeur
        else if Cle = 'CouleurTheme' then
          Cfg.CouleurTheme := Valeur
        else if Cle = 'TaillePolice' then
        begin
          try
            Cfg.TaillePolice := StrToInt(Valeur);
          except
            WriteLn('Attention : TaillePolice invalide, valeur par défaut utilisée');
          end;
        end;
      end;
    end;

    ChargerConfig := True;

  finally
    {$I-}
    Close(F);
    {$I+}
    IOResult;  // Vider le code d'erreur
  end;

  WriteLn('→ Configuration chargée avec succès');
end;

begin
  if ChargerConfig('config.ini', Config) then
  begin
    WriteLn;
    WriteLn('=== CONFIGURATION ===');
    WriteLn('Utilisateur  : ', Config.NomUtilisateur);
    WriteLn('Thème        : ', Config.CouleurTheme);
    WriteLn('Taille police: ', Config.TaillePolice);
  end
  else
    WriteLn('Impossible de charger la configuration !');
end.
```

**Fichier config.ini exemple :**
```ini
# Configuration de l'application
NomUtilisateur=Jean Dupont  
CouleurTheme=Vert  
TaillePolice=14  
```

---

## Comparaison des méthodes

| Méthode | Avantages | Inconvénients | Usage recommandé |
|---------|-----------|---------------|------------------|
| **{$I-} / IOResult** | Simple, compatible partout | Verbeux, répétitif | Code simple, compatibilité |
| **Try-Except** | Moderne, élégant, puissant | Nécessite SysUtils | Applications modernes |
| **Try-Finally** | Garantit le nettoyage | Ne gère pas les erreurs | Libération de ressources |
| **Combinaison** | Complet et robuste | Plus complexe | Applications professionnelles |

---

## Bonnes pratiques

### ✅ À faire

**Toujours gérer les erreurs** pour les opérations critiques (ouverture, lecture, écriture)

**Fournir des messages clairs** à l'utilisateur plutôt que des codes d'erreur bruts

**Fermer les fichiers** même en cas d'erreur (utiliser `try-finally`)

**Vérifier l'existence** avant d'ouvrir en lecture avec `FileExists()`

**Prévoir des valeurs par défaut** si le fichier de configuration est manquant

**Logger les erreurs** pour faciliter le débogage

**Tester les cas d'erreur** : fichier manquant, accès refusé, disque plein

**Documenter** les erreurs possibles dans vos fonctions

### ❌ À éviter

**Ne jamais ignorer** les erreurs silencieusement

**Ne pas laisser** le programme planter sans message explicatif

**Ne pas oublier** d'appeler `IOResult` après `{$I-}` (sinon l'erreur persiste)

**Ne pas appeler** `IOResult` plusieurs fois (il se réinitialise)

**Ne pas ouvrir** un fichier sans vérifier le résultat

**Ne pas supposer** qu'un fichier existe toujours

**Ne pas négliger** la fermeture des fichiers en cas d'erreur

---

## Récapitulatif : Stratégie de gestion d'erreurs

### Pour une opération simple

```pascal
{$I-}
Reset(F);
{$I+}

if IOResult <> 0 then  
begin  
  WriteLn('Erreur d''ouverture');
  Exit;
end;
```

### Pour une opération critique

```pascal
try
  Reset(F);
  try
    // Traitement
  finally
    Close(F);
  end;
except
  on E: Exception do
    WriteLn('Erreur : ', E.Message);
end;
```

### Pour une application robuste

```pascal
function TraiterFichier(Nom: string): Boolean;  
var  
  F: TextFile;
begin
  Result := False;

  if not FileExists(Nom) then
  begin
    LogErreur('Fichier introuvable : ' + Nom);
    Exit;
  end;

  Assign(F, Nom);

  try
    Reset(F);
    try
      // Traitement avec vérifications IOResult
    finally
      Close(F);
    end;
    Result := True;
  except
    on E: Exception do
    begin
      LogErreur('Erreur traitement : ' + E.Message);
      AfficherMessageUtilisateur('Erreur lors du traitement du fichier');
    end;
  end;
end;
```

---

## Résumé

La gestion des erreurs I/O est **essentielle** pour créer des programmes professionnels :

**Deux approches principales :**
1. **{$I-} / IOResult** : classique, simple, compatible
2. **Try-Except** : moderne, puissant, élégant

**Règles d'or :**
- Toujours gérer les erreurs d'ouverture
- Toujours fermer les fichiers (utilisez `finally`)
- Fournir des messages clairs à l'utilisateur
- Prévoir des valeurs par défaut sensées
- Tester les cas d'erreur

**Types d'erreurs à gérer :**
- Fichier inexistant (code 2)
- Accès refusé (code 5)
- Disque plein (code 101)
- Erreurs de lecture/écriture

Dans la section suivante, nous verrons comment manipuler les répertoires et les chemins de fichiers de manière portable !

---

> **Conseil professionnel :** Un programme qui gère bien ses erreurs inspire confiance. Prenez le temps de traiter correctement chaque cas d'erreur possible, vos utilisateurs vous en seront reconnaissants !

⏭️ [Manipulation de répertoires](/08-gestion-fichiers-io/06-manipulation-repertoires.md)
