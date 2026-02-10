🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 7.6 Sections initialization et finalization

## Qu'est-ce que initialization et finalization ?

Les sections `initialization` et `finalization` sont des blocs de code **optionnels** dans une unité qui s'exécutent automatiquement :

- **initialization** : Exécuté **au démarrage** du programme, avant le code du programme principal
- **finalization** : Exécuté **à la fin** du programme, après le code du programme principal

C'est comme les **préparatifs** et le **rangement** automatiques !

## Analogie : La bibliothèque

Imaginez une bibliothèque :

| Moment | Bibliothèque | Unité Pascal |
|--------|--------------|--------------|
| **Ouverture** | Allumer les lumières, déverrouiller les portes | **initialization** |
| **Journée** | Les gens empruntent et rendent des livres | **Programme principal** |
| **Fermeture** | Éteindre les lumières, verrouiller les portes | **finalization** |

## Structure d'une unité avec initialization et finalization

```pascal
unit MonUnite;

interface
  // Déclarations publiques

implementation
  // Code de l'unité

initialization
  // Code exécuté au DÉMARRAGE du programme
  WriteLn('Initialisation de MonUnite');

finalization
  // Code exécuté à la FIN du programme
  WriteLn('Nettoyage de MonUnite');

end.
```

## Exemple simple et complet

```pascal
{$mode objfpc}{$H+}
unit UniteCompteurInit;

interface

function ObtenirCompteur: Integer;  
procedure Incrementer;  

implementation

var
  Compteur: Integer;

function ObtenirCompteur: Integer;  
begin  
  Result := Compteur;
end;

procedure Incrementer;  
begin  
  Inc(Compteur);
end;

initialization
  // Initialiser le compteur à 0 au démarrage
  Compteur := 0;
  WriteLn('UniteCompteur : Compteur initialisé à 0');

finalization
  // Afficher la valeur finale
  WriteLn('UniteCompteur : Valeur finale du compteur = ', Compteur);
  Flush(Output);  // Nécessaire en finalization : sans Flush, la sortie peut être perdue

end.
```

**Programme utilisant cette unité :**

```pascal
program TestCompteurInit;

uses
  UniteCompteurInit;

begin
  WriteLn('Début du programme principal');

  Incrementer;
  Incrementer;
  Incrementer;
  WriteLn('Compteur actuel : ', ObtenirCompteur);

  WriteLn('Fin du programme principal');
end.
```

**Sortie du programme :**
```
UniteCompteur : Compteur initialisé à 0  
Début du programme principal  
Compteur actuel : 3  
Fin du programme principal  
UniteCompteur : Valeur finale du compteur = 3  
```

Remarquez l'ordre d'exécution !

## Quand s'exécutent ces sections ?

### Ordre d'exécution détaillé

```
1. initialization de toutes les unités (dans l'ordre de dépendances)
   ↓
2. begin..end du programme principal
   ↓
3. finalization de toutes les unités (dans l'ordre INVERSE)
```

### Exemple avec plusieurs unités

```pascal
// UniteInitA.pas
unit UniteInitA;

interface

implementation

initialization
  WriteLn('1. Initialisation UniteA');

finalization
  WriteLn('6. Finalisation UniteA');
  Flush(Output);

end.
```

```pascal
// UniteInitB.pas
unit UniteInitB;

interface

uses
  UniteInitA;  // UniteB dépend de UniteA

implementation

initialization
  WriteLn('2. Initialisation UniteB');

finalization
  WriteLn('5. Finalisation UniteB');
  Flush(Output);

end.
```

```pascal
// UniteInitC.pas
unit UniteInitC;

interface

uses
  UniteInitB;  // UniteC dépend de UniteB

implementation

initialization
  WriteLn('3. Initialisation UniteC');

finalization
  WriteLn('4. Finalisation UniteC');
  Flush(Output);

end.
```

```pascal
program TestOrdreInit;

uses
  UniteInitC;

begin
  WriteLn('─── Programme Principal ───');
end.
```

**Sortie :**
```
1. Initialisation UniteA
2. Initialisation UniteB
3. Initialisation UniteC
─── Programme Principal ───
4. Finalisation UniteC
5. Finalisation UniteB
6. Finalisation UniteA
```

**Règle importante :** Les sections `finalization` s'exécutent dans l'**ordre inverse** des `initialization` (dernier initialisé = premier finalisé).

## Cas d'usage typiques

### 1. Initialisation de variables globales

```pascal
unit ConfigGlobale;

interface

var
  CheminDonnees: String;
  ModeDebug: Boolean;

implementation

uses
  SysUtils;  // Pour GetCurrentDir

initialization
  CheminDonnees := GetCurrentDir + '/data/';
  ModeDebug := False;
  WriteLn('Configuration initialisée');

end.
```

### 2. Création d'objets globaux

```pascal
unit GestionnaireLog;

interface

uses
  Classes;

var
  LogGlobal: TStringList;

procedure AjouterLog(message: String);

implementation

procedure AjouterLog(message: String);  
begin  
  if Assigned(LogGlobal) then
    LogGlobal.Add(message);
end;

initialization
  // Créer l'objet au démarrage
  LogGlobal := TStringList.Create;
  AjouterLog('Système de log démarré');

finalization
  // Libérer l'objet à la fin
  if Assigned(LogGlobal) then
  begin
    AjouterLog('Système de log arrêté');
    LogGlobal.Free;
  end;

end.
```

### 3. Connexion à une base de données

```pascal
unit ConnexionDB;

interface

uses
  SQLdb, SysUtils;

var
  Connexion: TSQLConnection;

function EstConnecte: Boolean;

implementation

function EstConnecte: Boolean;  
begin  
  Result := Assigned(Connexion) and Connexion.Connected;
end;

initialization
  // Créer et configurer la connexion
  Connexion := TSQLConnection.Create(nil);
  Connexion.DatabaseName := 'mabase.db';

  try
    Connexion.Open;
    WriteLn('Connexion à la base de données réussie');
  except
    on E: Exception do
      WriteLn('Erreur connexion : ', E.Message);
  end;

finalization
  // Fermer proprement la connexion
  if Assigned(Connexion) then
  begin
    if Connexion.Connected then
      Connexion.Close;
    Connexion.Free;
    WriteLn('Connexion à la base de données fermée');
  end;

end.
```

### 4. Chargement de configuration

```pascal
unit UniteConfig;

interface

uses
  IniFiles;

var
  NomUtilisateur: String;
  DernierFichier: String;
  Langue: String;

implementation

var
  FichierIni: TIniFile;

initialization
  FichierIni := TIniFile.Create('config.ini');

  // Charger les paramètres
  NomUtilisateur := FichierIni.ReadString('User', 'Name', 'Anonyme');
  DernierFichier := FichierIni.ReadString('Files', 'Last', '');
  Langue := FichierIni.ReadString('Interface', 'Language', 'FR');

  WriteLn('Configuration chargée');

finalization
  // Sauvegarder et libérer
  if Assigned(FichierIni) then
  begin
    FichierIni.WriteString('User', 'Name', NomUtilisateur);
    FichierIni.WriteString('Files', 'Last', DernierFichier);
    FichierIni.Free;
    WriteLn('Configuration sauvegardée');
  end;

end.
```

## Sections optionnelles

Les sections `initialization` et `finalization` sont **optionnelles**. Vous avez plusieurs possibilités :

### Aucune section (le plus courant)

```pascal
unit UniteSimple;

interface
  procedure MaFonction;

implementation
  procedure MaFonction;
  begin
    WriteLn('Hello');
  end;

end.  // Pas de initialization ni finalization
```

### Seulement initialization

```pascal
unit UniteAvecInit;

interface

implementation

initialization
  WriteLn('Initialisation uniquement');
  // Pas de finalization

end.
```

### Seulement finalization (rare)

```pascal
unit UniteAvecFinal;

interface

implementation

finalization
  WriteLn('Finalisation uniquement');
  // Pas de initialization

end.
```

### Les deux sections

```pascal
unit UniteComplete;

interface

implementation

initialization
  WriteLn('Initialisation');

finalization
  WriteLn('Finalisation');

end.
```

## Gestion des erreurs

### Erreur dans initialization

Si une erreur se produit dans `initialization`, le programme s'arrête et les sections `finalization` **ne sont pas exécutées** pour cette unité.

```pascal
unit UniteAvecErreur;

interface

implementation

uses
  SysUtils;  // Pour Exception

var
  MonObjet: TObject;

initialization
  WriteLn('Début initialisation');
  MonObjet := TObject.Create;

  // Si une erreur se produit ici
  raise Exception.Create('Erreur critique !');

  // Ce code ne sera jamais exécuté
  WriteLn('Fin initialisation');

finalization
  // ⚠️ Cette section ne sera PAS exécutée si initialization échoue
  if Assigned(MonObjet) then
    MonObjet.Free;

end.
```

### Bonne pratique : Gestion sécurisée

```pascal
initialization
  MonObjet := nil;

  try
    MonObjet := TObject.Create;
    // Autres initialisations
  except
    on E: Exception do
    begin
      WriteLn('Erreur initialisation : ', E.Message);
      // Nettoyer ce qui a été créé
      if Assigned(MonObjet) then
        FreeAndNil(MonObjet);
    end;
  end;

finalization
  // Vérifier avant de libérer
  if Assigned(MonObjet) then
    FreeAndNil(MonObjet);
end.
```

## Différence avec le code normal

Pourquoi utiliser `initialization` au lieu de mettre le code dans une procédure ?

### ❌ Approche manuelle

```pascal
unit UniteConfig;

interface

var
  EstInitialise: Boolean;

procedure InitialiserUnite;  
procedure NettoyerUnite;  

implementation

var
  Ressource: TObject;

procedure InitialiserUnite;  
begin  
  if not EstInitialise then
  begin
    Ressource := TObject.Create;
    EstInitialise := True;
  end;
end;

procedure NettoyerUnite;  
begin  
  if Assigned(Ressource) then
    Ressource.Free;
end;

end.
```

**Problèmes :**
- L'utilisateur doit **se souvenir** d'appeler `InitialiserUnite`
- L'utilisateur doit **se souvenir** d'appeler `NettoyerUnite`
- Risque d'oubli = bugs !

### ✅ Approche automatique

```pascal
unit UniteConfig;

interface
  // Pas besoin de procédures d'initialisation

implementation

var
  Ressource: TObject;

initialization
  Ressource := TObject.Create;  // ✅ Automatique

finalization
  if Assigned(Ressource) then
    Ressource.Free;  // ✅ Automatique

end.
```

**Avantages :**
- ✅ Exécution **automatique** au démarrage
- ✅ Nettoyage **automatique** à la fin
- ✅ Impossible d'oublier !

## Bonnes pratiques

### 1. Garder les sections courtes et simples

```pascal
// ✅ Bon
initialization
  CompteurGlobal := 0;
  CheminDefaut := '/tmp/';

// ❌ Éviter - trop complexe
initialization
  for i := 1 to 1000 do
  begin
    // Beaucoup de code complexe
  end;
```

### 2. Ne pas dépendre de l'ordre d'autres unités

Évitez que votre `initialization` appelle des fonctions d'autres unités, car l'ordre n'est pas toujours prévisible dans les dépendances complexes.

### 3. Toujours libérer ce qui est créé

```pascal
initialization
  MaListe := TStringList.Create;  // Créé

finalization
  MaListe.Free;  // ✅ Libéré

end.
```

### 4. Vérifier avec Assigned avant de libérer

```pascal
finalization
  if Assigned(MonObjet) then
    FreeAndNil(MonObjet);
end.
```

### 5. Utiliser try-except pour la robustesse

```pascal
initialization
  try
    RessourceCritique := InitialiserRessource;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;
end.
```

## Débogage

Pour voir l'ordre d'exécution de vos unités :

```pascal
initialization
  WriteLn('Init: ', {$I %FILE%});  // Affiche le nom du fichier

finalization
  WriteLn('Fin: ', {$I %FILE%});
end.
```

Cela vous aide à comprendre dans quel ordre vos unités s'initialisent et se finalisent.

## Résumé

- **initialization** s'exécute **automatiquement au démarrage** du programme
- **finalization** s'exécute **automatiquement à la fin** du programme
- L'ordre : initialization (selon dépendances) → programme → finalization (ordre inverse)
- Utilisé pour : initialiser des variables globales, créer des objets, charger des ressources
- **Toujours libérer** dans finalization ce qui est créé dans initialization
- Les sections sont **optionnelles** (une, les deux, ou aucune)
- Éviter le code trop complexe dans ces sections
- Gérer les erreurs avec try-except pour plus de robustesse

Ces sections sont très pratiques pour gérer automatiquement le cycle de vie des ressources de vos unités !

Dans la prochaine section, nous découvrirons les unités standard du RTL (Run-Time Library) fournies par FreePascal.

⏭️ [Unités standard du RTL](/07-unites-organisation-code/07-unites-standard-rtl.md)
