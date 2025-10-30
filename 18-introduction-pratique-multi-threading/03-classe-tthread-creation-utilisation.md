🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.3 La classe TThread : création et utilisation

## Introduction

Maintenant que vous comprenez les concepts de base, passons à la pratique ! Dans cette section, vous allez créer votre premier thread et le voir s'exécuter en parallèle de votre interface graphique.

La bonne nouvelle : FreePascal rend cela relativement simple grâce à la classe **`TThread`**. Vous n'avez pas à comprendre les détails bas niveau de la gestion des threads par le système d'exploitation.

## Présentation de TThread

### Qu'est-ce que TThread ?

`TThread` est une classe abstraite fournie par FreePascal qui encapsule toute la complexité de la création et gestion d'un thread. Elle fait partie de l'unité `Classes`.

**Caractéristiques principales :**
- Multi-plateforme (même code sur Windows et Linux)
- Gère automatiquement le cycle de vie du thread
- Fournit des méthodes pour communiquer avec le thread principal
- Facilite la synchronisation et l'accès aux ressources partagées

### Hiérarchie de classe

```
TObject
  └── TThread
        └── VotreThreadPersonnalisé
```

`TThread` hérite de `TObject`, ce qui signifie qu'elle possède les méthodes classiques comme `Create`, `Free`, etc.

## Anatomie d'un thread personnalisé

Pour créer votre propre thread, vous devez :

1. **Déclarer une classe** qui hérite de `TThread`
2. **Redéfinir la méthode `Execute`** : c'est le "cœur" de votre thread
3. **Créer une instance** de votre classe
4. **Démarrer le thread** avec la méthode `Start`

### Structure de base

```pascal
type
  TMonThread = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TMonThread.Execute;
begin
  // Le code qui s'exécute dans le thread
  // C'est ICI que se passe le travail !
end;
```

C'est tout ! Cette structure minimale suffit pour créer un thread fonctionnel.

## Votre premier thread : un exemple simple

Créons un thread qui compte de 1 à 10 avec une pause entre chaque nombre.

### Déclaration de la classe

```pascal
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls;

type
  { TFormMain }
  TFormMain = class(TForm)
    ButtonStart: TButton;
    Memo1: TMemo;
    procedure ButtonStartClick(Sender: TObject);
  end;

  { TMonThreadCompteur }
  TMonThreadCompteur = class(TThread)
  protected
    procedure Execute; override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TMonThreadCompteur }

procedure TMonThreadCompteur.Execute;
var
  i: Integer;
begin
  // Boucle de 1 à 10
  for i := 1 to 10 do
  begin
    // Attendre 1 seconde
    Sleep(1000);

    // Note : on ne peut PAS écrire directement dans le Memo ici !
    // Nous verrons comment faire dans la section suivante
  end;
end;

{ TFormMain }

procedure TFormMain.ButtonStartClick(Sender: TObject);
var
  MonThread: TMonThreadCompteur;
begin
  // Créer le thread
  MonThread := TMonThreadCompteur.Create(True); // True = créé suspendu

  // Le libérer automatiquement à la fin
  MonThread.FreeOnTerminate := True;

  // Démarrer le thread
  MonThread.Start;

  Memo1.Lines.Add('Thread démarré !');
end;

end.
```

### Analyse du code

Décomposons ce qui se passe :

#### 1. Déclaration de la classe thread

```pascal
TMonThreadCompteur = class(TThread)
protected
  procedure Execute; override;
end;
```

- Notre classe hérite de `TThread`
- Nous redéfinissons `Execute` avec le mot-clé `override`
- La visibilité est `protected` (c'est la convention)

#### 2. Implémentation de Execute

```pascal
procedure TMonThreadCompteur.Execute;
var
  i: Integer;
begin
  for i := 1 to 10 do
  begin
    Sleep(1000);  // Pause d'une seconde
    // Traitement...
  end;
end;
```

- `Execute` est la méthode qui contient le code exécuté par le thread
- `Sleep(1000)` met le thread en pause pour 1000 millisecondes (1 seconde)
- Cette méthode s'exécute **dans le thread de travail**, pas dans le thread principal !

#### 3. Création du thread

```pascal
MonThread := TMonThreadCompteur.Create(True);
```

Le paramètre `True` signifie "créé suspendu" (CreateSuspended). Le thread est créé mais ne démarre pas immédiatement. C'est la méthode recommandée.

**Pourquoi créer suspendu ?**
- Vous pouvez configurer des propriétés avant le démarrage
- Vous contrôlez exactement quand le thread commence
- Plus sûr et plus prévisible

#### 4. Configuration FreeOnTerminate

```pascal
MonThread.FreeOnTerminate := True;
```

**Très important !** Cette propriété indique au thread de se libérer automatiquement de la mémoire quand il se termine.

- `True` : Le thread se détruit tout seul (recommandé pour les threads "fire and forget")
- `False` : Vous devez appeler `Free` manuellement (utilisé quand vous voulez récupérer des résultats)

#### 5. Démarrage du thread

```pascal
MonThread.Start;
```

À ce moment, le thread démarre réellement et la méthode `Execute` commence à s'exécuter **en parallèle** du reste de votre programme !

## Exemple plus détaillé : thread qui télécharge

Voici un exemple plus réaliste qui simule un téléchargement de fichier :

```pascal
type
  TThreadTelecharger = class(TThread)
  private
    FNomFichier: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const ANomFichier: string);
  end;

constructor TThreadTelecharger.Create(const ANomFichier: string);
begin
  // Appeler le constructeur parent avec CreateSuspended = True
  inherited Create(True);

  // Sauvegarder le paramètre
  FNomFichier := ANomFichier;

  // Configuration
  FreeOnTerminate := True;
end;

procedure TThreadTelecharger.Execute;
var
  i: Integer;
begin
  // Simulation d'un téléchargement de 100 "blocs"
  for i := 1 to 100 do
  begin
    // Vérifier si on doit s'arrêter
    if Terminated then
      Break;

    // Simuler le téléchargement d'un bloc (100ms)
    Sleep(100);

    // Ici, on téléchargerait vraiment un morceau du fichier
    // Par exemple : HttpClient.Get(...);
  end;

  // Le téléchargement est terminé
end;
```

### Utilisation

```pascal
procedure TFormMain.ButtonDownloadClick(Sender: TObject);
var
  Thread: TThreadTelecharger;
begin
  ButtonDownload.Enabled := False; // Désactiver pendant le téléchargement

  Thread := TThreadTelecharger.Create('fichier.zip');
  Thread.Start;
end;
```

### Points importants de cet exemple

#### Constructeur personnalisé

```pascal
constructor TThreadTelecharger.Create(const ANomFichier: string);
begin
  inherited Create(True);  // Toujours appeler le constructeur parent !
  FNomFichier := ANomFichier;
  FreeOnTerminate := True;
end;
```

Quand vous créez un constructeur personnalisé :
- **Toujours** appeler `inherited Create(True)` en premier
- Initialiser vos variables après
- Configurer les propriétés du thread (comme `FreeOnTerminate`)

#### Vérification de Terminated

```pascal
if Terminated then
  Break;
```

La propriété `Terminated` indique si quelqu'un a demandé au thread de s'arrêter (via la méthode `Terminate`).

**Bonne pratique** : Vérifier régulièrement `Terminated` dans vos boucles pour permettre un arrêt propre du thread.

## Les méthodes et propriétés importantes de TThread

### Méthodes principales

| Méthode | Description | Où l'appeler |
|---------|-------------|--------------|
| `Create(CreateSuspended)` | Crée le thread | Thread principal |
| `Start` | Démarre le thread | Thread principal |
| `Terminate` | Demande au thread de s'arrêter | Thread principal |
| `WaitFor` | Attend la fin du thread | Thread principal |
| `Execute` | Code exécuté par le thread | **Dans le thread** |
| `Sleep(ms)` | Met le thread en pause | Dans le thread |

### Propriétés importantes

| Propriété | Type | Description |
|-----------|------|-------------|
| `FreeOnTerminate` | Boolean | Si True, le thread se libère automatiquement |
| `Terminated` | Boolean | True si Terminate a été appelé |
| `Finished` | Boolean | True si Execute est terminé |
| `ThreadID` | TThreadID | Identifiant unique du thread |

## Deux patterns d'utilisation courants

### Pattern 1 : Fire and Forget (Tire et oublie)

Vous lancez le thread et ne vous en occupez plus. Il fait son travail et se termine.

```pascal
procedure TFormMain.ButtonActionClick(Sender: TObject);
var
  Thread: TMonThread;
begin
  Thread := TMonThread.Create(True);
  Thread.FreeOnTerminate := True;  // Important !
  Thread.Start;

  // On ne garde pas de référence, le thread se gère tout seul
end;
```

**Avantage** : Simple, pas de gestion de la mémoire
**Inconvénient** : Vous ne pouvez pas récupérer de résultat facilement

### Pattern 2 : Wait For Result (Attendre un résultat)

Vous créez le thread, le démarrez, et attendez qu'il termine pour récupérer un résultat.

```pascal
type
  TThreadCalcul = class(TThread)
  private
    FResultat: Double;
  protected
    procedure Execute; override;
  public
    property Resultat: Double read FResultat;
  end;

procedure TThreadCalcul.Execute;
begin
  // Calcul complexe qui prend du temps
  FResultat := CalculComplexe();
end;

procedure TFormMain.ButtonCalculerClick(Sender: TObject);
var
  Thread: TThreadCalcul;
begin
  Thread := TThreadCalcul.Create(True);
  Thread.FreeOnTerminate := False;  // On va le libérer nous-mêmes
  try
    Thread.Start;
    Thread.WaitFor;  // Attend que le thread termine

    // Maintenant on peut lire le résultat
    ShowMessage('Résultat : ' + FloatToStr(Thread.Resultat));
  finally
    Thread.Free;  // Libération manuelle
  end;
end;
```

**Attention** : `WaitFor` **bloque** le thread principal ! L'interface gèlera pendant l'attente. Ce pattern n'est utile que si le thread est très rapide ou si vous ne voulez pas que l'utilisateur fasse autre chose pendant ce temps.

## Erreurs courantes à éviter

### ❌ Erreur 1 : Oublier inherited Create

```pascal
constructor TMonThread.Create;
begin
  // ERREUR : Pas d'appel à inherited !
  FreeOnTerminate := True;
end;
```

**Conséquence** : Crash mystérieux ou comportement imprévisible.

**Solution** : Toujours appeler `inherited Create(True)` en premier.

### ❌ Erreur 2 : Appeler Start deux fois

```pascal
Thread := TMonThread.Create(True);
Thread.Start;
Thread.Start;  // ERREUR !
```

**Conséquence** : Exception ou comportement erratique.

**Solution** : Un thread ne peut être démarré qu'une seule fois.

### ❌ Erreur 3 : Accéder au thread après FreeOnTerminate

```pascal
Thread := TMonThread.Create(True);
Thread.FreeOnTerminate := True;
Thread.Start;

Sleep(100);
Thread.Terminate;  // ERREUR : le thread peut déjà être libéré !
```

**Conséquence** : Violation d'accès.

**Solution** : Si vous avez besoin de contrôler le thread après son démarrage, mettez `FreeOnTerminate := False` et gérez la libération vous-même.

### ❌ Erreur 4 : Créer sans True puis ne pas démarrer

```pascal
Thread := TMonThread.Create(False);  // Démarre immédiatement !
// Oups, on voulait configurer des choses avant...
```

**Conséquence** : Le thread démarre avant que vous ayez fini de le configurer.

**Solution** : Toujours utiliser `Create(True)` puis `Start` explicitement.

### ❌ Erreur 5 : Ne pas gérer Terminated

```pascal
procedure TMonThread.Execute;
var
  i: Integer;
begin
  for i := 1 to 1000000 do
  begin
    // Pas de vérification de Terminated !
    DoSomething(i);
  end;
end;
```

**Conséquence** : Le thread ne peut pas être arrêté proprement.

**Solution** : Vérifier régulièrement `Terminated` dans les boucles.

## Un exemple complet et commenté

Voici un exemple complet qui montre les bonnes pratiques :

```pascal
type
  { Thread qui compte et s'arrête proprement }
  TThreadCompteur = class(TThread)
  private
    FMax: Integer;
    FOnProgress: TNotifyEvent;  // Pour notifier la progression (section suivante)
  protected
    procedure Execute; override;
  public
    constructor Create(AMax: Integer);
  end;

constructor TThreadCompteur.Create(AMax: Integer);
begin
  // 1. Toujours appeler le constructeur parent en premier
  inherited Create(True);

  // 2. Initialiser les variables
  FMax := AMax;

  // 3. Configurer le thread
  FreeOnTerminate := True;
end;

procedure TThreadCompteur.Execute;
var
  i: Integer;
begin
  // Boucle principale du thread
  for i := 1 to FMax do
  begin
    // Vérification d'arrêt : TOUJOURS faire cela dans les boucles !
    if Terminated then
    begin
      // Sortie propre
      Exit;
    end;

    // Simulation de travail
    Sleep(100);

    // Note : Pour mettre à jour l'interface, il faut utiliser Synchronize
    // Nous verrons cela dans la section 18.5
  end;

  // Le thread se termine ici automatiquement
end;
```

## Visualisation de l'exécution

Quand vous créez et démarrez un thread, voici ce qui se passe :

```
THREAD PRINCIPAL              THREAD DE TRAVAIL
     |
     | MonThread.Create(True)
     |------------------------------>  [Thread créé mais suspendu]
     |
     | MonThread.Start
     |------------------------------>  [Thread démarre]
     |
     | Code continue...              Execute commence
     |   ShowMessage(...)            |
     |   ButtonX.Enabled := False    | for i := 1 to 10
     |   ...                         |   Sleep(1000)
     |                               |   // Travail...
     |                               | end
     |                               |
     |                               Execute se termine
     | [L'interface reste réactive]  [Thread se libère si FreeOnTerminate]
     |
```

Les deux threads s'exécutent **vraiment en parallèle** !

## Récapitulatif

### Pour créer un thread

1. Hériter de `TThread`
2. Redéfinir `Execute`
3. Créer avec `Create(True)`
4. Configurer (notamment `FreeOnTerminate`)
5. Démarrer avec `Start`

### Points clés

- `Execute` contient le code qui s'exécute dans le thread
- Toujours appeler `inherited Create(True)` dans votre constructeur
- Vérifier régulièrement `Terminated` pour permettre un arrêt propre
- `FreeOnTerminate := True` pour que le thread se libère automatiquement
- Un thread ne peut être démarré qu'une seule fois

### Ce que nous n'avons pas encore vu

- Comment mettre à jour l'interface depuis le thread (section 18.5)
- Comment passer des données au thread et en récupérer
- Comment gérer les variables partagées en sécurité

## Conclusion

Vous savez maintenant créer un thread basique avec `TThread` ! C'est un grand pas.

Mais il y a un problème : dans tous nos exemples, le thread fait son travail en silence. Comment faire pour qu'il communique avec l'interface ? Comment afficher une progression, ou le résultat final ?

C'est exactement ce que nous allons voir dans les sections suivantes, en commençant par la méthode `Synchronize` qui est LA solution pour communiquer en toute sécurité avec l'interface graphique.

Avant de continuer, assurez-vous d'avoir bien compris cette section. Créez quelques threads simples, faites-les compter, dormir, effectuer des calculs. Familiarisez-vous avec `Create`, `Start`, `Execute` et `Terminated`.

Une fois à l'aise avec ces bases, vous serez prêt pour la suite !

⏭️ [Cycle de vie d'un thread](/18-introduction-pratique-multi-threading/04-cycle-vie-thread.md)
