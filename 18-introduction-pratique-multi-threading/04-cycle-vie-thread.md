🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.4 Cycle de vie d'un thread

## Introduction

Dans la section précédente, nous avons appris à créer et démarrer un thread. Mais que se passe-t-il exactement entre le moment où vous créez un thread et le moment où il disparaît de la mémoire ? Comment s'assurer qu'un thread se termine proprement sans laisser de traces ?

Dans cette section, nous allons explorer le **cycle de vie complet** d'un thread, de sa naissance à sa mort. Comprendre ce cycle est essentiel pour éviter les fuites mémoire et les bugs difficiles à détecter.

## Les états d'un thread

Un thread passe par plusieurs états au cours de sa vie. Voici les principaux :

### 1. Non créé (Inexistant)

Le thread n'existe pas encore en mémoire. C'est l'état avant l'appel au constructeur.

```pascal
var
  MonThread: TMonThread;
begin
  // Ici, MonThread n'est qu'une variable non initialisée
  // Le thread n'existe pas encore
```

### 2. Créé et suspendu

Vous avez appelé `Create(True)`. Le thread existe en mémoire mais n'exécute pas encore son code.

```pascal
MonThread := TMonThread.Create(True);
// Le thread existe mais est suspendu
// Execute n'est pas encore appelé
```

**État** : Le thread est prêt mais en attente de démarrage.

### 3. En cours d'exécution (Running)

Vous avez appelé `Start`. La méthode `Execute` s'exécute activement.

```pascal
MonThread.Start;
// Maintenant Execute s'exécute en parallèle !
```

**État** : Le thread fait son travail.

### 4. Terminé (Terminated)

La méthode `Execute` est arrivée à sa fin, ou quelqu'un a appelé `Terminate`.

```pascal
// Fin naturelle de Execute
procedure TMonThread.Execute;  
begin  
  DoWork;
end;  // Ici, le thread passe à l'état "Terminé"
```

**État** : Le code du thread ne s'exécute plus, mais l'objet existe encore en mémoire.

### 5. Libéré (Freed)

L'objet thread a été libéré de la mémoire avec `Free` ou automatiquement via `FreeOnTerminate`.

```pascal
MonThread.Free;
// Le thread n'existe plus en mémoire
```

**État** : Le thread n'existe plus du tout.

## Diagramme du cycle de vie

Voici une représentation visuelle du cycle de vie d'un thread :

```
┌─────────────────┐
│   Non créé      │
└────────┬────────┘
         │ Create(True)
         ▼
┌─────────────────┐
│ Créé/Suspendu   │
└────────┬────────┘
         │ Start
         ▼
┌─────────────────┐
│  En exécution   │◄──┐
│   (Running)     │   │ Boucles, Sleep, etc.
└────────┬────────┘   │
         │            │
         │ Execute se termine
         │ OU Terminate + Check
         ▼
┌─────────────────┐
│    Terminé      │
│  (Finished)     │
└────────┬────────┘
         │
         │ FreeOnTerminate = True
         │ OU Free manuel
         ▼
┌─────────────────┐
│    Libéré       │
│   (N'existe     │
│   plus)         │
└─────────────────┘
```

## Cycle de vie détaillé : étape par étape

Prenons un exemple concret et suivons le thread tout au long de sa vie.

### Exemple de code

```pascal
type
  TThreadExample = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TThreadExample.Execute;  
var  
  i: Integer;
begin
  for i := 1 to 5 do
  begin
    Sleep(1000);
    if Terminated then
      Exit;
  end;
end;

procedure TFormMain.ButtonStartClick(Sender: TObject);  
var  
  MyThread: TThreadExample;
begin
  MyThread := TThreadExample.Create(True);  // Étape 1
  MyThread.FreeOnTerminate := True;         // Étape 2
  MyThread.Start;                           // Étape 3
  // Étapes 4-6 se passent automatiquement
end;
```

### Étape 1 : Création suspendue

```pascal
MyThread := TThreadExample.Create(True);
```

**Que se passe-t-il ?**
- L'objet `TThreadExample` est créé en mémoire
- Le système d'exploitation est informé qu'un nouveau thread existe
- Le thread reçoit un identifiant unique (ThreadID)
- Mais Execute **n'est pas encore appelé**
- L'état interne est "Suspendu"

**Propriétés à ce moment :**
- `Terminated = False`
- `Finished = False`
- `Suspended = True` (état interne)

### Étape 2 : Configuration

```pascal
MyThread.FreeOnTerminate := True;
```

**Que se passe-t-il ?**
- Vous configurez les propriétés du thread
- Ici, vous indiquez que le thread doit se libérer automatiquement
- Le thread est toujours suspendu

**C'est pour cela qu'on crée en mode suspendu** : pour pouvoir configurer avant le démarrage !

### Étape 3 : Démarrage

```pascal
MyThread.Start;
```

**Que se passe-t-il ?**
- Le thread passe de l'état "Suspendu" à "En exécution"
- Le système d'exploitation planifie l'exécution du thread
- La méthode `Execute` commence à s'exécuter **dans le thread de travail**
- Le code dans `ButtonStartClick` continue **dans le thread principal**

**À partir de maintenant, deux threads s'exécutent en parallèle !**

### Étape 4 : Exécution

```pascal
procedure TThreadExample.Execute;  
var  
  i: Integer;
begin
  for i := 1 to 5 do      // <-- Le thread est ICI
  begin
    Sleep(1000);
    if Terminated then
      Exit;
  end;
end;
```

**Que se passe-t-il ?**
- Le thread exécute son code ligne par ligne
- Il peut tourner en boucle, faire des Sleep, des calculs, etc.
- Il vérifie régulièrement `Terminated` pour savoir s'il doit s'arrêter
- Pendant ce temps, le thread principal gère l'interface

**Propriétés à ce moment :**
- `Terminated = False` (sauf si quelqu'un appelle `Terminate`)
- `Finished = False`

### Étape 5 : Fin de Execute

```pascal
end;  // <-- Fin de la méthode Execute
```

**Que se passe-t-il ?**
- La méthode `Execute` arrive à sa fin naturelle
- Le thread passe automatiquement à l'état "Terminé"
- Le code du thread ne s'exécute plus

**Propriétés à ce moment :**
- `Terminated = True` (mis automatiquement à True)
- `Finished = True`

**Important** : L'objet thread existe toujours en mémoire ! Il est juste inactif.

### Étape 6 : Libération automatique

```pascal
// Comme FreeOnTerminate = True...
```

**Que se passe-t-il ?**
- Le système détecte que `Execute` s'est terminé
- Il voit que `FreeOnTerminate = True`
- Il appelle automatiquement le destructeur du thread
- L'objet est libéré de la mémoire

**À ce moment, le thread n'existe plus du tout.**

## Les deux stratégies de gestion de la mémoire

Il existe deux façons principales de gérer la fin de vie d'un thread :

### Stratégie 1 : Libération automatique (FreeOnTerminate)

```pascal
procedure TFormMain.ButtonStartClick(Sender: TObject);  
var  
  MyThread: TThreadExample;
begin
  MyThread := TThreadExample.Create(True);
  MyThread.FreeOnTerminate := True;  // ← Clé !
  MyThread.Start;

  // On ne garde pas de référence
  // Le thread se gère tout seul
end;
```

**Avantages :**
- Simple : vous n'avez rien à faire
- Pas de risque de fuite mémoire
- Pas besoin de garder une référence

**Inconvénients :**
- Vous ne pouvez pas accéder au thread après son démarrage
- Vous ne pouvez pas récupérer facilement des résultats
- Vous ne pouvez pas attendre sa fin avec `WaitFor`

**Quand l'utiliser ?**
- Pour des tâches "fire and forget" (lance et oublie)
- Téléchargements, traitements en arrière-plan
- Quand vous n'avez pas besoin d'interagir avec le thread après son lancement

### Stratégie 2 : Libération manuelle

```pascal
procedure TFormMain.ButtonStartClick(Sender: TObject);  
var  
  MyThread: TThreadExample;
begin
  MyThread := TThreadExample.Create(True);
  MyThread.FreeOnTerminate := False;  // ← On gère nous-mêmes
  try
    MyThread.Start;
    MyThread.WaitFor;  // Attendre que le thread termine

    // Récupérer des résultats si besoin
    ShowMessage('Thread terminé !');
  finally
    MyThread.Free;  // Libération manuelle obligatoire
  end;
end;
```

**Avantages :**
- Vous contrôlez le cycle de vie
- Vous pouvez attendre la fin avec `WaitFor`
- Vous pouvez récupérer des résultats facilement

**Inconvénients :**
- Plus complexe
- Risque de fuite mémoire si vous oubliez `Free`
- `WaitFor` bloque le thread principal

**Quand l'utiliser ?**
- Quand vous avez besoin de récupérer un résultat
- Quand vous voulez attendre explicitement la fin
- Pour des threads de courte durée

## Gestion de l'arrêt d'un thread

### Arrêt naturel

Le thread s'arrête quand `Execute` arrive à sa fin :

```pascal
procedure TThreadExample.Execute;  
begin  
  DoSomething;
  DoSomethingElse;
  // Fin naturelle ici
end;
```

C'est le cas le plus simple et le plus propre.

### Arrêt demandé (Terminate)

Quelqu'un demande au thread de s'arrêter en appelant `Terminate` :

```pascal
// Dans le thread principal
MyThread.Terminate;
```

**Important** : `Terminate` ne tue pas brutalement le thread ! Elle met juste la propriété `Terminated` à `True`. C'est au thread de vérifier cette propriété et de sortir proprement :

```pascal
procedure TThreadExample.Execute;  
var  
  i: Integer;
begin
  for i := 1 to 1000 do
  begin
    if Terminated then  // ← Vérification
      Exit;             // ← Sortie propre

    DoWork(i);
  end;
end;
```

### Exemple complet avec arrêt

```pascal
type
  TFormMain = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
  private
    FThread: TThreadExample;
  end;

procedure TFormMain.ButtonStartClick(Sender: TObject);  
begin  
  // Créer et démarrer
  FThread := TThreadExample.Create(True);
  FThread.FreeOnTerminate := False;  // On gère manuellement
  FThread.Start;

  ButtonStart.Enabled := False;
  ButtonStop.Enabled := True;
end;

procedure TFormMain.ButtonStopClick(Sender: TObject);  
begin  
  if Assigned(FThread) then
  begin
    // Demander l'arrêt
    FThread.Terminate;

    // Attendre que le thread se termine
    FThread.WaitFor;

    // Libérer
    FThread.Free;
    FThread := nil;
  end;

  ButtonStart.Enabled := True;
  ButtonStop.Enabled := False;
end;
```

**Attention** : Dans cet exemple, `WaitFor` bloque l'interface ! Pour une vraie application, vous utiliseriez un mécanisme plus sophistiqué (que nous verrons dans la section 18.9).

## Propriétés liées au cycle de vie

### Terminated

```pascal
property Terminated: Boolean;
```

**Signification** : Indique si quelqu'un a demandé au thread de s'arrêter.

**Devient True quand :**
- Vous appelez `Terminate`
- `Execute` se termine naturellement

**Usage typique :**
```pascal
procedure TMyThread.Execute;  
begin  
  while not Terminated do
  begin
    DoWork;
    Sleep(100);
  end;
end;
```

### Finished

```pascal
property Finished: Boolean;
```

**Signification** : Indique si la méthode `Execute` s'est complètement terminée.

**Devient True quand :**
- `Execute` arrive à sa fin

**Usage typique :**
```pascal
// Vérifier si le thread est vraiment terminé
if MyThread.Finished then
  ShowMessage('Thread complètement terminé');
```

### FreeOnTerminate

```pascal
property FreeOnTerminate: Boolean;
```

**Signification** : Si True, le thread se libère automatiquement à la fin de `Execute`.

**À définir avant `Start` !**

## Pièges courants liés au cycle de vie

### ❌ Piège 1 : Accéder à un thread après FreeOnTerminate

```pascal
MyThread := TThreadExample.Create(True);  
MyThread.FreeOnTerminate := True;  
MyThread.Start;  

Sleep(1000);  
MyThread.Terminate;  // DANGER ! Le thread peut être déjà libéré !  
```

**Problème** : Si le thread se termine avant votre appel, l'objet n'existe plus et vous accédez à de la mémoire libérée.

**Solution** : N'accédez pas au thread après `Start` si `FreeOnTerminate = True`.

### ❌ Piège 2 : Oublier Free avec FreeOnTerminate = False

```pascal
procedure TForm.ButtonClick(Sender: TObject);  
var  
  MyThread: TThreadExample;
begin
  MyThread := TThreadExample.Create(True);
  MyThread.FreeOnTerminate := False;
  MyThread.Start;
  // Oups, on n'appelle jamais Free !
end;  // FUITE MÉMOIRE
```

**Problème** : Fuite mémoire. Chaque clic crée un thread qui ne sera jamais libéré.

**Solution** : Toujours libérer avec `Free` si `FreeOnTerminate = False`.

### ❌ Piège 3 : Double libération

```pascal
MyThread := TThreadExample.Create(True);  
MyThread.FreeOnTerminate := True;  
MyThread.Start;  
MyThread.WaitFor;  
MyThread.Free;  // ERREUR ! Déjà libéré automatiquement  
```

**Problème** : Le thread s'est libéré automatiquement, vous tentez de le libérer une deuxième fois.

**Solution** : Choisir une stratégie et s'y tenir (auto ou manuel, pas les deux).

### ❌ Piège 4 : WaitFor dans le thread principal d'une GUI

```pascal
MyThread := TThreadExample.Create(True);  
MyThread.Start;  
MyThread.WaitFor;  // L'interface gèle !  
```

**Problème** : `WaitFor` bloque le thread appelant. Si vous l'appelez dans le thread principal, l'interface gèle.

**Solution** : N'utilisez `WaitFor` que pour des threads très courts, ou utilisez d'autres mécanismes (callbacks, événements).

## Bonnes pratiques pour le cycle de vie

### ✓ Pratique 1 : Choisir la bonne stratégie dès le départ

Avant de créer votre thread, décidez :
- **Fire and forget** → `FreeOnTerminate := True`
- **Besoin de contrôle/résultat** → `FreeOnTerminate := False`

### ✓ Pratique 2 : Toujours vérifier Terminated

```pascal
procedure TMyThread.Execute;  
begin  
  while not Terminated do
  begin
    DoWork;
    Sleep(100);
  end;
  // Sortie propre
end;
```

### ✓ Pratique 3 : Utiliser try-finally dans Execute

```pascal
procedure TMyThread.Execute;  
var  
  Resource: TResource;
begin
  Resource := TResource.Create;
  try
    while not Terminated do
      DoWork(Resource);
  finally
    Resource.Free;  // Toujours libéré, même en cas d'exception
  end;
end;
```

### ✓ Pratique 4 : Garder une référence si FreeOnTerminate = False

```pascal
type
  TFormMain = class(TForm)
  private
    FMyThread: TThreadExample;  // Variable membre
  public
    destructor Destroy; override;
  end;

destructor TFormMain.Destroy;  
begin  
  if Assigned(FMyThread) then
  begin
    FMyThread.Terminate;
    FMyThread.WaitFor;
    FMyThread.Free;
  end;
  inherited;
end;
```

## Visualisation complète du cycle

Voici un exemple montrant tous les états :

```pascal
procedure TFormMain.DemoCompleteClick(Sender: TObject);  
var  
  MyThread: TThreadExample;
begin
  Memo1.Lines.Add('1. Thread non créé');

  MyThread := TThreadExample.Create(True);
  Memo1.Lines.Add('2. Thread créé (suspendu)');
  Memo1.Lines.Add('   Terminated = ' + BoolToStr(MyThread.Terminated, True));
  Memo1.Lines.Add('   Finished = ' + BoolToStr(MyThread.Finished, True));

  MyThread.FreeOnTerminate := False;
  MyThread.Start;
  Memo1.Lines.Add('3. Thread démarré (en exécution)');

  Sleep(2000);  // Laisser le thread travailler
  Memo1.Lines.Add('4. Thread en cours...');

  MyThread.WaitFor;
  Memo1.Lines.Add('5. Thread terminé');
  Memo1.Lines.Add('   Terminated = ' + BoolToStr(MyThread.Terminated, True));
  Memo1.Lines.Add('   Finished = ' + BoolToStr(MyThread.Finished, True));

  MyThread.Free;
  Memo1.Lines.Add('6. Thread libéré (n''existe plus)');
end;
```

## Récapitulatif

### Les états d'un thread

1. **Non créé** : N'existe pas encore
2. **Créé/Suspendu** : Existe mais n'exécute pas
3. **En exécution** : Execute s'exécute activement
4. **Terminé** : Execute fini, objet existe encore
5. **Libéré** : N'existe plus en mémoire

### Deux stratégies de libération

| Aspect | FreeOnTerminate = True | FreeOnTerminate = False |
|--------|------------------------|-------------------------|
| **Libération** | Automatique | Manuelle obligatoire |
| **Accès après Start** | NON | Oui |
| **WaitFor** | NON | Oui |
| **Récupération résultat** | Difficile | Facile |
| **Fuite mémoire** | Non (si bien fait) | Risque si oubli |
| **Cas d'usage** | Fire and forget | Contrôle total |

### Règles d'or

1. Toujours vérifier `Terminated` dans les boucles
2. Choisir une stratégie de libération et s'y tenir
3. Ne jamais accéder à un thread après `Start` si `FreeOnTerminate = True`
4. Toujours libérer avec `Free` si `FreeOnTerminate = False`
5. Éviter `WaitFor` dans le thread principal pour les threads longs

## Conclusion

Comprendre le cycle de vie d'un thread est essentiel pour :
- Éviter les fuites mémoire
- Éviter les crashs par accès à de la mémoire libérée
- Gérer proprement l'arrêt des threads
- Choisir la bonne stratégie de libération

Le cycle de vie peut sembler complexe au début, mais avec la pratique, il devient naturel. L'essentiel est de :
- Être cohérent dans votre approche
- Toujours penser à la fin de vie dès la création
- Vérifier régulièrement `Terminated`

Maintenant que vous maîtrisez la création et le cycle de vie des threads, il est temps de résoudre le problème le plus important : **comment communiquer avec l'interface graphique depuis un thread de travail ?**

C'est ce que nous allons voir dans la section suivante avec la méthode `Synchronize` !

⏭️ [TThread.Synchronize : communication thread-UI](/18-introduction-pratique-multi-threading/05-tthread-synchronize-communication.md)
