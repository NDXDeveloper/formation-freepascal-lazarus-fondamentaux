🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.6 TThread.Queue vs Synchronize

## Introduction

Dans la section précédente, nous avons découvert `Synchronize`, la méthode qui permet à un thread de travail de communiquer avec l'interface graphique. Mais il existe une autre méthode : **`Queue`**.

Ces deux méthodes ont le même objectif (exécuter du code dans le thread principal), mais elles fonctionnent différemment. Comprendre leur différence est crucial pour choisir la bonne approche selon les situations.

Dans cette section, nous allons comparer ces deux méthodes et apprendre quand utiliser l'une plutôt que l'autre.

## Rappel : Comment fonctionne Synchronize

Avant de parler de `Queue`, rappelons rapidement le fonctionnement de `Synchronize` :

```
THREAD DE TRAVAIL              THREAD PRINCIPAL
     |                              |
     | Traitement...                | Gère l'interface
     |                              |
     | Synchronize(@Methode)        |
     |----------------------------->|
     | [JE M'ARRÊTE ET J'ATTENDS]   | Exécute Methode
     |                              | Label.Caption := 'Ok'
     |                              |
     |<-----------------------------|
     | [Je reprends]                |
     |                              |
```

**Caractéristique clé** : Le thread de travail **s'arrête et attend** que le thread principal ait terminé.

## Présentation de Queue

### Qu'est-ce que Queue ?

`Queue` (prononcez "kiou", qui signifie "file d'attente") est une alternative à `Synchronize` qui **ne bloque pas** le thread de travail.

Au lieu d'attendre, le thread dit au thread principal : "Voici une tâche à faire quand tu auras le temps" et continue immédiatement son travail.

### Comment fonctionne Queue

```
THREAD DE TRAVAIL              THREAD PRINCIPAL
     |                              |
     | Traitement...                | Gère l'interface
     |                              |
     | Queue(@Methode)              |
     |----------------------------->| [Ajoute à la file d'attente]
     | [JE CONTINUE IMMÉDIATEMENT]  |
     |                              |
     | Traitement suite...          | [Quand disponible]
     |                              | Exécute Methode
     |                              | Label.Caption := 'Ok'
     |                              |
```

**Caractéristique clé** : Le thread de travail **ne s'arrête pas**. Il continue son exécution immédiatement.

### Analogie : le restaurant

**Synchronize = Service à table**
- Le serveur vient à votre table
- Il attend que vous passiez commande
- Il ne peut pas servir d'autres clients pendant ce temps
- Une fois votre commande prise, il continue

**Queue = Commande au comptoir**
- Vous passez commande au comptoir
- Vous recevez un numéro
- Vous retournez à votre table immédiatement
- Le restaurant prépare votre commande quand il peut
- On vous appelle quand c'est prêt

## Syntaxe de Queue

La syntaxe est identique à celle de `Synchronize` :

```pascal
// Avec une méthode
Queue(@MaMethode);

// Avec une procédure anonyme (FreePascal 3.2+)
Queue(procedure
begin
  Label1.Caption := 'Texte';
end);
```

**Important** : Comme pour `Synchronize`, la méthode passée à `Queue` s'exécute dans le thread principal.

## Différences principales : tableau comparatif

| Aspect | Synchronize | Queue |
|--------|-------------|-------|
| **Blocage** | Le thread attend | Le thread continue |
| **Ordre d'exécution** | Immédiat (si possible) | Différé (quand le thread principal peut) |
| **Garantie temporelle** | Exécuté avant de continuer | Exécuté "plus tard" |
| **Performance** | Plus lent (attente) | Plus rapide (pas d'attente) |
| **Utilisation** | Quand l'ordre est critique | Quand l'ordre n'est pas critique |
| **Complexité** | Plus simple | Peut créer des décalages |

## Exemple comparatif

Créons le même programme avec les deux approches pour voir la différence.

### Version avec Synchronize

```pascal
type
  TThreadSync = class(TThread)
  private
    FCompteur: Integer;
    procedure AfficherCompteur;
  protected
    procedure Execute; override;
  end;

procedure TThreadSync.AfficherCompteur;
begin
  FormMain.Memo1.Lines.Add('Compteur : ' + IntToStr(FCompteur));
end;

procedure TThreadSync.Execute;
var
  i: Integer;
begin
  for i := 1 to 5 do
  begin
    Sleep(500);  // Simuler un traitement

    FCompteur := i;
    Synchronize(@AfficherCompteur);  // ATTEND ici

    // Cette ligne s'exécute APRÈS l'affichage
    FormMain.Memo1.Lines.Add('  -> Traitement ' + IntToStr(i) + ' continué');
  end;
end;
```

**Résultat dans le Memo :**
```
Compteur : 1
  -> Traitement 1 continué
Compteur : 2
  -> Traitement 2 continué
Compteur : 3
  -> Traitement 3 continué
...
```

L'ordre est **garanti** car le thread attend après chaque `Synchronize`.

### Version avec Queue

```pascal
type
  TThreadQueue = class(TThread)
  private
    FCompteur: Integer;
    procedure AfficherCompteur;
  protected
    procedure Execute; override;
  end;

procedure TThreadQueue.AfficherCompteur;
begin
  FormMain.Memo1.Lines.Add('Compteur : ' + IntToStr(FCompteur));
end;

procedure TThreadQueue.Execute;
var
  i: Integer;
begin
  for i := 1 to 5 do
  begin
    Sleep(500);  // Simuler un traitement

    FCompteur := i;
    Queue(@AfficherCompteur);  // NE ATTEND PAS

    // Cette ligne s'exécute IMMÉDIATEMENT (sans attendre l'affichage)
    FormMain.Memo1.Lines.Add('  -> Traitement ' + IntToStr(i) + ' continué');
  end;
end;
```

**Résultat possible dans le Memo :**
```
  -> Traitement 1 continué
  -> Traitement 2 continué
Compteur : 1
  -> Traitement 3 continué
Compteur : 2
Compteur : 3
  -> Traitement 4 continué
...
```

L'ordre **n'est pas garanti** ! Les affichages peuvent se mélanger car le thread ne attend pas.

## Quand utiliser Synchronize ?

### Utilisez Synchronize quand :

#### 1. L'ordre d'exécution est critique

```pascal
procedure TMyThread.Execute;
begin
  // Calculer un résultat
  FResultat := CalculComplexe();

  // Afficher le résultat
  Synchronize(@AfficherResultat);

  // Utiliser ce résultat pour la suite
  // On a BESOIN que l'affichage soit terminé avant de continuer
  FResultat2 := AutreCalcul(FResultat);
  Synchronize(@AfficherResultat2);
end;
```

#### 2. Vous devez modifier des variables partagées

```pascal
procedure TMyThread.Execute;
begin
  FDonnee := 'Valeur importante';
  Synchronize(@MettreAJour);

  // On sait que FDonnee a été utilisée dans MettreAJour
  // On peut la modifier sans risque
  FDonnee := 'Nouvelle valeur';
end;
```

#### 3. Vous avez besoin d'un feedback immédiat

```pascal
procedure TMyThread.Execute;
begin
  // Demander confirmation à l'utilisateur
  FQuestion := 'Voulez-vous continuer ?';
  Synchronize(@DemanderConfirmation);

  // FReponse a été définie par l'utilisateur
  if FReponse then
    ContinuerTraitement
  else
    Exit;
end;
```

#### 4. Vous gérez des ressources partagées

Si votre méthode synchronisée accède à des ressources qui doivent être libérées dans un ordre précis.

### Exemple concret avec Synchronize

**Mise à jour d'une barre de progression critique :**

```pascal
procedure TThreadDownload.Execute;
var
  Chunk: TBytes;
begin
  while not FinDuFichier do
  begin
    Chunk := TelechargerProchainBloc();

    // Mettre à jour la progression
    FPourcentage := CalculerProgression();
    Synchronize(@UpdateProgress);

    // Écrire le bloc dans le fichier
    // On VEUT que la progression soit affichée avant d'écrire
    EcrireDansFichier(Chunk);
  end;
end;
```

## Quand utiliser Queue ?

### Utilisez Queue quand :

#### 1. L'ordre n'est pas important

```pascal
procedure TMyThread.Execute;
begin
  // Faire du logging
  FMessage := 'Étape 1 commencée';
  Queue(@AjouterLog);

  // Peu importe quand le log est affiché
  TraiterEtape1();

  FMessage := 'Étape 2 commencée';
  Queue(@AjouterLog);

  TraiterEtape2();
end;
```

#### 2. La performance est importante

```pascal
procedure TMyThread.Execute;
var
  i: Integer;
begin
  for i := 1 to 10000 do
  begin
    TraitementRapide(i);

    // Mettre à jour l'interface tous les 100 items
    if i mod 100 = 0 then
    begin
      FProgress := (i * 100) div 10000;
      Queue(@UpdateProgress);  // Pas d'attente !
    end;
  end;
end;
```

Le thread ne ralentit pas, l'interface sera mise à jour "quand elle pourra".

#### 3. Vous faites des mises à jour fréquentes non critiques

```pascal
procedure TMyThread.Execute;
begin
  while not Terminated do
  begin
    // Lire une valeur de capteur
    FValeurCapteur := LireCapteur();

    // Afficher (mais pas besoin d'attendre)
    Queue(@AfficherValeur);

    Sleep(10);  // 100 fois par seconde
  end;
end;
```

Avec `Queue`, le thread peut lire le capteur rapidement sans être ralenti par l'affichage.

#### 4. Vous voulez que le thread soit le plus réactif possible

```pascal
procedure TMyThread.Execute;
begin
  while not Terminated do
  begin
    // Vérifier Terminated souvent pour réactivité
    if Terminated then Break;

    DoWork();

    // Mettre à jour UI sans bloquer
    Queue(@UpdateStatus);

    // Le thread peut vérifier Terminated rapidement
  end;
end;
```

### Exemple concret avec Queue

**Affichage de logs en temps réel :**

```pascal
type
  TThreadLogger = class(TThread)
  private
    FLogMessage: string;
    procedure AddLogLine;
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;

procedure TThreadLogger.AddLogLine;
begin
  FormMain.MemoLogs.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + FLogMessage);
end;

procedure TThreadLogger.Execute;
begin
  while not Terminated do
  begin
    // Faire du travail
    ProcessData();

    // Logger (ordre pas critique)
    FLogMessage := 'Paquet traité';
    Queue(@AddLogLine);

    // Continuer immédiatement
    MoreWork();

    FLogMessage := 'Cycle terminé';
    Queue(@AddLogLine);

    Sleep(100);
  end;
end;
```

Les logs apparaîtront dans le Memo, peut-être pas immédiatement, mais le thread reste très réactif.

## Cas pratique : combinaison des deux

Vous pouvez combiner `Synchronize` et `Queue` dans le même thread !

```pascal
procedure TThreadSmartUpdate.Execute;
var
  i: Integer;
begin
  for i := 1 to 100 do
  begin
    if Terminated then Break;

    // Traitement
    ProcessItem(i);

    // Mise à jour légère et fréquente -> Queue
    if i mod 10 = 0 then
    begin
      FProgress := i;
      Queue(@UpdateProgressBar);
    end;
  end;

  // Calcul final
  FResultatFinal := CalculerResultat();

  // Affichage critique du résultat -> Synchronize
  Synchronize(@AfficherResultatFinal);

  // Log de fin (pas critique) -> Queue
  FLogMessage := 'Traitement terminé avec succès';
  Queue(@AddLog);
end;
```

**Stratégie** :
- `Queue` pour les mises à jour fréquentes et non critiques (progression)
- `Synchronize` pour les opérations importantes (résultat final)
- `Queue` pour le logging

## Pièges à éviter avec Queue

### ❌ Piège 1 : Supposer que Queue s'exécute immédiatement

```pascal
procedure TMyThread.Execute;
begin
  FValeur := 100;
  Queue(@Afficher);

  FValeur := 200;  // ❌ Afficher n'a peut-être pas encore eu lieu !
  // Quelle valeur sera affichée ? On ne sait pas !
end;
```

**Problème** : Le thread modifie `FValeur` avant que `Queue` ait exécuté `Afficher`.

**Solution** : Utiliser des variables différentes ou Synchronize si l'ordre compte.

```pascal
procedure TMyThread.Execute;
begin
  FValeur1 := 100;
  Queue(@Afficher1);

  FValeur2 := 200;
  Queue(@Afficher2);
end;
```

### ❌ Piège 2 : Accumuler trop de Queue

```pascal
procedure TMyThread.Execute;
var
  i: Integer;
begin
  for i := 1 to 1000000 do
  begin
    FValeur := i;
    Queue(@Update);  // ❌ Un million de Queue !
  end;
end;
```

**Problème** : Vous créez un million de tâches dans la file d'attente. Cela consomme de la mémoire et peut ralentir l'application.

**Solution** : Limiter la fréquence.

```pascal
if i mod 1000 = 0 then
  Queue(@Update);
```

### ❌ Piège 3 : Dépendre de l'ordre avec Queue

```pascal
procedure TMyThread.Execute;
begin
  FEtape := 1;
  Queue(@AfficherEtape);

  FEtape := 2;
  Queue(@AfficherEtape);

  // On espère voir "Étape 1" puis "Étape 2"
  // Mais l'ordre n'est PAS garanti !
end;
```

**Problème** : `Queue` ne garantit pas l'ordre strict dans tous les cas.

**Solution** : Utiliser `Synchronize` si l'ordre est important, ou utiliser des variables distinctes.

### ❌ Piège 4 : Modifier des données après Queue

```pascal
procedure TMyThread.Execute;
var
  MaListe: TStringList;
begin
  MaListe := TStringList.Create;
  try
    MaListe.Add('Item 1');
    MaListe.Add('Item 2');

    FListe := MaListe;
    Queue(@AfficherListe);

    MaListe.Clear;  // ❌ DANGER ! Queue n'a peut-être pas encore lu la liste
  finally
    MaListe.Free;
  end;
end;
```

**Problème** : La liste est modifiée/libérée avant que `Queue` l'ait utilisée.

**Solution** : Créer une copie ou utiliser `Synchronize`.

## Performances : comparaison

### Test de performance simplifié

Imaginons un thread qui fait 1000 mises à jour :

**Avec Synchronize :**
```
Temps total : 150 ms
- 100 ms de traitement
- 50 ms d'attentes cumulées
```

**Avec Queue :**
```
Temps total : 105 ms
- 100 ms de traitement
- 5 ms de surcharge Queue
- Pas d'attente !
```

**Gain** : ~30% plus rapide avec `Queue` dans ce scénario.

### Mais attention !

Si vous avez **besoin** de l'ordre ou de l'attente, `Queue` ne remplace pas `Synchronize`. Ce n'est pas juste une "version plus rapide", c'est une **approche différente**.

## Règles de décision

Voici un arbre de décision simple :

```
Avez-vous besoin d'attendre que l'UI soit mise à jour ?
│
├─ OUI → Synchronize
│
└─ NON → Devez-vous garantir l'ordre strict ?
          │
          ├─ OUI → Synchronize
          │
          └─ NON → Voulez-vous maximiser la performance ?
                   │
                   ├─ OUI → Queue
                   │
                   └─ Peu importe → Queue (plus simple)
```

## Exemples récapitulatifs

### Exemple 1 : Barre de progression (Queue recommandé)

```pascal
procedure TMyThread.Execute;
var
  i: Integer;
begin
  for i := 1 to 100 do
  begin
    ProcessData(i);

    // Mise à jour non critique, performance importante
    FProgress := i;
    Queue(@UpdateProgressBar);
  end;
end;
```

**Pourquoi Queue ?** L'ordre précis d'affichage de la barre n'est pas critique.

### Exemple 2 : Confirmation utilisateur (Synchronize obligatoire)

```pascal
procedure TMyThread.Execute;
begin
  ProcessFirstPart();

  // Demander confirmation
  FQuestion := 'Continuer le traitement ?';
  Synchronize(@AskUser);  // DOIT attendre la réponse !

  if FUserResponse then
    ProcessSecondPart();
end;
```

**Pourquoi Synchronize ?** On DOIT attendre la réponse avant de continuer.

### Exemple 3 : Logging (Queue parfait)

```pascal
procedure TMyThread.Execute;
begin
  FLog := 'Démarrage...';
  Queue(@AddLog);

  DoWork();

  FLog := 'Traitement en cours...';
  Queue(@AddLog);

  DoMoreWork();

  FLog := 'Terminé';
  Queue(@AddLog);
end;
```

**Pourquoi Queue ?** Les logs ne doivent pas ralentir le traitement.

### Exemple 4 : Mise à jour de base de données (Synchronize si nécessaire)

```pascal
procedure TMyThread.Execute;
begin
  FData := CollectData();

  // Si on doit attendre que l'UI valide les données
  Synchronize(@ValidateAndDisplay);

  if FValidated then
    SaveToDatabase(FData);
end;
```

**Pourquoi Synchronize ?** La validation doit être terminée avant la sauvegarde.

## Bonnes pratiques

### ✓ Pratique 1 : Par défaut, préférer Queue

Si vous n'avez pas de raison spécifique d'utiliser `Synchronize`, utilisez `Queue` :
- Plus rapide
- Moins de risques de deadlock
- Thread plus réactif

### ✓ Pratique 2 : Documenter votre choix

```pascal
// Utilise Queue car l'ordre n'est pas critique et on veut la performance
Queue(@UpdateStatus);

// Utilise Synchronize car on doit attendre la validation
Synchronize(@ValidateInput);
```

### ✓ Pratique 3 : Utiliser des variables distinctes avec Queue

```pascal
// Mauvais
FValeur := 100;
Queue(@Afficher);
FValeur := 200;  // Conflit !

// Bon
FValeur1 := 100;
Queue(@Afficher1);
FValeur2 := 200;
Queue(@Afficher2);
```

### ✓ Pratique 4 : Limiter la fréquence avec Queue

```pascal
// Bon : mise à jour tous les 100 items
if i mod 100 = 0 then
  Queue(@Update);

// Mauvais : mise à jour à chaque item
Queue(@Update);  // Millions de fois !
```

## Récapitulatif

| Caractéristique | Synchronize | Queue |
|-----------------|-------------|-------|
| **Attend la fin** | ✓ Oui | ✗ Non |
| **Ordre garanti** | ✓ Oui | ⚠ Limité |
| **Performance** | ⚠ Moins rapide | ✓ Plus rapide |
| **Confirmation utilisateur** | ✓ Idéal | ✗ Impossible |
| **Logs fréquents** | ⚠ Ralentit | ✓ Parfait |
| **Barre de progression** | ✓ Fonctionne | ✓ Recommandé |
| **Mise à jour critique** | ✓ Idéal | ⚠ Attention |
| **Complexité** | Simple | Simple |

### Quand utiliser quoi ?

**Synchronize** :
- Confirmation/validation utilisateur
- Ordre d'exécution critique
- Modification de variables partagées avec dépendances
- Besoin d'un résultat immédiat

**Queue** :
- Logs et traces
- Barres de progression
- Statistiques temps réel
- Toute mise à jour non critique et fréquente
- Maximiser la performance du thread

## Conclusion

`Synchronize` et `Queue` sont deux outils complémentaires, pas des alternatives directes :

- **Synchronize** = "Attends-moi, c'est important"
- **Queue** = "Fais-le quand tu peux, je continue"

La plupart du temps, vous utiliserez :
- `Queue` pour les mises à jour fréquentes (progression, logs)
- `Synchronize` pour les opérations critiques (résultats finaux, confirmations)

Le choix dépend de votre besoin spécifique. Dans le doute :
1. Si vous devez **attendre** → `Synchronize`
2. Si vous pouvez **continuer** → `Queue`

Maîtriser ces deux méthodes vous permet de créer des applications multi-threadées professionnelles, réactives et performantes !

Dans la section suivante, nous aborderons un sujet plus avancé mais crucial : la gestion des **variables partagées** et les **sections critiques**. C'est important quand plusieurs threads accèdent aux mêmes données simultanément.

⏭️ [Variables partagées et section critique](/18-introduction-pratique-multi-threading/07-variables-partagees-section-critique.md)
