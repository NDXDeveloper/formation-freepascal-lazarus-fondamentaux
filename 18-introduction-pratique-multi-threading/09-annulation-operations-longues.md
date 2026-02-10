🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.9 Annulation d'opérations longues

## Introduction

Imaginez : vous lancez un export de 10 000 enregistrements. Après 30 secondes, vous réalisez que vous avez fait une erreur dans les filtres. Que pouvez-vous faire ?

Si votre application n'offre pas de bouton "Annuler", vous êtes coincé. Vous devez attendre la fin (peut-être 10 minutes), puis recommencer. Frustrant, n'est-ce pas ?

Dans cette dernière section du chapitre sur le multi-threading, nous allons apprendre à implémenter une fonctionnalité essentielle : **permettre à l'utilisateur d'annuler une opération en cours**.

## Pourquoi l'annulation est cruciale

### Du point de vue de l'utilisateur

**Sans annulation :**
```
Utilisateur : "Oh non, je me suis trompé de fichier !"  
Application : "Désolé, attendez 5 minutes..."  
Utilisateur : 😤 [Ferme l'application de force]  
```

**Avec annulation :**
```
Utilisateur : "Oh non, je me suis trompé de fichier !"  
Utilisateur : [Clique sur Annuler]  
Application : "Opération annulée"  
Utilisateur : "Parfait, je recommence !" 😊  
```

### Scénarios d'utilisation

L'annulation est importante dans de nombreux cas :

1. **Erreur de l'utilisateur** : Mauvais fichier sélectionné, mauvais filtres
2. **Changement d'avis** : L'utilisateur n'a finalement plus besoin du résultat
3. **Opération trop longue** : L'utilisateur réalise que ça va prendre trop de temps
4. **Besoin urgent** : L'utilisateur doit faire quelque chose d'autre immédiatement
5. **Fermeture de l'application** : L'utilisateur veut quitter sans attendre

## Le mécanisme de base : Terminated

### Rappel sur Terminated

Nous avons déjà vu la propriété `Terminated` dans les sections précédentes. C'est elle qui permet l'annulation :

```pascal
property Terminated: Boolean;
```

- **False** par défaut : Le thread continue normalement
- **True** après appel à `Terminate` : Le thread devrait s'arrêter

### La méthode Terminate

Pour demander l'arrêt d'un thread, on appelle sa méthode `Terminate` :

```pascal
procedure Terminate;
```

**Important** : `Terminate` ne tue pas brutalement le thread ! Elle met juste `Terminated` à `True`. C'est au thread de vérifier cette propriété et de s'arrêter proprement.

### Le principe de base

```pascal
procedure TMyThread.Execute;  
var  
  i: Integer;
begin
  for i := 1 to 1000 do
  begin
    // Vérifier si on doit s'arrêter
    if Terminated then
      Exit;  // Sortie propre

    // Continuer le travail
    TraiterItem(i);
  end;
end;
```

C'est simple : vérifier régulièrement `Terminated` et sortir si elle vaut `True`.

## Implémentation basique avec bouton Annuler

### Le formulaire

```pascal
type
  TFormMain = class(TForm)
    ButtonStart: TButton;
    ButtonCancel: TButton;
    ProgressBar1: TProgressBar;
    LabelStatus: TLabel;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FThread: TThreadTraitement;
  end;
```

### Le thread

```pascal
type
  TThreadTraitement = class(TThread)
  private
    FProgression: Integer;
    FStatus: string;
    procedure MettreAJourUI;
  protected
    procedure Execute; override;
  end;

procedure TThreadTraitement.MettreAJourUI;  
begin  
  FormMain.ProgressBar1.Position := FProgression;
  FormMain.LabelStatus.Caption := FStatus;
end;

procedure TThreadTraitement.Execute;  
var  
  i: Integer;
begin
  for i := 1 to 100 do
  begin
    // VÉRIFICATION CRUCIALE
    if Terminated then
    begin
      FStatus := 'Opération annulée par l''utilisateur';
      Synchronize(@MettreAJourUI);
      Exit;  // Sortie immédiate
    end;

    // Traitement
    Sleep(100);
    TraiterItem(i);

    // Mise à jour de la progression
    FProgression := i;
    FStatus := Format('Traitement en cours... %d%%', [i]);
    Queue(@MettreAJourUI);
  end;

  // Traitement terminé normalement
  FStatus := 'Traitement terminé avec succès !';
  FProgression := 100;
  Synchronize(@MettreAJourUI);
end;
```

### Gestion des boutons

```pascal
procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  FThread := nil;
  ButtonCancel.Enabled := False;
end;

procedure TFormMain.ButtonStartClick(Sender: TObject);  
begin  
  // Créer et démarrer le thread
  FThread := TThreadTraitement.Create(True);
  FThread.FreeOnTerminate := False;  // On gère nous-mêmes
  FThread.Start;

  // Adapter l'interface
  ButtonStart.Enabled := False;
  ButtonCancel.Enabled := True;
  ProgressBar1.Position := 0;
  LabelStatus.Caption := 'Démarrage...';
end;

procedure TFormMain.ButtonCancelClick(Sender: TObject);  
begin  
  if Assigned(FThread) then
  begin
    // Demander l'arrêt
    FThread.Terminate;

    // Désactiver le bouton Annuler pour éviter les double-clics
    ButtonCancel.Enabled := False;
    LabelStatus.Caption := 'Annulation en cours...';

    // Attendre que le thread se termine proprement
    FThread.WaitFor;
    FThread.Free;
    FThread := nil;

    // Réactiver l'interface
    ButtonStart.Enabled := True;
  end;
end;
```

**Résultat** : Un bouton "Annuler" qui fonctionne et arrête proprement le thread.

## Le problème de WaitFor

Dans l'exemple précédent, nous avons utilisé `WaitFor` dans le bouton Annuler :

```pascal
FThread.Terminate;  
FThread.WaitFor;  // ⚠️ Bloque l'interface !  
```

**Problème** : `WaitFor` bloque le thread principal. Si le thread de travail met du temps à s'arrêter (par exemple, il est dans une opération qui dure 10 secondes), l'interface gèle pendant l'attente.

### Solution 1 : Annulation sans WaitFor (plus complexe)

```pascal
type
  TFormMain = class(TForm)
  private
    FThread: TThreadTraitement;
    procedure OnThreadTerminate(Sender: TObject);
  end;

type
  TThreadTraitement = class(TThread)
  private
    FOnFinished: TNotifyEvent;
  protected
    procedure Execute; override;
  public
    property OnFinished: TNotifyEvent read FOnFinished write FOnFinished;
  end;

procedure TThreadTraitement.Execute;  
begin  
  // ... code du thread ...

  // À la fin, notifier
  if Assigned(FOnFinished) then
    Synchronize(procedure
    begin
      FOnFinished(Self);
    end);
end;

procedure TFormMain.ButtonStartClick(Sender: TObject);  
begin  
  FThread := TThreadTraitement.Create(True);
  FThread.FreeOnTerminate := True;
  FThread.OnFinished := @OnThreadTerminate;
  FThread.Start;

  ButtonStart.Enabled := False;
  ButtonCancel.Enabled := True;
end;

procedure TFormMain.ButtonCancelClick(Sender: TObject);  
begin  
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    // Ne pas attendre, le thread se libérera automatiquement
    FThread := nil;

    ButtonCancel.Enabled := False;
    LabelStatus.Caption := 'Annulation en cours...';
  end;
end;

procedure TFormMain.OnThreadTerminate(Sender: TObject);  
begin  
  // Le thread s'est terminé
  FThread := nil;
  ButtonStart.Enabled := True;
  ButtonCancel.Enabled := False;

  if LabelStatus.Caption.Contains('Annulation') then
    LabelStatus.Caption := 'Opération annulée'
  else
    LabelStatus.Caption := 'Opération terminée';
end;
```

**Avantage** : L'interface ne gèle jamais, même pendant l'annulation.

### Solution 2 : WaitFor avec Application.ProcessMessages

Si vous devez utiliser `WaitFor`, vous pouvez faire :

```pascal
procedure TFormMain.ButtonCancelClick(Sender: TObject);  
var  
  TimeOut: TDateTime;
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    ButtonCancel.Enabled := False;
    LabelStatus.Caption := 'Annulation en cours...';

    TimeOut := Now + (5 / (24 * 60 * 60));  // 5 secondes

    // Attendre avec ProcessMessages
    while (not FThread.Finished) and (Now < TimeOut) do
    begin
      Application.ProcessMessages;
      Sleep(100);
    end;

    if FThread.Finished then
    begin
      FThread.Free;
      FThread := nil;
      ButtonStart.Enabled := True;
      LabelStatus.Caption := 'Opération annulée';
    end
    else
    begin
      // Le thread ne s'est pas arrêté dans les temps
      ShowMessage('Le thread ne répond pas. Fermeture forcée.');
      // Option dangereuse : FThread.Free quand même
    end;
  end;
end;
```

**Avantage** : L'interface reste réactive pendant l'attente.

**Inconvénient** : Plus complexe et nécessite un timeout.

## Annulation avec nettoyage des ressources

Quand votre thread utilise des ressources (fichiers, connexions, etc.), il faut les nettoyer proprement lors de l'annulation :

```pascal
procedure TThreadTraitement.Execute;  
var  
  Fichier: TextFile;
  i: Integer;
  FichierOuvert: Boolean;
begin
  FichierOuvert := False;

  try
    // Ouvrir le fichier
    AssignFile(Fichier, 'output.txt');
    Rewrite(Fichier);
    FichierOuvert := True;

    // Traitement
    for i := 1 to 1000 do
    begin
      if Terminated then
      begin
        // Annulation : nettoyer
        FStatus := 'Nettoyage après annulation...';
        Synchronize(@MettreAJourUI);
        Exit;  // Le finally s'occupera de la fermeture
      end;

      WriteLn(Fichier, Format('Ligne %d', [i]));

      if i mod 10 = 0 then
      begin
        FProgression := (i * 100) div 1000;
        FStatus := Format('Écriture ligne %d / 1000', [i]);
        Queue(@MettreAJourUI);
      end;
    end;

    FStatus := 'Traitement terminé !';
    Synchronize(@MettreAJourUI);

  finally
    // Nettoyage TOUJOURS exécuté
    if FichierOuvert then
    begin
      CloseFile(Fichier);

      // Si annulé, supprimer le fichier incomplet
      if Terminated then
        DeleteFile('output.txt');
    end;
  end;
end;
```

**Points clés** :
- Utiliser `try-finally` pour garantir le nettoyage
- Fermer les fichiers/connexions dans le `finally`
- Supprimer les fichiers incomplets si annulé
- Libérer toutes les ressources allouées

## Annulation avec confirmation

Pour des opérations critiques, demandez confirmation avant d'annuler :

```pascal
procedure TFormMain.ButtonCancelClick(Sender: TObject);  
var  
  Reponse: Integer;
begin
  if Assigned(FThread) then
  begin
    // Demander confirmation
    Reponse := MessageDlg(
      'Annuler l''opération ?',
      'Êtes-vous sûr de vouloir annuler ? Les données partielles seront perdues.',
      mtConfirmation,
      [mbYes, mbNo],
      0
    );

    if Reponse = mrYes then
    begin
      FThread.Terminate;
      ButtonCancel.Enabled := False;
      LabelStatus.Caption := 'Annulation en cours...';

      // Attendre l'arrêt
      FThread.WaitFor;
      FThread.Free;
      FThread := nil;

      ButtonStart.Enabled := True;
      LabelStatus.Caption := 'Opération annulée';
    end;
  end;
end;
```

**Usage** : Pour des opérations longues ou critiques où l'annulation a des conséquences importantes.

## Annulation "douce" vs "forcée"

### Annulation douce (recommandée)

Le thread vérifie régulièrement `Terminated` et s'arrête proprement :

```pascal
procedure TMyThread.Execute;  
begin  
  for i := 1 to 1000 do
  begin
    if Terminated then Exit;  // ✓ Sortie propre
    DoWork(i);
  end;
end;
```

**Avantage** : Propre, sûr, permet le nettoyage.

### Annulation "forcée" (dangereuse)

Dans certains langages, on peut tuer brutalement un thread. **En Pascal, cela n'existe pas de façon standard, et c'est une bonne chose !**

Certaines méthodes dangereuses existent (comme `TerminateThread` sur Windows), mais elles peuvent :
- Laisser des ressources non libérées
- Corrompre des données
- Causer des fuites mémoire
- Rendre l'application instable

**Règle d'or** : N'utilisez JAMAIS de méthode de terminaison forcée. Concevez toujours vos threads pour qu'ils puissent s'arrêter proprement.

## Fréquence de vérification de Terminated

### Trop rare

```pascal
for i := 1 to 100 do  
begin  
  TraitementTresLong(i);  // Dure 10 secondes

  if Terminated then Exit;  // ❌ Vérifié seulement toutes les 10 secondes !
end;
```

**Problème** : L'utilisateur doit attendre jusqu'à 10 secondes après avoir cliqué sur Annuler.

### Trop fréquent

```pascal
for i := 1 to 1000000 do  
begin  
  if Terminated then Exit;  // Vérifié 1 million de fois
  TraitementRapide(i);
  if Terminated then Exit;  // Encore !
  AutreTraitement(i);
  if Terminated then Exit;  // Et encore !
end;
```

**Problème** : Surcharge inutile, le code devient illisible.

### Idéal

```pascal
for i := 1 to 1000 do  
begin  
  if Terminated then Exit;  // ✓ Une fois par itération

  TraitementPartie1(i);
  TraitementPartie2(i);
  TraitementPartie3(i);
end;
```

**Règle** : Vérifier `Terminated` :
- Au début de chaque itération de boucle
- Avant chaque opération longue
- Idéalement toutes les 100-500 ms

## Erreurs courantes

### ❌ Erreur 1 : Ne jamais vérifier Terminated

```pascal
procedure TMyThread.Execute;  
begin  
  for i := 1 to 1000 do
  begin
    DoWork(i);  // Pas de vérification !
  end;
end;
```

**Conséquence** : Impossible d'annuler le thread.

**Solution** : Toujours vérifier `Terminated` dans les boucles.

### ❌ Erreur 2 : Vérifier mais ne pas sortir

```pascal
if Terminated then
  ShowMessage('Annulé');  // ❌ Mais on continue après !

DoWork();  // Exécuté même si Terminated !
```

**Conséquence** : Le thread continue malgré l'annulation.

**Solution** : Toujours utiliser `Exit` ou `Break` après la détection.

### ❌ Erreur 3 : Ne pas nettoyer les ressources

```pascal
procedure TMyThread.Execute;  
begin  
  Connexion := ConnecterBDD();

  for i := 1 to 1000 do
  begin
    if Terminated then Exit;  // ❌ Connexion non fermée !
    TraiterDonnees(i);
  end;

  Connexion.Free;  // Jamais appelé si annulé
end;
```

**Conséquence** : Fuite de ressources.

**Solution** : Utiliser `try-finally`.

```pascal
Connexion := ConnecterBDD();  
try  
  for i := 1 to 1000 do
  begin
    if Terminated then Exit;
    TraiterDonnees(i);
  end;
finally
  Connexion.Free;  // ✓ Toujours appelé
end;
```

### ❌ Erreur 4 : Double-clic sur Annuler

```pascal
procedure TFormMain.ButtonCancelClick(Sender: TObject);  
begin  
  FThread.Terminate;  // Premier clic
  // L'utilisateur reclique...
  FThread.Terminate;  // Deuxième clic : OK, pas de problème
  FThread.WaitFor;    // Mais on peut attendre deux fois !
end;
```

**Conséquence** : Comportement imprévisible.

**Solution** : Désactiver le bouton immédiatement.

```pascal
procedure TFormMain.ButtonCancelClick(Sender: TObject);  
begin  
  ButtonCancel.Enabled := False;  // ✓ Désactiver immédiatement
  FThread.Terminate;
  FThread.WaitFor;
  // ...
end;
```

### ❌ Erreur 5 : Référence à un thread libéré

```pascal
FThread.FreeOnTerminate := True;  
FThread.Start;  

// Plus tard...
FThread.Terminate;  // ❌ Peut être déjà libéré !
```

**Conséquence** : Violation d'accès.

**Solution** : Si `FreeOnTerminate = True`, ne gardez pas de référence, ou utilisez `False` et gérez manuellement.

## Annulation lors de la fermeture de l'application

Quand l'utilisateur ferme l'application, il faut arrêter proprement tous les threads :

```pascal
type
  TFormMain = class(TForm)
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FThread: TThreadTraitement;
  end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);  
begin  
  if Assigned(FThread) and not FThread.Finished then
  begin
    // Demander confirmation
    if MessageDlg(
      'Un traitement est en cours. Voulez-vous vraiment quitter ?',
      mtConfirmation,
      [mbYes, mbNo],
      0
    ) = mrYes then
    begin
      // Arrêter le thread
      FThread.Terminate;

      // Attendre un court instant
      FThread.WaitFor;
      FThread.Free;
      FThread := nil;

      CanClose := True;
    end
    else
      CanClose := False;  // Annuler la fermeture
  end
  else
    CanClose := True;
end;
```

**Important** : Toujours arrêter proprement les threads avant de fermer l'application !

## Exemple complet : téléchargement avec annulation

Voici un exemple complet montrant toutes les bonnes pratiques :

```pascal
type
  TThreadDownload = class(TThread)
  private
    FURL: string;
    FDestination: string;
    FProgression: Integer;
    FStatus: string;
    FBytesTotal: Int64;
    FBytesDownloaded: Int64;
    procedure MettreAJourUI;
  protected
    procedure Execute; override;
  public
    constructor Create(const AURL, ADest: string);
  end;

constructor TThreadDownload.Create(const AURL, ADest: string);  
begin  
  inherited Create(True);
  FURL := AURL;
  FDestination := ADest;
  FreeOnTerminate := False;
end;

procedure TThreadDownload.MettreAJourUI;  
begin  
  FormMain.ProgressBar1.Position := FProgression;
  FormMain.LabelStatus.Caption := FStatus;
end;

procedure TThreadDownload.Execute;  
var  
  HttpClient: TFPHttpClient;
  Stream: TFileStream;
  FichierCree: Boolean;
begin
  FichierCree := False;
  HttpClient := nil;
  Stream := nil;

  try
    HttpClient := TFPHttpClient.Create(nil);

    // Vérification avant de commencer
    if Terminated then Exit;

    Stream := TFileStream.Create(FDestination, fmCreate);
    FichierCree := True;

    // Callback de progression
    HttpClient.OnDataReceived := procedure(Sender: TObject;
      const ContentLength, CurrentPos: Int64)
    begin
      // Vérifier Terminated même dans le callback !
      if Terminated then
      begin
        // On ne peut pas Exit ici, on est dans un callback
        // On va juste arrêter le téléchargement
        HttpClient.Terminate;
        Exit;
      end;

      FBytesTotal := ContentLength;
      FBytesDownloaded := CurrentPos;

      if ContentLength > 0 then
      begin
        FProgression := Round((CurrentPos * 100) / ContentLength);
        FStatus := Format('Téléchargement : %d%% (%s / %s)',
          [FProgression,
           FormatByteSize(CurrentPos),
           FormatByteSize(ContentLength)]);

        if (FProgression mod 5 = 0) or (CurrentPos = ContentLength) then
          Queue(@MettreAJourUI);
      end;
    end;

    try
      // Télécharger
      FStatus := 'Connexion au serveur...';
      Queue(@MettreAJourUI);

      HttpClient.Get(FURL, Stream);

      // Vérifier si c'est une annulation ou un succès
      if not Terminated then
      begin
        FStatus := 'Téléchargement terminé avec succès !';
        FProgression := 100;
      end
      else
      begin
        FStatus := 'Téléchargement annulé';
        FProgression := 0;
      end;

      Synchronize(@MettreAJourUI);

    except
      on E: Exception do
      begin
        if not Terminated then
        begin
          FStatus := 'Erreur : ' + E.Message;
          Synchronize(@MettreAJourUI);
        end;
      end;
    end;

  finally
    // Nettoyage
    if Assigned(Stream) then
      Stream.Free;

    if Assigned(HttpClient) then
      HttpClient.Free;

    // Supprimer le fichier partiel si annulé
    if Terminated and FichierCree then
    begin
      try
        DeleteFile(FDestination);
      except
        // Ignorer les erreurs de suppression
      end;
    end;
  end;
end;

// Dans le formulaire
procedure TFormMain.ButtonDownloadClick(Sender: TObject);  
begin  
  FThread := TThreadDownload.Create(
    EditURL.Text,
    EditDestination.Text
  );
  FThread.Start;

  ButtonDownload.Enabled := False;
  ButtonCancel.Enabled := True;
end;

procedure TFormMain.ButtonCancelClick(Sender: TObject);  
begin  
  if Assigned(FThread) then
  begin
    ButtonCancel.Enabled := False;
    LabelStatus.Caption := 'Annulation en cours...';

    FThread.Terminate;
    FThread.WaitFor;
    FThread.Free;
    FThread := nil;

    ButtonDownload.Enabled := True;
    LabelStatus.Caption := 'Téléchargement annulé';
  end;
end;
```

## Bonnes pratiques récapitulatives

### ✓ Pratique 1 : Toujours vérifier Terminated

```pascal
// Dans chaque boucle
for i := 1 to Max do  
begin  
  if Terminated then Exit;
  DoWork(i);
end;

// Avant chaque opération longue
if Terminated then Exit;  
LongOperation();  
```

### ✓ Pratique 2 : Utiliser try-finally

```pascal
Resource := CreateResource();  
try  
  // Travail
  for i := 1 to 100 do
  begin
    if Terminated then Exit;
    UseResource(Resource, i);
  end;
finally
  Resource.Free;  // Toujours appelé
end;
```

### ✓ Pratique 3 : Nettoyer les données partielles

```pascal
if Terminated then  
begin  
  DeleteFile(PartialFile);
  RollbackTransaction();
  CleanupTemporaryData();
end;
```

### ✓ Pratique 4 : Désactiver le bouton Annuler immédiatement

```pascal
ButtonCancel.Enabled := False;  // Première chose !  
FThread.Terminate();  
```

### ✓ Pratique 5 : Informer l'utilisateur

```pascal
if Terminated then  
begin  
  FStatus := 'Opération annulée par l''utilisateur';
  Synchronize(@MettreAJourUI);
end;
```

### ✓ Pratique 6 : Gérer la fermeture de l'application

```pascal
procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);  
begin  
  if ThreadEnCours then
  begin
    Thread.Terminate;
    Thread.WaitFor;
    Thread.Free;
  end;
  CanClose := True;
end;
```

## Récapitulatif

### Mécanisme de base

- Propriété `Terminated` : indique si l'arrêt est demandé
- Méthode `Terminate` : met `Terminated` à `True`
- Le thread DOIT vérifier `Terminated` régulièrement
- Sortir avec `Exit` ou `Break` quand `Terminated = True`

### Points clés

1. **Vérification fréquente** : Au moins une fois par itération de boucle
2. **Nettoyage obligatoire** : Utiliser `try-finally` pour les ressources
3. **Feedback utilisateur** : Informer que l'annulation est en cours
4. **Désactivation du bouton** : Éviter les double-clics
5. **Gestion de la fermeture** : Arrêter les threads avant de quitter

### Erreurs à éviter

1. Ne jamais vérifier `Terminated`
2. Vérifier mais ne pas sortir
3. Ne pas nettoyer les ressources
4. Permettre les double-clics sur Annuler
5. Référencer un thread après FreeOnTerminate

### Trois approches pour WaitFor

1. **Simple** : `WaitFor` direct (bloque l'interface temporairement)
2. **Avec ProcessMessages** : Boucle avec `Application.ProcessMessages`
3. **Événement** : Callback quand le thread se termine (pas de blocage)

## Conclusion

L'annulation d'opérations longues est une fonctionnalité essentielle pour une application professionnelle. Elle transforme l'expérience utilisateur en donnant le contrôle à l'utilisateur.

**Les points essentiels à retenir :**

1. Utilisez la propriété `Terminated` et la méthode `Terminate`
2. Vérifiez `Terminated` régulièrement dans vos boucles
3. Nettoyez toujours les ressources avec `try-finally`
4. Informez l'utilisateur de l'état de l'annulation
5. Gérez proprement la fermeture de l'application

**Avec cette section, vous avez maintenant toutes les clés pour créer des applications multi-threadées complètes et professionnelles !**

Vous savez :
- Créer des threads qui ne bloquent pas l'interface
- Communiquer en toute sécurité avec l'UI
- Gérer les variables partagées
- Afficher des progressions et feedbacks
- Permettre l'annulation d'opérations

Le multi-threading n'est plus un mystère pour vous. Vous êtes maintenant capable de créer des applications modernes, réactives et agréables à utiliser. Bravo d'être arrivé jusqu'ici !

**La pratique est maintenant votre meilleure amie.** Commencez par de petits projets, ajoutez progressivement de la complexité, et vous maîtriserez bientôt le multi-threading comme un professionnel !

⏭️ [Développement Multi-plateforme en Pratique](/19-developpement-multi-plateforme-pratique/README.md)
