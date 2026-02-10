🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.5 TThread.Synchronize : communication thread-UI

## Introduction

Nous avons appris à créer des threads qui s'exécutent en parallèle. Mais il y a un problème majeur : comment faire pour que votre thread affiche quelque chose à l'écran ? Comment mettre à jour une barre de progression, changer le texte d'un label, ou afficher un résultat ?

Dans cette section, nous allons découvrir **la règle d'or du multi-threading graphique** et la solution pour communiquer en toute sécurité entre un thread de travail et l'interface utilisateur.

## La règle d'or : UN SEUL thread pour l'interface

### La règle absolue

**Seul le thread principal peut accéder aux composants visuels (TForm, TButton, TLabel, etc.).**

C'est une règle **absolue** imposée par tous les systèmes d'exploitation modernes : Windows, Linux, macOS. Vous ne pouvez pas la contourner.

### Pourquoi cette règle existe-t-elle ?

Les bibliothèques graphiques (LCL, VCL, GTK, Qt, Win32 API) ne sont **pas thread-safe**. Cela signifie qu'elles n'ont pas été conçues pour être accédées simultanément par plusieurs threads.

**Analogie : la cuisine**

Imaginez une cuisine avec un seul four. Si deux cuisiniers essaient d'ouvrir la porte du four en même temps, de changer la température, de sortir des plats simultanément, c'est le chaos !

De même, si deux threads tentent de modifier un `TLabel` en même temps, les résultats sont imprévisibles : affichage corrompu, crash, ou pire encore, un comportement aléatoire qui ne se reproduit pas toujours.

### Que se passe-t-il si vous enfreignez cette règle ?

```pascal
// CODE DANGEREUX - NE PAS FAIRE !
procedure TMyThread.Execute;  
begin  
  Label1.Caption := 'Bonjour du thread !';  // ❌ INTERDIT !
end;
```

**Conséquences possibles :**
- Crash immédiat de l'application
- Affichage corrompu
- Comportement erratique qui apparaît aléatoirement
- Plantage après plusieurs minutes d'utilisation
- "Ça marche sur mon PC" mais pas sur celui de l'utilisateur

C'est pour cela qu'il faut une solution : **`Synchronize`** !

## La solution : TThread.Synchronize

### Qu'est-ce que Synchronize ?

`Synchronize` est une méthode de la classe `TThread` qui permet à un thread de travail de **demander au thread principal d'exécuter du code à sa place**.

C'est comme envoyer un message : "Hé, thread principal, peux-tu mettre à jour ce Label pour moi ?"

### Comment ça fonctionne ?

```
THREAD DE TRAVAIL              THREAD PRINCIPAL
     |                              |
     | Je fais des calculs...       | Je gère l'interface
     |                              |
     | J'ai besoin d'afficher       |
     | un résultat !                |
     |                              |
     | Synchronize(MaMethode)       |
     |----------------------------->|
     |                              |
     | [Je m'arrête et j'attends]   | [J'exécute MaMethode]
     |                              | Label1.Caption := 'Ok!'
     |                              |
     |<-----------------------------|
     | [Je reprends mon travail]    |
     |                              |
```

**Points clés :**
1. Le thread de travail **s'arrête** pendant que Synchronize s'exécute
2. Le code passé à Synchronize s'exécute dans le **thread principal**
3. Une fois terminé, le thread de travail **reprend** son exécution

### Analogie : le serveur et le barman

Imaginez :
- **Thread de travail** = Un serveur qui prend les commandes dans la salle
- **Thread principal** = Le barman derrière le comptoir

Le serveur ne peut pas aller derrière le bar pour préparer les boissons lui-même (ce serait dangereux). Il doit demander au barman :

1. Le serveur appelle : "Un café, s'il te plaît !"
2. Le serveur attend que le barman prépare le café
3. Le barman prépare le café
4. Le serveur récupère le café et continue son service

C'est exactement le rôle de `Synchronize` !

## Syntaxe de Synchronize

### Forme de base

```pascal
Synchronize(@NomDeLaMethode);
```

Vous passez l'adresse d'une méthode (avec `@`) qui sera exécutée dans le thread principal.

### Structure complète d'un exemple

```pascal
type
  TMyThread = class(TThread)
  private
    FMessage: string;
    procedure AfficherMessage;  // Méthode qui sera synchronisée
  protected
    procedure Execute; override;
  end;

procedure TMyThread.AfficherMessage;  
begin  
  // CE CODE S'EXÉCUTE DANS LE THREAD PRINCIPAL
  // On peut accéder aux composants visuels en sécurité !
  Form1.Label1.Caption := FMessage;
end;

procedure TMyThread.Execute;  
begin  
  // Ce code s'exécute dans le thread de travail

  // Faire du travail...
  Sleep(1000);

  // Préparer le message
  FMessage := 'Traitement terminé !';

  // Demander au thread principal de l'afficher
  Synchronize(@AfficherMessage);

  // Continue le travail...
end;
```

### Points importants

1. **La méthode synchronisée doit être une méthode de la classe thread** (pas une procédure globale)
2. **Utilisez des variables membres** pour passer des données (comme `FMessage`)
3. **La méthode synchronisée s'exécute dans le thread principal**, elle peut donc accéder aux composants visuels

## Exemple complet : thread avec barre de progression

Créons un exemple concret qui affiche la progression d'un traitement long.

### Le formulaire

```pascal
type
  TFormMain = class(TForm)
    ButtonStart: TButton;
    ProgressBar1: TProgressBar;
    LabelStatus: TLabel;
    procedure ButtonStartClick(Sender: TObject);
  end;
```

### La classe thread

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
  // S'exécute dans le thread principal - SÉCURISÉ
  FormMain.ProgressBar1.Position := FProgression;
  FormMain.LabelStatus.Caption := FStatus;
end;

procedure TThreadTraitement.Execute;  
var  
  i: Integer;
begin
  // S'exécute dans le thread de travail
  for i := 1 to 100 do
  begin
    if Terminated then
      Break;

    // Simuler un traitement long
    Sleep(50);

    // Préparer les données à afficher
    FProgression := i;
    FStatus := Format('Traitement en cours... %d%%', [i]);

    // Demander la mise à jour de l'interface
    Synchronize(@MettreAJourUI);
  end;

  // Afficher le message final
  FStatus := 'Traitement terminé !';
  FProgression := 100;
  Synchronize(@MettreAJourUI);
end;
```

### Utilisation

```pascal
procedure TFormMain.ButtonStartClick(Sender: TObject);  
var  
  Thread: TThreadTraitement;
begin
  ProgressBar1.Position := 0;
  ButtonStart.Enabled := False;

  Thread := TThreadTraitement.Create(True);
  Thread.FreeOnTerminate := True;
  Thread.Start;
end;
```

### Ce qui se passe

1. L'utilisateur clique sur "Start"
2. Le thread démarre et commence sa boucle
3. À chaque itération :
   - Le thread fait son travail (Sleep)
   - Il met à jour `FProgression` et `FStatus`
   - Il appelle `Synchronize(@MettreAJourUI)`
   - Le thread **s'arrête temporairement**
   - Le thread principal exécute `MettreAJourUI`
   - L'interface est mise à jour
   - Le thread de travail reprend
4. Résultat : Une barre de progression fluide et un label mis à jour !

## Passer des données avec Synchronize

### Le problème

Comment passer des valeurs à la méthode synchronisée ?

**❌ Mauvaise approche : paramètres**

```pascal
procedure TMyThread.AfficherTexte(const Texte: string);  // ❌ Ne compile pas !  
begin  
  Label1.Caption := Texte;
end;

// Dans Execute :
Synchronize(@AfficherTexte);  // ❌ Comment passer le paramètre ?
```

**Problème** : `Synchronize` prend une méthode sans paramètre. On ne peut pas passer d'arguments.

### ✓ La bonne approche : variables membres

```pascal
type
  TMyThread = class(TThread)
  private
    FTexteAfficher: string;  // Variable membre
    FValeurAfficher: Integer;
    procedure AfficherDonnees;
  protected
    procedure Execute; override;
  end;

procedure TMyThread.AfficherDonnees;  
begin  
  // Utiliser les variables membres
  Label1.Caption := FTexteAfficher;
  Edit1.Text := IntToStr(FValeurAfficher);
end;

procedure TMyThread.Execute;  
begin  
  // Préparer les données
  FTexteAfficher := 'Résultat du calcul';
  FValeurAfficher := 42;

  // Synchroniser
  Synchronize(@AfficherDonnees);
end;
```

**Avantages :**
- Simple et clair
- Pas de limitation sur le nombre de valeurs
- Type-safe

## Méthodes anonymes avec Synchronize (FreePascal 3.2+)

À partir de FreePascal 3.2, vous pouvez utiliser des procédures anonymes avec `Synchronize`, ce qui est plus concis :

```pascal
procedure TMyThread.Execute;  
var  
  Resultat: Integer;
begin
  // Faire des calculs
  Resultat := CalculComplexe();

  // Synchroniser avec une procédure anonyme
  Synchronize(procedure
  begin
    Label1.Caption := 'Résultat : ' + IntToStr(Resultat);
    ProgressBar1.Position := 100;
  end);
end;
```

**Avantages :**
- Code plus court
- Pas besoin de créer une méthode séparée
- Les variables locales sont capturées automatiquement

**Inconvénients :**
- Nécessite FreePascal 3.2 ou plus récent
- Peut rendre le code moins lisible si la procédure est longue

## Erreurs courantes avec Synchronize

### ❌ Erreur 1 : Accès direct aux composants sans Synchronize

```pascal
procedure TMyThread.Execute;  
begin  
  // DANGER ! Accès direct sans Synchronize
  Label1.Caption := 'Bonjour';  // ❌ CRASH probable
end;
```

**Solution :** Toujours utiliser Synchronize.

### ❌ Erreur 2 : Synchronize dans le thread principal

```pascal
procedure TFormMain.ButtonClick(Sender: TObject);  
begin  
  // Pas dans un thread, pourtant on synchronise ?
  Synchronize(@MaMethode);  // ❌ Erreur ou comportement bizarre
end;
```

**Problème :** Synchronize n'a de sens que dans un thread de travail.

**Solution :** Appelez directement votre méthode si vous êtes déjà dans le thread principal.

### ❌ Erreur 3 : Synchronize trop fréquent

```pascal
procedure TMyThread.Execute;  
var  
  i: Integer;
begin
  for i := 1 to 1000000 do
  begin
    FValeur := i;
    Synchronize(@MettreAJourUI);  // ❌ 1 million de fois !
  end;
end;
```

**Problème :** Synchronize a un coût. L'appeler un million de fois ralentit tout.

**Solution :** Grouper les mises à jour.

```pascal
procedure TMyThread.Execute;  
var  
  i: Integer;
begin
  for i := 1 to 1000000 do
  begin
    DoWork(i);

    // Mettre à jour seulement tous les 1000 itérations
    if i mod 1000 = 0 then
    begin
      FValeur := i;
      Synchronize(@MettreAJourUI);
    end;
  end;
end;
```

### ❌ Erreur 4 : Oublier que Synchronize bloque

```pascal
procedure TMyThread.Execute;  
begin  
  Synchronize(@MethodeQuiPrendDuTemps);  // Si cette méthode prend 5 secondes...
  // Le thread attend 5 secondes !
end;

procedure TMyThread.MethodeQuiPrendDuTemps;  
begin  
  // Traitement long de 5 secondes
  for i := 1 to 1000000 do
    DoSomething;
end;
```

**Problème :** Le but du thread est d'éviter de bloquer. Si la méthode synchronisée fait du travail lourd, on perd l'intérêt du thread !

**Solution :** La méthode synchronisée doit être **rapide** : juste mettre à jour l'interface, rien d'autre.

### ❌ Erreur 5 : Variables locales dans Execute

```pascal
procedure TMyThread.Execute;  
var  
  Resultat: string;  // Variable locale
begin
  Resultat := 'Test';

  Synchronize(@procedure
  begin
    Label1.Caption := Resultat;  // ❌ Risqué avec méthodes normales
  end);
end;
```

**Avec méthodes anonymes (FreePascal 3.2+)** : OK, les variables sont capturées.

**Avec méthodes classiques** : ❌ La variable locale peut ne plus exister !

**Solution sûre :** Toujours utiliser des variables membres.

## Exemple complet : téléchargement avec progression

Voici un exemple réaliste qui combine tous les concepts :

```pascal
type
  TThreadDownload = class(TThread)
  private
    FURL: string;
    FDestination: string;
    FBytesDownloaded: Int64;
    FBytesTotal: Int64;
    FPourcentage: Integer;
    procedure UpdateProgress;
    procedure DownloadComplete;
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
  FreeOnTerminate := True;
end;

procedure TThreadDownload.UpdateProgress;  
begin  
  // S'exécute dans le thread principal
  FormMain.ProgressBar1.Max := 100;
  FormMain.ProgressBar1.Position := FPourcentage;
  FormMain.LabelStatus.Caption := Format(
    'Téléchargé : %d / %d octets (%d%%)',
    [FBytesDownloaded, FBytesTotal, FPourcentage]
  );
end;

procedure TThreadDownload.DownloadComplete;  
begin  
  // S'exécute dans le thread principal
  FormMain.LabelStatus.Caption := 'Téléchargement terminé !';
  FormMain.ButtonDownload.Enabled := True;
  ShowMessage('Fichier téléchargé avec succès !');
end;

procedure TThreadDownload.Execute;  
var  
  HttpClient: TFPHttpClient;
  Stream: TFileStream;
begin
  HttpClient := TFPHttpClient.Create(nil);
  Stream := TFileStream.Create(FDestination, fmCreate);
  try
    // Définir un callback pour la progression
    HttpClient.OnDataReceived := procedure(Sender: TObject; const ContentLength, CurrentPos: Int64)
    begin
      if Terminated then
        Exit;

      // Mettre à jour les variables
      FBytesTotal := ContentLength;
      FBytesDownloaded := CurrentPos;

      if ContentLength > 0 then
        FPourcentage := Round((CurrentPos * 100) / ContentLength);

      // Synchroniser seulement tous les 5% pour ne pas surcharger
      if (FPourcentage mod 5 = 0) or (CurrentPos = ContentLength) then
        Synchronize(@UpdateProgress);
    end;

    // Télécharger
    HttpClient.Get(FURL, Stream);

    // Notifier la fin
    Synchronize(@DownloadComplete);

  finally
    Stream.Free;
    HttpClient.Free;
  end;
end;

// Utilisation
procedure TFormMain.ButtonDownloadClick(Sender: TObject);  
var  
  Thread: TThreadDownload;
begin
  ButtonDownload.Enabled := False;
  Thread := TThreadDownload.Create(
    'http://example.com/fichier.zip',
    'fichier.zip'
  );
  Thread.Start;
end;
```

## Quand utiliser Synchronize ?

### ✓ Utiliser Synchronize pour :

- Mettre à jour des composants visuels (Labels, ProgressBar, etc.)
- Afficher des messages (ShowMessage, MessageDlg)
- Ajouter des lignes à un Memo ou ListBox
- Modifier le titre d'une fenêtre
- Activer/Désactiver des boutons
- Tout ce qui touche à l'interface graphique

### ✗ NE PAS utiliser Synchronize pour :

- Des calculs longs (faites-les dans le thread !)
- Lire/Écrire des fichiers (sauf si c'est pour l'UI)
- Accéder à une base de données (sauf si vraiment nécessaire)
- Tout traitement qui peut être fait dans le thread

**Règle** : Gardez les méthodes synchronisées **rapides et légères**. Elles ne doivent faire que de la mise à jour d'interface.

## Bonnes pratiques

### ✓ Pratique 1 : Nommer clairement les méthodes

```pascal
procedure UpdateProgressBar;  // Clair  
procedure MAJ_UI;             // Moins clair  
procedure DoIt;               // Pas clair du tout  
```

### ✓ Pratique 2 : Grouper les mises à jour

Au lieu de synchroniser 10 fois pour mettre à jour 10 composants, faites-le en une seule fois :

```pascal
procedure TMyThread.UpdateAllUI;  
begin  
  Label1.Caption := FMessage;
  ProgressBar1.Position := FProgress;
  Edit1.Text := FResult;
  ButtonStart.Enabled := FButtonEnabled;
end;
```

### ✓ Pratique 3 : Limiter la fréquence

```pascal
// Bon : mise à jour raisonnable
if (i mod 100 = 0) or (i = MaxValue) then
  Synchronize(@UpdateProgress);

// Mauvais : mise à jour à chaque itération
Synchronize(@UpdateProgress);
```

### ✓ Pratique 4 : Vérifier Terminated avant Synchronize

```pascal
if not Terminated then
  Synchronize(@UpdateUI);
```

Évite des synchronisations inutiles si le thread doit s'arrêter.

## Récapitulatif

### La règle d'or
**Seul le thread principal peut accéder à l'interface graphique.**

### Synchronize permet
- D'exécuter du code dans le thread principal
- Depuis un thread de travail
- De manière sûre et thread-safe

### Syntaxe
```pascal
Synchronize(@NomMethode);
// ou avec procédure anonyme (FPC 3.2+)
Synchronize(procedure begin ... end);
```

### Points clés
- Le thread de travail **s'arrête** pendant Synchronize
- La méthode s'exécute dans le **thread principal**
- Utilisez des **variables membres** pour passer des données
- Gardez les méthodes synchronisées **rapides**
- Ne synchronisez **pas trop souvent**

### Quand l'utiliser
- Pour toute modification de l'interface graphique
- ShowMessage, MessageDlg
- Mise à jour de composants visuels

## Conclusion

`Synchronize` est votre outil principal pour communiquer entre un thread de travail et l'interface graphique. C'est simple, sûr et efficace quand c'est bien utilisé.

Les points essentiels à retenir :
1. Toujours utiliser Synchronize pour accéder à l'UI
2. Garder les méthodes synchronisées rapides
3. Utiliser des variables membres pour passer les données
4. Ne pas abuser : limiter la fréquence des appels

Dans la section suivante, nous verrons **`Queue`**, une alternative à `Synchronize` qui ne bloque pas le thread de travail. Cela ouvre de nouvelles possibilités !

Mais avant de continuer, assurez-vous de bien maîtriser `Synchronize`. C'est la base de toute communication thread-UI et vous l'utiliserez dans la majorité de vos applications multi-threadées.

⏭️ [TThread.Queue vs Synchronize](/18-introduction-pratique-multi-threading/06-tthread-queue-vs-synchronize.md)
