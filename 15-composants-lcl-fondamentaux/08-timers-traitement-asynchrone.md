🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.8 Timers et traitement asynchrone

## Introduction

Jusqu'à présent, nos programmes réagissaient uniquement aux **actions de l'utilisateur** : clic sur un bouton, saisie dans un champ, etc. Mais que faire si nous voulons qu'une action se produise **automatiquement** à intervalles réguliers, ou que nous voulons exécuter une **tâche longue** sans bloquer l'interface ?

C'est là qu'interviennent :
- **Les Timers** : pour déclencher des actions périodiques
- **Le traitement asynchrone** : pour éviter le gel de l'interface

Dans ce chapitre, nous allons découvrir comment créer des applications dynamiques et réactives.

---

## Qu'est-ce qu'un Timer ?

### Concept

Un **Timer** (minuteur) est un composant qui déclenche un événement à **intervalles réguliers**, comme un métronome ou une alarme qui sonne périodiquement.

### Analogie

Imaginez un Timer comme :
- **Une horloge à sonnerie** : toutes les heures, elle sonne
- **Un métronome** : il bat la mesure à intervalle constant
- **Un réveil répétitif** : il sonne tous les jours à 7h

### Utilisations Courantes

✅ **Horloges et chronomètres** : afficher l'heure en temps réel  
✅ **Animations** : déplacer des objets, faire clignoter  
✅ **Surveillance** : vérifier périodiquement un état  
✅ **Timeout** : déclencher une action après un délai  
✅ **Auto-sauvegarde** : sauvegarder automatiquement  
✅ **Rafraîchissement** : mettre à jour des données périodiquement

---

## TTimer : Le Composant Timer

### Présentation

`TTimer` est un composant **non-visuel** qui déclenche l'événement `OnTimer` à intervalles réguliers définis.

### Hiérarchie

```
TObject
  └─ TPersistent
       └─ TComponent
            └─ TCustomTimer
                 └─ TTimer
```

**Note** : TTimer est un composant **non-visuel**, il n'apparaît pas à l'exécution (seulement dans l'IDE en mode design).

### Propriétés Principales

```pascal
property Enabled: Boolean;     // Active/désactive le timer
property Interval: Cardinal;   // Intervalle en millisecondes (ms)
property OnTimer: TNotifyEvent; // Événement déclenché périodiquement
```

### La Propriété Interval

`Interval` définit le **délai entre deux déclenchements** en **millisecondes**.

**Conversions utiles :**

```
1 seconde = 1000 millisecondes
1 minute = 60000 millisecondes
1 heure = 3600000 millisecondes

Exemples:
100 ms = 0.1 seconde (10 fois par seconde)
500 ms = 0.5 seconde (2 fois par seconde)
1000 ms = 1 seconde
5000 ms = 5 secondes
```

### Exemple de Base : Horloge

```pascal
type
  TForm1 = class(TForm)
    Timer1: TTimer;
    LabelHeure: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration du timer
  Timer1.Interval := 1000;  // 1 seconde
  Timer1.Enabled := True;   // Activer le timer
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // Cet événement est déclenché toutes les 1000 ms (1 seconde)
  LabelHeure.Caption := TimeToStr(Now);
end;
```

**Résultat** : L'heure affichée se met à jour chaque seconde automatiquement.

### Activer/Désactiver le Timer

```pascal
// Démarrer le timer
Timer1.Enabled := True;

// Arrêter le timer
Timer1.Enabled := False;

// Basculer (toggle)
Timer1.Enabled := not Timer1.Enabled;
```

### Changer l'Intervalle

```pascal
// Timer rapide (10 fois par seconde)
Timer1.Interval := 100;

// Timer lent (toutes les 5 secondes)
Timer1.Interval := 5000;

// Timer très lent (toutes les minutes)
Timer1.Interval := 60000;
```

---

## Exemples Pratiques de Timers

### Exemple 1 : Chronomètre

```pascal
type
  TForm1 = class(TForm)
    Timer1: TTimer;
    LabelTemps: TLabel;
    BtnStart: TButton;
    BtnStop: TButton;
    BtnReset: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure BtnResetClick(Sender: TObject);
  private
    FSecondesEcoulees: Integer;
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Timer1.Interval := 1000;  // 1 seconde
  Timer1.Enabled := False;  // Pas actif au départ
  FSecondesEcoulees := 0;
  LabelTemps.Caption := '00:00:00';
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  Heures, Minutes, Secondes: Integer;
begin
  Inc(FSecondesEcoulees);

  // Calculer heures, minutes, secondes
  Heures := FSecondesEcoulees div 3600;
  Minutes := (FSecondesEcoulees mod 3600) div 60;
  Secondes := FSecondesEcoulees mod 60;

  // Afficher
  LabelTemps.Caption := Format('%2.2d:%2.2d:%2.2d', [Heures, Minutes, Secondes]);
end;

procedure TForm1.BtnStartClick(Sender: TObject);
begin
  Timer1.Enabled := True;
  BtnStart.Enabled := False;
  BtnStop.Enabled := True;
end;

procedure TForm1.BtnStopClick(Sender: TObject);
begin
  Timer1.Enabled := False;
  BtnStart.Enabled := True;
  BtnStop.Enabled := False;
end;

procedure TForm1.BtnResetClick(Sender: TObject);
begin
  Timer1.Enabled := False;
  FSecondesEcoulees := 0;
  LabelTemps.Caption := '00:00:00';
  BtnStart.Enabled := True;
  BtnStop.Enabled := False;
end;
```

### Exemple 2 : Animation Simple (Déplacement)

```pascal
type
  TForm1 = class(TForm)
    Timer1: TTimer;
    Shape1: TShape;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FDirection: Integer;  // 1 = droite, -1 = gauche
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration du timer
  Timer1.Interval := 20;  // 50 fois par seconde (fluide)
  Timer1.Enabled := True;

  // Configuration du Shape
  Shape1.Shape := stCircle;
  Shape1.Width := 50;
  Shape1.Height := 50;
  Shape1.Brush.Color := clRed;

  FDirection := 1;  // Vers la droite
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // Déplacer le cercle
  Shape1.Left := Shape1.Left + (5 * FDirection);

  // Rebondir sur les bords
  if Shape1.Left + Shape1.Width >= ClientWidth then
    FDirection := -1  // Vers la gauche
  else if Shape1.Left <= 0 then
    FDirection := 1;  // Vers la droite
end;
```

### Exemple 3 : Clignotement

```pascal
type
  TForm1 = class(TForm)
    Timer1: TTimer;
    LabelAvertissement: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Timer1.Interval := 500;  // 0.5 seconde
  Timer1.Enabled := True;

  LabelAvertissement.Caption := 'ATTENTION !';
  LabelAvertissement.Font.Size := 20;
  LabelAvertissement.Font.Style := [fsBold];
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // Alterner la visibilité
  LabelAvertissement.Visible := not LabelAvertissement.Visible;
end;
```

### Exemple 4 : Barre de Progression Automatique

```pascal
type
  TForm1 = class(TForm)
    Timer1: TTimer;
    ProgressBar1: TProgressBar;
    LabelProgression: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ProgressBar1.Min := 0;
  ProgressBar1.Max := 100;
  ProgressBar1.Position := 0;

  Timer1.Interval := 100;  // Mise à jour tous les 100 ms
  Timer1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // Incrémenter
  ProgressBar1.Position := ProgressBar1.Position + 1;

  LabelProgression.Caption := IntToStr(ProgressBar1.Position) + '%';

  // Arrêter quand terminé
  if ProgressBar1.Position >= 100 then
  begin
    Timer1.Enabled := False;
    ShowMessage('Terminé !');
  end;
end;
```

### Exemple 5 : Auto-sauvegarde

```pascal
type
  TForm1 = class(TForm)
    Timer1: TTimer;
    Memo1: TMemo;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure SauvegarderDocument;
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Sauvegarder toutes les 5 minutes
  Timer1.Interval := 300000;  // 5 minutes = 300 000 ms
  Timer1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  SauvegarderDocument;
  StatusBar1.SimpleText := 'Sauvegarde automatique : ' + TimeToStr(Now);
end;

procedure TForm1.SauvegarderDocument;
begin
  // Sauvegarder le contenu du Memo
  if Memo1.Lines.Count > 0 then
    Memo1.Lines.SaveToFile('autosave.txt');
end;
```

---

## Traitement Asynchrone : Le Problème du Gel

### Le Problème

Lorsque votre code exécute une **opération longue** (calcul complexe, téléchargement, traitement de fichier), l'interface utilisateur **se fige** (freeze) pendant toute la durée de l'opération.

**Exemple de code qui gèle l'interface :**

```pascal
procedure TForm1.BtnCalculerClick(Sender: TObject);
var
  i: Integer;
  Resultat: Double;
begin
  Resultat := 0;

  // Opération très longue (plusieurs secondes)
  for i := 1 to 100000000 do
    Resultat := Resultat + Sqrt(i);

  Label1.Caption := FloatToStr(Resultat);

  // Pendant toute cette boucle, l'interface est gelée !
  // L'utilisateur ne peut rien faire, le curseur devient une horloge
end;
```

**Conséquences :**
- ❌ L'utilisateur ne peut plus cliquer sur rien
- ❌ La fenêtre ne se redessine pas
- ❌ Impression que l'application a planté
- ❌ Impossible d'annuler l'opération

### La Cause

Les applications graphiques fonctionnent avec une **boucle de messages** (event loop). Cette boucle traite les événements : clics de souris, frappes clavier, demandes de rafraîchissement, etc.

Quand votre code monopolise le processeur pendant longtemps, **la boucle de messages ne peut plus tourner**, et l'interface ne répond plus.

### Solution 1 : Application.ProcessMessages

La méthode `Application.ProcessMessages` permet de **traiter temporairement les messages** en attente, donnant l'impression que l'application répond.

```pascal
procedure TForm1.BtnCalculerClick(Sender: TObject);
var
  i: Integer;
  Resultat: Double;
begin
  Resultat := 0;
  ProgressBar1.Max := 100000000;
  ProgressBar1.Position := 0;

  for i := 1 to 100000000 do
  begin
    Resultat := Resultat + Sqrt(i);

    // Mettre à jour l'interface tous les 100000 itérations
    if (i mod 100000) = 0 then
    begin
      ProgressBar1.Position := i;
      Application.ProcessMessages;  // Permet de traiter les événements
    end;
  end;

  Label1.Caption := FloatToStr(Resultat);
end;
```

**Avantages :**
- ✅ L'interface reste réactive
- ✅ Simple à implémenter
- ✅ Peut mettre à jour une barre de progression

**Inconvénients :**
- ⚠️ Ralentit légèrement le traitement
- ⚠️ L'opération reste sur le thread principal
- ⚠️ Peut compliquer la gestion si l'utilisateur clique pendant le traitement

### Solution 2 : Utiliser un Timer pour Découper le Travail

Au lieu de tout faire d'un coup, on peut **découper le travail** en petites portions et laisser le Timer orchestrer.

```pascal
type
  TForm1 = class(TForm)
    Timer1: TTimer;
    BtnDemarrer: TButton;
    LabelResultat: TLabel;
    ProgressBar1: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure BtnDemarrerClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FIndexCourant: Integer;
    FResultat: Double;
    FTraitementEnCours: Boolean;
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Timer1.Interval := 10;  // 10 ms
  Timer1.Enabled := False;
  FTraitementEnCours := False;
end;

procedure TForm1.BtnDemarrerClick(Sender: TObject);
begin
  if not FTraitementEnCours then
  begin
    FIndexCourant := 1;
    FResultat := 0;
    FTraitementEnCours := True;
    ProgressBar1.Max := 100000;
    ProgressBar1.Position := 0;
    Timer1.Enabled := True;
    BtnDemarrer.Enabled := False;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  i: Integer;
const
  PORTION = 1000;  // Traiter 1000 éléments à chaque tick
begin
  // Traiter une portion
  for i := 1 to PORTION do
  begin
    FResultat := FResultat + Sqrt(FIndexCourant);
    Inc(FIndexCourant);

    if FIndexCourant > 100000 then
    begin
      // Terminé !
      Timer1.Enabled := False;
      FTraitementEnCours := False;
      LabelResultat.Caption := 'Résultat : ' + FloatToStr(FResultat);
      BtnDemarrer.Enabled := True;
      Exit;
    end;
  end;

  // Mettre à jour la progression
  ProgressBar1.Position := FIndexCourant;
end;
```

**Avantages :**
- ✅ Interface reste totalement réactive
- ✅ Peut annuler facilement (désactiver le Timer)
- ✅ Barre de progression fluide

**Inconvénients :**
- ⚠️ Plus long que le traitement d'un seul bloc
- ⚠️ Plus complexe à coder

---

## Synchrone vs Asynchrone

### Traitement Synchrone (par défaut)

**Synchrone** = les opérations s'exécutent **l'une après l'autre**, dans l'ordre.

```pascal
procedure TForm1.BtnAction1Click(Sender: TObject);
begin
  ShowMessage('Opération 1');  // S'exécute en premier
  ShowMessage('Opération 2');  // S'exécute ensuite
  ShowMessage('Opération 3');  // S'exécute à la fin
end;
```

**Schéma :**
```
Temps →
[Op1]─────[Op2]─────[Op3]─────
     ↑         ↑         ↑
   Attend   Attend   Attend
```

### Traitement Asynchrone

**Asynchrone** = les opérations peuvent s'exécuter **en parallèle** ou **être différées**.

```pascal
// Opération lancée, mais on ne l'attend pas
Timer1.Enabled := True;  // Le timer va travailler en arrière-plan
ShowMessage('Autre chose');  // S'exécute immédiatement
```

**Schéma :**
```
Temps →
[Op1]──────────────────────────
[Op2]──────────────────────────  ← En parallèle
[Op3]──────────────────────────
```

### Analogie

**Synchrone** = Faire la vaisselle, puis passer l'aspirateur, puis faire le lit.  
**Asynchrone** = Lancer la machine à laver (qui tourne seule), pendant ce temps passer l'aspirateur.  

---

## Introduction aux Threads (Niveau Débutant)

### Qu'est-ce qu'un Thread ?

Un **thread** (fil d'exécution) est une tâche qui s'exécute **en parallèle** du programme principal. C'est comme avoir **plusieurs employés** qui travaillent en même temps.

### Pourquoi Utiliser des Threads ?

- ✅ **Opérations longues** : calculs complexes, téléchargements
- ✅ **Interface réactive** : l'utilisateur peut continuer à interagir
- ✅ **Tâches multiples** : plusieurs opérations simultanées

### Le Thread Principal (UI Thread)

Votre application a toujours au moins **un thread** : le **thread principal** (ou UI thread), qui gère l'interface graphique.

⚠️ **Règle d'or** : Seul le thread principal peut modifier l'interface graphique !

### Exemple Simple avec TThread

Voici un exemple **très simplifié** d'utilisation de thread :

```pascal
type
  TMonThread = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TMonThread.Execute;
var
  i: Integer;
  Resultat: Double;
begin
  // Ce code s'exécute dans un thread séparé
  Resultat := 0;
  for i := 1 to 100000000 do
  begin
    Resultat := Resultat + Sqrt(i);

    // Vérifier si on doit s'arrêter
    if Terminated then
      Exit;
  end;

  // ⚠️ On NE PEUT PAS mettre à jour l'interface directement ici !
  // Il faut utiliser Synchronize (voir chapitre 18)
end;

// Lancer le thread
procedure TForm1.BtnDemarrerClick(Sender: TObject);
var
  MonThread: TMonThread;
begin
  MonThread := TMonThread.Create(True);  // Créer suspendu
  MonThread.FreeOnTerminate := True;     // Se libère automatiquement
  MonThread.Start;                       // Démarrer

  ShowMessage('Thread lancé, l''interface reste réactive !');
end;
```

**Note** : Les threads sont un sujet avancé qui sera couvert en détail dans le chapitre **18. Introduction Pratique au Multi-threading**.

---

## Différence entre Timer et Thread

| Aspect | Timer | Thread |
|--------|-------|--------|
| **Usage** | Tâches périodiques simples | Opérations longues |
| **Complexité** | Simple | Plus complexe |
| **Thread** | Thread principal (UI) | Thread séparé |
| **Interface** | Peut modifier directement | Doit synchroniser |
| **Performance** | Ralentit l'UI | N'affecte pas l'UI |
| **Exemples** | Horloge, animation | Téléchargement, calculs |

**Règle pratique :**
- **Timer** : pour des **petites tâches rapides** (<100 ms) répétées régulièrement
- **Thread** : pour des **opérations longues** (>1 seconde) qui bloqueraient l'interface

---

## Bonnes Pratiques avec les Timers

### 1. Désactiver Pendant le Traitement

```pascal
// ✅ BON : Éviter les appels multiples
procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;  // Désactiver pendant le traitement
  try
    // Faire le travail
    TraiterDonnees;
  finally
    Timer1.Enabled := True;  // Réactiver
  end;
end;

// ❌ MAUVAIS : Risque d'appels multiples si le traitement est lent
procedure TForm1.Timer1Timer(Sender: TObject);
begin
  TraiterDonnees;  // Si ça prend plus de 1000 ms, le timer va rappeler !
end;
```

### 2. Ne Pas Utiliser d'Intervalles Trop Petits

```pascal
// ✅ BON : Intervalle raisonnable
Timer1.Interval := 100;  // 10 fois par seconde, acceptable

// ⚠️ ATTENTION : Trop rapide
Timer1.Interval := 1;  // 1000 fois par seconde, surcharge le CPU !

// ✅ BON : Adapter l'intervalle au besoin
// Horloge : 1000 ms (1 seconde)
// Animation fluide : 20-50 ms
// Vérification périodique : 5000-10000 ms (5-10 secondes)
```

### 3. Libérer les Ressources

```pascal
// ✅ BON : Arrêter les timers avant de fermer
procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer1.Enabled := False;
  Timer2.Enabled := False;
  // ...
end;
```

### 4. Vérifier l'État Avant d'Agir

```pascal
// ✅ BON : Vérifier que l'objet existe encore
procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if Assigned(Label1) then
    Label1.Caption := TimeToStr(Now);
end;
```

### 5. Utiliser des Variables de Contrôle

```pascal
// ✅ BON : Éviter les opérations concurrentes
type
  TForm1 = class(TForm)
  private
    FTraitementEnCours: Boolean;
  end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if FTraitementEnCours then
    Exit;  // Sortir si déjà en cours

  FTraitementEnCours := True;
  try
    TraiterDonnees;
  finally
    FTraitementEnCours := False;
  end;
end;
```

### 6. Gérer les Erreurs

```pascal
// ✅ BON : Toujours gérer les exceptions
procedure TForm1.Timer1Timer(Sender: TObject);
begin
  try
    MettreAJourDonnees;
  except
    on E: Exception do
    begin
      // Logger l'erreur ou informer l'utilisateur
      StatusBar1.SimpleText := 'Erreur : ' + E.Message;
      // Optionnel : désactiver le timer si erreur critique
      // Timer1.Enabled := False;
    end;
  end;
end;
```

---

## Exemple Complet : Compte à Rebours avec Annulation

```pascal
type
  TFormCountdown = class(TForm)
    Timer1: TTimer;
    LabelTemps: TLabel;
    SpinEditMinutes: TSpinEdit;
    BtnDemarrer: TButton;
    BtnAnnuler: TButton;
    LabelTitre: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BtnDemarrerClick(Sender: TObject);
    procedure BtnAnnulerClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FSecondesRestantes: Integer;
    FCompteAReboursActif: Boolean;
    procedure AfficherTemps;
    procedure TerminerCompteARebours;
  end;

procedure TFormCountdown.FormCreate(Sender: TObject);
begin
  Timer1.Interval := 1000;  // 1 seconde
  Timer1.Enabled := False;
  FCompteAReboursActif := False;

  SpinEditMinutes.MinValue := 1;
  SpinEditMinutes.MaxValue := 60;
  SpinEditMinutes.Value := 5;

  LabelTemps.Font.Size := 48;
  LabelTemps.Font.Style := [fsBold];
  LabelTemps.Caption := '00:00';

  BtnAnnuler.Enabled := False;
end;

procedure TFormCountdown.BtnDemarrerClick(Sender: TObject);
begin
  // Démarrer le compte à rebours
  FSecondesRestantes := SpinEditMinutes.Value * 60;
  FCompteAReboursActif := True;

  AfficherTemps;

  Timer1.Enabled := True;
  BtnDemarrer.Enabled := False;
  BtnAnnuler.Enabled := True;
  SpinEditMinutes.Enabled := False;
end;

procedure TFormCountdown.BtnAnnulerClick(Sender: TObject);
begin
  // Annuler le compte à rebours
  Timer1.Enabled := False;
  FCompteAReboursActif := False;

  BtnDemarrer.Enabled := True;
  BtnAnnuler.Enabled := False;
  SpinEditMinutes.Enabled := True;
  LabelTemps.Caption := '00:00';
  LabelTemps.Font.Color := clBlack;
end;

procedure TFormCountdown.Timer1Timer(Sender: TObject);
begin
  if not FCompteAReboursActif then Exit;

  Dec(FSecondesRestantes);

  AfficherTemps;

  // Changer la couleur dans les dernières 10 secondes
  if FSecondesRestantes <= 10 then
    LabelTemps.Font.Color := clRed
  else
    LabelTemps.Font.Color := clBlack;

  // Vérifier si terminé
  if FSecondesRestantes <= 0 then
    TerminerCompteARebours;
end;

procedure TFormCountdown.AfficherTemps;
var
  Minutes, Secondes: Integer;
begin
  Minutes := FSecondesRestantes div 60;
  Secondes := FSecondesRestantes mod 60;
  LabelTemps.Caption := Format('%2.2d:%2.2d', [Minutes, Secondes]);
end;

procedure TFormCountdown.TerminerCompteARebours;
begin
  Timer1.Enabled := False;
  FCompteAReboursActif := False;

  // Alarme visuelle (clignotement)
  LabelTemps.Font.Color := clRed;
  LabelTemps.Caption := '00:00';

  // Son (si supporté)
  Beep;

  ShowMessage('Temps écoulé !');

  // Réinitialiser
  BtnDemarrer.Enabled := True;
  BtnAnnuler.Enabled := False;
  SpinEditMinutes.Enabled := True;
  LabelTemps.Font.Color := clBlack;
end;
```

---

## Exemple Complet : Jeu Simple (Attraper les Cercles)

```pascal
type
  TFormJeu = class(TForm)
    TimerJeu: TTimer;
    TimerSpawn: TTimer;
    LabelScore: TLabel;
    LabelTemps: TLabel;
    BtnDemarrer: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnDemarrerClick(Sender: TObject);
    procedure TimerJeuTimer(Sender: TObject);
    procedure TimerSpawnTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCercles: TList;
    FScore: Integer;
    FTempsRestant: Integer;
    procedure CreerCercle;
    procedure SupprimerTousCercles;
  end;

  TCercleCliquable = class(TShape)
  public
    procedure Click; override;
  end;

procedure TCercleCliquable.Click;
var
  Form: TFormJeu;
begin
  inherited;

  // Trouver le formulaire parent
  Form := Self.Owner as TFormJeu;

  // Incrémenter le score
  Inc(Form.FScore);
  Form.LabelScore.Caption := 'Score : ' + IntToStr(Form.FScore);

  // Se supprimer
  Form.FCercles.Remove(Self);
  Free;
end;

procedure TFormJeu.FormCreate(Sender: TObject);
begin
  FCercles := TList.Create;

  TimerJeu.Interval := 1000;  // 1 seconde
  TimerJeu.Enabled := False;

  TimerSpawn.Interval := 1500;  // Nouveau cercle toutes les 1.5 secondes
  TimerSpawn.Enabled := False;

  FScore := 0;
  FTempsRestant := 30;  // 30 secondes de jeu
end;

procedure TFormJeu.BtnDemarrerClick(Sender: TObject);
begin
  // Démarrer le jeu
  SupprimerTousCercles;
  FScore := 0;
  FTempsRestant := 30;

  LabelScore.Caption := 'Score : 0';
  LabelTemps.Caption := 'Temps : 30';

  TimerJeu.Enabled := True;
  TimerSpawn.Enabled := True;
  BtnDemarrer.Enabled := False;
end;

procedure TFormJeu.TimerJeuTimer(Sender: TObject);
begin
  // Décompte du temps
  Dec(FTempsRestant);
  LabelTemps.Caption := 'Temps : ' + IntToStr(FTempsRestant);

  if FTempsRestant <= 0 then
  begin
    // Fin du jeu
    TimerJeu.Enabled := False;
    TimerSpawn.Enabled := False;
    SupprimerTousCercles;

    ShowMessage(Format('Partie terminée ! Score final : %d', [FScore]));
    BtnDemarrer.Enabled := True;
  end;
end;

procedure TFormJeu.TimerSpawnTimer(Sender: TObject);
begin
  // Créer un nouveau cercle
  CreerCercle;
end;

procedure TFormJeu.CreerCercle;
var
  Cercle: TCercleCliquable;
  X, Y: Integer;
begin
  Cercle := TCercleCliquable.Create(Self);
  Cercle.Parent := Self;
  Cercle.Shape := stCircle;
  Cercle.Width := 50;
  Cercle.Height := 50;
  Cercle.Brush.Color := RGB(Random(256), Random(256), Random(256));

  // Position aléatoire
  X := Random(ClientWidth - 50);
  Y := Random(ClientHeight - 100) + 50;  // Éviter le haut
  Cercle.SetBounds(X, Y, 50, 50);

  FCercles.Add(Cercle);
end;

procedure TFormJeu.SupprimerTousCercles;
var
  i: Integer;
begin
  for i := FCercles.Count - 1 downto 0 do
  begin
    TShape(FCercles[i]).Free;
  end;
  FCercles.Clear;
end;

procedure TFormJeu.FormDestroy(Sender: TObject);
begin
  SupprimerTousCercles;
  FCercles.Free;
end;
```

---

## Cas d'Usage Avancés

### 1. Détection de Timeout

```pascal
type
  TForm1 = class(TForm)
  private
    FDerniereActivite: TDateTime;
    FTimeoutMinutes: Integer;
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTimeoutMinutes := 5;  // 5 minutes d'inactivité
  FDerniereActivite := Now;

  TimerTimeout.Interval := 60000;  // Vérifier toutes les minutes
  TimerTimeout.Enabled := True;
end;

// Capturer toute activité
procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FDerniereActivite := Now;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  FDerniereActivite := Now;
end;

procedure TForm1.TimerTimeoutTimer(Sender: TObject);
var
  MinutesInactif: Integer;
begin
  MinutesInactif := MinutesBetween(Now, FDerniereActivite);  // Nécessite uses DateUtils

  if MinutesInactif >= FTimeoutMinutes then
  begin
    ShowMessage('Session expirée pour cause d''inactivité');
    // Fermer ou déconnecter
    Close;
  end;
end;
```

### 2. Vérification de Connexion Réseau

```pascal
procedure TForm1.TimerConnexionTimer(Sender: TObject);
begin
  if VerifierConnexionInternet then
  begin
    ShapeConnexion.Brush.Color := clGreen;
    LabelStatut.Caption := 'Connecté';
  end
  else
  begin
    ShapeConnexion.Brush.Color := clRed;
    LabelStatut.Caption := 'Déconnecté';
  end;
end;

function TForm1.VerifierConnexionInternet: Boolean;
begin
  // Implémentation simplifiée
  // En réalité, vous feriez un ping ou une requête HTTP
  Result := True;  // Simuler
end;
```

### 3. Animation de Transition

```pascal
type
  TForm1 = class(TForm)
  private
    FOpaciteCible: Byte;
    procedure FadeIn;
    procedure FadeOut;
  end;

procedure TForm1.FadeIn;
begin
  FOpaciteCible := 255;
  AlphaBlendValue := 0;
  AlphaBlend := True;
  TimerFade.Enabled := True;
end;

procedure TForm1.FadeOut;
begin
  FOpaciteCible := 0;
  TimerFade.Enabled := True;
end;

procedure TForm1.TimerFadeTimer(Sender: TObject);
const
  VITESSE = 15;  // Pixels d'opacité par tick
begin
  if AlphaBlendValue < FOpaciteCible then
  begin
    AlphaBlendValue := Min(AlphaBlendValue + VITESSE, FOpaciteCible);  // Min nécessite uses Math
  end
  else if AlphaBlendValue > FOpaciteCible then
  begin
    AlphaBlendValue := Max(AlphaBlendValue - VITESSE, FOpaciteCible);  // Max nécessite uses Math
  end
  else
  begin
    // Opacité cible atteinte
    TimerFade.Enabled := False;
    if FOpaciteCible = 0 then
      Close;  // Fermer si fade out complet
  end;
end;
```

---

## Points Clés à Retenir

1. **TTimer** : déclenche un événement périodiquement
   - `Interval` : délai en millisecondes
   - `Enabled` : activer/désactiver
   - `OnTimer` : événement déclenché

2. **Conversions de temps** :
   - 1 seconde = 1000 ms
   - 1 minute = 60000 ms

3. **Gel de l'interface** : éviter les opérations longues sans retour
   - Solution simple : `Application.ProcessMessages`
   - Solution propre : découper avec Timer ou utiliser Thread

4. **Synchrone vs Asynchrone** :
   - Synchrone = séquentiel, bloquant
   - Asynchrone = parallèle, non-bloquant

5. **Threads** : pour opérations très longues (>1s)
   - Sujet avancé (chapitre 18)
   - Nécessite synchronisation avec l'UI

6. **Bonnes pratiques** :
   - Désactiver pendant traitement
   - Intervalles raisonnables (>10 ms)
   - Gérer les erreurs
   - Vérifier l'existence des objets
   - Libérer les ressources

7. **Timer vs Thread** :
   - Timer = tâches courtes et répétées
   - Thread = opérations longues uniques

---

## Conclusion

Les **Timers** sont des outils puissants pour créer des applications dynamiques et réactives. Ils permettent :

- **Animations** : déplacements, clignotements, transitions
- **Mise à jour automatique** : horloges, chronomètres, barres de progression
- **Surveillance** : vérifications périodiques, timeouts
- **Jeux** : logique de jeu, spawn d'ennemis

Le **traitement asynchrone** évite que l'interface se fige pendant les opérations longues. Selon la complexité :
- `Application.ProcessMessages` pour les cas simples
- Timer avec découpage pour les opérations moyennes
- Threads pour les opérations très longues (chapitre 18)

Maîtriser les Timers et comprendre les bases de l'asynchrone sont essentiels pour créer des applications professionnelles et agréables à utiliser.

Dans la section suivante, nous explorerons les **Actions et TActionList** pour centraliser et organiser les commandes de votre application.

---

**Prochaine étape :** 15.9 Actions et TActionList

⏭️ [Actions et TActionList](/15-composants-lcl-fondamentaux/09-actions-tactionlist.md)
