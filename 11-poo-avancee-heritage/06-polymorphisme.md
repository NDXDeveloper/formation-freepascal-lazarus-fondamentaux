🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.6 Polymorphisme

## Introduction

Le **polymorphisme** (du grec *poly* = plusieurs, *morphe* = formes) est l'un des concepts les plus puissants de la programmation orientée objet. Il permet à un même code de **fonctionner avec différents types d'objets**, chacun réagissant de manière appropriée selon sa nature.

### Analogie du monde réel

Imaginez un chef d'orchestre qui dit "Jouez !" à tous ses musiciens. Chaque musicien **répond différemment** selon son instrument :
- Le pianiste joue du piano
- Le violoniste joue du violon
- Le trompettiste joue de la trompette

Le chef donne la **même instruction** ("Jouez !"), mais chaque musicien l'**interprète selon sa spécialité**. C'est exactement le polymorphisme en programmation !

## Qu'est-ce que le polymorphisme ?

Le polymorphisme permet à une **variable de type parent** de référencer des **objets de types dérivés**, et d'appeler leurs méthodes spécifiques automatiquement.

```pascal
var
  Animal: TAnimal;  // Variable de type parent
begin
  // Peut pointer vers différents types d'animaux
  Animal := TChien.Create;
  Animal.FaireDuBruit;  // Appelle TChien.FaireDuBruit → "Wouaf"

  Animal := TChat.Create;
  Animal.FaireDuBruit;  // Appelle TChat.FaireDuBruit → "Miaou"

  Animal := TOiseau.Create;
  Animal.FaireDuBruit;  // Appelle TOiseau.FaireDuBruit → "Cui-cui"
end;
```

**Une seule variable, plusieurs formes** : c'est le polymorphisme !

## Les deux types de polymorphisme

### 1. Polymorphisme de substitution (subtyping)

C'est le polymorphisme que nous avons vu avec `virtual` et `override`. Un objet dérivé peut **remplacer** un objet parent.

```pascal
type
  TForme = class
    procedure Dessiner; virtual;
  end;

  TRectangle = class(TForme)
    procedure Dessiner; override;
  end;

var
  F: TForme;
begin
  F := TRectangle.Create;  // Un rectangle EST une forme
  F.Dessiner;              // Appelle TRectangle.Dessiner
end;
```

### 2. Polymorphisme ad-hoc (surcharge)

C'est la **surcharge de méthodes** : plusieurs méthodes avec le même nom mais des paramètres différents.

```pascal
type
  TCalculatrice = class
    function Additionner(A, B: Integer): Integer; overload;
    function Additionner(A, B: Real): Real; overload;
    function Additionner(A, B, C: Integer): Integer; overload;
  end;
```

Dans cette section, nous nous concentrons sur le **polymorphisme de substitution**.

## Exemple complet : Zoo polymorphe

Voici un exemple détaillé qui montre toute la puissance du polymorphisme :

```pascal
program ZooPolymorphe;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Classe de base }
  TAnimal = class
  protected
    FNom: string;
    FAge: Integer;
    FEspece: string;
  public
    constructor Create(ANom: string; AAge: Integer);
    procedure Afficher; virtual;
    procedure FaireDuBruit; virtual;
    procedure SeDeplacer; virtual;
    procedure Manger; virtual;
    function GetInfo: string;
  end;

  { Mammifères }
  TChien = class(TAnimal)
  public
    constructor Create(ANom: string; AAge: Integer);
    procedure FaireDuBruit; override;
    procedure SeDeplacer; override;
    procedure Manger; override;
  end;

  TChat = class(TAnimal)
  public
    constructor Create(ANom: string; AAge: Integer);
    procedure FaireDuBruit; override;
    procedure SeDeplacer; override;
    procedure Manger; override;
  end;

  TLion = class(TAnimal)
  public
    constructor Create(ANom: string; AAge: Integer);
    procedure FaireDuBruit; override;
    procedure SeDeplacer; override;
    procedure Manger; override;
  end;

  { Oiseaux }
  TPerroquet = class(TAnimal)
  public
    constructor Create(ANom: string; AAge: Integer);
    procedure FaireDuBruit; override;
    procedure SeDeplacer; override;
    procedure Manger; override;
  end;

  TAigle = class(TAnimal)
  public
    constructor Create(ANom: string; AAge: Integer);
    procedure FaireDuBruit; override;
    procedure SeDeplacer; override;
    procedure Manger; override;
  end;

  { Reptiles }
  TSerpent = class(TAnimal)
  public
    constructor Create(ANom: string; AAge: Integer);
    procedure FaireDuBruit; override;
    procedure SeDeplacer; override;
    procedure Manger; override;
  end;

{ === TAnimal === }

constructor TAnimal.Create(ANom: string; AAge: Integer);  
begin  
  inherited Create;
  FNom := ANom;
  FAge := AAge;
  FEspece := 'Animal générique';
end;

procedure TAnimal.Afficher;  
begin  
  WriteLn('--- ', FEspece, ' ---');
  WriteLn('Nom : ', FNom);
  WriteLn('Age : ', FAge, ' ans');
end;

procedure TAnimal.FaireDuBruit;  
begin  
  WriteLn('[Son générique d''animal]');
end;

procedure TAnimal.SeDeplacer;  
begin  
  WriteLn('[Déplacement générique]');
end;

procedure TAnimal.Manger;  
begin  
  WriteLn(FNom, ' mange de la nourriture');
end;

function TAnimal.GetInfo: string;  
begin  
  Result := Format('%s (%s, %d ans)', [FNom, FEspece, FAge]);
end;

{ === TChien === }

constructor TChien.Create(ANom: string; AAge: Integer);  
begin  
  inherited Create(ANom, AAge);
  FEspece := 'Chien';
end;

procedure TChien.FaireDuBruit;  
begin  
  WriteLn('🐕 ', FNom, ' aboie : Wouaf wouaf !');
end;

procedure TChien.SeDeplacer;  
begin  
  WriteLn('🐕 ', FNom, ' court à quatre pattes en remuant la queue');
end;

procedure TChien.Manger;  
begin  
  WriteLn('🐕 ', FNom, ' mange des croquettes');
end;

{ === TChat === }

constructor TChat.Create(ANom: string; AAge: Integer);  
begin  
  inherited Create(ANom, AAge);
  FEspece := 'Chat';
end;

procedure TChat.FaireDuBruit;  
begin  
  WriteLn('🐈 ', FNom, ' miaule : Miaou miaou !');
end;

procedure TChat.SeDeplacer;  
begin  
  WriteLn('🐈 ', FNom, ' se déplace silencieusement');
end;

procedure TChat.Manger;  
begin  
  WriteLn('🐈 ', FNom, ' mange du poisson');
end;

{ === TLion === }

constructor TLion.Create(ANom: string; AAge: Integer);  
begin  
  inherited Create(ANom, AAge);
  FEspece := 'Lion';
end;

procedure TLion.FaireDuBruit;  
begin  
  WriteLn('🦁 ', FNom, ' rugit : ROOAAAR !');
end;

procedure TLion.SeDeplacer;  
begin  
  WriteLn('🦁 ', FNom, ' marche majestueusement');
end;

procedure TLion.Manger;  
begin  
  WriteLn('🦁 ', FNom, ' dévore de la viande');
end;

{ === TPerroquet === }

constructor TPerroquet.Create(ANom: string; AAge: Integer);  
begin  
  inherited Create(ANom, AAge);
  FEspece := 'Perroquet';
end;

procedure TPerroquet.FaireDuBruit;  
begin  
  WriteLn('🦜 ', FNom, ' parle : Bonjour ! Bonjour !');
end;

procedure TPerroquet.SeDeplacer;  
begin  
  WriteLn('🦜 ', FNom, ' vole de branche en branche');
end;

procedure TPerroquet.Manger;  
begin  
  WriteLn('🦜 ', FNom, ' grignote des graines');
end;

{ === TAigle === }

constructor TAigle.Create(ANom: string; AAge: Integer);  
begin  
  inherited Create(ANom, AAge);
  FEspece := 'Aigle';
end;

procedure TAigle.FaireDuBruit;  
begin  
  WriteLn('🦅 ', FNom, ' crie : Crii crii !');
end;

procedure TAigle.SeDeplacer;  
begin  
  WriteLn('🦅 ', FNom, ' plane majestueusement dans le ciel');
end;

procedure TAigle.Manger;  
begin  
  WriteLn('🦅 ', FNom, ' chasse de petits animaux');
end;

{ === TSerpent === }

constructor TSerpent.Create(ANom: string; AAge: Integer);  
begin  
  inherited Create(ANom, AAge);
  FEspece := 'Serpent';
end;

procedure TSerpent.FaireDuBruit;  
begin  
  WriteLn('🐍 ', FNom, ' siffle : Ssssss !');
end;

procedure TSerpent.SeDeplacer;  
begin  
  WriteLn('🐍 ', FNom, ' rampe en ondulant');
end;

procedure TSerpent.Manger;  
begin  
  WriteLn('🐍 ', FNom, ' avale sa proie en entier');
end;

{ === Fonctions polymorphes === }

procedure PresentationAnimal(Animal: TAnimal);  
begin  
  WriteLn('=================================');
  Animal.Afficher;
  WriteLn;
  Write('Bruit : ');
  Animal.FaireDuBruit;
  Write('Déplacement : ');
  Animal.SeDeplacer;
  Write('Repas : ');
  Animal.Manger;
  WriteLn('=================================');
  WriteLn;
end;

procedure NourrirTousLesAnimaux(Animaux: array of TAnimal);  
var  
  i: Integer;
begin
  WriteLn('🍽️  HEURE DU REPAS DANS LE ZOO !');
  WriteLn;
  for i := 0 to High(Animaux) do
  begin
    WriteLn('→ ', Animaux[i].GetInfo);
    Animaux[i].Manger;
    WriteLn;
  end;
end;

procedure ConcertAnimal(Animaux: array of TAnimal);  
var  
  i: Integer;
begin
  WriteLn('🎵 CONCERT DES ANIMAUX !');
  WriteLn;
  for i := 0 to High(Animaux) do
  begin
    Write('♪ ');
    Animaux[i].FaireDuBruit;
  end;
  WriteLn;
end;

procedure CourseAnimale(Animaux: array of TAnimal);  
var  
  i: Integer;
begin
  WriteLn('🏃 GRANDE COURSE DES ANIMAUX !');
  WriteLn;
  for i := 0 to High(Animaux) do
  begin
    WriteLn('Concurrent ', i + 1, ' : ', Animaux[i].GetInfo);
    Animaux[i].SeDeplacer;
    WriteLn;
  end;
end;

{ === Programme principal === }
var
  Rex: TChien;
  Felix: TChat;
  Simba: TLion;
  Coco: TPerroquet;
  Zeus: TAigle;
  Kaa: TSerpent;

  Zoo: array[0..5] of TAnimal;
begin
  WriteLn('===============================================');
  WriteLn('    BIENVENUE AU ZOO POLYMORPHE !');
  WriteLn('===============================================');
  WriteLn;

  // Création des animaux
  Rex := TChien.Create('Rex', 5);
  Felix := TChat.Create('Félix', 3);
  Simba := TLion.Create('Simba', 8);
  Coco := TPerroquet.Create('Coco', 12);
  Zeus := TAigle.Create('Zeus', 6);
  Kaa := TSerpent.Create('Kaa', 4);

  // Remplissage du tableau polymorphe
  Zoo[0] := Rex;
  Zoo[1] := Felix;
  Zoo[2] := Simba;
  Zoo[3] := Coco;
  Zoo[4] := Zeus;
  Zoo[5] := Kaa;

  WriteLn('📋 LISTE DES PENSIONNAIRES :');
  WriteLn('----------------------------');
  WriteLn('1. ', Zoo[0].GetInfo);
  WriteLn('2. ', Zoo[1].GetInfo);
  WriteLn('3. ', Zoo[2].GetInfo);
  WriteLn('4. ', Zoo[3].GetInfo);
  WriteLn('5. ', Zoo[4].GetInfo);
  WriteLn('6. ', Zoo[5].GetInfo);
  WriteLn;
  WriteLn;

  // Test 1 : Présentation individuelle
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('TEST 1 : PRESENTATION INDIVIDUELLE');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;
  PresentationAnimal(Rex);
  PresentationAnimal(Simba);

  // Test 2 : Heure du repas
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('TEST 2 : HEURE DU REPAS');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;
  NourrirTousLesAnimaux(Zoo);

  // Test 3 : Concert
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('TEST 3 : CONCERT DES ANIMAUX');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;
  ConcertAnimal(Zoo);

  // Test 4 : Course
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('TEST 4 : GRANDE COURSE');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;
  CourseAnimale(Zoo);

  WriteLn('═══════════════════════════════════════════════');
  WriteLn('    FIN DE LA JOURNEE AU ZOO');
  WriteLn('═══════════════════════════════════════════════');

  // Libération
  Rex.Free;
  Felix.Free;
  Simba.Free;
  Coco.Free;
  Zeus.Free;
  Kaa.Free;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Les avantages du polymorphisme

### 1. Code générique et réutilisable

Vous écrivez **une seule fonction** qui fonctionne avec **tous les types** :

```pascal
procedure TraiterAnimal(A: TAnimal);  
begin  
  A.FaireDuBruit;  // Fonctionne avec Chien, Chat, Lion, etc.
  A.Manger;
end;
```

Sans polymorphisme, vous auriez besoin de :
```pascal
procedure TraiterChien(C: TChien);  
procedure TraiterChat(C: TChat);  
procedure TraiterLion(L: TLion);  
// ... une fonction par type !
```

### 2. Collections hétérogènes

Vous pouvez stocker **différents types** dans la **même collection** :

```pascal
var
  Animaux: array of TAnimal;
begin
  SetLength(Animaux, 3);
  Animaux[0] := TChien.Create('Rex', 5);
  Animaux[1] := TChat.Create('Felix', 3);
  Animaux[2] := TLion.Create('Simba', 8);

  // Boucle unique pour tous
  for i := 0 to High(Animaux) do
    Animaux[i].FaireDuBruit;
end;
```

### 3. Extensibilité sans modification

Vous pouvez ajouter de **nouveaux types** sans changer le code existant :

```pascal
// Ajout d'un nouveau type
type
  TElephant = class(TAnimal)
    procedure FaireDuBruit; override;
  end;

// Les fonctions existantes fonctionnent automatiquement !
var
  Elephant: TElephant;
begin
  Elephant := TElephant.Create('Dumbo', 10);
  TraiterAnimal(Elephant);  // Fonctionne immédiatement !
end;
```

### 4. Maintenance simplifiée

Un changement dans la classe de base se propage automatiquement :

```pascal
type
  TAnimal = class
    procedure Afficher; virtual;
    procedure Dormir; virtual;  // ← Nouvelle méthode ajoutée
  end;

// Toutes les classes dérivées en héritent automatiquement
```

## Le principe de substitution de Liskov

**Règle simple** : Un objet d'une classe dérivée doit pouvoir **remplacer** un objet de la classe parent sans casser le programme.

```pascal
procedure FaireQuelqueChose(Animal: TAnimal);  
begin  
  Animal.Manger;  // Doit fonctionner pour TOUS les animaux
end;

var
  A: TAnimal;
begin
  // Toutes ces substitutions doivent fonctionner
  A := TChien.Create('Rex', 5);
  FaireQuelqueChose(A);  // ✓ OK

  A := TChat.Create('Felix', 3);
  FaireQuelqueChose(A);  // ✓ OK

  A := TLion.Create('Simba', 8);
  FaireQuelqueChose(A);  // ✓ OK
end;
```

## Exemple pratique : Système de fichiers

Voici un exemple concret de polymorphisme appliqué à la gestion de fichiers :

```pascal
program SystemeFichiers;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Classe de base : Élément du système de fichiers }
  TElementFS = class
  protected
    FNom: string;
    FTaille: Int64;  // en octets
    FDateCreation: TDateTime;
  public
    constructor Create(ANom: string);
    function GetNom: string;
    function GetTaille: Int64; virtual;  // Polymorphe !
    function GetTailleFormatee: string;
    procedure Afficher; virtual;
    procedure Renommer(NouveauNom: string);
  end;

  { Fichier }
  TFichier = class(TElementFS)
  private
    FExtension: string;
  public
    constructor Create(ANom: string; ATaille: Int64);
    function GetTaille: Int64; override;
    procedure Afficher; override;
  end;

  { Dossier }
  TDossier = class(TElementFS)
  private
    FContenu: array of TElementFS;
  public
    constructor Create(ANom: string);
    destructor Destroy; override;
    procedure Ajouter(Element: TElementFS);
    function GetTaille: Int64; override;  // Taille = somme du contenu
    procedure Afficher; override;
    procedure ListerContenu;
  end;

{ === TElementFS === }

constructor TElementFS.Create(ANom: string);  
begin  
  inherited Create;
  FNom := ANom;
  FTaille := 0;
  FDateCreation := Now;
end;

function TElementFS.GetNom: string;  
begin  
  Result := FNom;
end;

function TElementFS.GetTaille: Int64;  
begin  
  Result := FTaille;
end;

function TElementFS.GetTailleFormatee: string;  
var  
  Taille: Int64;
begin
  Taille := GetTaille;  // Appel polymorphe !

  if Taille < 1024 then
    Result := Format('%d o', [Taille])
  else if Taille < 1024 * 1024 then
    Result := Format('%.2f Ko', [Taille / 1024])
  else if Taille < 1024 * 1024 * 1024 then
    Result := Format('%.2f Mo', [Taille / (1024 * 1024)])
  else
    Result := Format('%.2f Go', [Taille / (1024 * 1024 * 1024)]);
end;

procedure TElementFS.Afficher;  
begin  
  WriteLn(FNom, ' - ', GetTailleFormatee);
end;

procedure TElementFS.Renommer(NouveauNom: string);  
begin  
  WriteLn('Renommage : "', FNom, '" → "', NouveauNom, '"');
  FNom := NouveauNom;
end;

{ === TFichier === }

constructor TFichier.Create(ANom: string; ATaille: Int64);  
var  
  PosPoint: Integer;
begin
  inherited Create(ANom);
  FTaille := ATaille;

  // Extraire l'extension
  PosPoint := Pos('.', ANom);
  if PosPoint > 0 then
    FExtension := Copy(ANom, PosPoint + 1, Length(ANom))
  else
    FExtension := '';
end;

function TFichier.GetTaille: Int64;  
begin  
  Result := FTaille;  // Taille fixe pour un fichier
end;

procedure TFichier.Afficher;  
begin  
  Write('📄 ');
  inherited Afficher;
  if FExtension <> '' then
    WriteLn('   Type : Fichier .', FExtension);
end;

{ === TDossier === }

constructor TDossier.Create(ANom: string);  
begin  
  inherited Create(ANom);
  SetLength(FContenu, 0);
end;

destructor TDossier.Destroy;  
var  
  i: Integer;
begin
  // Libérer tous les éléments contenus
  for i := 0 to High(FContenu) do
    FContenu[i].Free;

  inherited Destroy;
end;

procedure TDossier.Ajouter(Element: TElementFS);  
var  
  Longueur: Integer;
begin
  Longueur := Length(FContenu);
  SetLength(FContenu, Longueur + 1);
  FContenu[Longueur] := Element;
end;

function TDossier.GetTaille: Int64;  
var  
  i: Integer;
  Total: Int64;
begin
  Total := 0;

  // Calcul polymorphe : la taille d'un dossier = somme de son contenu
  for i := 0 to High(FContenu) do
    Total := Total + FContenu[i].GetTaille;  // Appel polymorphe !

  Result := Total;
end;

procedure TDossier.Afficher;  
begin  
  Write('📁 ');
  inherited Afficher;
  WriteLn('   Contient : ', Length(FContenu), ' élément(s)');
end;

procedure TDossier.ListerContenu;  
var  
  i: Integer;
begin
  WriteLn;
  WriteLn('=== Contenu de "', FNom, '" ===');
  if Length(FContenu) = 0 then
  begin
    WriteLn('  (vide)');
  end
  else
  begin
    for i := 0 to High(FContenu) do
    begin
      Write('  ');
      FContenu[i].Afficher;  // Appel polymorphe !
    end;
  end;
  WriteLn('==============================');
end;

{ === Fonctions polymorphes === }

procedure AfficherInfosElement(Element: TElementFS);  
begin  
  WriteLn('╔══════════════════════════════════');
  WriteLn('║ Informations sur l''élément');
  WriteLn('╠══════════════════════════════════');
  WriteLn('║ Nom : ', Element.GetNom);
  WriteLn('║ Taille : ', Element.GetTailleFormatee);
  WriteLn('║ Créé le : ', DateTimeToStr(Element.FDateCreation));
  WriteLn('╚══════════════════════════════════');
  WriteLn;
end;

function CalculerTailleTotal(Elements: array of TElementFS): Int64;  
var  
  i: Integer;
  Total: Int64;
begin
  Total := 0;
  for i := 0 to High(Elements) do
    Total := Total + Elements[i].GetTaille;  // Polymorphe !

  Result := Total;
end;

{ === Programme principal === }
var
  RacineDossier: TDossier;
  DossierDocuments: TDossier;
  DossierImages: TDossier;

  Fichier1, Fichier2, Fichier3, Fichier4: TFichier;

  Tous: array[0..3] of TElementFS;
  TailleTotal: Int64;
begin
  WriteLn('═══════════════════════════════════════════');
  WriteLn('   SYSTEME DE FICHIERS POLYMORPHE');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;

  // Création de la structure
  RacineDossier := TDossier.Create('Racine');
  DossierDocuments := TDossier.Create('Documents');
  DossierImages := TDossier.Create('Images');

  Fichier1 := TFichier.Create('rapport.pdf', 2048000);      // 2 Mo
  Fichier2 := TFichier.Create('presentation.pptx', 5242880); // 5 Mo
  Fichier3 := TFichier.Create('photo1.jpg', 3145728);       // 3 Mo
  Fichier4 := TFichier.Create('photo2.jpg', 4194304);       // 4 Mo

  // Construction de l'arborescence
  DossierDocuments.Ajouter(Fichier1);
  DossierDocuments.Ajouter(Fichier2);

  DossierImages.Ajouter(Fichier3);
  DossierImages.Ajouter(Fichier4);

  RacineDossier.Ajouter(DossierDocuments);
  RacineDossier.Ajouter(DossierImages);

  // Affichage polymorphe
  WriteLn('--- Structure du système de fichiers ---');
  WriteLn;
  RacineDossier.Afficher;
  RacineDossier.ListerContenu;
  WriteLn;

  DossierDocuments.ListerContenu;
  WriteLn;

  DossierImages.ListerContenu;
  WriteLn;

  // Test des fonctions polymorphes
  WriteLn('--- Test des fonctions polymorphes ---');
  WriteLn;

  AfficherInfosElement(RacineDossier);    // Dossier
  AfficherInfosElement(Fichier1);         // Fichier
  AfficherInfosElement(DossierImages);    // Dossier

  // Calcul de taille avec tableau hétérogène
  Tous[0] := RacineDossier;
  Tous[1] := DossierDocuments;
  Tous[2] := Fichier1;
  Tous[3] := DossierImages;

  TailleTotal := CalculerTailleTotal(Tous);
  WriteLn('Taille totale calculée : ', TailleTotal, ' octets');
  WriteLn;

  // Libération (les fichiers seront libérés par les dossiers)
  RacineDossier.Free;

  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Vérification de type avec `is` et transtypage avec `as`

Parfois, vous devez savoir le **type réel** d'un objet :

### L'opérateur `is` : vérifier le type

```pascal
var
  Animal: TAnimal;
begin
  Animal := TChien.Create('Rex', 5);

  if Animal is TChien then
    WriteLn('C''est un chien !')
  else if Animal is TChat then
    WriteLn('C''est un chat !');
end;
```

### L'opérateur `as` : transtypage sécurisé

```pascal
var
  Animal: TAnimal;
  Chien: TChien;
begin
  Animal := TChien.Create('Rex', 5);

  // Transtypage sécurisé
  if Animal is TChien then
  begin
    Chien := Animal as TChien;
    // Maintenant on peut accéder aux méthodes spécifiques de TChien
    Chien.Aboyer;  // Si cette méthode existe
  end;
end;
```

## Quand utiliser le polymorphisme ?

### ✅ Utilisez le polymorphisme quand :

1. **Vous avez plusieurs types avec un comportement commun**
   ```pascal
   FaireDuBruit → Chien aboie, Chat miaule, Lion rugit
   ```

2. **Vous voulez traiter différents types de manière uniforme**
   ```pascal
   for i := 0 to High(Animaux) do
     Animaux[i].Manger;  // Fonctionne pour tous
   ```

3. **Vous construisez un système extensible**
   - Ajout de nouveaux types sans modification du code existant

4. **Vous créez des collections hétérogènes**
   ```pascal
   Formes: array of TForme;  // Peut contenir Rectangle, Cercle, Triangle
   ```

### ❌ N'utilisez PAS le polymorphisme quand :

1. **Les types n'ont rien en commun**
   - Pas de relation logique entre eux

2. **Une seule implémentation suffit**
   - Inutile de compliquer

3. **Performance critique**
   - Le polymorphisme a un léger coût (appel indirect)

## Avantages et inconvénients

### ✅ Avantages

- **Flexibilité** : ajout facile de nouveaux types
- **Réutilisabilité** : code générique fonctionnant avec tous les types
- **Maintenabilité** : modifications centralisées
- **Extensibilité** : principe ouvert/fermé (ouvert à l'extension, fermé à la modification)

### ⚠️ Inconvénients

- **Complexité** : hiérarchie de classes à comprendre
- **Performance** : appel indirect légèrement plus lent
- **Débogage** : parfois difficile de savoir quelle méthode est appelée

## Résumé

Le polymorphisme permet de :
- ✅ Écrire du code qui fonctionne avec plusieurs types d'objets
- ✅ Traiter différents types de manière uniforme
- ✅ Créer des collections hétérogènes
- ✅ Étendre facilement le système sans modifier le code existant

**Mécanisme :**
- Une variable de **type parent** peut pointer vers un objet de **type dérivé**
- L'appel de méthode utilise le **type réel** de l'objet (liaison dynamique)
- Nécessite des méthodes **virtuelles** (`virtual` / `override`)

**Principe clé :** "Un chien EST un animal" → Un TChien peut être utilisé partout où un TAnimal est attendu.

**Règle d'or :** Le polymorphisme est au cœur de la POO. Maîtrisez-le et vous pourrez créer des architectures logicielles élégantes et extensibles !

Dans les sections suivantes, nous explorerons d'autres aspects avancés de l'héritage et de la POO en FreePascal.

⏭️ [Transtypage (as, is)](/11-poo-avancee-heritage/07-transtypage-as-is.md)
