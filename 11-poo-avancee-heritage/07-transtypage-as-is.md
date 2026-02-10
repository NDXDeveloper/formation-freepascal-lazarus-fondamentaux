🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.7 Transtypage (as, is)

## Introduction

Le **transtypage** (ou *casting* en anglais) est l'opération qui consiste à traiter un objet d'un certain type comme s'il était d'un autre type. En Pascal, nous avons deux opérateurs spéciaux pour faire cela de manière sécurisée : **`is`** et **`as`**.

### Analogie du monde réel

Imaginez que vous avez une boîte étiquetée "Animaux". À l'intérieur, il peut y avoir des chiens, des chats ou des oiseaux.

- L'opérateur **`is`** vous permet de **vérifier** : "Est-ce que cette boîte contient un chien ?"
- L'opérateur **`as`** vous permet de **dire** : "Je sais que c'est un chien, donne-le-moi en tant que chien"

## Le problème du polymorphisme

Avec le polymorphisme, vous pouvez avoir une variable de type parent qui pointe vers un objet de type dérivé :

```pascal
var
  Animal: TAnimal;
  Chien: TChien;
begin
  Animal := TChien.Create('Rex', 5);  // Un chien vu comme un animal

  // Problème : comment accéder aux méthodes spécifiques de TChien ?
  // Animal.Aboyer;  // ❌ ERREUR : TAnimal n'a pas de méthode Aboyer
end;
```

**Question** : Comment accéder aux fonctionnalités spécifiques de `TChien` quand la variable est de type `TAnimal` ?

**Réponse** : Le transtypage avec `is` et `as` !

## L'opérateur `is` : Vérification de type

L'opérateur **`is`** permet de **vérifier** si un objet est d'un type particulier ou d'un type dérivé.

### Syntaxe

```pascal
if ObjetVariable is TypeRecherche then
  // Code si l'objet est du type recherché
```

### Exemple simple

```pascal
var
  Animal: TAnimal;
begin
  Animal := TChien.Create('Rex', 5);

  if Animal is TChien then
    WriteLn('C''est un chien !')
  else if Animal is TChat then
    WriteLn('C''est un chat !')
  else if Animal is TOiseau then
    WriteLn('C''est un oiseau !')
  else
    WriteLn('Type d''animal inconnu');
end;
```

### Comment ça fonctionne ?

L'opérateur `is` vérifie :
1. Si l'objet est **exactement** du type spécifié
2. OU si l'objet est d'un type **dérivé** du type spécifié

```pascal
type
  TAnimal = class
  end;

  TMammifere = class(TAnimal)
  end;

  TChien = class(TMammifere)
  end;

var
  Animal: TAnimal;
begin
  Animal := TChien.Create;

  WriteLn(Animal is TAnimal);      // ✅ True : TChien dérive de TAnimal
  WriteLn(Animal is TMammifere);   // ✅ True : TChien dérive de TMammifere
  WriteLn(Animal is TChien);       // ✅ True : c'est exactement un TChien
  WriteLn(Animal is TChat);        // ❌ False : ce n'est pas un TChat
end;
```

## L'opérateur `as` : Transtypage sécurisé

L'opérateur **`as`** permet de **convertir** une variable d'un type vers un autre type de manière sécurisée.

### Syntaxe

```pascal
VariableTypee := ObjetVariable as TypeCible;
```

### Exemple simple

```pascal
var
  Animal: TAnimal;
  Chien: TChien;
begin
  Animal := TChien.Create('Rex', 5);

  // Transtypage sécurisé
  Chien := Animal as TChien;

  // Maintenant on peut utiliser les méthodes de TChien
  Chien.Aboyer;
end;
```

### Que se passe-t-il en cas d'erreur ?

Si le transtypage est **impossible**, l'opérateur `as` lève une **exception** :

```pascal
var
  Animal: TAnimal;
  Chien: TChien;
begin
  Animal := TChat.Create('Felix', 3);  // C'est un CHAT

  try
    Chien := Animal as TChien;  // ❌ EXCEPTION : Animal est un TChat, pas un TChien
    Chien.Aboyer;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;
end;
```

## Combinaison de `is` et `as` : La bonne pratique

La meilleure approche consiste à **vérifier avec `is`** avant de **convertir avec `as`** :

```pascal
var
  Animal: TAnimal;
  Chien: TChien;
begin
  Animal := TChien.Create('Rex', 5);

  // Vérification PUIS transtypage
  if Animal is TChien then
  begin
    Chien := Animal as TChien;
    Chien.Aboyer;
    WriteLn('Race : ', Chien.Race);
  end
  else
    WriteLn('Ce n''est pas un chien');
end;
```

## Exemple complet : Zoo avec identification

```pascal
program TranstypageZoo;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Hiérarchie de classes }
  TAnimal = class
  protected
    FNom: string;
    FAge: Integer;
  public
    constructor Create(ANom: string; AAge: Integer);
    procedure SePresenter; virtual;
    procedure FaireDuBruit; virtual;
  end;

  { Mammifères }
  TMammifere = class(TAnimal)
  private
    FPoils: Boolean;
  public
    constructor Create(ANom: string; AAge: Integer; APoils: Boolean);
    procedure Allaiter;
  end;

  TChien = class(TMammifere)
  private
    FRace: string;
  public
    constructor Create(ANom: string; AAge: Integer; ARace: string);
    procedure Aboyer;
    procedure Garder;
    property Race: string read FRace;
  end;

  TChat = class(TMammifere)
  private
    FCouleur: string;
  public
    constructor Create(ANom: string; AAge: Integer; ACouleur: string);
    procedure Miauler;
    procedure Ronronner;
    property Couleur: string read FCouleur;
  end;

  { Oiseaux }
  TOiseau = class(TAnimal)
  private
    FEnvergure: Real;
  public
    constructor Create(ANom: string; AAge: Integer; AEnvergure: Real);
    procedure Voler;
    procedure ChercherNourriture;
    property Envergure: Real read FEnvergure;
  end;

  TPerroquet = class(TOiseau)
  private
    FVocabulaire: Integer;  // Nombre de mots
  public
    constructor Create(ANom: string; AAge: Integer; AEnvergure: Real; AVocabulaire: Integer);
    procedure Parler(const Phrase: string);
    property Vocabulaire: Integer read FVocabulaire;
  end;

  TAigle = class(TOiseau)
  public
    procedure Chasser;
  end;

{ === TAnimal === }

constructor TAnimal.Create(ANom: string; AAge: Integer);  
begin  
  inherited Create;
  FNom := ANom;
  FAge := AAge;
end;

procedure TAnimal.SePresenter;  
begin  
  WriteLn('Je suis ', FNom, ', j''ai ', FAge, ' ans');
end;

procedure TAnimal.FaireDuBruit;  
begin  
  WriteLn('[Bruit d''animal générique]');
end;

{ === TMammifere === }

constructor TMammifere.Create(ANom: string; AAge: Integer; APoils: Boolean);  
begin  
  inherited Create(ANom, AAge);
  FPoils := APoils;
end;

procedure TMammifere.Allaiter;  
begin  
  WriteLn(FNom, ' allaite ses petits');
end;

{ === TChien === }

constructor TChien.Create(ANom: string; AAge: Integer; ARace: string);  
begin  
  inherited Create(ANom, AAge, True);
  FRace := ARace;
end;

procedure TChien.Aboyer;  
begin  
  WriteLn('🐕 ', FNom, ' aboie : Wouaf wouaf !');
end;

procedure TChien.Garder;  
begin  
  WriteLn('🐕 ', FNom, ' monte la garde');
end;

{ === TChat === }

constructor TChat.Create(ANom: string; AAge: Integer; ACouleur: string);  
begin  
  inherited Create(ANom, AAge, True);
  FCouleur := ACouleur;
end;

procedure TChat.Miauler;  
begin  
  WriteLn('🐈 ', FNom, ' miaule : Miaou !');
end;

procedure TChat.Ronronner;  
begin  
  WriteLn('🐈 ', FNom, ' ronronne : Rrrrrr...');
end;

{ === TOiseau === }

constructor TOiseau.Create(ANom: string; AAge: Integer; AEnvergure: Real);  
begin  
  inherited Create(ANom, AAge);
  FEnvergure := AEnvergure;
end;

procedure TOiseau.Voler;  
begin  
  WriteLn('🦅 ', FNom, ' vole avec ', FEnvergure:0:2, ' m d''envergure');
end;

procedure TOiseau.ChercherNourriture;  
begin  
  WriteLn('🦅 ', FNom, ' cherche de la nourriture');
end;

{ === TPerroquet === }

constructor TPerroquet.Create(ANom: string; AAge: Integer; AEnvergure: Real; AVocabulaire: Integer);  
begin  
  inherited Create(ANom, AAge, AEnvergure);
  FVocabulaire := AVocabulaire;
end;

procedure TPerroquet.Parler(const Phrase: string);  
begin  
  WriteLn('🦜 ', FNom, ' dit : "', Phrase, '"');
end;

{ === TAigle === }

procedure TAigle.Chasser;  
begin  
  WriteLn('🦅 ', FNom, ' chasse sa proie depuis le ciel');
end;

{ === Fonctions utilisant le transtypage === }

procedure IdentifierAnimal(Animal: TAnimal);  
begin  
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('IDENTIFICATION D''UN ANIMAL');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  // Présentation générale
  Animal.SePresenter;
  WriteLn;

  // Identification du type précis avec is
  WriteLn('→ Analyse du type...');

  if Animal is TChien then
    WriteLn('✓ Type identifié : CHIEN')
  else if Animal is TChat then
    WriteLn('✓ Type identifié : CHAT')
  else if Animal is TPerroquet then
    WriteLn('✓ Type identifié : PERROQUET')
  else if Animal is TAigle then
    WriteLn('✓ Type identifié : AIGLE')
  else if Animal is TOiseau then
    WriteLn('✓ Type identifié : OISEAU (type générique)')
  else if Animal is TMammifere then
    WriteLn('✓ Type identifié : MAMMIFERE (type générique)')
  else
    WriteLn('✓ Type identifié : ANIMAL (type de base)');

  WriteLn;

  // Accès aux fonctionnalités spécifiques via transtypage
  WriteLn('→ Fonctionnalités spécifiques :');
  WriteLn;

  if Animal is TChien then
  begin
    // Transtypage sécurisé vers TChien
    with Animal as TChien do
    begin
      WriteLn('  Race : ', Race);
      Aboyer;
      Garder;
    end;
  end
  else if Animal is TChat then
  begin
    // Transtypage sécurisé vers TChat
    with Animal as TChat do
    begin
      WriteLn('  Couleur : ', Couleur);
      Miauler;
      Ronronner;
    end;
  end
  else if Animal is TPerroquet then
  begin
    // Transtypage sécurisé vers TPerroquet
    with Animal as TPerroquet do
    begin
      WriteLn('  Vocabulaire : ', Vocabulaire, ' mots');
      WriteLn('  Envergure : ', Envergure:0:2, ' m');
      Parler('Bonjour ! Bonjour !');
      Voler;
    end;
  end
  else if Animal is TAigle then
  begin
    // Transtypage sécurisé vers TAigle
    with Animal as TAigle do
    begin
      WriteLn('  Envergure : ', Envergure:0:2, ' m');
      Chasser;
      Voler;
    end;
  end
  else if Animal is TOiseau then
  begin
    // Transtypage vers TOiseau (type parent des oiseaux)
    with Animal as TOiseau do
    begin
      WriteLn('  Envergure : ', Envergure:0:2, ' m');
      Voler;
      ChercherNourriture;
    end;
  end
  else if Animal is TMammifere then
  begin
    // Transtypage vers TMammifere (type parent des mammifères)
    (Animal as TMammifere).Allaiter;
  end;

  WriteLn;
end;

procedure ComparaisonHierarchique(Animal: TAnimal);  
begin  
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('ANALYSE HIERARCHIQUE');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;
  WriteLn('Animal : ', Animal.FNom);
  WriteLn;
  WriteLn('Tests de type dans la hiérarchie :');
  WriteLn('  TAnimal ?       ', Animal is TAnimal);
  WriteLn('  TMammifere ?    ', Animal is TMammifere);
  WriteLn('  TChien ?        ', Animal is TChien);
  WriteLn('  TChat ?         ', Animal is TChat);
  WriteLn('  TOiseau ?       ', Animal is TOiseau);
  WriteLn('  TPerroquet ?    ', Animal is TPerroquet);
  WriteLn('  TAigle ?        ', Animal is TAigle);
  WriteLn;
end;

procedure NourrirAnimaux(Animaux: array of TAnimal);  
var  
  i: Integer;
  Chien: TChien;
  Chat: TChat;
  Oiseau: TOiseau;
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('HEURE DU REPAS');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  for i := 0 to High(Animaux) do
  begin
    WriteLn('→ ', Animaux[i].FNom, ' :');

    // Adaptation du repas selon le type
    if Animaux[i] is TChien then
    begin
      Chien := Animaux[i] as TChien;
      WriteLn('  Croquettes spéciales pour chien ', Chien.Race);
    end
    else if Animaux[i] is TChat then
    begin
      Chat := Animaux[i] as TChat;
      WriteLn('  Pâtée pour chat ', Chat.Couleur);
    end
    else if Animaux[i] is TOiseau then
    begin
      Oiseau := Animaux[i] as TOiseau;
      WriteLn('  Graines adaptées aux oiseaux');
    end
    else
      WriteLn('  Nourriture générique');

    WriteLn;
  end;
end;

{ === Programme principal === }
var
  Rex: TChien;
  Felix: TChat;
  Coco: TPerroquet;
  Zeus: TAigle;

  MesAnimaux: array[0..3] of TAnimal;
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('   DEMONSTRATION DU TRANSTYPAGE (is / as)');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  // Création des animaux
  Rex := TChien.Create('Rex', 5, 'Berger Allemand');
  Felix := TChat.Create('Felix', 3, 'Tigré');
  Coco := TPerroquet.Create('Coco', 12, 0.8, 50);
  Zeus := TAigle.Create('Zeus', 6, 2.2);

  WriteLn;

  // Test 1 : Identification de chaque animal
  IdentifierAnimal(Rex);
  IdentifierAnimal(Felix);
  IdentifierAnimal(Coco);
  IdentifierAnimal(Zeus);

  // Test 2 : Comparaison hiérarchique
  ComparaisonHierarchique(Rex);
  ComparaisonHierarchique(Coco);

  // Test 3 : Traitement polymorphe avec adaptation par type
  MesAnimaux[0] := Rex;
  MesAnimaux[1] := Felix;
  MesAnimaux[2] := Coco;
  MesAnimaux[3] := Zeus;

  NourrirAnimaux(MesAnimaux);

  // Libération
  Rex.Free;
  Felix.Free;
  Coco.Free;
  Zeus.Free;

  WriteLn('═══════════════════════════════════════════════');
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Transtypage traditionnel vs `as`

### Transtypage traditionnel (non sécurisé)

```pascal
var
  Animal: TAnimal;
  Chien: TChien;
begin
  Animal := TChien.Create('Rex', 5);

  // Transtypage "brutal"
  Chien := TChien(Animal);  // ⚠️ Pas de vérification !

  // Si Animal n'est pas un TChien, comportement indéfini !
end;
```

**Danger** : Si l'objet n'est **pas** du bon type, le programme peut **planter** ou avoir un comportement imprévisible.

### Transtypage avec `as` (sécurisé)

```pascal
var
  Animal: TAnimal;
  Chien: TChien;
begin
  Animal := TChien.Create('Rex', 5);

  // Transtypage sécurisé
  Chien := Animal as TChien;  // ✅ Vérifie le type

  // Si Animal n'est pas un TChien, exception levée
end;
```

**Avantage** : Si le transtypage est impossible, une **exception** est levée, ce qui est bien meilleur qu'un plantage mystérieux.

## Tableau comparatif

| Opération | Syntaxe | Résultat en cas d'erreur | Utilisation |
|-----------|---------|--------------------------|-------------|
| **`is`** | `Obj is Type` | Retourne `False` | Vérifier le type |
| **`as`** | `Obj as Type` | Lève une exception | Convertir le type |
| **Transtypage traditionnel** | `Type(Obj)` | Comportement indéfini ⚠️ | À éviter |

## Cas d'usage pratiques

### 1. Traitement différencié dans une boucle

```pascal
procedure TraiterCollection(Liste: array of TAnimal);  
var  
  i: Integer;
begin
  for i := 0 to High(Liste) do
  begin
    // Traitement de base pour tous
    Liste[i].SePresenter;

    // Traitement spécifique selon le type
    if Liste[i] is TChien then
      (Liste[i] as TChien).Garder
    else if Liste[i] is TChat then
      (Liste[i] as TChat).Ronronner
    else if Liste[i] is TOiseau then
      (Liste[i] as TOiseau).Voler;
  end;
end;
```

### 2. Gestionnaire d'événements génériques

```pascal
procedure TForm1.ComposantClick(Sender: TObject);  
begin  
  if Sender is TButton then
  begin
    ShowMessage('Bouton cliqué : ' + (Sender as TButton).Caption);
  end
  else if Sender is TEdit then
  begin
    (Sender as TEdit).SelectAll;
  end
  else if Sender is TCheckBox then
  begin
    if (Sender as TCheckBox).Checked then
      ShowMessage('Case cochée');
  end;
end;
```

### 3. Recherche dans une collection hétérogène

```pascal
function TrouverPremierChien(Animaux: array of TAnimal): TChien;  
var  
  i: Integer;
begin
  Result := nil;

  for i := 0 to High(Animaux) do
  begin
    if Animaux[i] is TChien then
    begin
      Result := Animaux[i] as TChien;
      Exit;
    end;
  end;
end;
```

### 4. Statistiques par type

```pascal
procedure AfficherStatistiques(Animaux: array of TAnimal);  
var  
  i, NbChiens, NbChats, NbOiseaux: Integer;
begin
  NbChiens := 0;
  NbChats := 0;
  NbOiseaux := 0;

  for i := 0 to High(Animaux) do
  begin
    if Animaux[i] is TChien then
      Inc(NbChiens)
    else if Animaux[i] is TChat then
      Inc(NbChats)
    else if Animaux[i] is TOiseau then
      Inc(NbOiseaux);
  end;

  WriteLn('Statistiques :');
  WriteLn('  Chiens : ', NbChiens);
  WriteLn('  Chats : ', NbChats);
  WriteLn('  Oiseaux : ', NbOiseaux);
end;
```

## Pièges à éviter

### Piège 1 : Oublier de vérifier avec `is`

```pascal
var
  Animal: TAnimal;
  Chien: TChien;
begin
  Animal := TChat.Create('Felix', 3);  // C'est un CHAT !

  // ❌ DANGER : pas de vérification
  Chien := Animal as TChien;  // Exception !
end;
```

**Solution** :
```pascal
if Animal is TChien then
  Chien := Animal as TChien
else
  WriteLn('Ce n''est pas un chien');
```

### Piège 2 : Vérifier mais ne pas transtyper

```pascal
var
  Animal: TAnimal;
begin
  Animal := TChien.Create('Rex', 5);

  if Animal is TChien then
  begin
    // ❌ ERREUR : Animal est toujours de type TAnimal
    Animal.Aboyer;  // Méthode Aboyer n'existe pas dans TAnimal
  end;
end;
```

**Solution** :
```pascal
if Animal is TChien then  
begin  
  // ✅ Transtypage nécessaire
  (Animal as TChien).Aboyer;
end;
```

### Piège 3 : Transtypage inutile

```pascal
var
  Chien: TChien;
begin
  Chien := TChien.Create('Rex', 5);

  // ❌ Inutile : Chien est déjà de type TChien
  (Chien as TChien).Aboyer;

  // ✅ Suffisant
  Chien.Aboyer;
end;
```

### Piège 4 : Tester le mauvais ordre dans la hiérarchie

```pascal
if Animal is TAnimal then        // ✓ Toujours vrai !
  WriteLn('Animal')
else if Animal is TMammifere then  // Ne sera jamais atteint
  WriteLn('Mammifère')
else if Animal is TChien then      // Ne sera jamais atteint
  WriteLn('Chien');
```

**Solution** : Tester du **plus spécifique au plus général** :
```pascal
if Animal is TChien then         // Plus spécifique d'abord
  WriteLn('Chien')
else if Animal is TMammifere then
  WriteLn('Mammifère')
else if Animal is TAnimal then   // Plus général à la fin
  WriteLn('Animal');
```

## Optimisation avec variables locales

Au lieu de transtyper plusieurs fois, stockez le résultat :

### ❌ Non optimisé

```pascal
if Animal is TChien then  
begin  
  WriteLn((Animal as TChien).Race);
  (Animal as TChien).Aboyer;
  (Animal as TChien).Garder;
  WriteLn((Animal as TChien).FAge);
  // 4 transtypages !
end;
```

### ✅ Optimisé

```pascal
var
  Chien: TChien;
begin
  if Animal is TChien then
  begin
    Chien := Animal as TChien;  // 1 seul transtypage

    WriteLn(Chien.Race);
    Chien.Aboyer;
    Chien.Garder;
    WriteLn(Chien.FAge);
  end;
end;
```

Ou avec `with` :

```pascal
if Animal is TChien then  
begin  
  with Animal as TChien do
  begin
    WriteLn(Race);
    Aboyer;
    Garder;
  end;
end;
```

## Transtypage et interfaces

Le transtypage fonctionne aussi avec les interfaces (vues au chapitre 12) :

```pascal
var
  Obj: TObject;
  Persistable: IPersistable;
begin
  Obj := MonObjet;

  if Obj is IPersistable then
  begin
    Persistable := Obj as IPersistable;
    Persistable.Sauvegarder;
  end;
end;
```

## Bonnes pratiques

### ✅ À FAIRE

1. **Toujours vérifier avec `is` avant d'utiliser `as`**
   ```pascal
   if Obj is TType then
     (Obj as TType).MethodeSpecifique;
   ```

2. **Stocker le résultat du transtypage si utilisé plusieurs fois**
   ```pascal
   var
     Chien: TChien;
   begin
     if Animal is TChien then
     begin
       Chien := Animal as TChien;
       // Utiliser Chien plusieurs fois
     end;
   end;
   ```

3. **Tester du plus spécifique au plus général**
   ```pascal
   if Animal is TChien then
     // ...
   else if Animal is TMammifere then
     // ...
   else if Animal is TAnimal then
     // ...
   ```

4. **Gérer les exceptions lors du transtypage**
   ```pascal
   try
     Chien := Animal as TChien;
   except
     on E: Exception do
       ShowMessage('Erreur de transtypage : ' + E.Message);
   end;
   ```

### ❌ À ÉVITER

1. **Utiliser le transtypage traditionnel `Type(Obj)`**
   - Préférez toujours `as` pour la sécurité

2. **Transtyper sans vérifier**
   - Risque d'exception

3. **Trop de `is`/`as` = mauvaise conception**
   - Si vous testez constamment les types, revoyez votre architecture
   - Utilisez plutôt le polymorphisme avec des méthodes virtuelles

4. **Oublier le `nil`**
   ```pascal
   if (Animal <> nil) and (Animal is TChien) then
     // ...
   ```

## Quand utiliser `is` et `as` ?

### ✅ Utilisez quand :

- Vous devez accéder à des méthodes/propriétés spécifiques d'un type dérivé
- Vous traitez une collection polymorphe nécessitant un traitement différencié
- Vous gérez des événements avec `Sender: TObject`
- Vous implémentez des fonctionnalités optionnelles (interfaces)

### ⚠️ Évitez quand :

- Vous pouvez utiliser le polymorphisme (méthodes virtuelles)
- Vous testez le type trop souvent (mauvaise conception)
- Vous savez déjà le type exact (pas besoin de vérifier)

**Règle d'or** : Si vous utilisez beaucoup `is` et `as`, demandez-vous si votre hiérarchie de classes et l'utilisation du polymorphisme sont bien conçues.

## Résumé

Les opérateurs `is` et `as` permettent de :
- ✅ **Vérifier le type réel** d'un objet (`is`)
- ✅ **Convertir de manière sécurisée** vers un type spécifique (`as`)
- ✅ Accéder aux fonctionnalités spécifiques des types dérivés
- ✅ Gérer des collections hétérogènes intelligemment

**Syntaxe clé :**
```pascal
if Objet is TypeCible then
  (Objet as TypeCible).MethodeSpecifique;
```

**Principe :**
- `is` pose la question : "Es-tu de ce type ?"
- `as` dit : "Donne-moi cet objet en tant que ce type"

**Analogie finale :** `is` est comme vérifier l'étiquette d'une boîte, `as` est comme ouvrir la boîte en sachant ce qu'elle contient.

Le transtypage est un outil puissant, mais rappelez-vous : un bon usage du polymorphisme réduit le besoin de transtypage !

⏭️ [Inherited et appel au parent](/11-poo-avancee-heritage/08-inherited-appel-parent.md)
