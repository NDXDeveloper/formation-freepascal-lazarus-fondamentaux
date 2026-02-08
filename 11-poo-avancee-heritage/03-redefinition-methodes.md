🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.3 Redéfinition de méthodes

## Introduction

La **redéfinition de méthodes** (ou *method overriding* en anglais) est l'un des mécanismes les plus puissants de la programmation orientée objet. Elle permet à une classe dérivée de fournir sa **propre version** d'une méthode héritée de sa classe parent, tout en conservant la même signature (nom et paramètres).

## Le problème : comportements différents selon les types

Imaginons que nous gérons différents types de comptes bancaires. Tous ont une méthode `Afficher()`, mais chaque type doit afficher des informations différentes :

```pascal
type
  TCompte = class
  protected
    FNumero: string;
    FSolde: Real;
  public
    procedure Afficher;
  end;

  TCompteEpargne = class(TCompte)
  private
    FTauxInteret: Real;
  public
    // On veut une version différente de Afficher() ici
  end;

  TCompteCourant = class(TCompte)
  private
    FDecouvert: Real;
  public
    // Et encore une version différente ici
  end;
```

**Question** : Comment faire pour que chaque classe affiche ses informations spécifiques ?

**Réponse** : La redéfinition de méthodes !

## Première tentative : le masquage (à éviter)

### Sans mot-clé spécial

Vous pourriez simplement créer une nouvelle méthode avec le même nom :

```pascal
type
  TCompte = class
  public
    procedure Afficher;
  end;

  TCompteEpargne = class(TCompte)
  public
    procedure Afficher;  // Même nom, mais PAS de redéfinition
  end;

procedure TCompte.Afficher;
begin
  WriteLn('Compte de base');
end;

procedure TCompteEpargne.Afficher;
begin
  WriteLn('Compte épargne');
end;
```

**Problème avec cette approche** :

```pascal
var
  Compte: TCompte;
  Epargne: TCompteEpargne;
begin
  Epargne := TCompteEpargne.Create;
  Compte := Epargne;  // Un TCompteEpargne vu comme un TCompte

  Compte.Afficher;    // ❌ Affiche "Compte de base" au lieu de "Compte épargne"

  Compte.Free;
end;
```

La méthode appelée dépend du **type de la variable**, pas du **type réel de l'objet**. C'est ce qu'on appelle le **masquage** (*hiding*), et ce n'est généralement pas ce qu'on veut.

## La bonne solution : la redéfinition avec `virtual` et `override`

Pour obtenir une vraie redéfinition de méthode, vous devez utiliser deux mots-clés :

1. **`virtual`** dans la classe parent : indique que la méthode peut être redéfinie
2. **`override`** dans la classe dérivée : indique qu'on redéfinit la méthode

### Syntaxe correcte

```pascal
type
  TCompte = class
  public
    procedure Afficher; virtual;  // ← Méthode virtuelle
  end;

  TCompteEpargne = class(TCompte)
  public
    procedure Afficher; override;  // ← Redéfinition
  end;

procedure TCompte.Afficher;
begin
  WriteLn('Compte de base');
end;

procedure TCompteEpargne.Afficher;
begin
  WriteLn('Compte épargne');
end;
```

**Maintenant, ça fonctionne** :

```pascal
var
  Compte: TCompte;
  Epargne: TCompteEpargne;
begin
  Epargne := TCompteEpargne.Create;
  Compte := Epargne;

  Compte.Afficher;    // ✅ Affiche "Compte épargne" !

  Compte.Free;
end;
```

La méthode appelée correspond maintenant au **type réel de l'objet**, pas au type de la variable. C'est le **polymorphisme** !

## Exemple complet et détaillé

Voici un exemple complet qui montre la différence entre masquage et redéfinition :

```pascal
program RedefinitionMethodes;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Classe de base : Animal }
  TAnimal = class
  protected
    FNom: string;
  public
    constructor Create(ANom: string);

    // Méthode NON virtuelle (masquage possible)
    procedure SePresenter;

    // Méthode virtuelle (redéfinition possible)
    procedure FaireDuBruit; virtual;

    // Méthode virtuelle
    procedure Manger; virtual;
  end;

  { Classe dérivée : Chien }
  TChien = class(TAnimal)
  private
    FRace: string;
  public
    constructor Create(ANom, ARace: string);

    // Masquage de SePresenter (pas de override)
    procedure SePresenter;

    // Redéfinition de FaireDuBruit
    procedure FaireDuBruit; override;

    // Redéfinition de Manger
    procedure Manger; override;
  end;

  { Classe dérivée : Chat }
  TChat = class(TAnimal)
  private
    FCouleur: string;
  public
    constructor Create(ANom, ACouleur: string);

    // Masquage de SePresenter
    procedure SePresenter;

    // Redéfinition de FaireDuBruit
    procedure FaireDuBruit; override;

    // Redéfinition de Manger
    procedure Manger; override;
  end;

{ === Implémentation de TAnimal === }

constructor TAnimal.Create(ANom: string);
begin
  inherited Create;
  FNom := ANom;
end;

procedure TAnimal.SePresenter;
begin
  WriteLn('Je suis un animal qui s''appelle ', FNom);
end;

procedure TAnimal.FaireDuBruit;
begin
  WriteLn('[Animal générique fait du bruit]');
end;

procedure TAnimal.Manger;
begin
  WriteLn(FNom, ' mange de la nourriture générique.');
end;

{ === Implémentation de TChien === }

constructor TChien.Create(ANom, ARace: string);
begin
  inherited Create(ANom);
  FRace := ARace;
end;

procedure TChien.SePresenter;
begin
  WriteLn('Je suis un chien ', FRace, ' qui s''appelle ', FNom);
end;

procedure TChien.FaireDuBruit;
begin
  WriteLn(FNom, ' aboie : Wouaf wouaf !');
end;

procedure TChien.Manger;
begin
  WriteLn(FNom, ' le chien mange des croquettes.');
end;

{ === Implémentation de TChat === }

constructor TChat.Create(ANom, ACouleur: string);
begin
  inherited Create(ANom);
  FCouleur := ACouleur;
end;

procedure TChat.SePresenter;
begin
  WriteLn('Je suis un chat ', FCouleur, ' qui s''appelle ', FNom);
end;

procedure TChat.FaireDuBruit;
begin
  WriteLn(FNom, ' miaule : Miaou miaou !');
end;

procedure TChat.Manger;
begin
  WriteLn(FNom, ' le chat mange du poisson.');
end;

{ === Programme principal === }

procedure TesterAvecType(Animal: TAnimal);
begin
  WriteLn('--- Test avec variable de type TAnimal ---');

  // SePresenter n'est PAS virtuelle : masquage
  WriteLn('SePresenter (masquée) :');
  Animal.SePresenter;  // Appelle TOUJOURS TAnimal.SePresenter

  WriteLn;

  // FaireDuBruit EST virtuelle : redéfinition
  WriteLn('FaireDuBruit (redéfinie) :');
  Animal.FaireDuBruit;  // Appelle la version du type REEL

  WriteLn;

  // Manger EST virtuelle : redéfinition
  WriteLn('Manger (redéfinie) :');
  Animal.Manger;  // Appelle la version du type REEL

  WriteLn;
end;

var
  MonChien: TChien;
  MonChat: TChat;
  UnAnimal: TAnimal;
begin
  WriteLn('=== DEMONSTRATION : MASQUAGE vs REDEFINITION ===');
  WriteLn;

  MonChien := TChien.Create('Rex', 'Berger Allemand');
  MonChat := TChat.Create('Félix', 'Tigré');

  WriteLn('========================================');
  WriteLn('TEST 1 : Utilisation directe (type exact)');
  WriteLn('========================================');
  WriteLn;

  WriteLn('--- Appel direct sur MonChien (type TChien) ---');
  MonChien.SePresenter;    // Version TChien
  MonChien.FaireDuBruit;   // Version TChien
  MonChien.Manger;         // Version TChien
  WriteLn;

  WriteLn('--- Appel direct sur MonChat (type TChat) ---');
  MonChat.SePresenter;     // Version TChat
  MonChat.FaireDuBruit;    // Version TChat
  MonChat.Manger;          // Version TChat
  WriteLn;

  WriteLn('========================================');
  WriteLn('TEST 2 : Via variable de type TAnimal');
  WriteLn('========================================');
  WriteLn;

  WriteLn('***** Chien vu comme Animal *****');
  UnAnimal := MonChien;
  TesterAvecType(UnAnimal);

  WriteLn('***** Chat vu comme Animal *****');
  UnAnimal := MonChat;
  TesterAvecType(UnAnimal);

  WriteLn('========================================');
  WriteLn('CONCLUSION');
  WriteLn('========================================');
  WriteLn('• Méthode NON virtuelle (SePresenter) :');
  WriteLn('  → Masquage : appelle toujours la version du TYPE DE LA VARIABLE');
  WriteLn;
  WriteLn('• Méthode virtuelle/override (FaireDuBruit, Manger) :');
  WriteLn('  → Redéfinition : appelle la version du TYPE REEL de l''objet');
  WriteLn('  → C''est le POLYMORPHISME !');
  WriteLn;

  MonChien.Free;
  MonChat.Free;

  ReadLn;
end.
```

## Résultat de l'exécution

```
=== DEMONSTRATION : MASQUAGE vs REDEFINITION ===

========================================
TEST 1 : Utilisation directe (type exact)
========================================

--- Appel direct sur MonChien (type TChien) ---
Je suis un chien Berger Allemand qui s'appelle Rex
Rex aboie : Wouaf wouaf !
Rex le chien mange des croquettes.

--- Appel direct sur MonChat (type TChat) ---
Je suis un chat Tigré qui s'appelle Félix
Félix miaule : Miaou miaou !
Félix le chat mange du poisson.

========================================
TEST 2 : Via variable de type TAnimal
========================================

***** Chien vu comme Animal *****
--- Test avec variable de type TAnimal ---
SePresenter (masquée) :
Je suis un animal qui s'appelle Rex      ← Version TAnimal appelée

FaireDuBruit (redéfinie) :
Rex aboie : Wouaf wouaf !                ← Version TChien appelée ✓

Manger (redéfinie) :
Rex le chien mange des croquettes.       ← Version TChien appelée ✓

***** Chat vu comme Animal *****
--- Test avec variable de type TAnimal ---
SePresenter (masquée) :
Je suis un animal qui s'appelle Félix    ← Version TAnimal appelée

FaireDuBruit (redéfinie) :
Félix miaule : Miaou miaou !             ← Version TChat appelée ✓

Manger (redéfinie) :
Félix le chat mange du poisson.          ← Version TChat appelée ✓
```

## Comprendre `virtual` et `override`

### Le mot-clé `virtual`

```pascal
procedure MaMethode; virtual;
```

Dans la classe **parent**, `virtual` signifie :
- "Cette méthode peut être redéfinie dans les classes dérivées"
- "Lors de l'appel, utilisez le type réel de l'objet, pas le type de la variable"
- "Active le mécanisme de liaison dynamique (*late binding*)"

### Le mot-clé `override`

```pascal
procedure MaMethode; override;
```

Dans la classe **dérivée**, `override` signifie :
- "Je redéfinis la méthode virtuelle héritée"
- "Ma version remplace celle du parent"
- "La signature (nom et paramètres) doit être identique"

### Règles importantes

1. **On ne peut pas `override` une méthode non `virtual`**
   ```pascal
   type
     TParent = class
       procedure Test;  // Pas virtual
     end;

     TEnfant = class(TParent)
       procedure Test; override;  // ❌ ERREUR de compilation
     end;
   ```

2. **La signature doit être identique**
   ```pascal
   type
     TParent = class
       procedure Calcul(X: Integer); virtual;
     end;

     TEnfant = class(TParent)
       procedure Calcul(X: Real); override;  // ❌ ERREUR : type différent
     end;
   ```

3. **Une méthode `override` reste virtuelle**
   ```pascal
   type
     TA = class
       procedure Test; virtual;
     end;

     TB = class(TA)
       procedure Test; override;  // Redéfinit et reste virtuelle
     end;

     TC = class(TB)
       procedure Test; override;  // Peut encore redéfinir
     end;
   ```

## Utilisation d'`inherited` dans les méthodes redéfinies

Vous pouvez appeler la version parent d'une méthode redéfinie avec `inherited` :

```pascal
type
  TCompte = class
  protected
    FSolde: Real;
  public
    procedure Deposer(Montant: Real); virtual;
  end;

  TCompteEpargne = class(TCompte)
  private
    FTauxInteret: Real;
  public
    procedure Deposer(Montant: Real); override;
  end;

procedure TCompte.Deposer(Montant: Real);
begin
  FSolde := FSolde + Montant;
  WriteLn('Dépôt de ', Montant:0:2, ' €');
end;

procedure TCompteEpargne.Deposer(Montant: Real);
begin
  // D'abord, on fait le dépôt normal
  inherited Deposer(Montant);  // Appelle TCompte.Deposer

  // Ensuite, on ajoute le comportement spécifique
  WriteLn('+ Application du taux d''intérêt');
  FSolde := FSolde * (1 + FTauxInteret / 100);
end;
```

**Résultat** :
```pascal
var
  Epargne: TCompteEpargne;
begin
  Epargne := TCompteEpargne.Create;
  Epargne.FTauxInteret := 2.5;
  Epargne.Deposer(1000);
  // Affiche :
  // Dépôt de 1000.00 €
  // + Application du taux d'intérêt
end.
```

## Exemple pratique : système de formes géométriques

Voici un exemple classique et très utile pour comprendre la redéfinition :

```pascal
program FormesGeometriques;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Classe de base abstraite }
  TForme = class
  protected
    FCouleur: string;
    FNom: string;
  public
    constructor Create(ACouleur: string);

    // Méthodes virtuelles à redéfinir
    function CalculerAire: Real; virtual;
    function CalculerPerimetre: Real; virtual;
    procedure Afficher; virtual;

    property Nom: string read FNom;
    property Couleur: string read FCouleur write FCouleur;
  end;

  { Rectangle }
  TRectangle = class(TForme)
  private
    FLargeur, FHauteur: Real;
  public
    constructor Create(ACouleur: string; ALargeur, AHauteur: Real);
    function CalculerAire: Real; override;
    function CalculerPerimetre: Real; override;
    procedure Afficher; override;
  end;

  { Cercle }
  TCercle = class(TForme)
  private
    FRayon: Real;
  public
    constructor Create(ACouleur: string; ARayon: Real);
    function CalculerAire: Real; override;
    function CalculerPerimetre: Real; override;
    procedure Afficher; override;
  end;

  { Triangle }
  TTriangle = class(TForme)
  private
    FCote1, FCote2, FCote3: Real;
  public
    constructor Create(ACouleur: string; ACote1, ACote2, ACote3: Real);
    function CalculerAire: Real; override;
    function CalculerPerimetre: Real; override;
    procedure Afficher; override;
  end;

{ === TForme === }

constructor TForme.Create(ACouleur: string);
begin
  inherited Create;
  FCouleur := ACouleur;
  FNom := 'Forme générique';
end;

function TForme.CalculerAire: Real;
begin
  Result := 0;
  WriteLn('Aire non définie pour une forme générique');
end;

function TForme.CalculerPerimetre: Real;
begin
  Result := 0;
  WriteLn('Périmètre non défini pour une forme générique');
end;

procedure TForme.Afficher;
begin
  WriteLn('Forme de couleur ', FCouleur);
end;

{ === TRectangle === }

constructor TRectangle.Create(ACouleur: string; ALargeur, AHauteur: Real);
begin
  inherited Create(ACouleur);
  FLargeur := ALargeur;
  FHauteur := AHauteur;
  FNom := 'Rectangle';
end;

function TRectangle.CalculerAire: Real;
begin
  Result := FLargeur * FHauteur;
end;

function TRectangle.CalculerPerimetre: Real;
begin
  Result := 2 * (FLargeur + FHauteur);
end;

procedure TRectangle.Afficher;
begin
  WriteLn('=== RECTANGLE ===');
  WriteLn('Couleur : ', FCouleur);
  WriteLn('Dimensions : ', FLargeur:0:2, ' x ', FHauteur:0:2);
  WriteLn('Aire : ', CalculerAire:0:2);
  WriteLn('Périmètre : ', CalculerPerimetre:0:2);
  WriteLn;
end;

{ === TCercle === }

constructor TCercle.Create(ACouleur: string; ARayon: Real);
begin
  inherited Create(ACouleur);
  FRayon := ARayon;
  FNom := 'Cercle';
end;

function TCercle.CalculerAire: Real;
begin
  Result := Pi * Sqr(FRayon);
end;

function TCercle.CalculerPerimetre: Real;
begin
  Result := 2 * Pi * FRayon;
end;

procedure TCercle.Afficher;
begin
  WriteLn('=== CERCLE ===');
  WriteLn('Couleur : ', FCouleur);
  WriteLn('Rayon : ', FRayon:0:2);
  WriteLn('Aire : ', CalculerAire:0:2);
  WriteLn('Périmètre : ', CalculerPerimetre:0:2);
  WriteLn;
end;

{ === TTriangle === }

constructor TTriangle.Create(ACouleur: string; ACote1, ACote2, ACote3: Real);
begin
  inherited Create(ACouleur);
  FCote1 := ACote1;
  FCote2 := ACote2;
  FCote3 := ACote3;
  FNom := 'Triangle';
end;

function TTriangle.CalculerAire: Real;
var
  S: Real;  // Demi-périmètre
begin
  // Formule de Héron
  S := CalculerPerimetre / 2;
  Result := Sqrt(S * (S - FCote1) * (S - FCote2) * (S - FCote3));
end;

function TTriangle.CalculerPerimetre: Real;
begin
  Result := FCote1 + FCote2 + FCote3;
end;

procedure TTriangle.Afficher;
begin
  WriteLn('=== TRIANGLE ===');
  WriteLn('Couleur : ', FCouleur);
  WriteLn('Côtés : ', FCote1:0:2, ', ', FCote2:0:2, ', ', FCote3:0:2);
  WriteLn('Aire : ', CalculerAire:0:2);
  WriteLn('Périmètre : ', CalculerPerimetre:0:2);
  WriteLn;
end;

{ === Programme principal === }

procedure AfficherInfosForme(Forme: TForme);
begin
  // Grâce au polymorphisme, la bonne méthode Afficher est appelée
  Forme.Afficher;
end;

procedure CalculerAireTotale(Formes: array of TForme);
var
  AireTotal: Real;
  i: Integer;
begin
  AireTotal := 0;
  for i := 0 to High(Formes) do
    AireTotal := AireTotal + Formes[i].CalculerAire;

  WriteLn('Aire totale de toutes les formes : ', AireTotal:0:2);
end;

var
  Rectangle: TRectangle;
  Cercle: TCercle;
  Triangle: TTriangle;
  Formes: array[0..2] of TForme;
begin
  WriteLn('=== DEMONSTRATION DES FORMES GEOMETRIQUES ===');
  WriteLn;

  // Création des formes
  Rectangle := TRectangle.Create('Rouge', 5.0, 3.0);
  Cercle := TCercle.Create('Bleu', 4.0);
  Triangle := TTriangle.Create('Vert', 3.0, 4.0, 5.0);

  // Affichage direct
  WriteLn('--- Affichage direct ---');
  Rectangle.Afficher;
  Cercle.Afficher;
  Triangle.Afficher;

  // Démonstration du polymorphisme
  WriteLn('--- Via polymorphisme (type TForme) ---');
  Formes[0] := Rectangle;
  Formes[1] := Cercle;
  Formes[2] := Triangle;

  AfficherInfosForme(Formes[0]);
  AfficherInfosForme(Formes[1]);
  AfficherInfosForme(Formes[2]);

  CalculerAireTotale(Formes);

  // Libération
  Rectangle.Free;
  Cercle.Free;
  Triangle.Free;

  ReadLn;
end.
```

## Bonnes pratiques

### ✅ À FAIRE

1. **Toujours utiliser `virtual` et `override` pour la redéfinition**
   - Évitez le masquage accidentel
   - Activez le polymorphisme

2. **Déclarer `virtual` dans la classe de base quand nécessaire**
   ```pascal
   type
     TBase = class
       procedure MaMethode; virtual;  // Permet la redéfinition
     end;
   ```

3. **Utiliser `inherited` quand on veut étendre le comportement**
   ```pascal
   procedure TEnfant.MaMethode;
   begin
     inherited MaMethode;  // Comportement du parent
     // + comportement spécifique
   end;
   ```

4. **Documenter les méthodes virtuelles**
   ```pascal
   { Calcule l'aire de la forme.
     Les classes dérivées doivent redéfinir cette méthode. }
   function CalculerAire: Real; virtual;
   ```

### ❌ À ÉVITER

1. **Oublier `virtual` dans la classe parent**
   - Résultat : masquage au lieu de redéfinition
   - Pas de polymorphisme

2. **Changer la signature lors de la redéfinition**
   ```pascal
   // Parent
   procedure Test(X: Integer); virtual;

   // Enfant - ERREUR !
   procedure Test(X: Real); override;  // Type différent
   ```

3. **Redéfinir sans raison**
   - Ne redéfinissez que si le comportement doit vraiment changer
   - Sinon, utilisez la méthode héritée telle quelle

## Tableau récapitulatif

| Concept | Syntaxe Parent | Syntaxe Enfant | Polymorphisme | Usage |
|---------|---------------|----------------|---------------|-------|
| **Méthode normale** | `procedure Test;` | `procedure Test;` | ❌ Non | Comportement fixe |
| **Masquage** | `procedure Test;` | `procedure Test;` | ❌ Non | À éviter généralement |
| **Redéfinition** | `procedure Test; virtual;` | `procedure Test; override;` | ✅ Oui | Comportement variable |

## Résumé

La redéfinition de méthodes permet de :
- ✅ Personnaliser le comportement des classes dérivées
- ✅ Activer le polymorphisme (appel dynamique)
- ✅ Créer des hiérarchies flexibles et extensibles
- ✅ Écrire du code générique qui fonctionne avec tous les types dérivés

**Mémo :**
- **Parent** : `virtual` = "peut être redéfini"
- **Enfant** : `override` = "je redéfinis"
- **Enfant** : `inherited` = "j'appelle la version du parent"

**Règle d'or :** Si vous voulez que les classes dérivées puissent changer le comportement d'une méthode, déclarez-la `virtual` dans la classe parent !

Dans la prochaine section, nous verrons les **méthodes virtuelles** et `override` en détail, ainsi que les **méthodes abstraites** qui **obligent** les classes dérivées à fournir une implémentation.

⏭️ [Méthodes virtuelles et override](/11-poo-avancee-heritage/04-methodes-virtuelles-override.md)
