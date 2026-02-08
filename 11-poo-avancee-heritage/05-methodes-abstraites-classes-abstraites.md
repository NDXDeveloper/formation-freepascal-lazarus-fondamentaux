🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.5 Méthodes abstraites et classes abstraites

## Introduction

Les **méthodes abstraites** et les **classes abstraites** sont des outils puissants pour créer des architectures logicielles robustes. Elles permettent de définir un **contrat** que les classes dérivées **doivent** respecter, garantissant ainsi une structure cohérente dans votre hiérarchie de classes.

### Analogie du monde réel

Imaginez un architecte qui dessine les plans d'un bâtiment. Il indique : "Ici, il **doit** y avoir une porte d'entrée" sans préciser si ce sera une porte en bois, en métal ou vitrée. C'est aux constructeurs (classes dérivées) de décider du type exact de porte, mais l'architecte (classe abstraite) impose qu'il y en ait une.

## Qu'est-ce qu'une méthode abstraite ?

Une **méthode abstraite** est une méthode qui :
- Est déclarée dans une classe parent
- N'a **pas d'implémentation** dans cette classe parent
- **Doit obligatoirement** être implémentée par les classes dérivées

### Syntaxe

```pascal
type
  TForme = class
    function CalculerAire: Real; virtual; abstract;
    //                              ↑        ↑
    //                           virtual  abstract
  end;
```

**Important** : Une méthode abstraite est toujours `virtual` ET `abstract`.

## Qu'est-ce qu'une classe abstraite ?

Une **classe abstraite** est une classe qui :
- Contient au moins une méthode abstraite
- Ne peut **pas être instanciée** directement (pas de `Create` possible)
- Sert de **modèle** pour les classes dérivées

```pascal
var
  F: TForme;
begin
  F := TForme.Create;  // ❌ ERREUR : TForme est abstraite !
end;
```

## Pourquoi utiliser des méthodes abstraites ?

### Problème sans méthodes abstraites

Imaginez une hiérarchie de formes géométriques :

```pascal
type
  TForme = class
    function CalculerAire: Real; virtual;
  end;

  TRectangle = class(TForme)
    // Si on oublie de redéfinir CalculerAire, ça compile quand même
  end;

function TForme.CalculerAire: Real;
begin
  Result := 0;  // Valeur par défaut sans sens
  WriteLn('Erreur : méthode non implémentée !');
end;
```

**Problème** : Le développeur peut **oublier** d'implémenter `CalculerAire` dans `TRectangle`, et le programme compile sans erreur mais donne des résultats incorrects.

### Solution avec méthodes abstraites

```pascal
type
  TForme = class
    function CalculerAire: Real; virtual; abstract;
    // Pas d'implémentation = obligation pour les classes dérivées
  end;

  TRectangle = class(TForme)
    // Si on oublie de redéfinir CalculerAire :
    // ❌ ERREUR DE COMPILATION !
    function CalculerAire: Real; override;  // ✅ OK
  end;
```

**Avantage** : Le compilateur **force** l'implémentation, éliminant les oublis.

## Exemple complet : Hiérarchie de formes géométriques

```pascal
program FormesAbstraites;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Classe abstraite : Forme }
  TForme = class
  protected
    FCouleur: string;
  public
    constructor Create(ACouleur: string);

    // Méthodes abstraites : DOIVENT être implémentées
    function CalculerAire: Real; virtual; abstract;
    function CalculerPerimetre: Real; virtual; abstract;
    procedure Dessiner; virtual; abstract;

    // Méthode concrète : peut être utilisée telle quelle
    procedure AfficherInfos; virtual;

    property Couleur: string read FCouleur write FCouleur;
  end;

  { Rectangle : implémentation concrète }
  TRectangle = class(TForme)
  private
    FLargeur: Real;
    FHauteur: Real;
  public
    constructor Create(ACouleur: string; ALargeur, AHauteur: Real);

    // Implémentation OBLIGATOIRE des méthodes abstraites
    function CalculerAire: Real; override;
    function CalculerPerimetre: Real; override;
    procedure Dessiner; override;

    property Largeur: Real read FLargeur;
    property Hauteur: Real read FHauteur;
  end;

  { Cercle : implémentation concrète }
  TCercle = class(TForme)
  private
    FRayon: Real;
  public
    constructor Create(ACouleur: string; ARayon: Real);

    // Implémentation OBLIGATOIRE des méthodes abstraites
    function CalculerAire: Real; override;
    function CalculerPerimetre: Real; override;
    procedure Dessiner; override;

    property Rayon: Real read FRayon;
  end;

  { Triangle : implémentation concrète }
  TTriangle = class(TForme)
  private
    FCote1, FCote2, FCote3: Real;
  public
    constructor Create(ACouleur: string; ACote1, ACote2, ACote3: Real);

    // Implémentation OBLIGATOIRE des méthodes abstraites
    function CalculerAire: Real; override;
    function CalculerPerimetre: Real; override;
    procedure Dessiner; override;

    function EstValide: Boolean;
  end;

{ === TForme === }

constructor TForme.Create(ACouleur: string);
begin
  inherited Create;
  FCouleur := ACouleur;
  WriteLn('[TForme] Création d''une forme ', ACouleur);
end;

procedure TForme.AfficherInfos;
begin
  WriteLn('╔═══════════════════════════════════');
  WriteLn('║ Informations sur la forme');
  WriteLn('╠═══════════════════════════════════');
  WriteLn('║ Couleur : ', FCouleur);
  WriteLn('║ Aire : ', CalculerAire:0:2);  // Appel polymorphe
  WriteLn('║ Périmètre : ', CalculerPerimetre:0:2);  // Appel polymorphe
  WriteLn('╚═══════════════════════════════════');
end;

{ === TRectangle === }

constructor TRectangle.Create(ACouleur: string; ALargeur, AHauteur: Real);
begin
  inherited Create(ACouleur);
  FLargeur := ALargeur;
  FHauteur := AHauteur;
  WriteLn('[TRectangle] Dimensions : ', ALargeur:0:2, ' x ', AHauteur:0:2);
end;

function TRectangle.CalculerAire: Real;
begin
  Result := FLargeur * FHauteur;
end;

function TRectangle.CalculerPerimetre: Real;
begin
  Result := 2 * (FLargeur + FHauteur);
end;

procedure TRectangle.Dessiner;
begin
  WriteLn('Dessin d''un rectangle ', FCouleur);
  WriteLn('┌────────────┐');
  WriteLn('│            │');
  WriteLn('│            │');
  WriteLn('└────────────┘');
end;

{ === TCercle === }

constructor TCercle.Create(ACouleur: string; ARayon: Real);
begin
  inherited Create(ACouleur);
  FRayon := ARayon;
  WriteLn('[TCercle] Rayon : ', ARayon:0:2);
end;

function TCercle.CalculerAire: Real;
begin
  Result := Pi * Sqr(FRayon);
end;

function TCercle.CalculerPerimetre: Real;
begin
  Result := 2 * Pi * FRayon;
end;

procedure TCercle.Dessiner;
begin
  WriteLn('Dessin d''un cercle ', FCouleur);
  WriteLn('    ****    ');
  WriteLn('  *      *  ');
  WriteLn(' *        * ');
  WriteLn('  *      *  ');
  WriteLn('    ****    ');
end;

{ === TTriangle === }

constructor TTriangle.Create(ACouleur: string; ACote1, ACote2, ACote3: Real);
begin
  inherited Create(ACouleur);
  FCote1 := ACote1;
  FCote2 := ACote2;
  FCote3 := ACote3;
  WriteLn('[TTriangle] Côtés : ', ACote1:0:2, ', ', ACote2:0:2, ', ', ACote3:0:2);

  if not EstValide then
    WriteLn('  ⚠️  Attention : triangle invalide (inégalité triangulaire non respectée)');
end;

function TTriangle.EstValide: Boolean;
begin
  // Vérification de l'inégalité triangulaire
  Result := (FCote1 + FCote2 > FCote3) and
            (FCote1 + FCote3 > FCote2) and
            (FCote2 + FCote3 > FCote1);
end;

function TTriangle.CalculerAire: Real;
var
  S: Real;  // Demi-périmètre
begin
  if not EstValide then
  begin
    Result := 0;
    Exit;
  end;

  // Formule de Héron
  S := CalculerPerimetre / 2;
  Result := Sqrt(S * (S - FCote1) * (S - FCote2) * (S - FCote3));
end;

function TTriangle.CalculerPerimetre: Real;
begin
  Result := FCote1 + FCote2 + FCote3;
end;

procedure TTriangle.Dessiner;
begin
  WriteLn('Dessin d''un triangle ', FCouleur);
  WriteLn('      /\      ');
  WriteLn('     /  \     ');
  WriteLn('    /    \    ');
  WriteLn('   /______\   ');
end;

{ === Fonctions de traitement polymorphes === }

procedure AfficherDetailsFormes(Formes: array of TForme);
var
  i: Integer;
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('   AFFICHAGE DES FORMES');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  for i := 0 to High(Formes) do
  begin
    WriteLn('--- Forme ', i + 1, ' ---');
    Formes[i].Dessiner;
    WriteLn;
    Formes[i].AfficherInfos;
    WriteLn;
  end;
end;

function CalculerAireTotale(Formes: array of TForme): Real;
var
  i: Integer;
  Total: Real;
begin
  Total := 0;
  for i := 0 to High(Formes) do
    Total := Total + Formes[i].CalculerAire;  // Appel polymorphe

  Result := Total;
end;

function TrouverPlusGrandeForme(Formes: array of TForme): Integer;
var
  i, IndexMax: Integer;
  AireMax, Aire: Real;
begin
  if Length(Formes) = 0 then
  begin
    Result := -1;
    Exit;
  end;

  IndexMax := 0;
  AireMax := Formes[0].CalculerAire;

  for i := 1 to High(Formes) do
  begin
    Aire := Formes[i].CalculerAire;
    if Aire > AireMax then
    begin
      AireMax := Aire;
      IndexMax := i;
    end;
  end;

  Result := IndexMax;
end;

{ === Programme principal === }
var
  Rectangle: TRectangle;
  Cercle: TCercle;
  Triangle: TTriangle;

  MesFormes: array[0..2] of TForme;
  AireTotal: Real;
  IndexMax: Integer;

  // FormeAbstraite: TForme;  // Pour démonstration
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('   DEMONSTRATION DES CLASSES ABSTRAITES');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  // Tentative d'instanciation d'une classe abstraite (commenté car erreur)
  {
  WriteLn('Tentative de création d''une TForme abstraite :');
  FormeAbstraite := TForme.Create('Gris');
  // ❌ ERREUR : "Cannot create instance of abstract class"
  WriteLn;
  }

  WriteLn('Création des formes concrètes :');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  Rectangle := TRectangle.Create('Rouge', 10.0, 5.0);
  WriteLn;

  Cercle := TCercle.Create('Bleu', 7.0);
  WriteLn;

  Triangle := TTriangle.Create('Vert', 6.0, 8.0, 10.0);
  WriteLn;

  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  // Stockage dans un tableau polymorphe
  MesFormes[0] := Rectangle;
  MesFormes[1] := Cercle;
  MesFormes[2] := Triangle;

  // Affichage de toutes les formes
  AfficherDetailsFormes(MesFormes);

  // Calculs sur l'ensemble des formes
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('   STATISTIQUES');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  AireTotal := CalculerAireTotale(MesFormes);
  WriteLn('Aire totale de toutes les formes : ', AireTotal:0:2);
  WriteLn;

  IndexMax := TrouverPlusGrandeForme(MesFormes);
  WriteLn('La plus grande forme est la forme #', IndexMax + 1);
  WriteLn('avec une aire de ', MesFormes[IndexMax].CalculerAire:0:2);
  WriteLn;

  // Libération
  Rectangle.Free;
  Cercle.Free;
  Triangle.Free;

  WriteLn('═══════════════════════════════════════════════');
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Exemple pratique : Système de persistence

Voici un exemple montrant comment les classes abstraites créent un contrat pour différents systèmes de stockage :

```pascal
program SystemePersistence;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  { Classe abstraite : Système de persistence }
  TPersistence = class
  public
    // Méthodes abstraites : toute implémentation DOIT les fournir
    function Sauvegarder(const Cle, Valeur: string): Boolean; virtual; abstract;
    function Charger(const Cle: string): string; virtual; abstract;
    function Supprimer(const Cle: string): Boolean; virtual; abstract;
    function Existe(const Cle: string): Boolean; virtual; abstract;

    // Méthode concrète : implémentation par défaut
    procedure AfficherStatut; virtual;
  end;

  { Persistence en fichier texte }
  TPersistenceFichier = class(TPersistence)
  private
    FNomFichier: string;
    FListe: TStringList;
  public
    constructor Create(ANomFichier: string);
    destructor Destroy; override;

    function Sauvegarder(const Cle, Valeur: string): Boolean; override;
    function Charger(const Cle: string): string; override;
    function Supprimer(const Cle: string): Boolean; override;
    function Existe(const Cle: string): Boolean; override;
    procedure AfficherStatut; override;
  end;

  { Persistence en mémoire }
  TPersistenceMemoire = class(TPersistence)
  private
    FDonnees: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function Sauvegarder(const Cle, Valeur: string): Boolean; override;
    function Charger(const Cle: string): string; override;
    function Supprimer(const Cle: string): Boolean; override;
    function Existe(const Cle: string): Boolean; override;
    procedure AfficherStatut; override;
  end;

{ === TPersistence === }

procedure TPersistence.AfficherStatut;
begin
  WriteLn('Système de persistence générique');
end;

{ === TPersistenceFichier === }

constructor TPersistenceFichier.Create(ANomFichier: string);
begin
  inherited Create;
  FNomFichier := ANomFichier;
  FListe := TStringList.Create;

  if FileExists(FNomFichier) then
  begin
    FListe.LoadFromFile(FNomFichier);
    WriteLn('✓ Fichier chargé : ', FNomFichier, ' (', FListe.Count, ' entrées)');
  end
  else
    WriteLn('→ Nouveau fichier : ', FNomFichier);
end;

destructor TPersistenceFichier.Destroy;
begin
  FListe.SaveToFile(FNomFichier);
  FListe.Free;
  inherited Destroy;
end;

function TPersistenceFichier.Sauvegarder(const Cle, Valeur: string): Boolean;
var
  Index: Integer;
begin
  Index := FListe.IndexOfName(Cle);

  if Index >= 0 then
    FListe.Values[Cle] := Valeur  // Mise à jour
  else
    FListe.Add(Cle + '=' + Valeur);  // Ajout

  FListe.SaveToFile(FNomFichier);
  Result := True;
end;

function TPersistenceFichier.Charger(const Cle: string): string;
begin
  Result := FListe.Values[Cle];
end;

function TPersistenceFichier.Supprimer(const Cle: string): Boolean;
var
  Index: Integer;
begin
  Index := FListe.IndexOfName(Cle);

  if Index >= 0 then
  begin
    FListe.Delete(Index);
    FListe.SaveToFile(FNomFichier);
    Result := True;
  end
  else
    Result := False;
end;

function TPersistenceFichier.Existe(const Cle: string): Boolean;
begin
  Result := FListe.IndexOfName(Cle) >= 0;
end;

procedure TPersistenceFichier.AfficherStatut;
begin
  WriteLn('📁 Persistence FICHIER');
  WriteLn('   Fichier : ', FNomFichier);
  WriteLn('   Entrées : ', FListe.Count);
end;

{ === TPersistenceMemoire === }

constructor TPersistenceMemoire.Create;
begin
  inherited Create;
  FDonnees := TStringList.Create;
  WriteLn('→ Persistence en mémoire créée');
end;

destructor TPersistenceMemoire.Destroy;
begin
  FDonnees.Free;
  inherited Destroy;
end;

function TPersistenceMemoire.Sauvegarder(const Cle, Valeur: string): Boolean;
var
  Index: Integer;
begin
  Index := FDonnees.IndexOfName(Cle);

  if Index >= 0 then
    FDonnees.Values[Cle] := Valeur
  else
    FDonnees.Add(Cle + '=' + Valeur);

  Result := True;
end;

function TPersistenceMemoire.Charger(const Cle: string): string;
begin
  Result := FDonnees.Values[Cle];
end;

function TPersistenceMemoire.Supprimer(const Cle: string): Boolean;
var
  Index: Integer;
begin
  Index := FDonnees.IndexOfName(Cle);

  if Index >= 0 then
  begin
    FDonnees.Delete(Index);
    Result := True;
  end
  else
    Result := False;
end;

function TPersistenceMemoire.Existe(const Cle: string): Boolean;
begin
  Result := FDonnees.IndexOfName(Cle) >= 0;
end;

procedure TPersistenceMemoire.AfficherStatut;
begin
  WriteLn('💾 Persistence MEMOIRE');
  WriteLn('   Entrées : ', FDonnees.Count);
end;

{ === Fonction polymorphe === }

procedure TesterPersistence(P: TPersistence; const Nom: string);
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('   TEST : ', Nom);
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  P.AfficherStatut;
  WriteLn;

  // Sauvegarde
  WriteLn('→ Sauvegarde de données...');
  P.Sauvegarder('nom', 'Jean Dupont');
  P.Sauvegarder('email', 'jean@example.com');
  P.Sauvegarder('age', '35');
  WriteLn('✓ 3 entrées sauvegardées');
  WriteLn;

  // Lecture
  WriteLn('→ Lecture des données...');
  WriteLn('Nom : ', P.Charger('nom'));
  WriteLn('Email : ', P.Charger('email'));
  WriteLn('Age : ', P.Charger('age'));
  WriteLn;

  // Vérification
  WriteLn('→ Vérification d''existence...');
  WriteLn('Clé "nom" existe ? ', P.Existe('nom'));
  WriteLn('Clé "inexistante" existe ? ', P.Existe('inexistante'));
  WriteLn;

  // Suppression
  WriteLn('→ Suppression de "age"...');
  if P.Supprimer('age') then
    WriteLn('✓ Supprimé')
  else
    WriteLn('✗ Échec');
  WriteLn;

  // Statut final
  WriteLn('→ Statut final :');
  P.AfficherStatut;
  WriteLn;
end;

{ === Programme principal === }
var
  PersistenceFichier: TPersistenceFichier;
  PersistenceMemoire: TPersistenceMemoire;
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('   SYSTEME DE PERSISTENCE ABSTRAIT');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  // Test avec fichier
  PersistenceFichier := TPersistenceFichier.Create('donnees.txt');
  TesterPersistence(PersistenceFichier, 'FICHIER');
  PersistenceFichier.Free;

  // Test avec mémoire
  PersistenceMemoire := TPersistenceMemoire.Create;
  TesterPersistence(PersistenceMemoire, 'MEMOIRE');
  PersistenceMemoire.Free;

  WriteLn('═══════════════════════════════════════════════');
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Classes abstraites vs classes concrètes

### Classe concrète

```pascal
type
  TAnimal = class
    procedure Manger; virtual;
  end;

var
  A: TAnimal;
begin
  A := TAnimal.Create;  // ✅ OK : peut être instanciée
  A.Manger;
  A.Free;
end;
```

### Classe abstraite

```pascal
type
  TAnimal = class
    procedure Manger; virtual; abstract;  // ← Méthode abstraite
  end;

var
  A: TAnimal;
begin
  A := TAnimal.Create;  // ❌ ERREUR : classe abstraite !

  // Il faut utiliser une classe dérivée concrète
  A := TChien.Create;   // ✅ OK si TChien implémente Manger
  A.Manger;
  A.Free;
end;
```

## Règles importantes

### 1. Héritage des méthodes abstraites

Une méthode abstraite reste abstraite jusqu'à ce qu'elle soit implémentée :

```pascal
type
  TA = class
    procedure Test; virtual; abstract;
  end;

  TB = class(TA)
    // Test reste abstraite ici aussi
    // TB est donc abstraite également
  end;

  TC = class(TB)
    procedure Test; override;  // Maintenant implémentée
    // TC devient concrète
  end;
```

### 2. Une classe avec au moins une méthode abstraite est abstraite

```pascal
type
  TMaClasse = class
    procedure MethodeNormale;
    procedure MethodeAbstraite; virtual; abstract;  // ← Rend la classe abstraite
  end;

var
  M: TMaClasse;
begin
  M := TMaClasse.Create;  // ❌ ERREUR : classe abstraite
end;
```

### 3. Les méthodes abstraites doivent être virtuelles

```pascal
type
  TMaClasse = class
    procedure Test; abstract;  // ❌ ERREUR : abstract implique virtual
    procedure Test; virtual; abstract;  // ✅ OK
  end;
```

### 4. Pas d'implémentation pour les méthodes abstraites

```pascal
type
  TBase = class
    procedure Test; virtual; abstract;
  end;

// PAS d'implémentation ici
// procedure TBase.Test;
// begin
//   // RIEN
// end;
```

## Avantages des classes abstraites

### ✅ Contrat garanti

Les classes dérivées **doivent** implémenter toutes les méthodes abstraites :

```pascal
type
  TAnimal = class
    procedure FaireDuBruit; virtual; abstract;
  end;

  TChien = class(TAnimal)
    // Si on oublie FaireDuBruit → ERREUR DE COMPILATION
  end;
```

### ✅ Documentation de l'intention

La classe abstraite documente clairement ce que les dérivées doivent faire :

```pascal
type
  IExportateur = class
    { Exporte les données au format spécifié.
      Les classes dérivées doivent implémenter le format exact. }
    procedure Exporter(const Donnees: string); virtual; abstract;
  end;
```

### ✅ Architecture propre

Sépare l'**interface** (ce qui doit être fait) de l'**implémentation** (comment le faire) :

```pascal
// Interface commune
type
  TBaseDeDonnees = class
    procedure Connecter; virtual; abstract;
    procedure Deconnecter; virtual; abstract;
    function ExecuterRequete(SQL: string): Boolean; virtual; abstract;
  end;

// Implémentations spécifiques
type
  TMySQL = class(TBaseDeDonnees)
    // Implémentation spécifique MySQL
  end;

  TPostgreSQL = class(TBaseDeDonnees)
    // Implémentation spécifique PostgreSQL
  end;
```

## Quand utiliser les classes abstraites ?

### ✅ Utilisez quand :

1. **Vous créez une hiérarchie où certaines méthodes doivent être implémentées**
   - Garantir que toutes les formes ont une méthode `CalculerAire`

2. **Vous voulez forcer un contrat**
   - Toutes les classes de connexion doivent avoir `Connecter` et `Deconnecter`

3. **Vous créez un framework ou une bibliothèque**
   - Définir l'interface que les utilisateurs doivent implémenter

4. **La classe parent n'a pas d'implémentation par défaut sensée**
   - Comment calculer l'aire d'une "forme générique" ?

### ❌ N'utilisez PAS quand :

1. **Vous avez une implémentation par défaut raisonnable**
   - Utilisez `virtual` simple au lieu de `abstract`

2. **Vous voulez pouvoir instancier la classe parent**
   - Rendez toutes les méthodes concrètes

3. **Vous avez juste besoin d'une interface**
   - En Pascal, utilisez plutôt les `interface` (vues au chapitre 12)

## Classes abstraites vs Interfaces

### Classe abstraite

```pascal
type
  TAnimal = class
    FNom: string;  // ✅ Peut avoir des attributs
    procedure Manger; virtual;  // ✅ Peut avoir des méthodes concrètes
    procedure FaireDuBruit; virtual; abstract;  // ✅ Peut avoir des méthodes abstraites
  end;
```

**Caractéristiques :**
- Peut contenir des attributs
- Peut mélanger méthodes concrètes et abstraites
- Héritage simple uniquement
- Peut avoir un constructeur

### Interface (aperçu)

```pascal
type
  IAnimal = interface
    procedure Manger;
    procedure FaireDuBruit;
  end;
```

**Caractéristiques :**
- Pas d'attributs
- Toutes les méthodes sont abstraites (implicitement)
- Héritage multiple possible
- Pas de constructeur

**Nous verrons les interfaces en détail au chapitre 12.**

## Bonnes pratiques

### ✅ À FAIRE

1. **Nommer clairement les classes abstraites**
   ```pascal
   TBaseForm, TAbstractShape, TGenericConnection
   ```

2. **Documenter les méthodes abstraites**
   ```pascal
   { Calcule le prix total incluant les taxes.
     Les classes dérivées doivent appliquer les règles fiscales appropriées. }
   function CalculerPrixTTC: Real; virtual; abstract;
   ```

3. **Fournir des méthodes concrètes utiles**
   ```pascal
   type
     TForme = class
       function CalculerAire: Real; virtual; abstract;

       // Méthode concrète utilisant la méthode abstraite
       function EstPlusGrandeQue(Autre: TForme): Boolean;
     end;

   function TForme.EstPlusGrandeQue(Autre: TForme): Boolean;
   begin
     Result := Self.CalculerAire > Autre.CalculerAire;
   end;
   ```

4. **Grouper les méthodes abstraites en haut**
   ```pascal
   type
     TMaClasse = class
     public
       // Méthodes abstraites d'abord
       procedure MethodeAbstraite1; virtual; abstract;
       procedure MethodeAbstraite2; virtual; abstract;

       // Puis méthodes concrètes
       procedure MethodeConcrete;
     end;
   ```

### ❌ À ÉVITER

1. **Trop de méthodes abstraites**
   - Si presque tout est abstrait, utilisez une interface

2. **Classe abstraite sans raison**
   - N'ajoutez `abstract` que si nécessaire

3. **Oublier de documenter**
   - Expliquez ce que les méthodes abstraites doivent faire

## Résumé

Les méthodes et classes abstraites permettent de :
- ✅ Définir un **contrat** que les classes dérivées doivent respecter
- ✅ **Forcer l'implémentation** de certaines méthodes (détection à la compilation)
- ✅ Créer des **architectures robustes** et bien structurées
- ✅ Séparer **interface** et **implémentation**

**Syntaxe clé :**
```pascal
type
  TBase = class
    procedure MaMethode; virtual; abstract;
  end;

  TDerivee = class(TBase)
    procedure MaMethode; override;  // OBLIGATOIRE
  end;
```

**Règle d'or :**
- Méthode abstraite = "Je définis **quoi** faire, mais pas **comment**"
- Classe abstraite = "Je suis un modèle, pas un objet utilisable directement"

**Analogie :** Une classe abstraite est comme un contrat : elle définit ce qui doit être fait, mais c'est aux classes concrètes de le faire réellement !

Dans la section suivante, nous avons exploré le polymorphisme qui devient vraiment puissant avec les classes abstraites !

⏭️ [Polymorphisme](/11-poo-avancee-heritage/06-polymorphisme.md)
