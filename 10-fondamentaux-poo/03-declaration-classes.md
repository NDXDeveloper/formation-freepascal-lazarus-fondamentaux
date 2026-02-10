🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.3 Déclaration de classes

## Structure générale d'une déclaration de classe

Une classe se déclare dans la section `type` d'un programme ou d'une unité. Voici sa structure complète :

```pascal
type
  TNomClasse = class
  private
    // Attributs privés
  protected
    // Attributs et méthodes protégés
  public
    // Constructeurs et destructeurs
    // Méthodes publiques
  published
    // Propriétés publiées (composants visuels)
  end;
```

## Syntaxe détaillée

### Déclaration de base

```pascal
type
  TNomClasse = class
    // Membres de la classe
  end;
```

**Éléments clés :**
- `type` : section où l'on déclare les types
- `TNomClasse` : nom de la classe (convention : commence par `T`)
- `class` : mot-clé indiquant qu'il s'agit d'une classe
- `end;` : fin de la déclaration (notez le point-virgule)

### Déclaration avec sections de visibilité

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FPrenom: string;
    FAge: Integer;
  public
    procedure Afficher;
    function ObtenirNomComplet: string;
  end;
```

**Organisation :**
- Chaque section de visibilité (`private`, `protected`, `public`, `published`) est facultative
- L'ordre conventionnel : `private` → `protected` → `public` → `published`
- On peut avoir plusieurs sections du même type, mais c'est déconseillé

## Convention de nommage Pascal

### Pour les classes

```pascal
type
  TVoiture = class        // ✓ Commence par T majuscule
  TCompteBancaire = class // ✓ Mots collés, majuscules (PascalCase)
  TGestionnaireUtilisateurs = class  // ✓ Nom descriptif
```

**Règles :**
- Toujours commencer par **T** (pour "Type")
- Utiliser le **PascalCase** (chaque mot commence par une majuscule)
- Choisir un nom **descriptif** et **au singulier**

### Pour les attributs

```pascal
private
  FNom: string;           // ✓ Commence par F majuscule
  FAge: Integer;          // ✓ Camel case après F
  FDateNaissance: TDateTime;  // ✓ Descriptif
```

**Règles :**
- Toujours commencer par **F** (pour "Field", champ)
- Utiliser le **PascalCase** après le F
- Les attributs sont **privés** (dans la section `private`)

### Pour les méthodes

```pascal
public
  procedure Afficher;                    // ✓ Verbe d'action
  function CalculerAge: Integer;         // ✓ Verbe + complément
  procedure DefinirNom(const Valeur: string);  // ✓ Définir/Obtenir
  function ObtenirPrenom: string;        // ✓ pour get/set
```

**Règles :**
- Commencer par une **majuscule** (PascalCase)
- Utiliser un **verbe d'action** pour les procédures
- Préfixes courants : `Definir`, `Obtenir`, `Calculer`, `Afficher`, `Verifier`

## Déclaration des attributs

### Syntaxe

```pascal
type
  TExemple = class
  private
    FAttribut: Type;
  end;
```

### Types d'attributs courants

```pascal
type
  TPersonne = class
  private
    // Types simples
    FNom: string;
    FAge: Integer;
    FSalaire: Real;
    FEstActif: Boolean;

    // Types structurés
    FAdresse: string;
    FDateNaissance: TDateTime;

    // Tableaux
    FNotes: array[1..10] of Real;

    // Autres objets
    FVoiture: TVoiture;  // Composition
  end;
```

### Ordre de déclaration recommandé

```pascal
type
  TEmploye = class
  private
    // 1. Types simples d'abord
    FIdentifiant: Integer;
    FNom: string;
    FPrenom: string;

    // 2. Types numériques
    FAge: Integer;
    FSalaire: Real;

    // 3. Types booléens
    FEstActif: Boolean;
    FEstManager: Boolean;

    // 4. Types complexes
    FDateEmbauche: TDateTime;

    // 5. Objets
    FDepartement: TDepartement;
  public
    // Méthodes...
  end;
```

## Déclaration des méthodes

### Dans la déclaration de la classe

On déclare seulement le **prototype** (signature) des méthodes :

```pascal
type
  TCalculatrice = class
  private
    FResultat: Real;
  public
    // Procédures
    procedure Additionner(A, B: Real);
    procedure Reinitialiser;

    // Fonctions
    function ObtenirResultat: Real;
    function Diviser(A, B: Real): Real;
  end;
```

### Implémentation des méthodes

L'**implémentation** se fait en dehors de la déclaration, en préfixant par `TNomClasse.` :

```pascal
procedure TCalculatrice.Additionner(A, B: Real);  
begin  
  FResultat := A + B;
end;

procedure TCalculatrice.Reinitialiser;  
begin  
  FResultat := 0;
end;

function TCalculatrice.ObtenirResultat: Real;  
begin  
  Result := FResultat;
end;

function TCalculatrice.Diviser(A, B: Real): Real;  
begin  
  if B <> 0 then
    Result := A / B
  else
    raise Exception.Create('Division par zéro');
end;
```

**Points importants :**
- Le nom complet : `TNomClasse.NomMethode`
- Les paramètres doivent être identiques à la déclaration
- Le type de retour (pour les fonctions) doit être identique

## Structure complète d'un fichier avec une classe

### Programme simple

```pascal
program GestionPersonnes;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  // Déclaration de la classe
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
  public
    procedure DefinirNom(const Valeur: string);
    procedure DefinirAge(Valeur: Integer);
    function ObtenirNom: string;
    function ObtenirAge: Integer;
    procedure Afficher;
  end;

// Implémentation des méthodes
procedure TPersonne.DefinirNom(const Valeur: string);  
begin  
  FNom := Valeur;
end;

procedure TPersonne.DefinirAge(Valeur: Integer);  
begin  
  if (Valeur >= 0) and (Valeur <= 150) then
    FAge := Valeur
  else
    WriteLn('Erreur : âge invalide');
end;

function TPersonne.ObtenirNom: string;  
begin  
  Result := FNom;
end;

function TPersonne.ObtenirAge: Integer;  
begin  
  Result := FAge;
end;

procedure TPersonne.Afficher;  
begin  
  WriteLn('Nom : ', FNom);
  WriteLn('Age : ', FAge, ' ans');
end;

// Programme principal
var
  Personne1: TPersonne;
begin
  Personne1 := TPersonne.Create;
  try
    Personne1.DefinirNom('Marie Curie');
    Personne1.DefinirAge(66);
    Personne1.Afficher;
  finally
    Personne1.Free;
  end;
end.
```

### Unité avec une classe

```pascal
unit UPersonne;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Déclaration dans la section interface
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
  public
    procedure DefinirNom(const Valeur: string);
    procedure DefinirAge(Valeur: Integer);
    function ObtenirNom: string;
    function ObtenirAge: Integer;
    procedure Afficher;
  end;

implementation

// Implémentation dans la section implementation
procedure TPersonne.DefinirNom(const Valeur: string);  
begin  
  FNom := Valeur;
end;

procedure TPersonne.DefinirAge(Valeur: Integer);  
begin  
  if (Valeur >= 0) and (Valeur <= 150) then
    FAge := Valeur;
end;

function TPersonne.ObtenirNom: string;  
begin  
  Result := FNom;
end;

function TPersonne.ObtenirAge: Integer;  
begin  
  Result := FAge;
end;

procedure TPersonne.Afficher;  
begin  
  WriteLn('Nom : ', FNom, ', Age : ', FAge, ' ans');
end;

end.
```

## Exemple complet : Classe TRectangle

Voici un exemple complet montrant toutes les bonnes pratiques de déclaration :

```pascal
program ExempleRectangle;

{$mode objfpc}{$H+}

type
  TRectangle = class
  private
    // Attributs privés
    FLargeur: Real;
    FHauteur: Real;

    // Méthode privée (helper interne)
    function ValiderDimension(Valeur: Real): Boolean;

  public
    // Méthodes d'accès (setters)
    procedure DefinirLargeur(Valeur: Real);
    procedure DefinirHauteur(Valeur: Real);

    // Méthodes d'accès (getters)
    function ObtenirLargeur: Real;
    function ObtenirHauteur: Real;

    // Méthodes de calcul
    function CalculerSurface: Real;
    function CalculerPerimetre: Real;

    // Méthodes utilitaires
    function EstCarre: Boolean;
    procedure Afficher;
    procedure Redimensionner(FacteurEchelle: Real);
  end;

// === IMPLÉMENTATION ===

function TRectangle.ValiderDimension(Valeur: Real): Boolean;  
begin  
  Result := Valeur > 0;
end;

procedure TRectangle.DefinirLargeur(Valeur: Real);  
begin  
  if ValiderDimension(Valeur) then
    FLargeur := Valeur
  else
    WriteLn('Erreur : largeur invalide');
end;

procedure TRectangle.DefinirHauteur(Valeur: Real);  
begin  
  if ValiderDimension(Valeur) then
    FHauteur := Valeur
  else
    WriteLn('Erreur : hauteur invalide');
end;

function TRectangle.ObtenirLargeur: Real;  
begin  
  Result := FLargeur;
end;

function TRectangle.ObtenirHauteur: Real;  
begin  
  Result := FHauteur;
end;

function TRectangle.CalculerSurface: Real;  
begin  
  Result := FLargeur * FHauteur;
end;

function TRectangle.CalculerPerimetre: Real;  
begin  
  Result := 2 * (FLargeur + FHauteur);
end;

function TRectangle.EstCarre: Boolean;  
begin  
  Result := FLargeur = FHauteur;
end;

procedure TRectangle.Afficher;  
begin  
  WriteLn('=== Rectangle ===');
  WriteLn('Largeur : ', FLargeur:0:2, ' cm');
  WriteLn('Hauteur : ', FHauteur:0:2, ' cm');
  WriteLn('Surface : ', CalculerSurface:0:2, ' cm²');
  WriteLn('Périmètre : ', CalculerPerimetre:0:2, ' cm');
  if EstCarre then
    WriteLn('C''est un carré !');
  WriteLn('================');
end;

procedure TRectangle.Redimensionner(FacteurEchelle: Real);  
begin  
  if ValiderDimension(FacteurEchelle) then
  begin
    FLargeur := FLargeur * FacteurEchelle;
    FHauteur := FHauteur * FacteurEchelle;
  end;
end;

// === PROGRAMME PRINCIPAL ===

var
  MonRectangle: TRectangle;
begin
  MonRectangle := TRectangle.Create;
  try
    MonRectangle.DefinirLargeur(10);
    MonRectangle.DefinirHauteur(5);
    MonRectangle.Afficher;

    WriteLn;
    WriteLn('Redimensionnement x2...');
    MonRectangle.Redimensionner(2);
    MonRectangle.Afficher;
  finally
    MonRectangle.Free;
  end;
end.
```

## Déclaration de plusieurs classes

On peut déclarer plusieurs classes dans la même section `type` :

```pascal
type
  // Première classe
  TAdresse = class
  private
    FRue: string;
    FVille: string;
    FCodePostal: string;
  public
    procedure Afficher;
  end;

  // Deuxième classe
  TPersonne = class
  private
    FNom: string;
    FAdresse: TAdresse;  // Utilise la première classe
  public
    procedure Afficher;
  end;

  // Troisième classe
  TEntreprise = class
  private
    FNom: string;
    FEmployes: array of TPersonne;
  public
    procedure AjouterEmploye(Employe: TPersonne);
  end;
```

**Attention à l'ordre :** Une classe ne peut référencer qu'une classe déclarée **avant** elle, sauf avec une déclaration anticipée.

## Déclaration anticipée (forward)

Quand deux classes doivent se référencer mutuellement :

```pascal
type
  // Déclaration anticipée
  TEmploye = class;

  // Première classe complète
  TDepartement = class
  private
    FNom: string;
    FResponsable: TEmploye;  // Utilise TEmploye
  end;

  // Deuxième classe complète
  TEmploye = class
  private
    FNom: string;
    FDepartement: TDepartement;  // Utilise TDepartement
  end;
```

## Erreurs courantes à éviter

### Erreur n°1 : Oublier le point-virgule final

```pascal
type
  TPersonne = class
  private
    FNom: string;
  end  // ✗ ERREUR : manque le point-virgule
```

### Erreur n°2 : Mauvais ordre des sections

```pascal
type
  TPersonne = class
  public              // ✗ DÉCONSEILLÉ
    procedure Afficher;
  private             // ✗ private devrait être en premier
    FNom: string;
  end;
```

### Erreur n°3 : Oublier le préfixe de classe dans l'implémentation

```pascal
procedure Afficher;  // ✗ ERREUR : manque TPersonne.  
begin  
  WriteLn(FNom);
end;

// ✓ CORRECT :
procedure TPersonne.Afficher;  
begin  
  WriteLn(FNom);
end;
```

### Erreur n°4 : Signatures différentes

```pascal
type
  TPersonne = class
    procedure DefinirAge(Age: Integer);  // Déclaration
  end;

// ✗ ERREUR : nom de paramètre différent (pas grave)
// mais type différent serait une vraie erreur
procedure TPersonne.DefinirAge(NouvelAge: Integer);  
begin  
  FAge := NouvelAge;
end;

// ✓ MEILLEUR : garder le même nom
procedure TPersonne.DefinirAge(Age: Integer);  
begin  
  FAge := Age;
end;
```

## Points clés à retenir

- Une classe se déclare dans la section `type`
- Structure : `TNomClasse = class ... end;`
- Convention : classes commencent par **T**, attributs par **F**
- Ordre recommandé : `private` → `protected` → `public` → `published`
- Les méthodes sont déclarées dans la classe, implémentées à l'extérieur
- Préfixe obligatoire : `TNomClasse.NomMethode` dans l'implémentation
- Utiliser des noms descriptifs et suivre le PascalCase
- Dans une unité : déclaration en `interface`, implémentation en `implementation`

## Vers la suite

Maintenant que vous savez déclarer correctement une classe, la section suivante abordera les **attributs et méthodes** de manière plus approfondie, avec des exemples concrets d'utilisation.

⏭️ [Attributs et méthodes](/10-fondamentaux-poo/04-attributs-methodes.md)
