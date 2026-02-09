🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.11 UML et diagrammes de classes basics

## Qu'est-ce que l'UML ?

**UML** (Unified Modeling Language) est un **langage de modélisation visuel** standardisé qui permet de représenter la structure et le comportement d'un système logiciel à l'aide de diagrammes.

**Analogie :** L'UML est comme le plan d'architecte pour une maison. Avant de construire (coder), on dessine les plans pour visualiser la structure et vérifier que tout est cohérent.

### Pourquoi utiliser l'UML ?

✓ **Visualiser** la structure du code avant de l'écrire  
✓ **Communiquer** avec d'autres développeurs  
✓ **Planifier** l'architecture de l'application  
✓ **Documenter** le code existant  
✓ **Identifier** les problèmes de conception tôt  

## Le diagramme de classes

Le **diagramme de classes** est le diagramme UML le plus utilisé en POO. Il représente les classes, leurs attributs, leurs méthodes et les relations entre elles.

## Représentation d'une classe simple

### Structure de base

Une classe est représentée par un rectangle divisé en **trois compartiments** :

```
┌─────────────────────┐
│     NomClasse       │  ← Nom de la classe
├─────────────────────┤
│   attributs         │  ← Attributs (données)
├─────────────────────┤
│   méthodes()        │  ← Méthodes (actions)
└─────────────────────┘
```

### Exemple : Classe Personne

```
┌─────────────────────┐
│     TPersonne       │
├─────────────────────┤
│ - FNom: string      │
│ - FAge: Integer     │
├─────────────────────┤
│ + Create()          │
│ + Afficher()        │
│ + Vieillir()        │
└─────────────────────┘
```

**Correspondance avec le code Pascal :**

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
  public
    constructor Create;
    procedure Afficher;
    procedure Vieillir;
  end;
```

## Notation de la visibilité

Les symboles indiquent la visibilité des membres :

| Symbole | Visibilité | Signification | Équivalent Pascal |
|---------|------------|---------------|-------------------|
| `+` | Public | Accessible partout | `public` |
| `-` | Private | Accessible uniquement dans la classe | `private` |
| `#` | Protected | Accessible dans la classe et ses descendants | `protected` |
| `~` | Package | Accessible dans le même package/unité | (spécifique au langage) |

### Exemple avec différentes visibilités

```
┌─────────────────────────────┐
│       TCompteBancaire       │
├─────────────────────────────┤
│ - FNumeroCompte: string     │  ← Private
│ - FSolde: Real              │  ← Private
│ # FTitulaire: string        │  ← Protected
├─────────────────────────────┤
│ + Create()                  │  ← Public
│ + Crediter(Montant: Real)   │  ← Public
│ + Debiter(Montant: Real)    │  ← Public
│ + ObtenirSolde(): Real      │  ← Public
│ - ValiderMontant(): Boolean │  ← Private
└─────────────────────────────┘
```

## Notation des types et paramètres

### Format général

```
attribut: Type
méthode(paramètre: Type): TypeRetour
```

### Exemples détaillés

```
┌─────────────────────────────────────┐
│           TRectangle                │
├─────────────────────────────────────┤
│ - FLargeur: Real                    │
│ - FHauteur: Real                    │
├─────────────────────────────────────┤
│ + Create(L: Real, H: Real)          │
│ + CalculerSurface(): Real           │
│ + CalculerPerimetre(): Real         │
│ + Redimensionner(Facteur: Real)     │
│ + EstCarre(): Boolean               │
└─────────────────────────────────────┘
```

**Code Pascal correspondant :**

```pascal
type
  TRectangle = class
  private
    FLargeur: Real;
    FHauteur: Real;
  public
    constructor Create(L: Real; H: Real);
    function CalculerSurface: Real;
    function CalculerPerimetre: Real;
    procedure Redimensionner(Facteur: Real);
    function EstCarre: Boolean;
  end;
```

## Propriétés en UML

Les propriétés peuvent être représentées de différentes manières :

### Méthode 1 : Comme des attributs avec stéréotype

```
┌─────────────────────────────┐
│        TPersonne            │
├─────────────────────────────┤
│ - FNom: string              │
│ - FAge: Integer             │
│ «property» Nom: string      │  ← Propriété
│ «property» Age: Integer     │  ← Propriété
├─────────────────────────────┤
│ + Create()                  │
│ + Afficher()                │
└─────────────────────────────┘
```

### Méthode 2 : Avec getters/setters explicites

```
┌─────────────────────────────┐
│        TPersonne            │
├─────────────────────────────┤
│ - FNom: string              │
│ - FAge: Integer             │
├─────────────────────────────┤
│ + GetNom(): string          │
│ + SetNom(Valeur: string)    │
│ + GetAge(): Integer         │
│ + SetAge(Valeur: Integer)   │
└─────────────────────────────┘
```

### Méthode 3 : Simplifiée (la plus courante)

```
┌─────────────────────────────┐
│        TPersonne            │
├─────────────────────────────┤
│ - FNom: string              │
│ - FAge: Integer             │
├─────────────────────────────┤
│ + Nom: string               │  ← Propriété simplifiée
│ + Age: Integer              │  ← Propriété simplifiée
│ + Afficher()                │
└─────────────────────────────┘
```

## Relations entre classes

Les diagrammes UML montrent aussi comment les classes sont liées entre elles.

### 1. Association simple

Une classe "connaît" ou "utilise" une autre classe.

```
┌──────────────┐                  ┌──────────────┐
│  TPersonne   │ ────────────────>│   TAdresse   │
└──────────────┘                  └──────────────┘
```

**Code Pascal :**

```pascal
type
  TAdresse = class
    // ...
  end;

  TPersonne = class
  private
    FAdresse: TAdresse;
  end;
```

### 2. Association avec multiplicité

La multiplicité indique combien d'instances sont liées.

```
┌──────────────┐  1        0..*   ┌──────────────┐
│    TClasse   │ ────────────────>│  TEtudiant   │
└──────────────┘  contient        └──────────────┘
```

**Multiplicités courantes :**
- `1` : exactement un
- `0..1` : zéro ou un
- `*` ou `0..*` : zéro ou plusieurs
- `1..*` : un ou plusieurs
- `n` : exactement n

**Code Pascal :**

```pascal
type
  TClasse = class
  private
    FEtudiants: array of TEtudiant;  // 0..* étudiants
  end;
```

### 3. Composition (diamant plein)

Une classe **possède** une autre classe. Si la classe conteneur est détruite, les objets contenus le sont aussi.

```
┌──────────────┐  ◆              ┌──────────────┐
│  TEntreprise │ ───────────────>│ TDepartement │
└──────────────┘                 └──────────────┘
```

**Signification :** Les départements n'existent que dans le contexte de l'entreprise.

**Code Pascal :**

```pascal
type
  TDepartement = class
    // ...
  end;

  TEntreprise = class
  private
    FDepartements: array of TDepartement;
  public
    constructor Create;
    destructor Destroy; override;  // Libère les départements
  end;

destructor TEntreprise.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FDepartements) do
    FDepartements[I].Free;  // L'entreprise détruit ses départements
  inherited Destroy;
end;
```

### 4. Agrégation (diamant vide)

Une classe **utilise** une autre classe, mais ne la possède pas. Les objets peuvent exister indépendamment.

```
┌──────────────┐  ◇              ┌──────────────┐
│    TEquipe   │ ───────────────>│   TJoueur    │
└──────────────┘                 └──────────────┘
```

**Signification :** Les joueurs peuvent exister sans l'équipe.

**Code Pascal :**

```pascal
type
  TJoueur = class
    // ...
  end;

  TEquipe = class
  private
    FJoueurs: array of TJoueur;  // Références, pas propriétaire
  public
    destructor Destroy; override;
  end;

destructor TEquipe.Destroy;
begin
  SetLength(FJoueurs, 0);  // Libère le tableau, mais pas les joueurs
  inherited Destroy;
end;
```

### 5. Héritage (flèche vide)

Une classe hérite d'une autre (nous verrons cela en détail dans le chapitre suivant).

```
┌──────────────┐
│   TAnimal    │
└──────┬───────┘
       △
       │
       ├─────────────────┐
       │                 │
┌──────┴───────┐  ┌──────┴───────┐
│    TChien    │  │    TChat     │
└──────────────┘  └──────────────┘
```

**Code Pascal :**

```pascal
type
  TAnimal = class
    // Classe de base
  end;

  TChien = class(TAnimal)  // Hérite de TAnimal
    // Spécifique au chien
  end;

  TChat = class(TAnimal)  // Hérite de TAnimal
    // Spécifique au chat
  end;
```

## Exemple complet : Système de bibliothèque

### Diagramme UML

```
┌─────────────────────────────┐       1      0..*   ┌─────────────────────────────┐
│      TBibliotheque          │◆───────────────────>│         TLivre              │
├─────────────────────────────┤                     ├─────────────────────────────┤
│ - FNom: string              │                     │ - FTitre: string            │
│ - FAdresse: string          │                     │ - FAuteur: string           │
├─────────────────────────────┤                     │ - FISBN: string             │
│ + AjouterLivre(Livre)       │                     │ - FEstEmprunte: Boolean     │
│ + RetirerLivre(ISBN)        │                     ├─────────────────────────────┤
│ + RechercherLivre(ISBN)     │                     │ + Create(T,A,I: string)     │
│ + AfficherCatalogue()       │                     │ + Emprunter()               │
└─────────────────────────────┘                     │ + Retourner()               │
                                                    │ + Afficher()                │
                                                    └─────────────────────────────┘
```

### Code Pascal correspondant

```pascal
type
  TLivre = class
  private
    FTitre: string;
    FAuteur: string;
    FISBN: string;
    FEstEmprunte: Boolean;
  public
    constructor Create(const Titre, Auteur, ISBN: string);
    procedure Emprunter;
    procedure Retourner;
    procedure Afficher;
    property Titre: string read FTitre;
    property ISBN: string read FISBN;
    property EstEmprunte: Boolean read FEstEmprunte;
  end;

  TBibliotheque = class
  private
    FNom: string;
    FAdresse: string;
    FLivres: array of TLivre;
  public
    constructor Create(const Nom, Adresse: string);
    destructor Destroy; override;
    procedure AjouterLivre(Livre: TLivre);
    procedure RetirerLivre(const ISBN: string);
    function RechercherLivre(const ISBN: string): TLivre;
    procedure AfficherCatalogue;
  end;
```

## Exemple 2 : Système de gestion scolaire

### Diagramme UML

```
                    ┌─────────────────────────────┐
                    │       TPersonne             │
                    ├─────────────────────────────┤
                    │ # FNom: string              │
                    │ # FPrenom: string           │
                    │ # FDateNaissance: TDateTime │
                    ├─────────────────────────────┤
                    │ + Create()                  │
                    │ + ObtenirNomComplet()       │
                    │ + CalculerAge()             │
                    └──────────┬──────────────────┘
                               △
                               │
                 ┌─────────────┴────────────┐
                 │                          │
    ┌────────────┴─────────┐    ┌───────────┴──────────┐
    │     TEtudiant        │    │    TProfesseur       │
    ├──────────────────────┤    ├──────────────────────┤
    │ - FNumeroEtudiant    │    │ - FSpecialite        │
    │ - FNotes: array      │    │ - FSalaire: Real     │
    ├──────────────────────┤    ├──────────────────────┤
    │ + AjouterNote()      │    │ + Enseigner()        │
    │ + CalculerMoyenne()  │    │ + AugmenterSalaire() │
    └──────────────────────┘    └──────────────────────┘


    ┌─────────────────────────────┐      *        1     ┌─────────────────────────────┐
    │       TClasse               │◇───────────────────>│     TProfesseur             │
    ├─────────────────────────────┤                     └─────────────────────────────┘
    │ - FNom: string              │
    │ - FNiveau: string           │      *        *
    ├─────────────────────────────┤◇───────────────────>┌─────────────────────────────┐
    │ + AjouterEtudiant()         │                     │      TEtudiant              │
    │ + AfficherListe()           │                     └─────────────────────────────┘
    └─────────────────────────────┘
```

**Explications :**
- `TPersonne` est la classe de base
- `TEtudiant` et `TProfesseur` héritent de `TPersonne`
- Une `TClasse` a plusieurs étudiants (agrégation)
- Une `TClasse` a un professeur (association)

## Notes et commentaires en UML

Vous pouvez ajouter des notes explicatives :

```
┌─────────────────────────────┐
│      TCompteBancaire        │
├─────────────────────────────┤      ╔═══════════════════════════╗
│ - FSolde: Real              │      ║ Note:                     ║
├─────────────────────────────┤      ║ Le solde ne peut jamais   ║
│ + Crediter(Montant: Real)   │------║ être négatif              ║
│ + Debiter(Montant: Real)    │      ╚═══════════════════════════╝
└─────────────────────────────┘
```

## Stéréotypes

Les stéréotypes permettent d'ajouter des informations supplémentaires :

```
┌─────────────────────────────┐
│   «interface»               │
│      IComparable            │
├─────────────────────────────┤
│ + ComparerAvec()            │
└─────────────────────────────┘


┌─────────────────────────────┐
│   «abstract»                │
│      TForme                 │
├─────────────────────────────┤
│ + CalculerSurface()         │  ← Méthode abstraite en italique
└─────────────────────────────┘
```

## De l'UML au code : processus complet

### Étape 1 : Concevoir le diagramme

```
┌─────────────────────────────┐
│      TCalculatrice          │
├─────────────────────────────┤
│ - FResultat: Real           │
├─────────────────────────────┤
│ + Additionner(A,B: Real)    │
│ + Soustraire(A,B: Real)     │
│ + ObtenirResultat(): Real   │
└─────────────────────────────┘
```

### Étape 2 : Traduire en code

```pascal
type
  TCalculatrice = class
  private
    FResultat: Real;
  public
    procedure Additionner(A, B: Real);
    procedure Soustraire(A, B: Real);
    function ObtenirResultat: Real;
  end;
```

### Étape 3 : Implémenter

```pascal
procedure TCalculatrice.Additionner(A, B: Real);
begin
  FResultat := A + B;
end;

procedure TCalculatrice.Soustraire(A, B: Real);
begin
  FResultat := A - B;
end;

function TCalculatrice.ObtenirResultat: Real;
begin
  Result := FResultat;
end;
```

## Outils pour créer des diagrammes UML

### Outils en ligne (gratuits)
- **Draw.io** (diagrams.net) - Gratuit, puissant
- **PlantUML** - Génère des diagrammes depuis du texte
- **Lucidchart** - Version gratuite limitée
- **Creately** - Version gratuite disponible

### Logiciels de bureau
- **StarUML** - Gratuit et open source
- **Umbrello** - Open source pour Linux
- **Visual Paradigm** - Version communautaire gratuite
- **ArgoUML** - Gratuit et open source

### Dans Lazarus
- Certains plugins permettent de générer du code depuis des diagrammes UML

## Conseils pour bien utiliser UML

### 1. Commencez simple

```
┌─────────────────┐
│   TPersonne     │
├─────────────────┤
│ - FNom: string  │
├─────────────────┤
│ + Afficher()    │
└─────────────────┘
```

Puis enrichissez progressivement.

### 2. Utilisez des noms clairs

```
✓ BON:
┌─────────────────────────────┐
│    TGestionnaireEmployes    │
└─────────────────────────────┘

✗ MAUVAIS:
┌─────────────────────────────┐
│         TGestEmp            │
└─────────────────────────────┘
```

### 3. Ne surchargez pas

Ne mettez que l'essentiel dans les premiers diagrammes :

```
Version simplifiée (pour vue d'ensemble):
┌─────────────────┐
│   TPersonne     │
├─────────────────┤
│ + Afficher()    │
└─────────────────┘

Version détaillée (pour implémentation):
┌─────────────────────────────┐
│        TPersonne            │
├─────────────────────────────┤
│ - FNom: string              │
│ - FPrenom: string           │
│ - FAge: Integer             │
│ - FEmail: string            │
├─────────────────────────────┤
│ + Create()                  │
│ + GetNom(): string          │
│ + SetNom(V: string)         │
│ + Afficher()                │
│ + EnvoyerEmail(Msg: string) │
└─────────────────────────────┘
```

### 4. Gardez la cohérence

Si vous utilisez un symbole ou une convention, gardez-la dans tout le diagramme.

### 5. Documentez les décisions importantes

Ajoutez des notes pour expliquer les choix de conception.

## Exemple pratique complet : Application de gestion de tâches

### Diagramme UML

```
┌─────────────────────────────────────┐
│            TTache                   │
├─────────────────────────────────────┤
│ - FTitre: string                    │
│ - FDescription: string              │
│ - FDateCreation: TDateTime          │
│ - FDateEcheance: TDateTime          │
│ - FEstTerminee: Boolean             │
│ - FPriorite: Integer                │
├─────────────────────────────────────┤
│ + Create(Titre: string)             │
│ + Terminer()                        │
│ + ModifierPriorite(P: Integer)      │
│ + EstEnRetard(): Boolean            │
│ + Afficher()                        │
└─────────────────────────────────────┘
                    △
                    │ 1
                    │
           0..*     │
┌─────────────────────────────────────┐
│       TGestionnaireTaches           │
├─────────────────────────────────────┤
│ - FTaches: array of TTache          │
├─────────────────────────────────────┤
│ + AjouterTache(T: TTache)           │
│ + SupprimerTache(Index: Integer)    │
│ + ObtenirTache(Index: Integer)      │
│ + AfficherTachesNonTerminees()      │
│ + AfficherTachesEnRetard()          │
│ + TrierParPriorite()                │
└─────────────────────────────────────┘
```

### Code Pascal correspondant

```pascal
type
  TTache = class
  private
    FTitre: string;
    FDescription: string;
    FDateCreation: TDateTime;
    FDateEcheance: TDateTime;
    FEstTerminee: Boolean;
    FPriorite: Integer;
  public
    constructor Create(const Titre: string);
    procedure Terminer;
    procedure ModifierPriorite(P: Integer);
    function EstEnRetard: Boolean;
    procedure Afficher;
    property Titre: string read FTitre write FTitre;
    property EstTerminee: Boolean read FEstTerminee;
  end;

  TGestionnaireTaches = class
  private
    FTaches: array of TTache;
  public
    destructor Destroy; override;
    procedure AjouterTache(T: TTache);
    procedure SupprimerTache(Index: Integer);
    function ObtenirTache(Index: Integer): TTache;
    procedure AfficherTachesNonTerminees;
    procedure AfficherTachesEnRetard;
    procedure TrierParPriorite;
  end;
```

## Avantages de l'UML en développement

### 1. Vision d'ensemble

L'UML permet de voir toute l'architecture d'un coup d'œil.

### 2. Communication

Facilite les discussions avec l'équipe sans entrer dans le code.

### 3. Détection d'erreurs précoce

On peut repérer des problèmes de conception avant de coder.

### 4. Documentation

Le diagramme sert de documentation visuelle du projet.

### 5. Planification

Aide à estimer le travail nécessaire.

## Points clés à retenir

- **UML** est un langage de modélisation visuel standardisé
- Le **diagramme de classes** représente les classes et leurs relations
- Une classe se dessine en **trois compartiments** (nom, attributs, méthodes)
- Les symboles de visibilité : `+` public, `-` private, `#` protected
- Les **relations** : association, composition (◆), agrégation (◇), héritage (△)
- Les **multiplicités** indiquent combien d'instances sont liées
- L'UML aide à **concevoir** avant de coder
- Gardez les diagrammes **simples et clairs**
- Utilisez l'UML pour **communiquer** et **documenter**
- L'UML n'est pas obligatoire mais très utile pour les projets complexes

## Conclusion du chapitre 10

Félicitations ! Vous avez maintenant acquis les **fondamentaux de la Programmation Orientée Objet** :

✓ Concepts de classes et d'objets  
✓ Encapsulation et visibilité  
✓ Attributs et méthodes  
✓ Constructeurs et destructeurs  
✓ Le mot-clé Self  
✓ Propriétés  
✓ Comparaison procédural vs objet  
✓ Diagrammes UML  

Dans le **chapitre 11**, nous approfondirons ces concepts avec l'**héritage**, le **polymorphisme**, et les **méthodes virtuelles**, qui vous permettront de créer des hiérarchies de classes plus sophistiquées et réutilisables.

⏭️ [POO Avancée - Héritage](/11-poo-avancee-heritage/README.md)
