🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.2 Encapsulation et visibilité

## Qu'est-ce que l'encapsulation ?

L'**encapsulation** est l'un des principes fondamentaux de la Programmation Orientée Objet. C'est l'idée de **protéger les données** d'un objet en contrôlant la façon dont on peut y accéder et les modifier.

### Analogie de la voiture

Imaginez une voiture :
- Vous pouvez conduire la voiture en utilisant le **volant, les pédales et le levier de vitesse** (interface publique)
- Vous **ne touchez pas directement** au moteur, à la transmission ou au système d'injection (détails internes)
- Le constructeur a caché la complexité interne et vous donne seulement des commandes simples

C'est exactement ce que fait l'encapsulation en POO : **cacher les détails internes** et exposer seulement ce qui est nécessaire.

## Pourquoi l'encapsulation est-elle importante ?

### 1. Protection des données

Sans encapsulation, n'importe quelle partie du programme peut modifier les données, ce qui peut causer des erreurs :

```pascal
// Sans protection
type
  TCompteBancaire = class
    Solde: Real;  // Public, accessible partout
  end;

var
  MonCompte: TCompteBancaire;
begin
  MonCompte := TCompteBancaire.Create;
  MonCompte.Solde := 1000;

  // DANGER : modification directe, sans contrôle !
  MonCompte.Solde := -500;  // Solde négatif non désiré !
end;
```

### 2. Validation des données

Avec l'encapsulation, on peut contrôler les modifications :

```pascal
// Avec protection
type
  TCompteBancaire = class
  private
    FSolde: Real;  // Protégé
  public
    procedure Debiter(Montant: Real);
    function ObtenirSolde: Real;
  end;

procedure TCompteBancaire.Debiter(Montant: Real);  
begin  
  if FSolde - Montant >= 0 then
    FSolde := FSolde - Montant
  else
    WriteLn('Solde insuffisant !');
end;
```

### 3. Facilité de maintenance

Si les données sont cachées, on peut changer leur implémentation interne sans affecter le reste du programme.

## Les niveaux de visibilité en Pascal

Pascal propose quatre niveaux de visibilité pour contrôler l'accès aux membres d'une classe :

### 1. **private** (privé)

Les membres `private` sont accessibles **uniquement à l'intérieur de la classe**.

```pascal
type
  TPersonne = class
  private
    FNom: string;        // Accessible seulement dans TPersonne
    FAge: Integer;       // Accessible seulement dans TPersonne
    procedure MethodePrivee;  // Idem
  end;
```

**Usage** : Pour les détails d'implémentation qui ne doivent pas être accessibles de l'extérieur.

### 2. **protected** (protégé)

Les membres `protected` sont accessibles dans la classe **et dans ses classes dérivées** (héritage, que nous verrons plus tard).

```pascal
type
  TPersonne = class
  protected
    FIdentifiant: Integer;  // Accessible dans TPersonne et ses descendants
  end;
```

**Usage** : Pour les éléments qui doivent être accessibles aux classes filles, mais pas au reste du programme.

### 3. **public** (public)

Les membres `public` sont accessibles **partout**, depuis n'importe quelle partie du programme.

```pascal
type
  TPersonne = class
  public
    procedure Afficher;      // Accessible partout
    function ObtenirAge: Integer;  // Accessible partout
  end;
```

**Usage** : Pour l'interface publique de la classe, ce que les autres parties du programme peuvent utiliser.

### 4. **published** (publié)

Les membres `published` sont comme `public`, mais avec des informations supplémentaires disponibles à l'exécution (RTTI - Run-Time Type Information). Principalement utilisé pour les composants visuels Lazarus.

```pascal
type
  TMonComposant = class
  published
    property Couleur: TColor;  // Visible dans l'inspecteur d'objets
  end;
```

**Usage** : Pour les propriétés qui doivent apparaître dans l'inspecteur d'objets de Lazarus.

## Règles de bonne pratique

### Règle n°1 : Les attributs doivent être private

Les attributs (données) d'une classe doivent **presque toujours** être `private` :

```pascal
type
  TVoiture = class
  private
    FMarque: string;      // ✓ Correct : attribut private
    FVitesse: Integer;    // ✓ Correct : attribut private
  public
    // Les méthodes publiques permettent l'accès contrôlé
    procedure Accelerer(Increment: Integer);
    function ObtenirVitesse: Integer;
  end;
```

### Règle n°2 : Les méthodes d'interface doivent être public

Les méthodes qui constituent l'interface de la classe doivent être `public` :

```pascal
type
  TCalculatrice = class
  private
    FResultat: Real;
    procedure VerifierDivisionParZero(Diviseur: Real);  // Méthode interne
  public
    procedure Additionner(A, B: Real);   // ✓ Interface publique
    procedure Soustraire(A, B: Real);    // ✓ Interface publique
    function ObtenirResultat: Real;      // ✓ Interface publique
  end;
```

### Règle n°3 : Utilisez protected pour l'héritage

Si vous prévoyez que d'autres classes hériteront de votre classe, utilisez `protected` pour les éléments qu'elles doivent pouvoir utiliser :

```pascal
type
  TAnimal = class
  protected
    FNom: string;  // Les classes dérivées pourront y accéder
  public
    procedure Afficher;
  end;
```

## Exemple complet : Classe TCompteBancaire

Voici un exemple qui illustre l'encapsulation et les différents niveaux de visibilité :

```pascal
program ExempleEncapsulation;

{$mode objfpc}{$H+}

type
  TCompteBancaire = class
  private
    // Attributs privés : détails d'implémentation
    FNumeroCompte: string;
    FSolde: Real;
    FTitulaire: string;

    // Méthode privée : utilisée seulement en interne
    function VerifierMontantValide(Montant: Real): Boolean;

  public
    // Méthodes publiques : interface de la classe
    procedure Crediter(Montant: Real);
    procedure Debiter(Montant: Real);
    function ObtenirSolde: Real;
    procedure AfficherInfos;
    procedure DefinirTitulaire(const Nom: string);
  end;

// Implémentation de la méthode privée
function TCompteBancaire.VerifierMontantValide(Montant: Real): Boolean;  
begin  
  Result := Montant > 0;
end;

// Implémentation des méthodes publiques
procedure TCompteBancaire.Crediter(Montant: Real);  
begin  
  if VerifierMontantValide(Montant) then
  begin
    FSolde := FSolde + Montant;
    WriteLn('Crédit de ', Montant:0:2, ' € effectué.');
  end
  else
    WriteLn('Erreur : montant invalide.');
end;

procedure TCompteBancaire.Debiter(Montant: Real);  
begin  
  if not VerifierMontantValide(Montant) then
  begin
    WriteLn('Erreur : montant invalide.');
    Exit;
  end;

  if FSolde - Montant >= 0 then
  begin
    FSolde := FSolde - Montant;
    WriteLn('Débit de ', Montant:0:2, ' € effectué.');
  end
  else
    WriteLn('Erreur : solde insuffisant.');
end;

function TCompteBancaire.ObtenirSolde: Real;  
begin  
  Result := FSolde;
end;

procedure TCompteBancaire.AfficherInfos;  
begin  
  WriteLn('=== Informations du compte ===');
  WriteLn('Titulaire : ', FTitulaire);
  WriteLn('Numéro : ', FNumeroCompte);
  WriteLn('Solde : ', FSolde:0:2, ' €');
  WriteLn('==============================');
end;

procedure TCompteBancaire.DefinirTitulaire(const Nom: string);  
begin  
  if Length(Nom) > 0 then
    FTitulaire := Nom
  else
    WriteLn('Erreur : nom invalide.');
end;

// Programme principal
var
  MonCompte: TCompteBancaire;
begin
  MonCompte := TCompteBancaire.Create;

  // Utilisation de l'interface publique
  MonCompte.DefinirTitulaire('Jean Dupont');
  MonCompte.Crediter(1000);
  MonCompte.Debiter(250);
  MonCompte.AfficherInfos;

  // IMPOSSIBLE : les attributs privés ne sont pas accessibles
  // MonCompte.FSolde := 5000;  // Erreur de compilation !

  // On doit passer par les méthodes publiques
  WriteLn('Solde actuel : ', MonCompte.ObtenirSolde:0:2, ' €');

  MonCompte.Free;
end.
```

### Analyse de l'exemple

**Ce qui est protégé (private) :**
- `FNumeroCompte`, `FSolde`, `FTitulaire` : les données sensibles
- `VerifierMontantValide` : détail d'implémentation interne

**Ce qui est exposé (public) :**
- `Crediter`, `Debiter` : opérations contrôlées sur le solde
- `ObtenirSolde` : lecture sécurisée du solde
- `AfficherInfos` : affichage des informations
- `DefinirTitulaire` : modification contrôlée du titulaire

**Avantages :**
1. Le solde ne peut pas être modifié directement
2. Les montants sont validés avant toute opération
3. Le découvert est impossible (vérification dans `Debiter`)
4. L'implémentation interne peut changer sans affecter le code qui utilise la classe

## Tableau récapitulatif des visibilités

| Visibilité | Classe elle-même | Classes dérivées | Reste du programme | Usage principal |
|------------|------------------|------------------|--------------------|-----------------|
| `private` | ✓ Oui | ✗ Non | ✗ Non | Détails d'implémentation |
| `protected` | ✓ Oui | ✓ Oui | ✗ Non | Éléments pour l'héritage |
| `public` | ✓ Oui | ✓ Oui | ✓ Oui | Interface publique |
| `published` | ✓ Oui | ✓ Oui | ✓ Oui | Composants visuels Lazarus |

## Erreurs courantes à éviter

### Erreur n°1 : Tout mettre en public

```pascal
// ✗ MAUVAIS : tout est accessible
type
  TPersonne = class
  public
    Nom: string;
    Age: Integer;
    Salaire: Real;
  end;
```

**Pourquoi c'est mauvais :** Aucun contrôle, n'importe quelle partie du programme peut modifier ces données.

### Erreur n°2 : Accéder directement aux attributs privés

```pascal
var
  P: TPersonne;
begin
  P := TPersonne.Create;
  P.FNom := 'Jean';  // ✗ Erreur de compilation si FNom est private !
end;
```

**Solution :** Utiliser les méthodes publiques fournies par la classe.

### Erreur n°3 : Oublier de valider les données

```pascal
// ✗ MAUVAIS : pas de validation
procedure TPersonne.DefinirAge(NouvelAge: Integer);  
begin  
  FAge := NouvelAge;  // Et si NouvelAge est négatif ?
end;

// ✓ BON : avec validation
procedure TPersonne.DefinirAge(NouvelAge: Integer);  
begin  
  if (NouvelAge >= 0) and (NouvelAge <= 150) then
    FAge := NouvelAge
  else
    raise Exception.Create('Age invalide');
end;
```

## Points clés à retenir

- **L'encapsulation** protège les données en contrôlant leur accès
- Les attributs doivent être **private** (convention : préfixe `F`)
- Les méthodes d'interface doivent être **public**
- **private** : accessible seulement dans la classe
- **protected** : accessible dans la classe et ses descendants
- **public** : accessible partout
- **published** : comme public, mais avec RTTI (composants Lazarus)
- Toujours valider les données dans les méthodes publiques
- L'encapsulation permet de modifier l'implémentation interne sans affecter le code externe

## Vers la suite

Dans la section suivante, nous verrons comment déclarer et structurer correctement une classe complète, en appliquant ces principes d'encapsulation et de visibilité.

⏭️ [Déclaration de classes](/10-fondamentaux-poo/03-declaration-classes.md)
