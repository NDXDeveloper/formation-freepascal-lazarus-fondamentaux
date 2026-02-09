🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.8 Properties avec getters/setters

## Introduction : Contrôler l'accès aux données

Vous avez appris à utiliser des attributs privés et des méthodes publiques. Les **properties** (propriétés) sont une fonctionnalité élégante de Pascal qui permet de **contrôler l'accès** aux données tout en gardant une **syntaxe simple**.

**Analogie : La boîte aux lettres**
- Vous ne pouvez pas entrer directement dans la maison (attribut privé)
- Vous passez par la boîte aux lettres pour déposer ou récupérer du courrier (property)
- Le propriétaire peut vérifier, trier ou refuser le courrier (getter/setter avec logique)

---

## Le problème : Accès direct aux attributs

### ❌ Code sans encapsulation

```pascal
type
  TCompte = class
  public
    Solde: Currency;  // ⚠️ Public : n'importe qui peut modifier
  end;

var
  Compte: TCompte;
begin
  Compte := TCompte.Create;

  // ❌ DANGER : Modification sans contrôle
  Compte.Solde := -1000;  // Solde négatif sans vérification !

  Compte.Free;
end;
```

**Problèmes :**
- Pas de validation des valeurs
- Impossible d'ajouter de la logique plus tard
- Difficile de déboguer (qui a modifié la valeur ?)
- Violation de l'encapsulation

---

## La solution : Properties avec getters/setters

### ✅ Code avec encapsulation

```pascal
type
  TCompte = class
  private
    FSolde: Currency;  // ✅ Privé : protégé

    // Setter : contrôle l'écriture
    procedure SetSolde(Valeur: Currency);
  public
    // Property : interface publique élégante
    property Solde: Currency read FSolde write SetSolde;
  end;

procedure TCompte.SetSolde(Valeur: Currency);
begin
  // ✅ Validation avant modification
  if Valeur < 0 then
  begin
    WriteLn('❌ Erreur : Le solde ne peut pas être négatif');
    Exit;
  end;

  WriteLn('✅ Solde modifié : ', FSolde:0:2, ' → ', Valeur:0:2);
  FSolde := Valeur;
end;

var
  Compte: TCompte;
begin
  Compte := TCompte.Create;

  // Syntaxe simple (comme un attribut)
  Compte.Solde := 1000;   // ✅ Passe par SetSolde (validé)
  Compte.Solde := -500;   // ❌ Rejeté par SetSolde

  WriteLn('Solde final : ', Compte.Solde:0:2);  // Lecture via FSolde

  Compte.Free;
end.
```

**Résultat :**
```
✅ Solde modifié : 0.00 → 1000.00
❌ Erreur : Le solde ne peut pas être négatif
Solde final : 1000.00
```

**Avantages :**
- ✅ Validation automatique
- ✅ Syntaxe élégante (`Compte.Solde` au lieu de `Compte.GetSolde()`)
- ✅ Logique métier centralisée
- ✅ Facile à déboguer et à tracer

---

## Syntaxe des properties

### Structure de base

```pascal
type
  TClasse = class
  private
    FAttribut: Type;  // Attribut de stockage (convention : F + nom)

    function GetAttribut: Type;         // Getter (optionnel)
    procedure SetAttribut(Value: Type); // Setter (optionnel)
  public
    property Attribut: Type read GetAttribut write SetAttribut;
  end;
```

### Mots-clés

- **`read`** : Spécifie comment **lire** la valeur (getter)
- **`write`** : Spécifie comment **écrire** la valeur (setter)
- **`read`** peut être :
  - Un attribut privé : `read FNom`
  - Une méthode getter : `read GetNom`
- **`write`** peut être :
  - Un attribut privé : `write FNom`
  - Une méthode setter : `write SetNom`

---

## Types de properties

### 1. Property en lecture seule

```pascal
type
  TPersonne = class
  private
    FDateNaissance: TDateTime;
  public
    constructor Create(DateNaissance: TDateTime);

    // ✅ Lecture seule : pas de write
    property DateNaissance: TDateTime read FDateNaissance;
  end;

constructor TPersonne.Create(DateNaissance: TDateTime);
begin
  FDateNaissance := DateNaissance;
end;

var
  Personne: TPersonne;
begin
  Personne := TPersonne.Create(EncodeDate(1990, 5, 15));

  WriteLn('Date de naissance : ', DateToStr(Personne.DateNaissance));  // ✅ OK

  // Personne.DateNaissance := Now;  // ❌ ERREUR : propriété en lecture seule

  Personne.Free;
end;
```

**Usage :** Données qui ne doivent jamais changer après création (identifiants, dates de création, etc.)

---

### 2. Property en écriture seule

```pascal
type
  TAuthentification = class
  private
    FMotDePasseHache: string;

    procedure SetMotDePasse(const Value: string);
  public
    // ✅ Écriture seule : pas de read
    property MotDePasse: string write SetMotDePasse;

    function VerifierMotDePasse(const Tentative: string): Boolean;
  end;

procedure TAuthentification.SetMotDePasse(const Value: string);
begin
  // Hachage du mot de passe (simplifié ici)
  FMotDePasseHache := 'HASH_' + Value;
  WriteLn('🔒 Mot de passe défini (haché)');
end;

function TAuthentification.VerifierMotDePasse(const Tentative: string): Boolean;
begin
  Result := FMotDePasseHache = 'HASH_' + Tentative;
end;

var
  Auth: TAuthentification;
begin
  Auth := TAuthentification.Create;

  Auth.MotDePasse := 'MonSecret123';  // ✅ OK : définir le mot de passe

  // WriteLn(Auth.MotDePasse);  // ❌ ERREUR : propriété en écriture seule

  if Auth.VerifierMotDePasse('MonSecret123') then
    WriteLn('✅ Authentification réussie');

  Auth.Free;
end;
```

**Usage :** Données sensibles (mots de passe, clés) qu'on ne doit jamais pouvoir lire directement.

---

### 3. Property en lecture-écriture

```pascal
type
  TProduit = class
  private
    FNom: string;
    FPrix: Currency;

    procedure SetPrix(Value: Currency);
  public
    // ✅ Lecture ET écriture
    property Nom: string read FNom write FNom;      // Accès direct
    property Prix: Currency read FPrix write SetPrix; // Avec validation
  end;

procedure TProduit.SetPrix(Value: Currency);
begin
  if Value <= 0 then
  begin
    WriteLn('❌ Le prix doit être positif');
    Exit;
  end;

  FPrix := Value;
  WriteLn('✅ Prix défini : ', Value:0:2, ' €');
end;

var
  Produit: TProduit;
begin
  Produit := TProduit.Create;

  Produit.Nom := 'Ordinateur';   // ✅ Lecture et écriture
  Produit.Prix := 999.99;         // ✅ Avec validation

  WriteLn('Produit : ', Produit.Nom, ' - ', Produit.Prix:0:2, ' €');

  Produit.Free;
end;
```

---

## Getters et Setters avec logique

### Getter avec calcul

Un getter peut **calculer** une valeur au lieu de simplement la retourner :

```pascal
type
  TRectangle = class
  private
    FLargeur: Integer;
    FHauteur: Integer;

    // Getter qui calcule l'aire
    function GetAire: Integer;
  public
    property Largeur: Integer read FLargeur write FLargeur;
    property Hauteur: Integer read FHauteur write FHauteur;

    // ✅ Property calculée (pas d'attribut FAire)
    property Aire: Integer read GetAire;
  end;

function TRectangle.GetAire: Integer;
begin
  Result := FLargeur * FHauteur;
  WriteLn('📐 Calcul de l''aire : ', FLargeur, ' × ', FHauteur, ' = ', Result);
end;

var
  Rect: TRectangle;
begin
  Rect := TRectangle.Create;

  Rect.Largeur := 10;
  Rect.Hauteur := 5;

  // L'aire est calculée à chaque lecture
  WriteLn('Aire : ', Rect.Aire);  // 50

  Rect.Largeur := 20;
  WriteLn('Aire : ', Rect.Aire);  // 100 (recalculé)

  Rect.Free;
end.
```

**Avantage :** La valeur est toujours à jour, pas besoin de la recalculer manuellement.

---

### Setter avec effets de bord

Un setter peut **déclencher des actions** lors d'une modification :

```pascal
type
  TLumiere = class
  private
    FAllumee: Boolean;
    FIntensite: Integer;

    procedure SetAllumee(Value: Boolean);
    procedure SetIntensite(Value: Integer);
  public
    property Allumee: Boolean read FAllumee write SetAllumee;
    property Intensite: Integer read FIntensite write SetIntensite;
  end;

procedure TLumiere.SetAllumee(Value: Boolean);
begin
  FAllumee := Value;

  if Value then
  begin
    WriteLn('💡 Lumière allumée');
    // On pourrait envoyer un signal au matériel ici
  end
  else
  begin
    WriteLn('🌑 Lumière éteinte');
    FIntensite := 0;  // ✅ Effet de bord : réinitialiser l'intensité
  end;
end;

procedure TLumiere.SetIntensite(Value: Integer);
begin
  if not FAllumee then
  begin
    WriteLn('⚠️  Impossible de régler l''intensité : lumière éteinte');
    Exit;
  end;

  if (Value < 0) or (Value > 100) then
  begin
    WriteLn('❌ Intensité doit être entre 0 et 100');
    Exit;
  end;

  FIntensite := Value;
  WriteLn('🔆 Intensité réglée à ', Value, '%');
end;

var
  Lumiere: TLumiere;
begin
  Lumiere := TLumiere.Create;

  Lumiere.Intensite := 50;   // ⚠️ Lumière éteinte

  Lumiere.Allumee := True;   // Allume la lumière
  Lumiere.Intensite := 75;   // ✅ OK maintenant

  Lumiere.Allumee := False;  // Éteint et réinitialise l'intensité

  Lumiere.Free;
end.
```

**Résultat :**
```
⚠️  Impossible de régler l'intensité : lumière éteinte
💡 Lumière allumée
🔆 Intensité réglée à 75%
🌑 Lumière éteinte
```

---

## Validation dans les setters

### Exemple : Validation d'âge

```pascal
type
  TUtilisateur = class
  private
    FNom: string;
    FAge: Integer;
    FEmail: string;

    procedure SetAge(Value: Integer);
    procedure SetEmail(const Value: string);
  public
    property Nom: string read FNom write FNom;
    property Age: Integer read FAge write SetAge;
    property Email: string read FEmail write SetEmail;
  end;

procedure TUtilisateur.SetAge(Value: Integer);
begin
  if (Value < 0) or (Value > 150) then
  begin
    WriteLn('❌ Âge invalide : ', Value);
    raise Exception.Create('L''âge doit être entre 0 et 150');
  end;

  FAge := Value;
  WriteLn('✅ Âge défini : ', Value, ' ans');
end;

procedure TUtilisateur.SetEmail(const Value: string);
begin
  // Validation simple (juste vérifier la présence d'un @)
  if Pos('@', Value) = 0 then
  begin
    WriteLn('❌ Email invalide : ', Value);
    raise Exception.Create('L''email doit contenir un @');
  end;

  FEmail := Value;
  WriteLn('✅ Email défini : ', Value);
end;

var
  User: TUtilisateur;
begin
  User := TUtilisateur.Create;

  try
    User.Nom := 'Jean Dupont';
    User.Age := 30;                          // ✅ OK
    User.Email := 'jean@exemple.com';        // ✅ OK

    WriteLn('');
    WriteLn('Utilisateur créé :');
    WriteLn('  Nom : ', User.Nom);
    WriteLn('  Âge : ', User.Age);
    WriteLn('  Email : ', User.Email);

    // User.Age := 200;                      // ❌ Exception
    // User.Email := 'email-invalide';       // ❌ Exception
  finally
    User.Free;
  end;
end.
```

---

## Property calculée (sans attribut de stockage)

### Exemple : Nom complet

```pascal
type
  TPersonne = class
  private
    FPrenom: string;
    FNom: string;

    function GetNomComplet: string;
    procedure SetNomComplet(const Value: string);
  public
    property Prenom: string read FPrenom write FPrenom;
    property Nom: string read FNom write FNom;

    // ✅ Property sans attribut de stockage
    property NomComplet: string read GetNomComplet write SetNomComplet;
  end;

function TPersonne.GetNomComplet: string;
begin
  // Calcul à partir des autres attributs
  Result := FPrenom + ' ' + FNom;
end;

procedure TPersonne.SetNomComplet(const Value: string);
var
  Position: Integer;
begin
  // Séparer le nom complet en prénom et nom
  Position := Pos(' ', Value);

  if Position > 0 then
  begin
    FPrenom := Copy(Value, 1, Position - 1);
    FNom := Copy(Value, Position + 1, Length(Value));
  end
  else
  begin
    FPrenom := Value;
    FNom := '';
  end;

  WriteLn('✅ Nom complet défini : ', Value);
  WriteLn('   Prénom : ', FPrenom);
  WriteLn('   Nom : ', FNom);
end;

var
  Personne: TPersonne;
begin
  Personne := TPersonne.Create;

  // Méthode 1 : Définir séparément
  Personne.Prenom := 'Marie';
  Personne.Nom := 'Curie';
  WriteLn('Nom complet : ', Personne.NomComplet);
  WriteLn('');

  // Méthode 2 : Définir d'un coup
  Personne.NomComplet := 'Albert Einstein';
  WriteLn('Prénom : ', Personne.Prenom);
  WriteLn('Nom : ', Personne.Nom);

  Personne.Free;
end.
```

**Résultat :**
```
Nom complet : Marie Curie

✅ Nom complet défini : Albert Einstein
   Prénom : Albert
   Nom : Einstein
Prénom : Albert
Nom : Einstein
```

---

## Properties avec logique métier complexe

### Exemple : Température avec conversion

```pascal
type
  TThermometre = class
  private
    FCelsius: Double;

    function GetFahrenheit: Double;
    procedure SetFahrenheit(Value: Double);

    function GetKelvin: Double;
    procedure SetKelvin(Value: Double);
  public
    // Température en Celsius (stockage)
    property Celsius: Double read FCelsius write FCelsius;

    // Température en Fahrenheit (calculée)
    property Fahrenheit: Double read GetFahrenheit write SetFahrenheit;

    // Température en Kelvin (calculée)
    property Kelvin: Double read GetKelvin write SetKelvin;
  end;

function TThermometre.GetFahrenheit: Double;
begin
  Result := (FCelsius * 9 / 5) + 32;
end;

procedure TThermometre.SetFahrenheit(Value: Double);
begin
  FCelsius := (Value - 32) * 5 / 9;
  WriteLn('🌡️  Température définie : ', Value:0:1, '°F = ', FCelsius:0:1, '°C');
end;

function TThermometre.GetKelvin: Double;
begin
  Result := FCelsius + 273.15;
end;

procedure TThermometre.SetKelvin(Value: Double);
begin
  FCelsius := Value - 273.15;
  WriteLn('🌡️  Température définie : ', Value:0:1, 'K = ', FCelsius:0:1, '°C');
end;

var
  Thermo: TThermometre;
begin
  Thermo := TThermometre.Create;

  // Définir en Celsius
  Thermo.Celsius := 20;
  WriteLn('20°C = ', Thermo.Fahrenheit:0:1, '°F = ', Thermo.Kelvin:0:1, 'K');
  WriteLn('');

  // Définir en Fahrenheit
  Thermo.Fahrenheit := 68;
  WriteLn('Celsius : ', Thermo.Celsius:0:1, '°C');
  WriteLn('');

  // Définir en Kelvin
  Thermo.Kelvin := 300;
  WriteLn('Celsius : ', Thermo.Celsius:0:1, '°C');
  WriteLn('Fahrenheit : ', Thermo.Fahrenheit:0:1, '°F');

  Thermo.Free;
end.
```

**Résultat :**
```
20°C = 68.0°F = 293.2K

🌡️  Température définie : 68.0°F = 20.0°C
Celsius : 20.0°C

🌡️  Température définie : 300.0K = 26.9°C
Celsius : 26.9°C
Fahrenheit : 80.4°F
```

---

## Properties et interfaces

### Déclarer des properties dans une interface

```pascal
type
  IConfigurable = interface
    ['{12345678-1234-1234-1234-123456789012}']

    // Getters et setters
    function GetNom: string;
    procedure SetNom(const Value: string);

    function GetActif: Boolean;
    procedure SetActif(Value: Boolean);

    // ✅ Properties dans l'interface
    property Nom: string read GetNom write SetNom;
    property Actif: Boolean read GetActif write SetActif;
  end;

  TConfiguration = class(TInterfacedObject, IConfigurable)
  private
    FNom: string;
    FActif: Boolean;
  public
    function GetNom: string;
    procedure SetNom(const Value: string);

    function GetActif: Boolean;
    procedure SetActif(Value: Boolean);

    property Nom: string read GetNom write SetNom;
    property Actif: Boolean read GetActif write SetActif;
  end;

// Implémentation
function TConfiguration.GetNom: string;
begin
  Result := FNom;
end;

procedure TConfiguration.SetNom(const Value: string);
begin
  FNom := Value;
  WriteLn('✅ Nom configuré : ', Value);
end;

function TConfiguration.GetActif: Boolean;
begin
  Result := FActif;
end;

procedure TConfiguration.SetActif(Value: Boolean);
begin
  FActif := Value;
  if Value then
    WriteLn('✅ Configuration activée')
  else
    WriteLn('⏸️  Configuration désactivée');
end;

var
  Config: IConfigurable;
begin
  Config := TConfiguration.Create;

  Config.Nom := 'MaConfig';
  Config.Actif := True;

  WriteLn('Configuration : ', Config.Nom, ' (actif: ', Config.Actif, ')');
end.
```

---

## Conventions de nommage

### Standard Pascal/Delphi

```pascal
type
  TPersonne = class
  private
    FNom: string;        // F + nom de l'attribut
    FAge: Integer;

    function GetNom: string;      // Get + nom de la property
    procedure SetNom(const Value: string);  // Set + nom de la property

    function GetAge: Integer;
    procedure SetAge(Value: Integer);
  public
    property Nom: string read GetNom write SetNom;
    property Age: Integer read GetAge write SetAge;
  end;
```

**Conventions :**
- **Attribut privé** : `F` + nom (ex: `FNom`, `FAge`)
- **Getter** : `Get` + nom (ex: `GetNom`, `GetAge`)
- **Setter** : `Set` + nom (ex: `SetNom`, `SetAge`)
- **Property** : Nom sans préfixe (ex: `Nom`, `Age`)
- **Paramètre du setter** : souvent `Value` ou `AValue`

---

## Bonnes pratiques

### 1. Toujours utiliser des attributs privés

**❌ Mauvais :**
```pascal
type
  TClasse = class
  public
    Attribut: Integer;  // ❌ Public direct
  end;
```

**✅ Bon :**
```pascal
type
  TClasse = class
  private
    FAttribut: Integer;  // ✅ Privé
  public
    property Attribut: Integer read FAttribut write FAttribut;
  end;
```

---

### 2. Valider dans les setters

**✅ Toujours valider les données :**

```pascal
procedure TCompte.SetSolde(Value: Currency);
begin
  if Value < 0 then
    raise Exception.Create('Le solde ne peut pas être négatif');
  FSolde := Value;
end;
```

---

### 3. Property calculée vs attribut stocké

**Choisissez selon le cas :**

```pascal
type
  TRectangle = class
  private
    FLargeur: Integer;
    FHauteur: Integer;

    // ✅ Calculée : toujours à jour
    function GetAire: Integer;
  public
    property Aire: Integer read GetAire;  // Recalculé à chaque lecture
  end;
```

**Vs :**

```pascal
type
  TRectangle = class
  private
    FLargeur: Integer;
    FHauteur: Integer;
    FAire: Integer;  // ⚠️ Stocké : doit être mis à jour

    procedure SetLargeur(Value: Integer);
    procedure SetHauteur(Value: Integer);
  public
    property Largeur: Integer read FLargeur write SetLargeur;
    property Hauteur: Integer read FHauteur write SetHauteur;
    property Aire: Integer read FAire;
  end;

procedure TRectangle.SetLargeur(Value: Integer);
begin
  FLargeur := Value;
  FAire := FLargeur * FHauteur;  // ⚠️ Recalcul nécessaire
end;
```

**Conseil :** Préférez les properties calculées sauf si le calcul est coûteux.

---

### 4. Logging et débogage

**Les setters sont parfaits pour le logging :**

```pascal
procedure TUtilisateur.SetConnecte(Value: Boolean);
begin
  FConnecte := Value;

  // ✅ Logging automatique
  if Value then
    WriteLn('[', DateTimeToStr(Now), '] Utilisateur ', FNom, ' connecté')
  else
    WriteLn('[', DateTimeToStr(Now), '] Utilisateur ', FNom, ' déconnecté');
end;
```

---

### 5. Properties en lecture seule pour les identifiants

```pascal
type
  TEntite = class
  private
    FID: Integer;
  public
    constructor Create(ID: Integer);

    // ✅ Lecture seule : l'ID ne change jamais
    property ID: Integer read FID;
  end;
```

---

## Exemple complet : Classe avec plusieurs types de properties

```pascal
type
  TArticle = class
  private
    FID: Integer;
    FTitre: string;
    FPrixHT: Currency;
    FTauxTVA: Double;
    FStock: Integer;

    procedure SetPrixHT(Value: Currency);
    procedure SetTauxTVA(Value: Double);
    procedure SetStock(Value: Integer);

    function GetPrixTTC: Currency;
    function GetDisponible: Boolean;
  public
    constructor Create(ID: Integer; const Titre: string);

    // Property lecture seule (ID ne change jamais)
    property ID: Integer read FID;

    // Property lecture-écriture simple
    property Titre: string read FTitre write FTitre;

    // Property avec validation
    property PrixHT: Currency read FPrixHT write SetPrixHT;
    property TauxTVA: Double read FTauxTVA write SetTauxTVA;
    property Stock: Integer read FStock write SetStock;

    // Property calculée (lecture seule)
    property PrixTTC: Currency read GetPrixTTC;
    property Disponible: Boolean read GetDisponible;
  end;

constructor TArticle.Create(ID: Integer; const Titre: string);
begin
  FID := ID;
  FTitre := Titre;
  FTauxTVA := 0.20;  // 20% par défaut
  FStock := 0;
end;

procedure TArticle.SetPrixHT(Value: Currency);
begin
  if Value < 0 then
    raise Exception.Create('Le prix ne peut pas être négatif');
  FPrixHT := Value;
end;

procedure TArticle.SetTauxTVA(Value: Double);
begin
  if (Value < 0) or (Value > 1) then
    raise Exception.Create('Le taux de TVA doit être entre 0 et 1');
  FTauxTVA := Value;
end;

procedure TArticle.SetStock(Value: Integer);
begin
  if Value < 0 then
    raise Exception.Create('Le stock ne peut pas être négatif');
  FStock := Value;
end;

function TArticle.GetPrixTTC: Currency;
begin
  Result := FPrixHT * (1 + FTauxTVA);
end;

function TArticle.GetDisponible: Boolean;
begin
  Result := FStock > 0;
end;

// Utilisation
var
  Article: TArticle;
begin
  Article := TArticle.Create(1, 'Clavier mécanique');

  Article.PrixHT := 79.99;
  Article.Stock := 15;

  WriteLn('Article #', Article.ID);
  WriteLn('Titre : ', Article.Titre);
  WriteLn('Prix HT : ', Article.PrixHT:0:2, ' €');
  WriteLn('Prix TTC : ', Article.PrixTTC:0:2, ' €');
  WriteLn('Stock : ', Article.Stock);
  WriteLn('Disponible : ', Article.Disponible);

  Article.Free;
end.
```

---

## Résumé

### Qu'est-ce qu'une property ?
- Interface publique pour accéder aux données privées
- Syntaxe élégante (`Objet.Property` au lieu de `Objet.GetProperty()`)
- Permet de contrôler l'accès en lecture et en écriture

### Types de properties
- **Lecture seule** : `property X: Type read FX;`
- **Écriture seule** : `property X: Type write SetX;`
- **Lecture-écriture** : `property X: Type read FX write SetX;`

### Getters et Setters
- **Getter** : Fonction qui retourne la valeur (peut calculer)
- **Setter** : Procédure qui modifie la valeur (peut valider)

### Avantages
✅ Encapsulation respectée  
✅ Validation des données  
✅ Logique métier centralisée  
✅ Facilite le débogage  
✅ Syntaxe simple pour l'utilisateur  
✅ Flexibilité future (on peut changer l'implémentation)  

### Bonnes pratiques
- Toujours utiliser des attributs privés (`F` + nom)
- Valider dans les setters
- Utiliser des properties calculées quand c'est possible
- Mettre les identifiants en lecture seule
- Logger les changements importants dans les setters

### Convention de nommage
```
FAttribut       → Attribut privé
GetAttribut     → Getter
SetAttribut     → Setter
Attribut        → Property publique
```

---

## Prochaine étape

Dans la section suivante (12.9), vous découvrirez les **méthodes de classe** (class methods), une fonctionnalité avancée qui permet d'appeler des méthodes sans créer d'instance d'objet.

⏭️ [Méthodes de classe (class methods)](/12-interfaces-poo-avancee/09-methodes-classe-class-methods.md)
