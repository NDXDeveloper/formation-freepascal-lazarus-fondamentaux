🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.4 Méthodes virtuelles et override

## Introduction

Dans cette section, nous allons approfondir le mécanisme des **méthodes virtuelles** et leur redéfinition avec **override**. Vous allez comprendre comment Pascal/FreePascal décide quelle méthode appeler, et pourquoi ce mécanisme est si puissant pour créer du code flexible et extensible.

## Liaison statique vs liaison dynamique

### Liaison statique (early binding)

Avec les méthodes **normales** (non virtuelles), le compilateur décide **à la compilation** quelle méthode appeler, en fonction du **type de la variable** :

```pascal
type
  TAnimal = class
    procedure Crier;  // Méthode normale (non virtuelle)
  end;

  TChien = class(TAnimal)
    procedure Crier;  // Masquage
  end;

var
  Animal: TAnimal;
  Chien: TChien;
begin
  Chien := TChien.Create;
  Animal := Chien;  // Le chien est vu comme un animal

  Animal.Crier;  // Appelle TAnimal.Crier (type de la variable)
  Chien.Crier;   // Appelle TChien.Crier (type de la variable)
end;
```

**Décision prise au moment de la compilation** : "Animal est de type TAnimal, donc j'appelle TAnimal.Crier".

### Liaison dynamique (late binding)

Avec les méthodes **virtuelles**, la décision est reportée **à l'exécution**, en fonction du **type réel de l'objet** :

```pascal
type
  TAnimal = class
    procedure Crier; virtual;  // Méthode virtuelle
  end;

  TChien = class(TAnimal)
    procedure Crier; override;  // Redéfinition
  end;

var
  Animal: TAnimal;
  Chien: TChien;
begin
  Chien := TChien.Create;
  Animal := Chien;  // Le chien est vu comme un animal

  Animal.Crier;  // Appelle TChien.Crier (type réel de l'objet) ✓
  Chien.Crier;   // Appelle TChien.Crier
end;
```

**Décision prise à l'exécution** : "Animal pointe vers un TChien, donc j'appelle TChien.Crier".

## Comment ça fonctionne techniquement ?

### La table des méthodes virtuelles (VMT)

Chaque classe possède une **table des méthodes virtuelles** (VMT - Virtual Method Table). C'est un tableau qui contient les adresses des méthodes virtuelles de la classe.

```
TAnimal.VMT:
  [0] → adresse de TAnimal.Crier
  [1] → adresse de TAnimal.Manger
  ...

TChien.VMT:
  [0] → adresse de TChien.Crier      ← Redéfini !
  [1] → adresse de TAnimal.Manger    ← Hérité
  ...
```

Quand vous appelez une méthode virtuelle :
1. Le système regarde le **type réel** de l'objet
2. Il consulte sa VMT
3. Il appelle la méthode pointée dans la VMT

C'est ce qu'on appelle le **polymorphisme** : un même appel peut exécuter différentes méthodes selon le type réel de l'objet.

## Exemple complet illustrant la liaison dynamique

```pascal
program LiaisonDynamique;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Classe de base }
  TVehicule = class
  protected
    FMarque: string;
    FModele: string;
  public
    constructor Create(AMarque, AModele: string);

    // Méthode NON virtuelle
    procedure AfficherType;

    // Méthodes virtuelles
    procedure Demarrer; virtual;
    procedure Accelerer; virtual;
    procedure Freiner; virtual;
    function GetVitesseMax: Integer; virtual;
  end;

  { Voiture }
  TVoiture = class(TVehicule)
  public
    procedure Demarrer; override;
    procedure Accelerer; override;
    procedure Freiner; override;
    function GetVitesseMax: Integer; override;
  end;

  { Moto }
  TMoto = class(TVehicule)
  public
    procedure Demarrer; override;
    procedure Accelerer; override;
    procedure Freiner; override;
    function GetVitesseMax: Integer; override;
  end;

  { Camion }
  TCamion = class(TVehicule)
  public
    procedure Demarrer; override;
    procedure Accelerer; override;
    procedure Freiner; override;
    function GetVitesseMax: Integer; override;
  end;

{ === TVehicule === }

constructor TVehicule.Create(AMarque, AModele: string);  
begin  
  inherited Create;
  FMarque := AMarque;
  FModele := AModele;
end;

procedure TVehicule.AfficherType;  
begin  
  WriteLn('Type : Véhicule générique');
end;

procedure TVehicule.Demarrer;  
begin  
  WriteLn('[Véhicule] Démarrage générique');
end;

procedure TVehicule.Accelerer;  
begin  
  WriteLn('[Véhicule] Accélération générique');
end;

procedure TVehicule.Freiner;  
begin  
  WriteLn('[Véhicule] Freinage générique');
end;

function TVehicule.GetVitesseMax: Integer;  
begin  
  Result := 100;
end;

{ === TVoiture === }

procedure TVoiture.Demarrer;  
begin  
  WriteLn('🚗 Tournez la clé, le moteur de la voiture ronronne');
end;

procedure TVoiture.Accelerer;  
begin  
  WriteLn('🚗 La voiture accélère en douceur');
end;

procedure TVoiture.Freiner;  
begin  
  WriteLn('🚗 Freinage progressif de la voiture');
end;

function TVoiture.GetVitesseMax: Integer;  
begin  
  Result := 180;
end;

{ === TMoto === }

procedure TMoto.Demarrer;  
begin  
  WriteLn('🏍️  Vrrooom ! La moto démarre en trombe');
end;

procedure TMoto.Accelerer;  
begin  
  WriteLn('🏍️  Accélération fulgurante de la moto !');
end;

procedure TMoto.Freiner;  
begin  
  WriteLn('🏍️  Freinage sportif de la moto');
end;

function TMoto.GetVitesseMax: Integer;  
begin  
  Result := 220;
end;

{ === TCamion === }

procedure TCamion.Demarrer;  
begin  
  WriteLn('🚚 Le camion démarre lentement avec un bruit sourd');
end;

procedure TCamion.Accelerer;  
begin  
  WriteLn('🚚 Le camion accélère péniblement');
end;

procedure TCamion.Freiner;  
begin  
  WriteLn('🚚 Long freinage du camion chargé');
end;

function TCamion.GetVitesseMax: Integer;  
begin  
  Result := 110;
end;

{ === Procédures de test === }

procedure TestVehicule(V: TVehicule);  
begin  
  WriteLn('--- Test du véhicule : ', V.FMarque, ' ', V.FModele, ' ---');
  WriteLn;

  // Méthode NON virtuelle : liaison statique
  WriteLn('Appel de AfficherType (NON virtuelle) :');
  V.AfficherType;  // Appelle toujours TVehicule.AfficherType
  WriteLn;

  // Méthodes virtuelles : liaison dynamique
  WriteLn('Appels des méthodes virtuelles :');
  V.Demarrer;      // Appelle la version du type réel
  V.Accelerer;     // Appelle la version du type réel
  V.Freiner;       // Appelle la version du type réel
  WriteLn('Vitesse max : ', V.GetVitesseMax, ' km/h');
  WriteLn;
end;

procedure ComparerVehicules(V1, V2: TVehicule);  
begin  
  WriteLn('=== COMPARAISON DE VEHICULES ===');
  WriteLn('Véhicule 1 : vitesse max = ', V1.GetVitesseMax, ' km/h');
  WriteLn('Véhicule 2 : vitesse max = ', V2.GetVitesseMax, ' km/h');

  if V1.GetVitesseMax > V2.GetVitesseMax then
    WriteLn('→ Le véhicule 1 est plus rapide')
  else if V1.GetVitesseMax < V2.GetVitesseMax then
    WriteLn('→ Le véhicule 2 est plus rapide')
  else
    WriteLn('→ Même vitesse maximale');
  WriteLn;
end;

{ === Programme principal === }
var
  MaVoiture: TVoiture;
  MaMoto: TMoto;
  MonCamion: TCamion;
  UnVehicule: TVehicule;
begin
  WriteLn('=== DEMONSTRATION DES METHODES VIRTUELLES ===');
  WriteLn;

  // Création des véhicules
  MaVoiture := TVoiture.Create('Peugeot', '308');
  MaMoto := TMoto.Create('Yamaha', 'MT-07');
  MonCamion := TCamion.Create('Renault', 'Master');

  WriteLn('========================================');
  WriteLn('TEST 1 : Polymorphisme en action');
  WriteLn('========================================');
  WriteLn;

  // Chaque véhicule vu comme un TVehicule
  TestVehicule(MaVoiture);
  TestVehicule(MaMoto);
  TestVehicule(MonCamion);

  WriteLn('========================================');
  WriteLn('TEST 2 : Comparaisons polymorphes');
  WriteLn('========================================');
  WriteLn;

  ComparerVehicules(MaVoiture, MaMoto);
  ComparerVehicules(MaMoto, MonCamion);
  ComparerVehicules(MaVoiture, MonCamion);

  WriteLn('========================================');
  WriteLn('TEST 3 : Changement dynamique de type');
  WriteLn('========================================');
  WriteLn;

  // Une seule variable qui change de type
  WriteLn('UnVehicule pointe maintenant vers la voiture :');
  UnVehicule := MaVoiture;
  UnVehicule.Demarrer;
  WriteLn;

  WriteLn('UnVehicule pointe maintenant vers la moto :');
  UnVehicule := MaMoto;
  UnVehicule.Demarrer;
  WriteLn;

  WriteLn('UnVehicule pointe maintenant vers le camion :');
  UnVehicule := MonCamion;
  UnVehicule.Demarrer;
  WriteLn;

  // Libération
  MaVoiture.Free;
  MaMoto.Free;
  MonCamion.Free;

  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Les différents niveaux de virtualité

### 1. Méthode normale (non virtuelle)

```pascal
procedure MaMethode;
```

- Liaison **statique** (au moment de la compilation)
- Pas de polymorphisme
- Plus rapide (pas de recherche dans la VMT)
- Utilisez-la quand le comportement ne doit **jamais** changer

### 2. Méthode virtuelle (`virtual`)

```pascal
procedure MaMethode; virtual;
```

- Première déclaration d'une méthode redéfinissable
- Liaison **dynamique** (à l'exécution)
- Active le polymorphisme
- Peut avoir une implémentation par défaut

### 3. Redéfinition (`override`)

```pascal
procedure MaMethode; override;
```

- Remplace une méthode `virtual` existante
- Doit avoir **exactement** la même signature
- Reste virtuelle (peut être re-redéfinie)

### 4. Méthode finale (`override` + directive)

```pascal
procedure MaMethode; override; final;
```

- Redéfinit mais **empêche** les redéfinitions ultérieures
- Optimisation possible par le compilateur
- Rarement utilisé en pratique

## Cas d'usage avancés

### Exemple 1 : Système de paiement polymorphe

```pascal
program SystemePaiement;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Interface de paiement }
  TModePaiement = class
  protected
    FMontant: Real;
  public
    constructor Create(AMontant: Real);

    // Méthodes virtuelles communes
    function Valider: Boolean; virtual;
    procedure Traiter; virtual;
    function ObtenirRecu: string; virtual;
  end;

  { Paiement par carte bancaire }
  TPaiementCarte = class(TModePaiement)
  private
    FNumeroCarte: string;
    FCodeCVV: string;
  public
    constructor Create(AMontant: Real; ANumero, ACVV: string);
    function Valider: Boolean; override;
    procedure Traiter; override;
    function ObtenirRecu: string; override;
  end;

  { Paiement par PayPal }
  TPaiementPayPal = class(TModePaiement)
  private
    FEmail: string;
  public
    constructor Create(AMontant: Real; AEmail: string);
    function Valider: Boolean; override;
    procedure Traiter; override;
    function ObtenirRecu: string; override;
  end;

  { Paiement en espèces }
  TPaiementEspeces = class(TModePaiement)
  private
    FMontantDonne: Real;
  public
    constructor Create(AMontant, AMontantDonne: Real);
    function Valider: Boolean; override;
    procedure Traiter; override;
    function ObtenirRecu: string; override;
    function CalculerRendu: Real;
  end;

{ === TModePaiement === }

constructor TModePaiement.Create(AMontant: Real);  
begin  
  inherited Create;
  FMontant := AMontant;
end;

function TModePaiement.Valider: Boolean;  
begin  
  Result := FMontant > 0;
end;

procedure TModePaiement.Traiter;  
begin  
  WriteLn('Traitement générique du paiement de ', FMontant:0:2, ' €');
end;

function TModePaiement.ObtenirRecu: string;  
begin  
  Result := Format('Reçu - Montant : %.2f €', [FMontant]);
end;

{ === TPaiementCarte === }

constructor TPaiementCarte.Create(AMontant: Real; ANumero, ACVV: string);  
begin  
  inherited Create(AMontant);
  FNumeroCarte := ANumero;
  FCodeCVV := ACVV;
end;

function TPaiementCarte.Valider: Boolean;  
begin  
  Result := inherited Valider;
  if Result then
  begin
    Result := (Length(FNumeroCarte) = 16) and (Length(FCodeCVV) = 3);
    if not Result then
      WriteLn('❌ Carte invalide');
  end;
end;

procedure TPaiementCarte.Traiter;  
begin  
  WriteLn('💳 Traitement paiement par carte...');
  WriteLn('   Numéro : **** **** **** ', Copy(FNumeroCarte, 13, 4));
  WriteLn('   Connexion à la banque...');
  WriteLn('   Autorisation reçue');
  WriteLn('   ✅ Paiement de ', FMontant:0:2, ' € accepté');
end;

function TPaiementCarte.ObtenirRecu: string;  
begin  
  Result := inherited ObtenirRecu + #13#10 +
            'Mode : Carte bancaire' + #13#10 +
            'Carte : **** ' + Copy(FNumeroCarte, 13, 4);
end;

{ === TPaiementPayPal === }

constructor TPaiementPayPal.Create(AMontant: Real; AEmail: string);  
begin  
  inherited Create(AMontant);
  FEmail := AEmail;
end;

function TPaiementPayPal.Valider: Boolean;  
begin  
  Result := inherited Valider;
  if Result then
  begin
    Result := Pos('@', FEmail) > 0;
    if not Result then
      WriteLn('❌ Email PayPal invalide');
  end;
end;

procedure TPaiementPayPal.Traiter;  
begin  
  WriteLn('💰 Traitement paiement PayPal...');
  WriteLn('   Email : ', FEmail);
  WriteLn('   Redirection vers PayPal...');
  WriteLn('   Authentification réussie');
  WriteLn('   ✅ Paiement de ', FMontant:0:2, ' € accepté');
end;

function TPaiementPayPal.ObtenirRecu: string;  
begin  
  Result := inherited ObtenirRecu + #13#10 +
            'Mode : PayPal' + #13#10 +
            'Compte : ' + FEmail;
end;

{ === TPaiementEspeces === }

constructor TPaiementEspeces.Create(AMontant, AMontantDonne: Real);  
begin  
  inherited Create(AMontant);
  FMontantDonne := AMontantDonne;
end;

function TPaiementEspeces.Valider: Boolean;  
begin  
  Result := inherited Valider;
  if Result then
  begin
    Result := FMontantDonne >= FMontant;
    if not Result then
      WriteLn('❌ Montant insuffisant');
  end;
end;

procedure TPaiementEspeces.Traiter;  
var  
  Rendu: Real;
begin
  WriteLn('💵 Traitement paiement en espèces...');
  WriteLn('   Montant à payer : ', FMontant:0:2, ' €');
  WriteLn('   Montant donné : ', FMontantDonne:0:2, ' €');
  Rendu := CalculerRendu;
  if Rendu > 0 then
    WriteLn('   Rendu à rendre : ', Rendu:0:2, ' €')
  else
    WriteLn('   Montant exact, pas de rendu');
  WriteLn('   ✅ Paiement accepté');
end;

function TPaiementEspeces.CalculerRendu: Real;  
begin  
  Result := FMontantDonne - FMontant;
end;

function TPaiementEspeces.ObtenirRecu: string;  
var  
  Rendu: Real;
begin
  Rendu := CalculerRendu;
  Result := inherited ObtenirRecu + #13#10 +
            'Mode : Espèces' + #13#10 +
            Format('Donné : %.2f €', [FMontantDonne]);
  if Rendu > 0 then
    Result := Result + #13#10 + Format('Rendu : %.2f €', [Rendu]);
end;

{ === Fonction polymorphe === }

procedure ProcesserPaiement(Paiement: TModePaiement);  
begin  
  WriteLn('========================================');
  WriteLn('TRAITEMENT D''UN PAIEMENT');
  WriteLn('========================================');
  WriteLn;

  // Validation
  WriteLn('→ Validation...');
  if not Paiement.Valider then
  begin
    WriteLn('❌ Paiement refusé');
    Exit;
  end;
  WriteLn('✓ Validation OK');
  WriteLn;

  // Traitement
  WriteLn('→ Traitement...');
  Paiement.Traiter;  // Méthode virtuelle : appelle la bonne version
  WriteLn;

  // Reçu
  WriteLn('→ Génération du reçu...');
  WriteLn('--- RECU ---');
  WriteLn(Paiement.ObtenirRecu);
  WriteLn('------------');
  WriteLn;
end;

{ === Programme principal === }
var
  PaiementCarte: TPaiementCarte;
  PaiementPayPal: TPaiementPayPal;
  PaiementEspeces: TPaiementEspeces;
begin
  WriteLn('=== SYSTEME DE PAIEMENT POLYMORPHE ===');
  WriteLn;

  // Test 1 : Paiement par carte
  PaiementCarte := TPaiementCarte.Create(49.99, '1234567812345678', '123');
  ProcesserPaiement(PaiementCarte);
  PaiementCarte.Free;

  // Test 2 : Paiement PayPal
  PaiementPayPal := TPaiementPayPal.Create(29.90, 'user@example.com');
  ProcesserPaiement(PaiementPayPal);
  PaiementPayPal.Free;

  // Test 3 : Paiement en espèces
  PaiementEspeces := TPaiementEspeces.Create(15.50, 20.00);
  ProcesserPaiement(PaiementEspeces);
  PaiementEspeces.Free;

  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Exemple 2 : Système de notifications

```pascal
type
  TNotification = class
  protected
    FMessage: string;
    FDestinataire: string;
  public
    constructor Create(ADestinataire, AMessage: string);
    procedure Envoyer; virtual;
    function GetStatut: string; virtual;
  end;

  TNotificationEmail = class(TNotification)
  public
    procedure Envoyer; override;
    function GetStatut: string; override;
  end;

  TNotificationSMS = class(TNotification)
  public
    procedure Envoyer; override;
    function GetStatut: string; override;
  end;

  TNotificationPush = class(TNotification)
  public
    procedure Envoyer; override;
    function GetStatut: string; override;
  end;

procedure NotifierUtilisateur(Notif: TNotification);  
begin  
  // Fonction polymorphe : fonctionne avec tous les types de notifications
  Notif.Envoyer;
  WriteLn('Statut : ', Notif.GetStatut);
end;
```

## Pièges courants et solutions

### Piège 1 : Oublier `virtual` dans la classe parent

```pascal
type
  TParent = class
    procedure Test;  // ❌ Pas virtual
  end;

  TEnfant = class(TParent)
    procedure Test; override;  // ❌ ERREUR : ne peut pas override
  end;
```

**Solution** : Toujours mettre `virtual` dans la classe parent.

### Piège 2 : Signature différente

```pascal
type
  TParent = class
    procedure Test(X: Integer); virtual;
  end;

  TEnfant = class(TParent)
    procedure Test(X: string); override;  // ❌ ERREUR : signature différente
  end;
```

**Solution** : La signature doit être **exactement** identique.

### Piège 3 : Oublier `override`

```pascal
type
  TParent = class
    procedure Test; virtual;
  end;

  TEnfant = class(TParent)
    procedure Test;  // ⚠️ Masquage, pas redéfinition !
  end;
```

**Solution** : Toujours utiliser `override` pour redéfinir.

### Piège 4 : Ne pas appeler `inherited` quand nécessaire

```pascal
procedure TEnfant.Create;  
begin  
  // ❌ On oublie d'appeler inherited
  FMonAttribut := 10;
end;
```

**Solution** : Appeler `inherited` au début du constructeur.

## Performances : virtual a-t-il un coût ?

### Coût en performance

Les méthodes virtuelles sont **légèrement** plus lentes que les méthodes normales :
- Méthode normale : appel direct (1 instruction)
- Méthode virtuelle : recherche dans la VMT + appel indirect (2-3 instructions)

### En pratique

- Le coût est **négligeable** dans 99% des cas
- Les avantages (flexibilité, extensibilité) l'emportent largement
- N'optimisez que si le profiling montre un réel problème

**Règle d'or** : Privilégiez la clarté et la flexibilité. Optimisez seulement si nécessaire.

## Bonnes pratiques

### ✅ À FAIRE

1. **Déclarer `virtual` dès la conception**
   - Si vous pensez qu'une méthode pourrait être redéfinie, faites-la virtuelle

2. **Utiliser `override` systématiquement**
   - Ne masquez pas accidentellement

3. **Documenter les méthodes virtuelles**
   ```pascal
   { Calcule le prix TTC.
     Les classes dérivées peuvent appliquer des remises spécifiques. }
   function CalculerPrix: Real; virtual;
   ```

4. **Tester le polymorphisme**
   - Vérifiez que vos méthodes fonctionnent via le type parent

5. **Utiliser `inherited` intelligemment**
   - Pour étendre le comportement, pas le dupliquer

### ❌ À ÉVITER

1. **Tout rendre virtuel "au cas où"**
   - Uniquement ce qui a du sens d'être redéfini

2. **Changer la sémantique dans `override`**
   - La méthode redéfinie doit faire "la même chose" conceptuellement

3. **Oublier de tester tous les types dérivés**

## Quand utiliser `virtual` / `override` ?

### ✅ Utilisez quand :

- Vous créez une hiérarchie de classes avec comportements variables
- Vous voulez du code générique qui fonctionne avec tous les types
- Vous créez un framework ou une bibliothèque extensible
- Vous avez une interface commune avec implémentations multiples

### ❌ N'utilisez pas quand :

- La méthode ne sera jamais redéfinie
- Vous avez juste besoin de réutiliser du code (utilisez la composition)
- La performance est critique ET le profiling le prouve

## Résumé

Les méthodes virtuelles et `override` permettent de :
- ✅ Créer du code polymorphe et flexible
- ✅ Implémenter différents comportements avec une interface commune
- ✅ Écrire des fonctions génériques qui fonctionnent avec tous les types dérivés
- ✅ Faciliter l'extension du code sans modification

**Mécanisme :**
- `virtual` dans la classe parent = "peut être redéfini"
- `override` dans la classe dérivée = "je fournis ma propre implémentation"
- Liaison dynamique = décision à l'exécution selon le type réel
- VMT = table qui stocke les adresses des méthodes virtuelles

**Règle simple :** Si une méthode doit pouvoir avoir différentes implémentations selon les types, rendez-la `virtual` !

Dans la section suivante, nous verrons les **méthodes abstraites** qui vont encore plus loin en **obligeant** les classes dérivées à fournir une implémentation.

⏭️ [Méthodes abstraites et classes abstraites](/11-poo-avancee-heritage/05-methodes-abstraites-classes-abstraites.md)
