🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.5 Comptage de références

## Introduction : La gestion automatique de la mémoire

Dans la section précédente, vous avez découvert que `IInterface` utilise un comptage de références. Maintenant, il est temps de comprendre **en profondeur** comment cela fonctionne dans la pratique et comment en tirer parti dans vos programmes.

**Rappel simple :**
Le comptage de références est comme un **compteur de personnes** qui gardent la porte d'un magasin ouverte. Tant qu'il y a quelqu'un (compteur > 0), le magasin reste ouvert. Quand la dernière personne sort (compteur = 0), on ferme et on fait le ménage.

---

## Comparaison : Gestion manuelle vs automatique

### Avec les classes (gestion manuelle)

```pascal
var
  Obj1, Obj2: TMonObjet;
begin
  Obj1 := TMonObjet.Create;
  Obj2 := Obj1;  // Obj2 pointe vers le même objet

  // ⚠️ DANGER : Qui doit appeler Free ?
  Obj1.Free;  // Obj1 détruit l'objet

  // ❌ BUG : Obj2 pointe maintenant vers un objet détruit !
  Obj2.FaireQuelqueChose;  // CRASH !
end;
```

**Problèmes :**
- Qui est responsable de libérer l'objet ?
- Que se passe-t-il si on libère trop tôt ?
- Que se passe-t-il si on oublie de libérer ?

### Avec les interfaces (gestion automatique)

```pascal
var
  Obj1, Obj2: IMonInterface;
begin
  Obj1 := TImplementation.Create;  // Compteur = 1
  Obj2 := Obj1;                     // Compteur = 2

  Obj1 := nil;                      // Compteur = 1
  Obj2.FaireQuelqueChose;           // ✅ OK, objet toujours valide

  Obj2 := nil;                      // Compteur = 0 → destruction automatique
end;
```

**Avantages :**
- ✅ Pas de confusion sur qui libère
- ✅ Pas de risque de libération trop tôt
- ✅ Pas de fuite mémoire si on oublie de libérer

---

## Le cycle de vie complet d'un objet interface

Voici un programme qui montre **chaque étape** du cycle de vie avec le compteur :

```pascal
program CycleDeVie;

{$mode objfpc}{$H+}

type
  IMessage = interface
    ['{12345678-9ABC-DEF0-1234-567890ABCDEF}']
    procedure Dire(const Texte: string);
  end;

  TMessagerie = class(TInterfacedObject, IMessage)
  private
    FNom: string;
  public
    constructor Create(const Nom: string);
    destructor Destroy; override;
    procedure Dire(const Texte: string);
    procedure AfficherCompteur;
  end;

constructor TMessagerie.Create(const Nom: string);
begin
  inherited Create;
  FNom := Nom;
  WriteLn('╔════════════════════════════════════════╗');
  WriteLn('║ ✅ Création de "', FNom, '"');
  WriteLn('║ Compteur initial: ', RefCount);
  WriteLn('╚════════════════════════════════════════╝');
end;

destructor TMessagerie.Destroy;
begin
  WriteLn('╔════════════════════════════════════════╗');
  WriteLn('║ ❌ Destruction de "', FNom, '"');
  WriteLn('║ Le compteur a atteint 0');
  WriteLn('╚════════════════════════════════════════╝');
  inherited;
end;

procedure TMessagerie.Dire(const Texte: string);
begin
  WriteLn('💬 ', FNom, ' dit: "', Texte, '"');
end;

procedure TMessagerie.AfficherCompteur;
begin
  WriteLn('📊 Compteur actuel de "', FNom, '": ', RefCount);
end;

var
  Obj: TMessagerie;            // Variable objet : accès à AfficherCompteur (hors interface)
  Ref1, Ref2, Ref3: IMessage;  // Variables interface : participent au comptage de références
begin
  WriteLn('');
  WriteLn('═══════════════════════════════════════════════════');
  WriteLn('   DÉMONSTRATION DU COMPTAGE DE RÉFÉRENCES');
  WriteLn('═══════════════════════════════════════════════════');
  WriteLn('');

  WriteLn('▶ Étape 1: Création et assignation à Ref1');
  Obj := TMessagerie.Create('Assistant');
  Ref1 := Obj;
  Obj.AfficherCompteur;
  WriteLn('');

  WriteLn('▶ Étape 2: Assignation à Ref2 (même objet)');
  Ref2 := Ref1;
  Obj.AfficherCompteur;
  WriteLn('');

  WriteLn('▶ Étape 3: Assignation à Ref3 (toujours le même)');
  Ref3 := Ref1;
  Obj.AfficherCompteur;
  WriteLn('');

  WriteLn('▶ Étape 4: Utilisation via différentes références');
  Ref1.Dire('Bonjour depuis Ref1');
  Ref2.Dire('Bonjour depuis Ref2');
  Ref3.Dire('Bonjour depuis Ref3');
  WriteLn('   (C''est le MÊME objet qui parle 3 fois !)');
  WriteLn('');

  WriteLn('▶ Étape 5: Libération de Ref1');
  Ref1 := nil;
  WriteLn('   ℹ️ Objet toujours vivant (Ref2 et Ref3 existent)');
  Obj.AfficherCompteur;
  WriteLn('');

  WriteLn('▶ Étape 6: Libération de Ref2');
  Ref2 := nil;
  WriteLn('   ℹ️ Objet toujours vivant (Ref3 existe)');
  Obj.AfficherCompteur;
  WriteLn('');

  WriteLn('▶ Étape 7: Libération de Ref3 (dernière référence)');
  Ref3 := nil;
  WriteLn('   ℹ️ Le compteur atteint 0 → Destruction automatique !');
  WriteLn('');

  WriteLn('═══════════════════════════════════════════════════');
  WriteLn('   FIN DU PROGRAMME');
  WriteLn('═══════════════════════════════════════════════════');
end.
```

**Résultat attendu :**
```
═══════════════════════════════════════════════════
   DÉMONSTRATION DU COMPTAGE DE RÉFÉRENCES
═══════════════════════════════════════════════════

▶ Étape 1: Création et assignation à Ref1
╔════════════════════════════════════════╗
║ ✅ Création de "Assistant"
║ Compteur initial: 1
╚════════════════════════════════════════╝
📊 Compteur actuel de "Assistant": 1

▶ Étape 2: Assignation à Ref2 (même objet)
📊 Compteur actuel de "Assistant": 2

▶ Étape 3: Assignation à Ref3 (toujours le même)
📊 Compteur actuel de "Assistant": 3

▶ Étape 4: Utilisation via différentes références
💬 Assistant dit: "Bonjour depuis Ref1"
💬 Assistant dit: "Bonjour depuis Ref2"
💬 Assistant dit: "Bonjour depuis Ref3"
   (C'est le MÊME objet qui parle 3 fois !)

▶ Étape 5: Libération de Ref1
   ℹ️ Objet toujours vivant (Ref2 et Ref3 existent)
📊 Compteur actuel de "Assistant": 2

▶ Étape 6: Libération de Ref2
   ℹ️ Objet toujours vivant (Ref3 existe)
📊 Compteur actuel de "Assistant": 1

▶ Étape 7: Libération de Ref3 (dernière référence)
╔════════════════════════════════════════╗
║ ❌ Destruction de "Assistant"
║ Le compteur a atteint 0
╚════════════════════════════════════════╝
   ℹ️ Le compteur atteint 0 → Destruction automatique !

═══════════════════════════════════════════════════
   FIN DU PROGRAMME
═══════════════════════════════════════════════════
```

---

## Cas pratique 1 : Passage d'interface en paramètre

### Comportement lors d'un appel de fonction

```pascal
type
  ICalculateur = interface
    ['{AAAAAAAA-BBBB-CCCC-DDDD-EEEEEEEEEEEE}']
    function Calculer: Integer;
  end;

  TCalculateur = class(TInterfacedObject, ICalculateur)
  private
    FValeur: Integer;
  public
    constructor Create(Valeur: Integer);
    destructor Destroy; override;
    function Calculer: Integer;
  end;

constructor TCalculateur.Create(Valeur: Integer);
begin
  inherited Create;
  FValeur := Valeur;
  WriteLn('✅ Calculateur créé avec valeur ', FValeur, ' (Compteur: ', RefCount, ')');
end;

destructor TCalculateur.Destroy;
begin
  WriteLn('❌ Calculateur détruit');
  inherited;
end;

function TCalculateur.Calculer: Integer;
begin
  Result := FValeur * 2;
  WriteLn('🔢 Calcul effectué (Compteur: ', RefCount, ')');
end;

// Fonction qui reçoit une interface
procedure UtiliserCalculateur(Calc: ICalculateur);
begin
  WriteLn('📥 Entrée dans UtiliserCalculateur (Compteur: ', TCalculateur(Calc).RefCount, ')');
  WriteLn('   Résultat: ', Calc.Calculer);
  WriteLn('📤 Sortie de UtiliserCalculateur (Compteur: ', TCalculateur(Calc).RefCount, ')');
end;

var
  MonCalc: ICalculateur;
begin
  WriteLn('▶ Création du calculateur');
  MonCalc := TCalculateur.Create(10);
  WriteLn('');

  WriteLn('▶ Appel de la fonction');
  UtiliserCalculateur(MonCalc);  // Compteur temporairement à 2
  WriteLn('');

  WriteLn('▶ Après la fonction (Compteur: ', TCalculateur(MonCalc).RefCount, ')');
  WriteLn('');

  WriteLn('▶ Libération de MonCalc');
  MonCalc := nil;
  WriteLn('');
end.
```

**Résultat :**
```
▶ Création du calculateur
✅ Calculateur créé avec valeur 10 (Compteur: 1)

▶ Appel de la fonction
📥 Entrée dans UtiliserCalculateur (Compteur: 2)
🔢 Calcul effectué (Compteur: 2)
   Résultat: 20
📤 Sortie de UtiliserCalculateur (Compteur: 2)

▶ Après la fonction (Compteur: 1)

▶ Libération de MonCalc
❌ Calculateur détruit
```

**Analyse :**
- À l'entrée de la fonction : compteur passe de 1 à 2
- Pendant la fonction : compteur reste à 2
- À la sortie de la fonction : compteur revient à 1
- Quand `MonCalc := nil` : compteur atteint 0 → destruction

---

## Cas pratique 2 : Retour d'interface depuis une fonction

### Fabrique d'objets (Factory)

```pascal
type
  IVehicule = interface
    ['{11111111-2222-3333-4444-555555555555}']
    procedure Rouler;
  end;

  TVoiture = class(TInterfacedObject, IVehicule)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Rouler;
  end;

constructor TVoiture.Create;
begin
  inherited Create;
  WriteLn('🚗 Voiture créée (Compteur: ', RefCount, ')');
end;

destructor TVoiture.Destroy;
begin
  WriteLn('🔧 Voiture détruite');
  inherited;
end;

procedure TVoiture.Rouler;
begin
  WriteLn('🛣️  La voiture roule (Compteur: ', RefCount, ')');
end;

// Fonction qui crée et retourne une interface
function CreerVehicule: IVehicule;
begin
  WriteLn('🏭 Fabrication d''un véhicule...');
  Result := TVoiture.Create;
  WriteLn('   Dans CreerVehicule (Compteur: ', TVoiture(Result).RefCount, ')');
end;

var
  MonVehicule: IVehicule;
begin
  WriteLn('▶ Appel de la fabrique');
  MonVehicule := CreerVehicule;
  WriteLn('   Après retour (Compteur: ', TVoiture(MonVehicule).RefCount, ')');
  WriteLn('');

  WriteLn('▶ Utilisation du véhicule');
  MonVehicule.Rouler;
  WriteLn('');

  WriteLn('▶ Fin de l''utilisation');
  MonVehicule := nil;
  WriteLn('');
end.
```

**Résultat :**
```
▶ Appel de la fabrique
🏭 Fabrication d'un véhicule...
🚗 Voiture créée (Compteur: 1)
   Dans CreerVehicule (Compteur: 1)
   Après retour (Compteur: 1)

▶ Utilisation du véhicule
🛣️  La voiture roule (Compteur: 1)

▶ Fin de l'utilisation
🔧 Voiture détruite
```

**Analyse :**
- La fonction crée l'objet (compteur = 1)
- L'objet est retourné à l'appelant
- Le compteur reste à 1 (transfert de propriété)
- Quand l'appelant libère, l'objet est détruit

---

## Cas pratique 3 : Collections d'interfaces

### Liste d'objets interface

```pascal
uses
  Classes, Generics.Collections;

type
  IEmploye = interface
    ['{BBBBBBBB-CCCC-DDDD-EEEE-FFFFFFFFFFFF}']
    function ObtenirNom: string;
    procedure Travailler;
  end;

  TEmploye = class(TInterfacedObject, IEmploye)
  private
    FNom: string;
  public
    constructor Create(const Nom: string);
    destructor Destroy; override;
    function ObtenirNom: string;
    procedure Travailler;
  end;

constructor TEmploye.Create(const Nom: string);
begin
  inherited Create;
  FNom := Nom;
  WriteLn('✅ ', FNom, ' embauché(e) (Compteur: ', RefCount, ')');
end;

destructor TEmploye.Destroy;
begin
  WriteLn('❌ ', FNom, ' a quitté l''entreprise');
  inherited;
end;

function TEmploye.ObtenirNom: string;
begin
  Result := FNom;
end;

procedure TEmploye.Travailler;
begin
  WriteLn('💼 ', FNom, ' travaille (Compteur: ', RefCount, ')');
end;

var
  Equipe: TList<IEmploye>;
  Employe: IEmploye;
  i: Integer;
begin
  Equipe := TList<IEmploye>.Create;

  WriteLn('▶ Constitution de l''équipe');
  Equipe.Add(TEmploye.Create('Alice'));   // Compteur = 1 (dans la liste)
  Equipe.Add(TEmploye.Create('Bob'));     // Compteur = 1
  Equipe.Add(TEmploye.Create('Charlie')); // Compteur = 1
  WriteLn('');

  WriteLn('▶ Mise au travail de l''équipe');
  for Employe in Equipe do
  begin
    Employe.Travailler;  // Compteur temporairement à 2
  end;
  WriteLn('');

  WriteLn('▶ Retrait d''un employé (Bob)');
  Equipe.Delete(1);  // Compteur de Bob = 0 → destruction
  WriteLn('');

  WriteLn('▶ Licenciement collectif (clear)');
  Equipe.Clear;  // Tous les compteurs = 0 → destructions
  WriteLn('');

  WriteLn('▶ Fermeture de l''entreprise');
  Equipe.Free;
  WriteLn('');
end.
```

**Résultat :**
```
▶ Constitution de l'équipe
✅ Alice embauché(e) (Compteur: 1)
✅ Bob embauché(e) (Compteur: 1)
✅ Charlie embauché(e) (Compteur: 1)

▶ Mise au travail de l'équipe
💼 Alice travaille (Compteur: 2)
💼 Bob travaille (Compteur: 2)
💼 Charlie travaille (Compteur: 2)

▶ Retrait d'un employé (Bob)
❌ Bob a quitté l'entreprise

▶ Licenciement collectif (clear)
❌ Alice a quitté l'entreprise
❌ Charlie a quitté l'entreprise

▶ Fermeture de l'entreprise
```

---

## Pièges courants et solutions

### Piège 1 : Mélanger objets et interfaces

**❌ Code dangereux :**

```pascal
var
  Obj: TVoiture;
  IntfRef: IVehicule;
begin
  Obj := TVoiture.Create;
  IntfRef := Obj;  // ⚠️ Danger !

  Obj.Free;        // Détruit l'objet
  IntfRef.Rouler;  // ❌ CRASH ! Objet déjà détruit
end;
```

**✅ Solution : Choisir un seul mode**

```pascal
// Option 1 : Tout en interface (recommandé)
var
  IntfRef: IVehicule;
begin
  IntfRef := TVoiture.Create;
  IntfRef.Rouler;
  // Destruction automatique
end;

// Option 2 : Tout en classe (si pas besoin d'interface)
var
  Obj: TVoiture;
begin
  Obj := TVoiture.Create;
  Obj.Rouler;
  Obj.Free;
end;
```

---

### Piège 2 : Oublier que plusieurs variables partagent le même objet

**❌ Comportement mal compris :**

```pascal
var
  Ref1, Ref2: IMonInterface;
begin
  Ref1 := TImplementation.Create;
  Ref2 := Ref1;  // Même objet !

  Ref1.ModifierValeur(10);
  WriteLn(Ref2.ObtenirValeur);  // Affiche 10, pas une valeur différente !
end;
```

**Comprendre :**
- `Ref1` et `Ref2` pointent vers le **MÊME objet**
- Modifier via `Ref1` affecte ce que voit `Ref2`
- C'est comme deux télécommandes pour la même télévision

---

### Piège 3 : Référence circulaire (cycle)

**❌ Fuite mémoire :**

```pascal
type
  IPersonne = interface;

  IPersonne = interface
    ['{...}']
    procedure DefinirParent(Parent: IPersonne);
  end;

  TPersonne = class(TInterfacedObject, IPersonne)
  private
    FParent: IPersonne;  // ⚠️ Référence qui peut créer un cycle
  public
    procedure DefinirParent(Parent: IPersonne);
  end;

procedure TPersonne.DefinirParent(Parent: IPersonne);
begin
  FParent := Parent;
end;

var
  Parent, Enfant: IPersonne;
begin
  Parent := TPersonne.Create;
  Enfant := TPersonne.Create;

  Enfant.DefinirParent(Parent);  // Enfant → Parent
  Parent.DefinirParent(Enfant);  // Parent → Enfant (CYCLE !)

  // Parent et Enfant ne seront JAMAIS détruits !
  // Compteurs restent > 0
end;
```

**✅ Solution 1 : Éviter les cycles**

Repenser la structure pour éviter les références mutuelles.

**✅ Solution 2 : Utiliser une référence faible (avancé)**

```pascal
type
  TPersonne = class(TInterfacedObject, IPersonne)
  private
    FParentFaible: Pointer;  // Pointeur non comptabilisé
  public
    procedure DefinirParent(Parent: IPersonne);
  end;
```

**Pour les débutants :** Évitez simplement de créer des cycles dans votre design.

---

## Quand le comptage de références se déclenche

### Opérations qui incrémentent (+1)

```pascal
Ref2 := Ref1;                    // Assignation
MaFonction(Ref1);                // Passage en paramètre
Ref1 := MaFonction();            // Récupération depuis fonction
Liste.Add(Ref1);                 // Ajout à une collection
```

### Opérations qui décrémentent (-1)

```pascal
Ref1 := nil;                     // Mise à nil explicite
Ref1 := AutreObjet;              // Réassignation
end;                             // Fin de portée (sortie de begin..end)
Liste.Delete(0);                 // Retrait d'une collection
Liste.Clear;                     // Vidage d'une collection
```

### Opérations neutres (compteur inchangé)

```pascal
Ref1.UneMethode();               // Appel de méthode
if Ref1 <> nil then              // Test de validité
Supports(Ref1, IAutreInterface)  // Test d'interface
```

---

## Visualisation du compteur en temps réel

### Créer une classe de débogage

```pascal
type
  IDebug = interface
    ['{CCCCCCCC-DDDD-EEEE-FFFF-000000000000}']
    procedure Action;
  end;

  TDebug = class(TInterfacedObject, IDebug)
  private
    FID: Integer;
    procedure LogCompteur(const Contexte: string);
  public
    constructor Create(ID: Integer);
    destructor Destroy; override;
    procedure Action;
  end;

constructor TDebug.Create(ID: Integer);
begin
  inherited Create;
  FID := ID;
  LogCompteur('Création');
end;

destructor TDebug.Destroy;
begin
  WriteLn('[Objet ', FID, '] ❌ DESTRUCTION');
  inherited;
end;

procedure TDebug.LogCompteur(const Contexte: string);
begin
  WriteLn('[Objet ', FID, '] ', Contexte, ' - Compteur: ', RefCount);
end;

procedure TDebug.Action;
begin
  LogCompteur('Action');
end;

// Utilisation
var
  A, B, C: IDebug;
begin
  WriteLn('=== Test de comptage ===');
  A := TDebug.Create(1);          // Compteur = 1
  A.Action;

  B := A;                         // Compteur = 2
  B.Action;

  C := A;                         // Compteur = 3
  C.Action;

  A := nil;                       // Compteur = 2
  B.Action;

  B := nil;                       // Compteur = 1
  C.Action;

  C := nil;                       // Compteur = 0 → destruction
  WriteLn('Fin du test');
end.
```

---

## Bonnes pratiques

### ✅ À faire

```pascal
// 1. Utilisez des interfaces pour le polymorphisme
var
  Animal: IAnimal;
begin
  if Condition then
    Animal := TChien.Create
  else
    Animal := TChat.Create;
  Animal.Manger;  // Polymorphisme
end;

// 2. Retournez des interfaces depuis les fonctions
function CreerLogger: ILogger;
begin
  Result := TFileLogger.Create;
end;

// 3. Stockez des interfaces dans des collections
var
  ListeTaches: TList<ITache>;
begin
  ListeTaches := TList<ITache>.Create;
  ListeTaches.Add(TTache.Create);
  // ...
  ListeTaches.Free;
end;
```

### ❌ À éviter

```pascal
// 1. Ne mélangez pas classes et interfaces
var
  Obj: TMonObjet;
  Intf: IMonInterface;
begin
  Obj := TMonObjet.Create;
  Intf := Obj;
  Obj.Free;      // ❌ Dangereux !
end;

// 2. N'appelez pas _AddRef/_Release manuellement
Intf._AddRef;    // ❌ Ne jamais faire !
Intf._Release;   // ❌ Ne jamais faire !

// 3. Ne créez pas de cycles de références
// ❌ Parent → Enfant → Parent
```

---

## Performances du comptage de références

### Coût en performance

Le comptage de références a un **léger coût** :
- Incrémentation/décrémentation à chaque assignation
- Vérification du compteur à chaque décrémentation
- Appel du destructeur quand compteur = 0

**Impact réel :** Négligeable pour la plupart des applications.

### Quand s'en préoccuper ?

Préoccupez-vous des performances **seulement si** :
- Vous manipulez des millions d'objets
- Vous faites des assignations dans des boucles très rapides
- Le profiling montre que c'est un goulot d'étranglement

**Pour 99% des cas :** Les avantages (pas de fuite, code plus sûr) dépassent largement le coût.

---

## Résumé

### Principe du comptage de références
- Compteur interne qui suit le nombre de références actives
- Incrémentation automatique lors d'assignations
- Décrémentation automatique lors de libérations
- Destruction automatique quand compteur = 0

### Avantages majeurs
✅ Pas de fuite mémoire  
✅ Pas de `Free` à gérer  
✅ Partage sécurisé d'objets  
✅ Code plus simple et plus sûr  

### Pièges à éviter
❌ Mélanger classes et interfaces  
❌ Créer des références circulaires  
❌ Appeler `Free` sur une interface  
❌ Appeler `_AddRef`/`_Release` manuellement  

### Règle d'or
**Si vous utilisez une interface, laissez FreePascal gérer la mémoire automatiquement !**

---

## Prochaine étape

Dans la section suivante (12.6), vous découvrirez les différences entre **interfaces et classes abstraites**, et comment choisir entre les deux selon vos besoins.

⏭️ [Interfaces vs classes abstraites](/12-interfaces-poo-avancee/06-interfaces-vs-classes-abstraites.md)
