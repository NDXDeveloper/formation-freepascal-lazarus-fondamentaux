🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.5 Exceptions personnalisées

## Introduction

FreePascal fournit de nombreuses classes d'exceptions standard comme `EConvertError`, `EDivByZero`, etc. Mais parfois, ces exceptions génériques ne suffisent pas. Vous avez besoin d'exceptions spécifiques à votre application, qui reflètent les erreurs métier de votre domaine. C'est là qu'entrent en jeu les **exceptions personnalisées**.

## Pourquoi créer ses propres exceptions ?

### 1. Clarté du code

Comparez ces deux approches :

```pascal
// Approche générique
if solde < montant then
  raise Exception.Create('Opération impossible');

// Approche avec exception personnalisée
if solde < montant then
  raise ESoldeInsuffisant.Create('Solde insuffisant');
```

La deuxième version est immédiatement compréhensible : c'est un problème de solde insuffisant, pas une erreur générique.

### 2. Gestion ciblée

Avec des exceptions personnalisées, vous pouvez les capturer spécifiquement :

```pascal
try
  EffectuerRetrait(montant);
except
  on E: ESoldeInsuffisant do
    ProposerDecouvert;
  on E: ECompteBloque do
    ContacterService;
  on E: Exception do
    LoggerErreur(E);
end;
```

### 3. Information supplémentaire

Vos exceptions peuvent transporter des données spécifiques :

```pascal
raise ESoldeInsuffisant.Create(soldeActuel, montantDemande);
```

### 4. Sémantique métier

Les exceptions personnalisées reflètent le vocabulaire de votre domaine métier :
- `EFactureNonPayee` plutôt que `Exception`
- `EProduitIndisponible` plutôt que `EInOutError`
- `EUtilisateurNonAuthorise` plutôt que `EAccessViolation`

## Créer une exception simple

### Syntaxe de base

La déclaration d'une exception personnalisée est étonnamment simple :

```pascal
type
  EMonException = class(Exception);
```

C'est tout ! Vous avez créé une nouvelle classe d'exception.

### Exemple complet minimal

```pascal
program ExempleExceptionSimple;

uses
  SysUtils;

type
  EAgeInvalide = class(Exception);  // Préfixe E par convention ; hérite de Exception (SysUtils)

procedure VerifierAge(age: Integer);
begin
  if (age < 0) or (age > 150) then
    raise EAgeInvalide.Create('L''âge doit être entre 0 et 150');  // raise crée et déclenche l'exception

  WriteLn('Âge valide : ', age);
end;

begin
  try
    VerifierAge(200);
  except
    on E: EAgeInvalide do  // on E: Type do — capture l'exception dans E
      WriteLn('Erreur d''âge : ', E.Message);
  end;
end.
```

**Résultat :**
```
Erreur d'âge : L'âge doit être entre 0 et 150
```

## Héritage et hiérarchie personnalisée

Vous pouvez créer votre propre hiérarchie d'exceptions, exactement comme la hiérarchie standard.

### Exemple : hiérarchie bancaire

```pascal
type
  // Exception de base pour toutes les erreurs bancaires
  EBanqueException = class(Exception);

  // Exceptions spécifiques
  ESoldeInsuffisant = class(EBanqueException);
  ECompteBloque = class(EBanqueException);
  ELimiteDepassee = class(EBanqueException);
  ECarteInvalide = class(EBanqueException);
```

Maintenant vous pouvez capturer soit une erreur spécifique, soit toute erreur bancaire :

```pascal
try
  RetirerArgent(montant);
except
  on E: ESoldeInsuffisant do
    WriteLn('Solde insuffisant');
  on E: EBanqueException do
    WriteLn('Erreur bancaire : ', E.Message);  // Capture toutes les erreurs bancaires
end;
```

### Exemple : hiérarchie de validation

```pascal
type
  // Base
  EValidationException = class(Exception);

  // Catégories
  EChampObligatoire = class(EValidationException);
  EFormatInvalide = class(EValidationException);
  EValeurHorsLimites = class(EValidationException);

  // Sous-catégories spécifiques
  EEmailInvalide = class(EFormatInvalide);
  ETelephoneInvalide = class(EFormatInvalide);
  EDateInvalide = class(EFormatInvalide);
```

## Ajouter des propriétés personnalisées

Les exceptions personnalisées peuvent stocker des informations supplémentaires.

### Exemple : Exception avec valeurs

```pascal
type
  ESoldeInsuffisant = class(Exception)
  private
    FSoldeActuel: Double;
    FMontantDemande: Double;
  public
    constructor Create(solde, montant: Double);
    property SoldeActuel: Double read FSoldeActuel;
    property MontantDemande: Double read FMontantDemande;
  end;

constructor ESoldeInsuffisant.Create(solde, montant: Double);
begin
  FSoldeActuel := solde;
  FMontantDemande := montant;

  inherited CreateFmt(
    'Solde insuffisant : vous avez %.2f€ mais vous demandez %.2f€',
    [solde, montant]
  );
end;
```

### Utilisation

```pascal
procedure RetirerArgent(montant: Double);
begin
  if SoldeCompte < montant then
    raise ESoldeInsuffisant.Create(SoldeCompte, montant);

  SoldeCompte := SoldeCompte - montant;
end;

// Capture avec accès aux propriétés
try
  RetirerArgent(100.0);
except
  on E: ESoldeInsuffisant do
  begin
    WriteLn('Manque : ', E.MontantDemande - E.SoldeActuel:0:2, '€');
    ProposerRechargement(E.MontantDemande - E.SoldeActuel);
  end;
end;
```

## Exception avec code d'erreur

Certaines applications utilisent des codes d'erreur numériques :

```pascal
type
  EApplicationException = class(Exception)
  private
    FCodeErreur: Integer;
  public
    constructor Create(code: Integer; const msg: String);
    property CodeErreur: Integer read FCodeErreur;
  end;

constructor EApplicationException.Create(code: Integer; const msg: String);
begin
  FCodeErreur := code;
  inherited CreateFmt('[Erreur %d] %s', [code, msg]);
end;
```

### Utilisation avec constantes

```pascal
const
  ERR_FICHIER_INTROUVABLE = 1001;
  ERR_ACCES_REFUSE = 1002;
  ERR_FORMAT_INVALIDE = 1003;

procedure ChargerFichier(const nom: String);
begin
  if not FileExists(nom) then
    raise EApplicationException.Create(
      ERR_FICHIER_INTROUVABLE,
      'Fichier introuvable : ' + nom
    );
end;

try
  ChargerFichier('config.xml');
except
  on E: EApplicationException do
  begin
    WriteLn('Code erreur : ', E.CodeErreur);
    WriteLn('Message : ', E.Message);
    LoggerErreur(E.CodeErreur, E.Message);
  end;
end;
```

## Exception avec contexte détaillé

Pour le débogage, il peut être utile de stocker le contexte complet de l'erreur :

```pascal
type
  EContexteException = class(Exception)
  private
    FNomFichier: String;
    FNumeroLigne: Integer;
    FOperation: String;
  public
    constructor Create(const operation, fichier: String; ligne: Integer; const msg: String);
    property NomFichier: String read FNomFichier;
    property NumeroLigne: Integer read FNumeroLigne;
    property Operation: String read FOperation;
  end;

constructor EContexteException.Create(
  const operation, fichier: String;
  ligne: Integer;
  const msg: String
);
begin
  FOperation := operation;
  FNomFichier := fichier;
  FNumeroLigne := ligne;

  inherited CreateFmt(
    '%s [Fichier: %s, Ligne: %d] - %s',
    [operation, fichier, ligne, msg]
  );
end;
```

### Utilisation

```pascal
procedure TraiterLigne(const fichier: String; numLigne: Integer; const contenu: String);
begin
  if Trim(contenu) = '' then
    raise EContexteException.Create(
      'TraiterLigne',
      fichier,
      numLigne,
      'Ligne vide détectée'
    );
end;

try
  TraiterLigne('donnees.txt', 42, '');
except
  on E: EContexteException do
  begin
    WriteLn('Erreur dans : ', E.Operation);
    WriteLn('Fichier : ', E.NomFichier);
    WriteLn('Ligne : ', E.NumeroLigne);
    WriteLn('Détail : ', E.Message);
  end;
end;
```

## Exemple complet : système de validation

Voici un exemple réaliste d'un système de validation avec exceptions personnalisées :

```pascal
type
  // Base de toutes les erreurs de validation
  EValidationError = class(Exception)
  private
    FNomChamp: String;
  public
    constructor Create(const champ, message: String);
    property NomChamp: String read FNomChamp;
  end;

  // Exceptions spécifiques
  EChampVide = class(EValidationError)
  public
    constructor Create(const champ: String);
  end;

  EChampTropLong = class(EValidationError)
  private
    FLongueurMax: Integer;
    FLongueurActuelle: Integer;
  public
    constructor Create(const champ: String; longueurMax, longueurActuelle: Integer);
    property LongueurMax: Integer read FLongueurMax;
    property LongueurActuelle: Integer read FLongueurActuelle;
  end;

  EFormatEmail = class(EValidationError)
  public
    constructor Create(const email: String);
  end;

// Implémentation
constructor EValidationError.Create(const champ, message: String);
begin
  FNomChamp := champ;
  inherited CreateFmt('Champ "%s" : %s', [champ, message]);
end;

constructor EChampVide.Create(const champ: String);
begin
  inherited Create(champ, 'ce champ est obligatoire');
end;

constructor EChampTropLong.Create(const champ: String; longueurMax, longueurActuelle: Integer);
begin
  FLongueurMax := longueurMax;
  FLongueurActuelle := longueurActuelle;
  inherited Create(
    champ,
    Format('maximum %d caractères (actuellement %d)', [longueurMax, longueurActuelle])
  );
end;

constructor EFormatEmail.Create(const email: String);
begin
  inherited Create('email', Format('"%s" n''est pas un email valide', [email]));
end;

// Utilisation dans un validateur
procedure ValiderFormulaire(const nom, email: String);
begin
  // Validation du nom
  if Trim(nom) = '' then
    raise EChampVide.Create('nom');

  if Length(nom) > 50 then
    raise EChampTropLong.Create('nom', 50, Length(nom));

  // Validation de l'email
  if Trim(email) = '' then
    raise EChampVide.Create('email');

  if Pos('@', email) = 0 then
    raise EFormatEmail.Create(email);
end;

// Dans l'interface utilisateur
procedure SoumettreFormulaire;
begin
  try
    ValiderFormulaire(EditNom.Text, EditEmail.Text);
    ShowMessage('Formulaire valide !');
  except
    on E: EChampVide do
      ShowMessage('Erreur : ' + E.Message);
    on E: EChampTropLong do
      ShowMessage(Format('%s (réduisez de %d caractères)',
        [E.Message, E.LongueurActuelle - E.LongueurMax]));
    on E: EFormatEmail do
      ShowMessage(E.Message);
    on E: EValidationError do
      ShowMessage('Erreur de validation : ' + E.Message);
  end;
end;
```

## Exceptions avec méthodes personnalisées

Vos exceptions peuvent aussi avoir des méthodes :

```pascal
type
  EReseauException = class(Exception)
  private
    FCodeHTTP: Integer;
    FURL: String;
  public
    constructor Create(code: Integer; const url, message: String);
    function EstErreurServeur: Boolean;
    function EstErreurClient: Boolean;
    function PeutReessayer: Boolean;
    property CodeHTTP: Integer read FCodeHTTP;
    property URL: String read FURL;
  end;

constructor EReseauException.Create(code: Integer; const url, message: String);
begin
  FCodeHTTP := code;
  FURL := url;
  inherited CreateFmt('[HTTP %d] %s - %s', [code, url, message]);
end;

function EReseauException.EstErreurServeur: Boolean;
begin
  Result := (FCodeHTTP >= 500) and (FCodeHTTP < 600);
end;

function EReseauException.EstErreurClient: Boolean;
begin
  Result := (FCodeHTTP >= 400) and (FCodeHTTP < 500);
end;

function EReseauException.PeutReessayer: Boolean;
begin
  // On peut réessayer pour certaines erreurs serveur
  Result := EstErreurServeur or (FCodeHTTP = 429); // Too Many Requests
end;
```

### Utilisation

```pascal
try
  ReponseAPI := AppelerAPI('https://api.exemple.com/data');
except
  on E: EReseauException do
  begin
    if E.PeutReessayer then
    begin
      WriteLn('Erreur temporaire, nouvelle tentative...');
      Sleep(1000);
      ReessayerAppel;
    end
    else
      WriteLn('Erreur définitive : ', E.Message);
  end;
end;
```

## Conventions de nommage

### Préfixe E

Par convention, toutes les classes d'exceptions commencent par **E** (pour Exception) :

```pascal
// ✓ BON
EMonErreur
ESoldeInsuffisant
EValidationError

// ✗ MAUVAIS
MonErreur
SoldeInsuffisantException
ValidationError
```

### Noms descriptifs

Utilisez des noms clairs qui décrivent l'erreur :

```pascal
// ✓ BON
EEmailInvalide
EFichierNonTrouve
ECompteBloque

// ✗ MAUVAIS
EErreur1
EProbleme
EX
```

## Bonnes pratiques

### 1. Hériter de la classe appropriée

```pascal
// ✓ BON : hérite d'Exception ou d'une sous-classe pertinente
type
  EMonErreur = class(Exception);
  ESoldeInsuffisant = class(EBanqueException);

// ✗ MAUVAIS : n'hérite pas d'Exception
type
  EMonErreur = class(TObject);  // Ne sera pas traité comme une exception !
```

### 2. Fournir des constructeurs pratiques

```pascal
type
  ESoldeInsuffisant = class(Exception)
  public
    constructor Create(solde, montant: Double); overload;
    constructor Create(const msg: String); overload;
  end;

// Permet plusieurs façons de créer l'exception
raise ESoldeInsuffisant.Create(100.0, 150.0);
raise ESoldeInsuffisant.Create('Solde insuffisant');
```

### 3. Ne pas exposer trop de détails techniques

```pascal
// ✗ MAUVAIS : trop technique pour l'utilisateur final
raise Exception.Create('SQLException: ORA-00942 table or view does not exist');

// ✓ BON : message compréhensible
raise Exception.Create('Les données n''ont pas pu être chargées. Veuillez contacter le support.');
```

### 4. Documenter vos exceptions

```pascal
/// Exception levée quand le solde du compte est insuffisant
/// pour effectuer l'opération demandée
type
  ESoldeInsuffisant = class(EBanqueException)
  public
    /// Crée une exception avec le solde actuel et le montant demandé
    /// @param solde Le solde actuel du compte
    /// @param montant Le montant demandé pour l'opération
    constructor Create(solde, montant: Double);
  end;
```

### 5. Organiser vos exceptions dans des unités dédiées

```pascal
unit BanqueExceptions;

interface

uses
  SysUtils;

type
  EBanqueException = class(Exception);
  ESoldeInsuffisant = class(EBanqueException);
  ECompteBloque = class(EBanqueException);
  // ... toutes vos exceptions bancaires

implementation
// ... implémentation des constructeurs
end.
```

## Comparaison : Exception standard vs personnalisée

| Aspect | Exception standard | Exception personnalisée |
|--------|-------------------|------------------------|
| Rapidité | Rapide à écrire | Nécessite déclaration |
| Clarté | Générique | Très spécifique |
| Capture ciblée | Difficile | Facile |
| Info supplémentaire | Message seulement | Propriétés personnalisées |
| Maintenance | Moins facile | Plus facile |
| Débogage | Plus difficile | Plus simple |

## Quand créer une exception personnalisée ?

### OUI, créez une exception personnalisée quand :

- ✓ L'erreur est spécifique à votre domaine métier
- ✓ Vous avez besoin de stocker des informations supplémentaires
- ✓ Vous voulez gérer cette erreur de manière spécifique
- ✓ L'erreur peut survenir à plusieurs endroits dans votre code
- ✓ L'erreur nécessite une action utilisateur spécifique

### NON, utilisez une exception standard quand :

- ✗ L'erreur est temporaire et très localisée
- ✗ Une exception standard exprime parfaitement le problème
- ✗ Vous n'avez pas besoin d'informations supplémentaires
- ✗ L'erreur ne sera jamais capturée spécifiquement

## Conclusion

Les exceptions personnalisées sont un outil puissant pour créer des applications robustes et maintenables :

- Elles rendent votre code plus expressif et compréhensible
- Elles permettent une gestion d'erreurs ciblée et précise
- Elles peuvent transporter des informations contextuelles riches
- Elles facilitent le débogage et la maintenance

N'hésitez pas à créer vos propres exceptions pour modéliser les erreurs de votre domaine métier. C'est un investissement qui rendra votre code plus professionnel et plus facile à maintenir.

---

**Points clés à retenir :**

- Syntaxe simple : `type EMonException = class(Exception);`
- Toujours préfixer avec **E** par convention
- Hériter d'`Exception` ou d'une sous-classe appropriée
- Ajouter des propriétés pour stocker des informations contextuelles
- Créer des constructeurs pratiques avec paramètres significatifs
- Organiser les exceptions en hiérarchies logiques
- Documenter vos exceptions personnalisées
- Les exceptions personnalisées améliorent la clarté et la maintenabilité du code

⏭️ [Exceptions et ressources](/13-gestion-exceptions/06-exceptions-ressources.md)
