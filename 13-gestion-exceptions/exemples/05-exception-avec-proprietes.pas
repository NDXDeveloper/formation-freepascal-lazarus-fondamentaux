{ ============================================================================
  Section 13.5 : Exceptions personnalisees (avance)
  Description : Exceptions avec proprietes, code d'erreur, hierarchie metier
  Fichier source : 05-exceptions-personnalisees.md
  ============================================================================ }
program ExceptionAvecProprietes;

{$mode objfpc}{$H+}

uses
  SysUtils;

{ === Types d'exceptions personnalisees === }
type
  { Exception avec proprietes (solde) }
  ESoldeInsuffisant = class(Exception)
  private
    FSoldeActuel: Double;
    FMontantDemande: Double;
  public
    constructor Create(solde, montant: Double);
    property SoldeActuel: Double read FSoldeActuel;
    property MontantDemande: Double read FMontantDemande;
  end;

  { Exception avec code d'erreur }
  EApplicationException = class(Exception)
  private
    FCodeErreur: Integer;
  public
    constructor Create(code: Integer; const msg: String);
    property CodeErreur: Integer read FCodeErreur;
  end;

  { Hierarchie d'exceptions metier bancaire }
  EBanqueException = class(Exception);
  ECompteBloque = class(EBanqueException);
  ELimiteDepassee = class(EBanqueException);

{ === Implementation des constructeurs === }

constructor ESoldeInsuffisant.Create(solde, montant: Double);
begin
  FSoldeActuel := solde;
  FMontantDemande := montant;
  inherited CreateFmt(
    'Solde insuffisant : vous avez %.2f EUR mais vous demandez %.2f EUR',
    [solde, montant]
  );
end;

constructor EApplicationException.Create(code: Integer; const msg: String);
begin
  FCodeErreur := code;
  inherited CreateFmt('[Erreur %d] %s', [code, msg]);
end;

{ === Demonstrations === }

{ --- 1. Exception avec proprietes (ESoldeInsuffisant) --- }
var
  SoldeCompte: Double;

procedure RetirerArgent(montant: Double);
begin
  if SoldeCompte < montant then
    raise ESoldeInsuffisant.Create(SoldeCompte, montant);

  SoldeCompte := SoldeCompte - montant;
  WriteLn('  Retrait de ', montant:0:2, ' EUR effectue. Nouveau solde : ', SoldeCompte:0:2, ' EUR');
end;

procedure DemoSoldeInsuffisant;
begin
  WriteLn('=== 1. Exception avec proprietes (ESoldeInsuffisant) ===');
  SoldeCompte := 50.0;
  WriteLn('  Solde initial : ', SoldeCompte:0:2, ' EUR');

  { Retrait possible }
  try
    RetirerArgent(30.0);
  except
    on E: ESoldeInsuffisant do
      WriteLn('  Manque : ', (E.MontantDemande - E.SoldeActuel):0:2, ' EUR');
  end;

  { Retrait impossible }
  try
    RetirerArgent(100.0);
  except
    on E: ESoldeInsuffisant do
    begin
      WriteLn('  ', E.Message);
      WriteLn('  Manque : ', (E.MontantDemande - E.SoldeActuel):0:2, ' EUR');
    end;
  end;

  WriteLn;
end;

{ --- 2. Exception avec code d'erreur --- }
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

procedure DemoCodeErreur;
begin
  WriteLn('=== 2. Exception avec code d''erreur ===');

  try
    ChargerFichier('config_inexistant.xml');
  except
    on E: EApplicationException do
    begin
      WriteLn('  Code erreur : ', E.CodeErreur);
      WriteLn('  Message : ', E.Message);
    end;
  end;

  { Tester avec d'autres codes }
  try
    raise EApplicationException.Create(ERR_FORMAT_INVALIDE, 'Format CSV attendu');
  except
    on E: EApplicationException do
    begin
      WriteLn('  Code erreur : ', E.CodeErreur);
      WriteLn('  Message : ', E.Message);
    end;
  end;

  WriteLn;
end;

{ --- 3. Hierarchie d'exceptions metier --- }
procedure EffectuerOperation(typeOp: Integer);
begin
  case typeOp of
    1: raise ESoldeInsuffisant.Create(100.0, 500.0);
    2: raise ECompteBloque.Create('Compte bloque suite a activite suspecte');
    3: raise ELimiteDepassee.Create('Limite de retrait journalier depassee');
  end;
end;

procedure DemoHierarchie;
var
  i: Integer;
begin
  WriteLn('=== 3. Hierarchie d''exceptions metier ===');

  for i := 1 to 3 do
  begin
    try
      EffectuerOperation(i);
    except
      on E: ESoldeInsuffisant do
        WriteLn('  Solde insuffisant : ', E.Message);
      on E: ECompteBloque do
        WriteLn('  Compte bloque : ', E.Message);
      on E: EBanqueException do
        WriteLn('  Erreur bancaire : ', E.Message);
    end;
  end;

  WriteLn;
end;

{ --- 4. Hierarchie de validation --- }
type
  EValidationError = class(Exception)
  private
    FNomChamp: String;
  public
    constructor Create(const champ, msg: String);
    property NomChamp: String read FNomChamp;
  end;

  EChampVide = class(EValidationError)
  public
    constructor Create(const champ: String);
  end;

  EFormatEmail = class(EValidationError)
  public
    constructor Create(const email: String);
  end;

constructor EValidationError.Create(const champ, msg: String);
begin
  FNomChamp := champ;
  inherited CreateFmt('Champ "%s" : %s', [champ, msg]);
end;

constructor EChampVide.Create(const champ: String);
begin
  inherited Create(champ, 'ce champ est obligatoire');
end;

constructor EFormatEmail.Create(const email: String);
begin
  inherited Create('email', Format('"%s" n''est pas un email valide', [email]));
end;

procedure ValiderFormulaire(const nom, email: String);
begin
  if Trim(nom) = '' then
    raise EChampVide.Create('nom');

  if Trim(email) = '' then
    raise EChampVide.Create('email');

  if Pos('@', email) = 0 then
    raise EFormatEmail.Create(email);

  WriteLn('  Formulaire valide pour : ', nom, ' <', email, '>');
end;

procedure DemoValidation;
begin
  WriteLn('=== 4. Systeme de validation ===');

  try
    ValiderFormulaire('', 'test@mail.com');
  except
    on E: EChampVide do
      WriteLn('  Champ vide : ', E.Message);
    on E: EFormatEmail do
      WriteLn('  Email invalide : ', E.Message);
    on E: EValidationError do
      WriteLn('  Validation : ', E.Message);
  end;

  try
    ValiderFormulaire('Alice', 'pas-un-email');
  except
    on E: EChampVide do
      WriteLn('  Champ vide : ', E.Message);
    on E: EFormatEmail do
      WriteLn('  Email invalide : ', E.Message);
    on E: EValidationError do
      WriteLn('  Validation : ', E.Message);
  end;

  try
    ValiderFormulaire('Bob', 'bob@example.com');
  except
    on E: EValidationError do
      WriteLn('  Validation : ', E.Message);
  end;

  WriteLn;
end;

{ === Programme principal === }
begin
  WriteLn('--- Chapitre 13.5 : Exceptions personnalisees avancees ---');
  WriteLn;

  DemoSoldeInsuffisant;
  DemoCodeErreur;
  DemoHierarchie;
  DemoValidation;

  WriteLn('--- Fin des demonstrations ---');
end.
