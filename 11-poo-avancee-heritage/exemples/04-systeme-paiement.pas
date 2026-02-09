{ ============================================================================
  Section 11.4 : Methodes virtuelles et override
  Description : Systeme de paiement polymorphe (carte, PayPal, especes)
  Fichier source : 04-methodes-virtuelles-override.md
  ============================================================================ }
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
