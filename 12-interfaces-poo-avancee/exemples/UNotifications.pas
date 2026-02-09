{ ============================================================================
  Section 12.2 : Déclaration et implémentation
  Description : Unite de notifications avec interface INotificateur
  Fichier source : 02-declaration-implementation.md
  ============================================================================ }
unit UNotifications;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // 1. DÉCLARATION DE L'INTERFACE
  INotificateur = interface
    ['{D4C3B2A1-9E8F-7D6C-5B4A-3E2F1D0C9B8A}']  // GUID : identifiant unique de l'interface
    procedure EnvoyerMessage(const Message: string);
    function ObtenirNomService: string;
  end;

  // 2. IMPLÉMENTATION PAR EMAIL
  TNotificateurEmail = class(TInterfacedObject, INotificateur)
  private
    FAdresseEmail: string;
  public
    constructor Create(const AdresseEmail: string);
    procedure EnvoyerMessage(const Message: string);
    function ObtenirNomService: string;
  end;

  // 3. IMPLÉMENTATION PAR SMS
  TNotificateurSMS = class(TInterfacedObject, INotificateur)
  private
    FNumeroTelephone: string;
  public
    constructor Create(const NumeroTel: string);
    procedure EnvoyerMessage(const Message: string);
    function ObtenirNomService: string;
  end;

implementation

{ TNotificateurEmail }

constructor TNotificateurEmail.Create(const AdresseEmail: string);
begin
  inherited Create;
  FAdresseEmail := AdresseEmail;
end;

procedure TNotificateurEmail.EnvoyerMessage(const Message: string);
begin
  WriteLn('📧 Envoi email à ', FAdresseEmail);
  WriteLn('   Message: ', Message);
end;

function TNotificateurEmail.ObtenirNomService: string;
begin
  Result := 'Service Email';
end;

{ TNotificateurSMS }

constructor TNotificateurSMS.Create(const NumeroTel: string);
begin
  inherited Create;
  FNumeroTelephone := NumeroTel;
end;

procedure TNotificateurSMS.EnvoyerMessage(const Message: string);
begin
  WriteLn('📱 Envoi SMS au ', FNumeroTelephone);
  WriteLn('   Message: ', Message);
end;

function TNotificateurSMS.ObtenirNomService: string;
begin
  Result := 'Service SMS';
end;

end.
