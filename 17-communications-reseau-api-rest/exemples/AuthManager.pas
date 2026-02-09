{ ============================================================================
  Section 17.9 : Headers et authentification basique
  Description : Unite de gestion des differents types d'authentification HTTP
  Fichier source : 09-headers-authentification-basique.md
  ============================================================================ }
unit AuthManager;

{$mode objfpc}{$H+}

interface

uses
  fphttpclient, Base64, SysUtils;

type
  TAuthType = (atNone, atBasic, atAPIKey, atBearer);

  TAuthManager = class
  private
    FAuthType: TAuthType;
    FUsername: String;
    FPassword: String;
    FAPIKey: String;
    FAPIKeyHeader: String;
    FBearerToken: String;
  public
    constructor Create;

    // Configuration
    procedure SetBasicAuth(const Username, Password: String);
    procedure SetAPIKeyAuth(const APIKey: String; const HeaderName: String = 'X-API-Key');
    procedure SetBearerAuth(const Token: String);
    procedure ClearAuth;

    // Application de l'authentification
    procedure ApplyAuth(Client: TFPHttpClient);

    // Propriétés
    property AuthType: TAuthType read FAuthType;
  end;

implementation

constructor TAuthManager.Create;
begin
  inherited Create;
  FAuthType := atNone;
  FAPIKeyHeader := 'X-API-Key';
end;

procedure TAuthManager.SetBasicAuth(const Username, Password: String);
begin
  FAuthType := atBasic;
  FUsername := Username;
  FPassword := Password;
end;

procedure TAuthManager.SetAPIKeyAuth(const APIKey: String; const HeaderName: String);
begin
  FAuthType := atAPIKey;
  FAPIKey := APIKey;
  FAPIKeyHeader := HeaderName;
end;

procedure TAuthManager.SetBearerAuth(const Token: String);
begin
  FAuthType := atBearer;
  FBearerToken := Token;
end;

procedure TAuthManager.ClearAuth;
begin
  FAuthType := atNone;
  FUsername := '';
  FPassword := '';
  FAPIKey := '';
  FBearerToken := '';
end;

procedure TAuthManager.ApplyAuth(Client: TFPHttpClient);
var
  Credentials: String;
begin
  case FAuthType of
    atBasic:
    begin
      Credentials := FUsername + ':' + FPassword;
      Client.AddHeader('Authorization', 'Basic ' + EncodeStringBase64(Credentials));
    end;

    atAPIKey:
    begin
      Client.AddHeader(FAPIKeyHeader, FAPIKey);
    end;

    atBearer:
    begin
      Client.AddHeader('Authorization', 'Bearer ' + FBearerToken);
    end;

    // atNone : rien à faire
  end;
end;

end.
