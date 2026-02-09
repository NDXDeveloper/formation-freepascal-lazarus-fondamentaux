{ ============================================================================
  Section 17.6 : Consommation d'API publiques
  Description : Unite helper reutilisable pour consommer des API REST
  Fichier source : 06-consommation-api-publiques.md
  ============================================================================ }
unit APIHelper;

{$mode objfpc}{$H+}

interface

uses
  fphttpclient, opensslsockets, Classes, SysUtils;

type
  TAPIHelper = class
  private
    FClient: TFPHttpClient;
    FBaseURL: String;
    FAPIKey: String;
  public
    constructor Create(const BaseURL: String; const APIKey: String = '');
    destructor Destroy; override;

    function Get(const Endpoint: String): String;
    function Post(const Endpoint: String; const JsonData: String): String;
    function Put(const Endpoint: String; const JsonData: String): String;
    function Delete(const Endpoint: String): Boolean;

    function GetStatusCode: Integer;
    function GetHeader(const HeaderName: String): String;

    procedure AddHeader(const Name, Value: String);
    procedure SetTimeout(Milliseconds: Integer);
  end;

implementation

constructor TAPIHelper.Create(const BaseURL: String; const APIKey: String = '');
begin
  inherited Create;
  FBaseURL := BaseURL;
  FAPIKey := APIKey;

  FClient := TFPHttpClient.Create(nil);
  FClient.AddHeader('Accept', 'application/json');
  FClient.AddHeader('User-Agent', 'FreePascal-App/1.0');

  if APIKey <> '' then
    FClient.AddHeader('X-API-Key', APIKey);
end;

destructor TAPIHelper.Destroy;
begin
  FClient.Free;
  inherited Destroy;
end;

function TAPIHelper.Get(const Endpoint: String): String;
var
  URL: String;
begin
  Result := '';
  try
    URL := FBaseURL + Endpoint;
    Result := FClient.Get(URL);
  except
    on E: Exception do
      WriteLn('Erreur GET : ', E.Message);
  end;
end;

function TAPIHelper.Post(const Endpoint: String; const JsonData: String): String;
var
  URL: String;
begin
  Result := '';
  try
    URL := FBaseURL + Endpoint;
    FClient.AddHeader('Content-Type', 'application/json');
    FClient.RequestBody := TRawByteStringStream.Create(JsonData);
    try
      Result := FClient.Post(URL);
    finally
      FClient.RequestBody.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur POST : ', E.Message);
  end;
end;

function TAPIHelper.Put(const Endpoint: String; const JsonData: String): String;
var
  URL: String;
begin
  Result := '';
  try
    URL := FBaseURL + Endpoint;
    FClient.AddHeader('Content-Type', 'application/json');
    FClient.RequestBody := TRawByteStringStream.Create(JsonData);
    try
      Result := FClient.Put(URL);
    finally
      FClient.RequestBody.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur PUT : ', E.Message);
  end;
end;

function TAPIHelper.Delete(const Endpoint: String): Boolean;
var
  URL: String;
begin
  Result := False;
  try
    URL := FBaseURL + Endpoint;
    FClient.Delete(URL);
    Result := (FClient.ResponseStatusCode = 200) or
              (FClient.ResponseStatusCode = 204);
  except
    on E: Exception do
      WriteLn('Erreur DELETE : ', E.Message);
  end;
end;

function TAPIHelper.GetStatusCode: Integer;
begin
  Result := FClient.ResponseStatusCode;
end;

function TAPIHelper.GetHeader(const HeaderName: String): String;
begin
  Result := FClient.GetHeader(FClient.ResponseHeaders, HeaderName);
end;

procedure TAPIHelper.AddHeader(const Name, Value: String);
begin
  FClient.AddHeader(Name, Value);
end;

procedure TAPIHelper.SetTimeout(Milliseconds: Integer);
begin
  FClient.ConnectTimeout := Milliseconds;
end;

end.
