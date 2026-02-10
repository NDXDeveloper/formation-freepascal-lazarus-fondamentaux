{ ============================================================================
  Section 17.8 : Gestion des erreurs reseau
  Description : Classe complete de gestion HTTP avec retry et logging
  Fichier source : 08-gestion-erreurs-reseau.md
  ============================================================================ }
unit HTTPManager;

{$mode objfpc}{$H+}

interface

uses
  fphttpclient, opensslsockets, Classes, SysUtils, Math;

type
  THTTPResult = record
    Success: Boolean;
    StatusCode: Integer;
    Data: String;
    ErrorMessage: String;
  end;

  THTTPManager = class
  private
    FClient: TFPHttpClient;
    FTimeout: Integer;
    FMaxRetries: Integer;
    FLogErrors: Boolean;

    procedure LogError(const Message: String);
    function ShouldRetry(StatusCode: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Get(const URL: String): THTTPResult;
    function Post(const URL: String; const JsonData: String): THTTPResult;

    property Timeout: Integer read FTimeout write FTimeout;
    property MaxRetries: Integer read FMaxRetries write FMaxRetries;
    property LogErrors: Boolean read FLogErrors write FLogErrors;
  end;

implementation

constructor THTTPManager.Create;  
begin  
  inherited Create;
  FClient := TFPHttpClient.Create(nil);
  FClient.AddHeader('User-Agent', 'FreePascal-App/1.0');
  FClient.AddHeader('Accept', 'application/json');

  FTimeout := 10000;      // 10 secondes par défaut
  FMaxRetries := 3;       // 3 tentatives par défaut
  FLogErrors := True;     // Logger par défaut
end;

destructor THTTPManager.Destroy;  
begin  
  FClient.Free;
  inherited Destroy;
end;

procedure THTTPManager.LogError(const Message: String);  
var  
  F: TextFile;
begin
  if not FLogErrors then
    Exit;

  try
    AssignFile(F, 'network_errors.log');
    if FileExists('network_errors.log') then
      Append(F)
    else
      Rewrite(F);

    WriteLn(F, '[', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), '] ', Message);
    CloseFile(F);
  except
    // Silencieux si échec
  end;
end;

function THTTPManager.ShouldRetry(StatusCode: Integer): Boolean;  
begin  
  Result := False;
  case StatusCode of  // case et non 'in' : les sets FPC ne supportent pas les valeurs > 255
    0, 500, 502, 503, 504: Result := True;
  end;
end;

function THTTPManager.Get(const URL: String): THTTPResult;  
var  
  Attempt: Integer;
  WaitTime: Integer;
begin
  Result.Success := False;
  Result.StatusCode := 0;
  Result.Data := '';
  Result.ErrorMessage := '';

  for Attempt := 1 to FMaxRetries do
  begin
    try
      FClient.ConnectTimeout := FTimeout;
      Result.Data := FClient.Get(URL);
      Result.StatusCode := FClient.ResponseStatusCode;

      if Result.StatusCode = 200 then
      begin
        Result.Success := True;
        Break;
      end
      else if not ShouldRetry(Result.StatusCode) then
      begin
        Result.ErrorMessage := Format('Erreur HTTP %d', [Result.StatusCode]);
        LogError(Format('GET %s : %s', [URL, Result.ErrorMessage]));
        Break;
      end;

    except
      on E: Exception do
      begin
        Result.ErrorMessage := E.Message;
        Result.StatusCode := 0;
        LogError(Format('GET %s : %s', [URL, E.Message]));
      end;
    end;

    // Attendre avant de réessayer (backoff exponentiel)
    if (not Result.Success) and (Attempt < FMaxRetries) then
    begin
      WaitTime := Round(Power(2, Attempt - 1)) * 1000;
      Sleep(WaitTime);
    end;
  end;

  if not Result.Success and (Result.ErrorMessage = '') then
    Result.ErrorMessage := Format('Échec après %d tentatives', [FMaxRetries]);
end;

function THTTPManager.Post(const URL: String; const JsonData: String): THTTPResult;  
begin  
  Result.Success := False;
  Result.StatusCode := 0;
  Result.Data := '';
  Result.ErrorMessage := '';

  try
    FClient.ConnectTimeout := FTimeout;
    FClient.AddHeader('Content-Type', 'application/json');
    FClient.RequestBody := TRawByteStringStream.Create(JsonData);
    try
      Result.Data := FClient.Post(URL);
      Result.StatusCode := FClient.ResponseStatusCode;

      if Result.StatusCode in [200, 201] then
        Result.Success := True
      else
        Result.ErrorMessage := Format('Erreur HTTP %d', [Result.StatusCode]);

    finally
      FClient.RequestBody.Free;
    end;
  except
    on E: Exception do
    begin
      Result.ErrorMessage := E.Message;
      LogError(Format('POST %s : %s', [URL, E.Message]));
    end;
  end;
end;

end.
