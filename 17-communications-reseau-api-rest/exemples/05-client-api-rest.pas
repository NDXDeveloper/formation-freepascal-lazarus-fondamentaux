{ ============================================================================
  Section 17.5 : Utilisation de TFPHttpClient
  Description : Client API REST complet avec CRUD (jsonplaceholder)
  Fichier source : 05-utilisation-tfphttpclient.md
  ============================================================================ }
program RestApiClient;

{$mode objfpc}{$H+}

uses
  fphttpclient, opensslsockets, Classes, SysUtils;

const
  API_BASE_URL = 'https://jsonplaceholder.typicode.com';

type
  TApiClient = class
  private
    FClient: TFPHttpClient;
  public
    constructor Create;
    destructor Destroy; override;

    function GetUsers: String;
    function GetUser(UserID: Integer): String;
    function CreateUser(const JsonData: String): String;
    function UpdateUser(UserID: Integer; const JsonData: String): String;
    function DeleteUser(UserID: Integer): Boolean;
  end;

constructor TApiClient.Create;
begin
  inherited Create;
  FClient := TFPHttpClient.Create(nil);
  FClient.AddHeader('Content-Type', 'application/json');
  FClient.AddHeader('Accept', 'application/json');
end;

destructor TApiClient.Destroy;
begin
  FClient.Free;
  inherited Destroy;
end;

function TApiClient.GetUsers: String;
begin
  try
    Result := FClient.Get(API_BASE_URL + '/users');
  except
    on E: Exception do
    begin
      WriteLn('Erreur GetUsers : ', E.Message);
      Result := '';
    end;
  end;
end;

function TApiClient.GetUser(UserID: Integer): String;
begin
  try
    Result := FClient.Get(API_BASE_URL + '/users/' + IntToStr(UserID));
  except
    on E: Exception do
    begin
      WriteLn('Erreur GetUser : ', E.Message);
      Result := '';
    end;
  end;
end;

function TApiClient.CreateUser(const JsonData: String): String;
begin
  try
    FClient.RequestBody := TRawByteStringStream.Create(JsonData);  // Corps JSON de la requête
    try
      Result := FClient.Post(API_BASE_URL + '/users');
      WriteLn('Code statut : ', FClient.ResponseStatusCode);
    finally
      FClient.RequestBody.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Erreur CreateUser : ', E.Message);
      Result := '';
    end;
  end;
end;

function TApiClient.UpdateUser(UserID: Integer; const JsonData: String): String;
begin
  try
    FClient.RequestBody := TRawByteStringStream.Create(JsonData);
    try
      Result := FClient.Put(API_BASE_URL + '/users/' + IntToStr(UserID));
    finally
      FClient.RequestBody.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Erreur UpdateUser : ', E.Message);
      Result := '';
    end;
  end;
end;

function TApiClient.DeleteUser(UserID: Integer): Boolean;
begin
  try
    FClient.Delete(API_BASE_URL + '/users/' + IntToStr(UserID));
    Result := (FClient.ResponseStatusCode = 200) or
              (FClient.ResponseStatusCode = 204);
  except
    on E: Exception do
    begin
      WriteLn('Erreur DeleteUser : ', E.Message);
      Result := False;
    end;
  end;
end;

// Programme principal
var
  ApiClient: TApiClient;
  Response: String;
  NewUser: String;
begin
  ApiClient := TApiClient.Create;
  try
    // Lister tous les utilisateurs
    WriteLn('=== Tous les utilisateurs ===');
    Response := ApiClient.GetUsers;
    if Response <> '' then
      WriteLn(Copy(Response, 1, 200), '...'); // Afficher un extrait

    WriteLn;

    // Obtenir un utilisateur spécifique
    WriteLn('=== Utilisateur #1 ===');
    Response := ApiClient.GetUser(1);
    if Response <> '' then
      WriteLn(Response);

    WriteLn;

    // Créer un nouvel utilisateur
    WriteLn('=== Créer un utilisateur ===');
    NewUser := '{"name":"Jean Dupont","email":"jean@example.com"}';
    Response := ApiClient.CreateUser(NewUser);
    if Response <> '' then
      WriteLn(Response);

    WriteLn;

    // Mettre à jour un utilisateur
    WriteLn('=== Mettre à jour l''utilisateur #1 ===');
    NewUser := '{"id":1,"name":"Jean Martin","email":"jean.martin@example.com"}';
    Response := ApiClient.UpdateUser(1, NewUser);
    if Response <> '' then
      WriteLn(Response);

    WriteLn;

    // Supprimer un utilisateur
    WriteLn('=== Supprimer l''utilisateur #1 ===');
    if ApiClient.DeleteUser(1) then
      WriteLn('Suppression réussie')
    else
      WriteLn('Échec de la suppression');

  finally
    ApiClient.Free;
  end;
end.
