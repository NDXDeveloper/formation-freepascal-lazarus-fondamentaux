{ ============================================================================
  Section 17.9 : Headers et authentification basique
  Description : Utilisation de la classe AuthManager pour differents types d'auth
  Fichier source : 09-headers-authentification-basique.md
  ============================================================================ }
program UseAuthManager;

{$mode objfpc}{$H+}

uses
  AuthManager, fphttpclient, opensslsockets, SysUtils;

var
  Auth: TAuthManager;
  Client: TFPHttpClient;
  Response: String;
begin
  Auth := TAuthManager.Create;
  Client := TFPHttpClient.Create(nil);
  try
    // === Exemple 1 : Basic Auth ===
    WriteLn('=== Test Basic Auth ===');
    Auth.SetBasicAuth('user', 'password');
    Auth.ApplyAuth(Client);

    try
      Response := Client.Get('https://httpbin.org/basic-auth/user/password');
      if Client.ResponseStatusCode = 200 then
        WriteLn('Succès Basic Auth : ', Response)
      else
        WriteLn('Code : ', Client.ResponseStatusCode);
    except
      on E: Exception do
        WriteLn('Erreur : ', E.Message);
    end;

    WriteLn;

    // === Exemple 2 : API Key ===
    WriteLn('=== Test API Key ===');
    Auth.SetAPIKeyAuth('abc123def456');
    WriteLn('Type d''auth configuré : API Key');

    WriteLn;

    // === Exemple 3 : Bearer Token ===
    WriteLn('=== Test Bearer Token ===');
    Auth.SetBearerAuth('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...');
    WriteLn('Type d''auth configuré : Bearer Token');

  finally
    Client.Free;
    Auth.Free;
  end;
end.
