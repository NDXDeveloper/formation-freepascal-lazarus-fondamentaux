{ ============================================================================
  Section 17.6 : Consommation d'API publiques
  Description : Requete vers JSONPlaceholder (API de test)
  Fichier source : 06-consommation-api-publiques.md
  ============================================================================ }
program JsonPlaceholderExample;

{$mode objfpc}{$H+}

uses
  fphttpclient, opensslsockets, SysUtils;

procedure ListUsers;
var
  Client: TFPHttpClient;
  Response: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    try
      WriteLn('Récupération des utilisateurs...');
      Response := Client.Get('https://jsonplaceholder.typicode.com/users');

      if Client.ResponseStatusCode = 200 then
      begin
        WriteLn('=== Utilisateurs (JSON brut) ===');
        WriteLn(Response);
      end
      else
        WriteLn('Erreur : Code ', Client.ResponseStatusCode);

    except
      on E: Exception do
        WriteLn('Erreur : ', E.Message);
    end;
  finally
    Client.Free;
  end;
end;

begin
  ListUsers;
end.
