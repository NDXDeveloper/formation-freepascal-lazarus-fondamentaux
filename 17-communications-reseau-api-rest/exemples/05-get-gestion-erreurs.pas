{ ============================================================================
  Section 17.5 : Utilisation de TFPHttpClient
  Description : Requete GET avec gestion des erreurs
  Fichier source : 05-utilisation-tfphttpclient.md
  ============================================================================ }
program GetWithErrorHandling;

{$mode objfpc}{$H+}

uses
  fphttpclient, opensslsockets, SysUtils;

var
  Client: TFPHttpClient;
  Response: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    try
      Response := Client.Get('https://jsonplaceholder.typicode.com/users/1');
      WriteLn('Succès ! Données reçues :');
      WriteLn(Response);
    except
      on E: Exception do
      begin
        WriteLn('Erreur lors de la requête :');
        WriteLn(E.Message);
      end;
    end;
  finally
    Client.Free;
  end;
end.
