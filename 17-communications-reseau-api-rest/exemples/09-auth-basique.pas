{ ============================================================================
  Section 17.9 : Headers et authentification basique
  Description : Authentification HTTP basique (Basic Auth) manuelle
  Fichier source : 09-headers-authentification-basique.md
  ============================================================================ }
program BasicAuthManual;

{$mode objfpc}{$H+}

uses
  fphttpclient, opensslsockets, Base64,  // Base64 : encodage pour Basic Auth
  SysUtils;

var
  Client: TFPHttpClient;
  Response: String;
  Username, Password: String;
  Credentials, EncodedCredentials: String;
begin
  Username := 'jean.dupont';
  Password := 'monMotDePasse123';

  // Créer la chaîne "username:password"
  Credentials := Username + ':' + Password;

  // Encoder en Base64
  EncodedCredentials := EncodeStringBase64(Credentials);

  WriteLn('Credentials encodées : ', EncodedCredentials);

  // Utiliser dans une requête
  Client := TFPHttpClient.Create(nil);
  try
    Client.AddHeader('Authorization', 'Basic ' + EncodedCredentials);

    try
      Response := Client.Get('https://httpbin.org/basic-auth/jean.dupont/monMotDePasse123');

      if Client.ResponseStatusCode = 200 then
        WriteLn('Authentification réussie !')
      else if Client.ResponseStatusCode = 401 then
        WriteLn('Échec d''authentification')
      else
        WriteLn('Code : ', Client.ResponseStatusCode);

    except
      on E: Exception do
        WriteLn('Erreur : ', E.Message);
    end;
  finally
    Client.Free;
  end;
end.
