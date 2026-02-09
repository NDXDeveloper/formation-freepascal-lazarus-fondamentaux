{ ============================================================================
  Section 17.5 : Utilisation de TFPHttpClient
  Description : Requete GET simple vers une API
  Fichier source : 05-utilisation-tfphttpclient.md
  ============================================================================ }
program SimpleGet;

{$mode objfpc}{$H+}

uses
  fphttpclient, opensslsockets,  // opensslsockets : nécessaire pour HTTPS
  SysUtils;

var
  Client: TFPHttpClient;
  Response: String;
begin
  // Créer une instance du client HTTP
  Client := TFPHttpClient.Create(nil);
  try
    // GitHub exige un User-Agent
    Client.AddHeader('User-Agent', 'FreePascal-App/1.0');

    // Effectuer une requête GET
    Response := Client.Get('https://api.github.com');

    // Afficher la réponse
    WriteLn('Réponse reçue :');
    WriteLn(Response);
  finally
    // Ne pas oublier de libérer la mémoire
    Client.Free;
  end;
end.
