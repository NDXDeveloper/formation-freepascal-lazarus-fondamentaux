{ ============================================================================
  Section 17.7 : Parsing JSON avec fpjson
  Description : Parser la liste des utilisateurs de JSONPlaceholder
  Fichier source : 07-parsing-json-fpjson.md
  ============================================================================ }
program ParseUsers;

{$mode objfpc}{$H+}

uses
  fphttpclient, opensslsockets, fpjson, jsonparser, SysUtils;

procedure ListUsers;
var
  Client: TFPHttpClient;
  Response: String;
  JsonData: TJSONData;
  JsonArray: TJSONArray;
  JsonObj, AddressObj, CompanyObj: TJSONObject;
  i: Integer;
  Name, Email, City, CompanyName: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    Response := Client.Get('https://jsonplaceholder.typicode.com/users');

    if Client.ResponseStatusCode = 200 then
    begin
      // Parser le JSON (c'est un tableau)
      JsonData := GetJSON(Response);
      try
        JsonArray := TJSONArray(JsonData);

        WriteLn('=== Liste des utilisateurs ===');
        WriteLn('Total : ', JsonArray.Count, ' utilisateurs');
        WriteLn;

        for i := 0 to JsonArray.Count - 1 do
        begin
          JsonObj := TJSONObject(JsonArray.Items[i]);

          // Extraire les données de base
          Name := JsonObj.Get('name', '');
          Email := JsonObj.Get('email', '');

          // Extraire l'adresse (objet imbriqué)
          AddressObj := JsonObj.GetPath('address') as TJSONObject;
          City := AddressObj.Get('city', '');

          // Extraire l'entreprise (objet imbriqué)
          CompanyObj := JsonObj.GetPath('company') as TJSONObject;
          CompanyName := CompanyObj.Get('name', '');

          // Afficher
          WriteLn(Format('%d. %s', [i+1, Name]));
          WriteLn('   Email : ', Email);
          WriteLn('   Ville : ', City);
          WriteLn('   Entreprise : ', CompanyName);
          WriteLn;
        end;

      finally
        JsonData.Free;
      end;
    end;

  finally
    Client.Free;
  end;
end;

begin
  ListUsers;
end.
