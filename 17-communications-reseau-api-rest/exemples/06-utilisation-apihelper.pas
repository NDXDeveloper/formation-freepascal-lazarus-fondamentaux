{ ============================================================================
  Section 17.6 : Consommation d'API publiques
  Description : Utilisation de l'unite APIHelper avec JSONPlaceholder
  Fichier source : 06-consommation-api-publiques.md
  ============================================================================ }
program UseAPIHelper;

{$mode objfpc}{$H+}

uses
  APIHelper, SysUtils;

var
  API: TAPIHelper;
  Response: String;
begin
  // Utilisation avec JSONPlaceholder
  API := TAPIHelper.Create('https://jsonplaceholder.typicode.com');
  try
    // GET
    Response := API.Get('/users/1');
    if API.GetStatusCode = 200 then
      WriteLn(Response);

    // POST
    Response := API.Post('/posts', '{"title":"Test","body":"Contenu","userId":1}');
    if API.GetStatusCode = 201 then
      WriteLn('Créé : ', Response);

  finally
    API.Free;
  end;
end.
