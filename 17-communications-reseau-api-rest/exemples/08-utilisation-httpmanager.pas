{ ============================================================================
  Section 17.8 : Gestion des erreurs reseau
  Description : Utilisation de la classe HTTPManager
  Fichier source : 08-gestion-erreurs-reseau.md
  ============================================================================ }
program UseHTTPManager;

{$mode objfpc}{$H+}

uses
  HTTPManager, SysUtils;

var
  HTTP: THTTPManager;
  Res: THTTPResult;
begin
  HTTP := THTTPManager.Create;
  try
    // Configuration
    HTTP.Timeout := 15000;   // 15 secondes
    HTTP.MaxRetries := 3;
    HTTP.LogErrors := True;

    // Effectuer une requête GET
    Res := HTTP.Get('https://jsonplaceholder.typicode.com/users/1');

    if Res.Success then
    begin
      WriteLn('Succès !');
      WriteLn(Res.Data);
    end
    else
    begin
      WriteLn('Échec : ', Res.ErrorMessage);
      WriteLn('Code : ', Res.StatusCode);
    end;

  finally
    HTTP.Free;
  end;
end.
