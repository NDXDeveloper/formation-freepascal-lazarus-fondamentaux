{ ============================================================================
  Section 17.7 : Parsing JSON avec fpjson
  Description : Parsing JSON simple avec GetJSON
  Fichier source : 07-parsing-json-fpjson.md
  ============================================================================ }
program SimpleJSONParsing;

{$mode objfpc}{$H+}

uses
  fpjson, jsonparser,  // fpjson : types JSON ; jsonparser : fonction GetJSON
  SysUtils;

var
  JsonString: String;
  JsonData: TJSONData;
begin
  // Notre chaîne JSON
  JsonString := '{"nom":"Dupont","prenom":"Jean","age":30}';

  try
    // Parser le JSON
    JsonData := GetJSON(JsonString);
    try
      WriteLn('JSON parsé avec succès !');
      WriteLn('Type : ', JsonData.JSONType);
    finally
      // Libérer la mémoire
      JsonData.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur de parsing : ', E.Message);
  end;
end.
