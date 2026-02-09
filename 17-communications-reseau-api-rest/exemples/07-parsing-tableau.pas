{ ============================================================================
  Section 17.7 : Parsing JSON avec fpjson
  Description : Parsing d'un tableau JSON
  Fichier source : 07-parsing-json-fpjson.md
  ============================================================================ }
program ParseArray;

{$mode objfpc}{$H+}

uses
  fpjson, jsonparser, SysUtils;

var
  JsonString: String;
  JsonData: TJSONData;
  JsonArray: TJSONArray;
  i: Integer;
begin
  JsonString := '["pomme","banane","orange","fraise"]';

  JsonData := GetJSON(JsonString);
  try
    if JsonData.JSONType = jtArray then
    begin
      JsonArray := TJSONArray(JsonData);

      WriteLn('Nombre d''éléments : ', JsonArray.Count);
      WriteLn('=== Liste des fruits ===');

      for i := 0 to JsonArray.Count - 1 do
      begin
        WriteLn(i + 1, '. ', JsonArray.Items[i].AsString);
      end;
    end;
  finally
    JsonData.Free;
  end;
end.
