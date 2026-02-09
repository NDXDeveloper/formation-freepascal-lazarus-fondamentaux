{ ============================================================================
  Section 17.7 : Parsing JSON avec fpjson
  Description : Gestion des erreurs de parsing JSON
  Fichier source : 07-parsing-json-fpjson.md
  ============================================================================ }
program HandleParsingErrors;

{$mode objfpc}{$H+}

uses
  fpjson, jsonparser, SysUtils;

procedure TryParseJSON(const JsonString: String);
var
  JsonData: TJSONData;
begin
  try
    JsonData := GetJSON(JsonString);
    try
      WriteLn('JSON valide !');
    finally
      JsonData.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur de parsing : ', E.Message);
  end;
end;

begin
  // JSON valide
  WriteLn('=== Test 1 : JSON valide ===');
  TryParseJSON('{"nom":"Dupont"}');
  WriteLn;

  // JSON invalide : guillemets simples
  WriteLn('=== Test 2 : Guillemets simples (invalide) ===');
  TryParseJSON('{"nom":''Dupont''}');
  WriteLn;

  // JSON invalide : virgule finale
  WriteLn('=== Test 3 : Virgule finale (invalide) ===');
  TryParseJSON('{"nom":"Dupont",}');
  WriteLn;

  // JSON invalide : clé sans guillemets
  WriteLn('=== Test 4 : Clé sans guillemets (invalide) ===');
  TryParseJSON('{nom:"Dupont"}');
end.
