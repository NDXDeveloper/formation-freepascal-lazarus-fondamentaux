{ ============================================================================
  Section 17.7 : Parsing JSON avec fpjson
  Description : Extraction de valeurs depuis un objet JSON
  Fichier source : 07-parsing-json-fpjson.md
  ============================================================================ }
program ExtractFromObject;

{$mode objfpc}{$H+}

uses
  fpjson, jsonparser, SysUtils;

var
  JsonString: String;
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  Nom, Prenom: String;
  Age: Integer;
begin
  JsonString := '{"nom":"Dupont","prenom":"Jean","age":30}';

  JsonData := GetJSON(JsonString);
  try
    // Vérifier que c'est bien un objet
    if JsonData.JSONType = jtObject then
    begin
      JsonObj := TJSONObject(JsonData);

      // Extraire les valeurs
      Nom := JsonObj.Get('nom', '');        // Valeur par défaut : ''
      Prenom := JsonObj.Get('prenom', '');
      Age := JsonObj.Get('age', 0);         // Valeur par défaut : 0

      WriteLn('Nom complet : ', Prenom, ' ', Nom);
      WriteLn('Age : ', Age, ' ans');
    end;
  finally
    JsonData.Free;
  end;
end.
