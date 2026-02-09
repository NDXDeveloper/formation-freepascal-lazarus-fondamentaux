{ ============================================================================
  Section 17.7 : Parsing JSON avec fpjson
  Description : Parsing de JSON imbrique (objets dans des objets)
  Fichier source : 07-parsing-json-fpjson.md
  ============================================================================ }
program NestedJSON;

{$mode objfpc}{$H+}

uses
  fpjson, jsonparser, SysUtils;

var
  JsonString: String;
  JsonData: TJSONData;
  JsonObj, AdresseObj: TJSONObject;
  Nom, Ville, Rue: String;
begin
  JsonString := '{' +
    '"nom":"Dupont",' +
    '"prenom":"Jean",' +
    '"adresse":{' +
      '"rue":"123 rue de la Paix",' +
      '"ville":"Paris",' +
      '"code_postal":"75001"' +
    '}' +
  '}';

  JsonData := GetJSON(JsonString);
  try
    JsonObj := TJSONObject(JsonData);

    // Extraire les données du niveau principal
    Nom := JsonObj.Get('nom', '');

    // Extraire l'objet imbriqué "adresse"
    AdresseObj := JsonObj.GetPath('adresse') as TJSONObject;
    Ville := AdresseObj.Get('ville', '');
    Rue := AdresseObj.Get('rue', '');

    WriteLn('Nom : ', Nom);
    WriteLn('Adresse : ', Rue, ', ', Ville);
  finally
    JsonData.Free;
  end;
end.
