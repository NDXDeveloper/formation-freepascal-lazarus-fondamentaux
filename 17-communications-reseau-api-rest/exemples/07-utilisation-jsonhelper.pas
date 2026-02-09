{ ============================================================================
  Section 17.7 : Parsing JSON avec fpjson
  Description : Utilisation de l'unite JSONHelper
  Fichier source : 07-parsing-json-fpjson.md
  ============================================================================ }
program UseJSONHelper;

{$mode objfpc}{$H+}

uses
  JSONHelper, fpjson, SysUtils;

var
  JsonString: String;
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  Name: String;
  Age: Integer;
begin
  JsonString := '{"nom":"Dupont","prenom":"Jean","age":30}';

  JsonData := TJSONHelper.ParseString(JsonString);
  if Assigned(JsonData) then
  try
    JsonObj := TJSONObject(JsonData);

    // Utilisation simplifiée
    Name := TJSONHelper.GetString(JsonObj, 'nom', 'Inconnu');
    Age := TJSONHelper.GetInteger(JsonObj, 'age', 0);

    WriteLn('Nom : ', Name);
    WriteLn('Age : ', Age);

    // Vérifier l'existence d'une clé
    if TJSONHelper.HasKey(JsonObj, 'email') then
      WriteLn('Email : ', TJSONHelper.GetString(JsonObj, 'email'))
    else
      WriteLn('Email non renseigné');

  finally
    JsonData.Free;
  end
  else
    WriteLn('Erreur de parsing JSON');
end.
