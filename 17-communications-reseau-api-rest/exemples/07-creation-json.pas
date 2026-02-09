{ ============================================================================
  Section 17.7 : Parsing JSON avec fpjson
  Description : Creation d'objets JSON avec fpjson
  Fichier source : 07-parsing-json-fpjson.md
  ============================================================================ }
program CreateJSON;

{$mode objfpc}{$H+}

uses
  fpjson, SysUtils;

var
  JsonObj: TJSONObject;
  JsonString: String;
begin
  // Créer un objet JSON
  JsonObj := TJSONObject.Create;
  try
    // Ajouter des paires clé-valeur
    JsonObj.Add('nom', 'Dupont');
    JsonObj.Add('prenom', 'Jean');
    JsonObj.Add('age', 30);
    JsonObj.Add('actif', True);

    // Convertir en chaîne JSON
    JsonString := JsonObj.AsJSON;
    WriteLn('JSON créé :');
    WriteLn(JsonString);

  finally
    JsonObj.Free;
  end;
end.
