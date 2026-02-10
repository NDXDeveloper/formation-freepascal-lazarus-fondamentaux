{ ============================================================================
  Section 17.7 : Parsing JSON avec fpjson
  Description : Unite helper pour simplifier le parsing JSON
  Fichier source : 07-parsing-json-fpjson.md
  ============================================================================ }
unit JSONHelper;

{$mode objfpc}{$H+}

interface

uses
  fpjson, jsonparser, SysUtils;

type
  TJSONHelper = class
  public
    class function ParseString(const JsonString: String): TJSONData;
    class function GetString(JsonObj: TJSONObject; const Key: String; const Default: String = ''): String;
    class function GetInteger(JsonObj: TJSONObject; const Key: String; const Default: Integer = 0): Integer;
    class function GetFloat(JsonObj: TJSONObject; const Key: String; const Default: Double = 0.0): Double;
    class function GetBoolean(JsonObj: TJSONObject; const Key: String; const Default: Boolean = False): Boolean;
    class function GetObject(JsonObj: TJSONObject; const Key: String): TJSONObject;
    class function GetArray(JsonObj: TJSONObject; const Key: String): TJSONArray;
    class function HasKey(JsonObj: TJSONObject; const Key: String): Boolean;
  end;

implementation

class function TJSONHelper.ParseString(const JsonString: String): TJSONData;  
begin  
  try
    Result := GetJSON(JsonString);
  except
    Result := nil;
  end;
end;

class function TJSONHelper.GetString(JsonObj: TJSONObject; const Key: String; const Default: String): String;  
begin  
  if HasKey(JsonObj, Key) then
    Result := JsonObj.Get(Key, Default)
  else
    Result := Default;
end;

class function TJSONHelper.GetInteger(JsonObj: TJSONObject; const Key: String; const Default: Integer): Integer;  
begin  
  if HasKey(JsonObj, Key) then
    Result := JsonObj.Get(Key, Default)
  else
    Result := Default;
end;

class function TJSONHelper.GetFloat(JsonObj: TJSONObject; const Key: String; const Default: Double): Double;  
begin  
  if HasKey(JsonObj, Key) then
    Result := JsonObj.Get(Key, Default)
  else
    Result := Default;
end;

class function TJSONHelper.GetBoolean(JsonObj: TJSONObject; const Key: String; const Default: Boolean): Boolean;  
begin  
  if HasKey(JsonObj, Key) then
    Result := JsonObj.Get(Key, Default)
  else
    Result := Default;
end;

class function TJSONHelper.GetObject(JsonObj: TJSONObject; const Key: String): TJSONObject;  
begin  
  if HasKey(JsonObj, Key) then
    Result := JsonObj.GetPath(Key) as TJSONObject
  else
    Result := nil;
end;

class function TJSONHelper.GetArray(JsonObj: TJSONObject; const Key: String): TJSONArray;  
begin  
  if HasKey(JsonObj, Key) then
    Result := JsonObj.GetPath(Key) as TJSONArray
  else
    Result := nil;
end;

class function TJSONHelper.HasKey(JsonObj: TJSONObject; const Key: String): Boolean;  
begin  
  Result := JsonObj.IndexOfName(Key) >= 0;
end;

end.
