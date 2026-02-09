{ ============================================================================
  Section 7.7 : Unites standard du RTL
  Description : Demonstration de la gestion des exceptions avec SysUtils
  Fichier source : 07-unites-standard-rtl.md
  ============================================================================ }
{$mode objfpc}{$H+}
program SysUtilsExceptions;

uses
  SysUtils;

var
  nombre: Integer;

begin
  try
    nombre := StrToInt('abc');  // Erreur : pas un nombre
  except
    on E: EConvertError do
      WriteLn('Erreur de conversion : ', E.Message);
  end;
end.
