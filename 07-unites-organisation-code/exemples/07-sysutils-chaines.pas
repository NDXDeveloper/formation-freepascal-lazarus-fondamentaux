{ ============================================================================
  Section 7.7 : Unites standard du RTL
  Description : Demonstration de SysUtils pour la manipulation de chaines
  Fichier source : 07-unites-standard-rtl.md
  ============================================================================ }
{$mode objfpc}{$H+}
program SysUtilsChaines;

uses
  SysUtils;

var
  texte, maj, min: String;
  i: Integer;

begin
  texte := '  Bonjour le Monde  ';

  // Conversion de casse
  maj := UpperCase(texte);     // '  BONJOUR LE MONDE  '
  min := LowerCase(texte);     // '  bonjour le monde  '
  WriteLn('Majuscules : ', maj);
  WriteLn('Minuscules : ', min);

  // Suppression des espaces
  texte := Trim(texte);        // 'Bonjour le Monde'
  WriteLn('Trim : ', texte);

  // Conversion en entier
  i := StrToInt('123');        // 123
  WriteLn('StrToInt : ', i);

  // Conversion d'entier en chaine
  texte := IntToStr(456);      // '456'
  WriteLn('IntToStr : ', texte);

  // Formatage
  texte := Format('J''ai %d ans', [25]);  // 'J'ai 25 ans'
  WriteLn('Format : ', texte);
end.
