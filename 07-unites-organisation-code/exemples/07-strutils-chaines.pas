{ ============================================================================
  Section 7.7 : Unites standard du RTL
  Description : Demonstration de StrUtils pour manipulation avancee de chaines
  Fichier source : 07-unites-standard-rtl.md
  ============================================================================ }
{$mode objfpc}{$H+}
program StrUtilsChaines;

uses
  SysUtils, StrUtils;

var
  texte, resultat: String;

begin
  texte := 'Bonjour le monde';

  // Extraire la gauche ou la droite
  resultat := LeftStr(texte, 7);    // 'Bonjour'
  WriteLn('LeftStr : ', resultat);

  resultat := RightStr(texte, 5);   // 'monde'
  WriteLn('RightStr : ', resultat);

  // Extraire le milieu
  resultat := MidStr(texte, 9, 2);  // 'le'
  WriteLn('MidStr : ', resultat);

  // Inverser une chaine
  resultat := ReverseString(texte);  // 'ednom el ruojnoB'
  WriteLn('ReverseString : ', resultat);

  // Remplacer du texte
  resultat := StringReplace(texte, 'monde', 'Pascal', [rfReplaceAll]);
  WriteLn('StringReplace : ', resultat);  // 'Bonjour le Pascal'

  // Verifier si un texte contient une sous-chaine
  if AnsiContainsStr(texte, 'monde') then
    WriteLn('Le texte contient "monde"');

  // Repeter une chaine
  resultat := DupeString('Ha', 3);  // 'HaHaHa'
  WriteLn('DupeString : ', resultat);
end.
