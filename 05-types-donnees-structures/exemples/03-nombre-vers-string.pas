{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Conversion de nombres vers chaines (IntToStr, Str, FloatToStr)
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program NombreVersString;
uses SysUtils;
var
  nombre: Integer;
  texte: String;
begin
  nombre := 42;

  // Méthode 1 : IntToStr
  texte := IntToStr(nombre);
  WriteLn('Texte : ', texte);

  // Méthode 2 : Str
  Str(nombre, texte);
  WriteLn('Texte : ', texte);

  // Pour les réels
  texte := FloatToStr(3.14);
  WriteLn('Pi : ', texte);
end.
