{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Conversion de chaines vers nombres (StrToInt, Val, StrToFloat)
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program StringVersNombre;
uses SysUtils;
var
  texte: String;
  nombre: Integer;
  valeur: Real;
  code: Integer;
begin
  // Méthode 1 : StrToInt (peut générer une exception)
  texte := '123';
  nombre := StrToInt(texte);
  WriteLn('Nombre : ', nombre);

  // Méthode 2 : Val (plus sûre)
  Val(texte, nombre, code);  // Val : code vaut 0 si OK, sinon position de l'erreur
  if code = 0 then
    WriteLn('Conversion réussie : ', nombre)
  else
    WriteLn('Erreur à la position : ', code);

  // Pour les réels
  valeur := StrToFloat('3.14');
  WriteLn('Valeur : ', valeur:0:2);
end.
