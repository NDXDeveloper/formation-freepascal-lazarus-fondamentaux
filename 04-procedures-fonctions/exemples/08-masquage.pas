{ ============================================================================
  Section 4.8 : Variables locales vs globales
  Description : Masquage (shadowing) d'une variable globale par une locale
  Fichier source : 08-variables-locales-vs-globales.md
  ============================================================================ }
program Masquage;

var
  nombre: Integer;  // Variable globale

procedure Test;
var
  nombre: Integer;  // Variable locale (même nom)
begin
  nombre := 50;  // Modifie la variable LOCALE
  WriteLn('Dans Test : ', nombre);  // 50
end;

begin
  nombre := 100;  // Modifie la variable GLOBALE
  WriteLn('Avant Test : ', nombre);  // 100

  Test;

  WriteLn('Après Test : ', nombre);  // 100 (inchangé !)
end.
