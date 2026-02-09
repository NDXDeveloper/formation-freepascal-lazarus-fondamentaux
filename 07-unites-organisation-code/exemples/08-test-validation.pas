{ ============================================================================
  Section 7.8 : Creation de bibliotheques reutilisables
  Description : Programme testant la bibliotheque UniteValidation
  Fichier source : 08-creation-bibliotheques-reutilisables.md
  ============================================================================ }
{$mode objfpc}{$H+}
program TestValidation;

uses
  SysUtils, UniteValidation;

var
  email, tel, cp: String;

begin
  WriteLn('=== Test de la bibliothèque de validation ===');
  WriteLn;

  // Test email
  email := 'jean.dupont@example.com';
  if EstEmailValide(email) then
    WriteLn('V ', email, ' est valide')
  else
    WriteLn('X ', email, ' est invalide');

  // Test telephone
  tel := '06 01 02 03 04';
  if EstTelephoneValide(tel) then
    WriteLn('V ', tel, ' est valide')
  else
    WriteLn('X ', tel, ' est invalide');

  // Test code postal
  cp := '75001';
  if EstCodePostalValide(cp) then
    WriteLn('V ', cp, ' est valide')
  else
    WriteLn('X ', cp, ' est invalide');
end.
