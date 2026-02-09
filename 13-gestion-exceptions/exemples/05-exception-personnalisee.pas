{ ============================================================================
  Section 13.5 : Exceptions personnalisées
  Description : Exception personnalisee EAgeInvalide avec validation
  Fichier source : 05-exceptions-personnalisees.md
  ============================================================================ }
program ExempleExceptionSimple;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  EAgeInvalide = class(Exception);  // Préfixe E par convention ; hérite de Exception (SysUtils)

procedure VerifierAge(age: Integer);
begin
  if (age < 0) or (age > 150) then
    raise EAgeInvalide.Create('L''âge doit être entre 0 et 150');  // raise crée et déclenche l'exception

  WriteLn('Âge valide : ', age);
end;

begin
  try
    VerifierAge(200);
  except
    on E: EAgeInvalide do  // on E: Type do — capture l'exception dans E
      WriteLn('Erreur d''âge : ', E.Message);
  end;
end.
