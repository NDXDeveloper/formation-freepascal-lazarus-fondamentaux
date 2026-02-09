{ ============================================================================
  Section 7.8 : Creation de bibliotheques reutilisables
  Description : Bibliotheque de validation de donnees
  Fichier source : 08-creation-bibliotheques-reutilisables.md
  ============================================================================ }

{*******************************************************************************
  UniteValidation - Bibliotheque de validation de donnees

  Cette unite fournit des fonctions pour valider differents formats :
  - Adresses email
  - Numeros de telephone francais
  - Codes postaux francais
  - Dates

  Auteur  : Formation FreePascal
  Version : 1.0
*******************************************************************************}

unit UniteValidation;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, StrUtils;

function EstEmailValide(const email: String): Boolean;
function EstTelephoneValide(const telephone: String): Boolean;
function EstCodePostalValide(const codePostal: String): Boolean;
function EstDateDansIntervalle(date, dateMin, dateMax: TDateTime): Boolean;
function EstNonVide(const texte: String): Boolean;

implementation

function EstEmailValide(const email: String): Boolean;
var
  posArobase, posPoint: Integer;
begin
  Result := False;

  // Verifications de base
  if Length(email) < 5 then
    Exit;  // Trop court (a@b.c minimum)

  // Rechercher @
  posArobase := Pos('@', email);
  if posArobase <= 1 then
    Exit;  // @ absent ou au debut

  // Rechercher . apres @
  posPoint := PosEx('.', email, posArobase);
  if posPoint <= posArobase + 1 then
    Exit;  // . absent ou juste apres @

  // Verifier qu'il y a quelque chose apres le dernier point
  if posPoint >= Length(email) then
    Exit;

  Result := True;
end;

function EstTelephoneValide(const telephone: String): Boolean;
var
  numeros: String;
  i: Integer;
begin
  Result := False;
  numeros := '';

  // Extraire seulement les chiffres
  for i := 1 to Length(telephone) do
  begin
    if telephone[i] in ['0'..'9'] then
      numeros := numeros + telephone[i];
  end;

  // Verifier : 10 chiffres, commence par 0
  if (Length(numeros) = 10) and (numeros[1] = '0') then
    Result := True;
end;

function EstCodePostalValide(const codePostal: String): Boolean;
var
  i: Integer;
begin
  Result := False;

  // Doit faire exactement 5 caracteres
  if Length(codePostal) <> 5 then
    Exit;

  // Tous les caracteres doivent etre des chiffres
  for i := 1 to 5 do
  begin
    if not (codePostal[i] in ['0'..'9']) then
      Exit;
  end;

  Result := True;
end;

function EstDateDansIntervalle(date, dateMin, dateMax: TDateTime): Boolean;
begin
  Result := (date >= dateMin) and (date <= dateMax);
end;

function EstNonVide(const texte: String): Boolean;
begin
  Result := Length(Trim(texte)) > 0;
end;

end.
