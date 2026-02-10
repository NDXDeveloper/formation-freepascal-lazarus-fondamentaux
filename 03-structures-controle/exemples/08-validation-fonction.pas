{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Fonction de validation - code reutilisable pour entier valide
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program ValidationFonction;

function EstNombreEntier(s: String): Boolean;  
var  
  i: Integer;
begin
  EstNombreEntier := True;

  if Length(s) = 0 then
  begin
    EstNombreEntier := False;
    exit;  // exit quitte immédiatement la fonction (comme return en C/Java)
  end;

  for i := 1 to Length(s) do
  begin
    if not (s[i] in ['0'..'9']) then
    begin
      EstNombreEntier := False;
      exit;
    end;
  end;
end;

var
  saisie: String;
begin
  repeat
    Write('Entrez un nombre entier : ');
    ReadLn(saisie);

    if not EstNombreEntier(saisie) then
      WriteLn('❌ Ce n''est pas un nombre entier valide');
  until EstNombreEntier(saisie);

  WriteLn('✓ Nombre valide : ', saisie);
end.
