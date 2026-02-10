{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Validation de chaine non vide avec suppression des espaces
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program ValidationNonVide;  
uses  
  SysUtils;
var
  nom: String;
begin
  repeat
    Write('Nom : ');
    ReadLn(nom);

    // Supprimer les espaces au début et à la fin
    nom := Trim(nom);

    if Length(nom) = 0 then
      WriteLn('❌ Le nom ne peut pas être vide');
  until Length(nom) > 0;

  WriteLn('✓ Bonjour ', nom, ' !');
end.
