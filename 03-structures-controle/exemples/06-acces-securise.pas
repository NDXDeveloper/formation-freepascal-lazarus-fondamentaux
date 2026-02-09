{ ============================================================================
  Section 3.6 : Imbrication de structures
  Description : IF a plusieurs niveaux - controle d'acces securise en cascade
  Fichier source : 06-imbrication-structures.md
  ============================================================================ }
program AccesSecurise;
var
  age: Integer;
  membre: Boolean;
  cotisationAJour: Boolean;
  reponse: String;
begin
  Write('Âge : ');
  ReadLn(age);
  Write('Membre ? (true/false) : ');
  ReadLn(reponse);
  membre := (reponse = 'true');

  if age >= 18 then
  begin
    WriteLn('✓ Âge valide');

    if membre then
    begin
      WriteLn('✓ Vous êtes membre');

      Write('Cotisation à jour ? (true/false) : ');
      ReadLn(reponse);
      cotisationAJour := (reponse = 'true');

      if cotisationAJour then
      begin
        WriteLn('✓ Cotisation à jour');
        WriteLn;
        WriteLn('=== ACCÈS AUTORISÉ ===');
      end
      else
      begin
        WriteLn('✗ Cotisation non payée');
        WriteLn('Veuillez régulariser votre situation.');
      end;
    end
    else
    begin
      WriteLn('✗ Vous n''êtes pas membre');
      WriteLn('Veuillez vous inscrire.');
    end;
  end
  else
  begin
    WriteLn('✗ Âge insuffisant');
    WriteLn('Accès réservé aux majeurs.');
  end;
end.
