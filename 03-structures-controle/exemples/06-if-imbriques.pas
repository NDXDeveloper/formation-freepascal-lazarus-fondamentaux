{ ============================================================================
  Section 3.6 : Imbrication de structures
  Description : IF imbriques - verification age et permis de conduire
  Fichier source : 06-imbrication-structures.md
  ============================================================================ }
program IfImbriques;
var
  age: Integer;
  permis: Boolean;
  reponse: String;
begin
  Write('Âge : ');
  ReadLn(age);
  Write('Avez-vous le permis ? (true/false) : ');
  ReadLn(reponse);
  permis := (reponse = 'true');

  if age >= 18 then
  begin
    WriteLn('Vous êtes majeur.');

    if permis then
      WriteLn('Vous pouvez conduire.')
    else
      WriteLn('Vous devez passer le permis.');
  end  // Pas de point-virgule avant else : en Pascal, ";" termine
  else // l'instruction if, donc le else n'aurait plus de if associe
    WriteLn('Vous êtes trop jeune pour conduire.');
end.
