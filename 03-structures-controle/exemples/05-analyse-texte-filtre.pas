{ ============================================================================
  Section 3.5 : Instructions break et continue
  Description : Analyse de texte avec filtrage des non-lettres via continue
  Fichier source : 05-instructions-break-continue.md
  ============================================================================ }
program AnalyseTexteFiltre;  
var  
  texte: String;
  i: Integer;
  caractere: Char;
  compteurLettres: Integer;
begin
  compteurLettres := 0;

  Write('Entrez un texte : ');
  ReadLn(texte);
  WriteLn;
  WriteLn('Lettres trouvées :');

  for i := 1 to Length(texte) do
  begin
    caractere := texte[i];

    // Ignorer les espaces et la ponctuation
    if not (caractere in ['A'..'Z', 'a'..'z']) then
      continue;  // Passe au caractère suivant

    // Afficher les lettres
    Write(caractere, ' ');
    compteurLettres := compteurLettres + 1;

    // Arrêter après 50 lettres
    if compteurLettres >= 50 then
    begin
      WriteLn('...');
      break;
    end;
  end;

  WriteLn;
  WriteLn('Total de lettres : ', compteurLettres);
end.
