{ ============================================================================
  Section 5.8 : Types ensemble (Set)
  Description : Comptage de voyelles et consonnes avec ensemble de caracteres
  Fichier source : 08-types-ensemble-set.md
  ============================================================================ }
program VoyellesConsonnes;  
type  
  TLettres = set of 'A'..'Z';

var
  voyelles: TLettres;
  phrase: String;
  i: Integer;
  c: Char;
  nbVoyelles, nbConsonnes: Integer;

begin
  // Définir les voyelles
  voyelles := ['A', 'E', 'I', 'O', 'U', 'Y'];

  Write('Entrez une phrase : ');
  ReadLn(phrase);

  nbVoyelles := 0;
  nbConsonnes := 0;

  for i := 1 to Length(phrase) do
  begin
    c := UpCase(phrase[i]);  // Convertir en majuscule

    if (c >= 'A') and (c <= 'Z') then
    begin
      if c in voyelles then
        nbVoyelles := nbVoyelles + 1
      else
        nbConsonnes := nbConsonnes + 1;
    end;
  end;

  WriteLn('Voyelles : ', nbVoyelles);
  WriteLn('Consonnes : ', nbConsonnes);
end.
