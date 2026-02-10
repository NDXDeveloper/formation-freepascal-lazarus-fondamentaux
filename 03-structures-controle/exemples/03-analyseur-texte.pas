{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Analyse d'un texte saisi (voyelles, consonnes, chiffres, espaces)
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program AnalyseurTexte;  
var  
  texte: String;
  i: Integer;
  caractere: Char;
  compteurVoyelles, compteurConsonnes: Integer;
  compteurChiffres, compteurEspaces: Integer;
begin
  compteurVoyelles := 0;
  compteurConsonnes := 0;
  compteurChiffres := 0;
  compteurEspaces := 0;
  WriteLn('=== ANALYSEUR DE TEXTE ===');
  WriteLn;
  Write('Entrez un texte : ');
  ReadLn(texte);
  WriteLn;
  WriteLn('Analyse en cours...');
  WriteLn;
  for i := 1 to Length(texte) do
  begin
    caractere := texte[i];
    case caractere of
      'A', 'E', 'I', 'O', 'U', 'Y',
      'a', 'e', 'i', 'o', 'u', 'y':
        compteurVoyelles := compteurVoyelles + 1;
      { 'B'..'D' = plage de caracteres, equivaut a 'B','C','D' }
      'B'..'D', 'F'..'H', 'J'..'N', 'P'..'T', 'V'..'X', 'Z',
      'b'..'d', 'f'..'h', 'j'..'n', 'p'..'t', 'v'..'x', 'z':
        compteurConsonnes := compteurConsonnes + 1;
      '0'..'9':
        compteurChiffres := compteurChiffres + 1;
      ' ':
        compteurEspaces := compteurEspaces + 1;
    end;
  end;
  WriteLn('=== RÉSULTATS ===');
  WriteLn('Longueur totale : ', Length(texte), ' caractères');
  WriteLn('Voyelles : ', compteurVoyelles);
  WriteLn('Consonnes : ', compteurConsonnes);
  WriteLn('Chiffres : ', compteurChiffres);
  WriteLn('Espaces : ', compteurEspaces);
  WriteLn('Autres caractères : ',
    Length(texte) - compteurVoyelles - compteurConsonnes -
    compteurChiffres - compteurEspaces);
  WriteLn;
  WriteLn('Aperçu caractère par caractère :');
  for i := 1 to Length(texte) do
  begin
    Write(texte[i]);
    if i mod 10 = 0 then
      WriteLn;
  end;
  WriteLn;
end.
