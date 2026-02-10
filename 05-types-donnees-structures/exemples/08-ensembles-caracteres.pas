{ ============================================================================
  Section 5.8 : Types ensemble (Set)
  Description : Ensembles de caracteres pour la validation de texte
  Fichier source : 08-types-ensemble-set.md
  ============================================================================ }
program EnsemblesCaracteres;  
type  
  TCaracteres = set of Char;

var
  chiffres: TCaracteres;
  lettres: TCaracteres;
  voyelles: TCaracteres;
  caracteresSaisis: String;
  c: Char;
  i: Integer;

begin
  // Définir des ensembles de caractères
  chiffres := ['0'..'9'];
  lettres := ['a'..'z', 'A'..'Z'];
  voyelles := ['a', 'e', 'i', 'o', 'u', 'y', 'A', 'E', 'I', 'O', 'U', 'Y'];

  Write('Entrez du texte : ');
  ReadLn(caracteresSaisis);

  WriteLn('Analyse :');
  for i := 1 to Length(caracteresSaisis) do
  begin
    c := caracteresSaisis[i];

    Write('  "', c, '" : ');
    if c in chiffres then Write('chiffre ');
    if c in lettres then Write('lettre ');
    if c in voyelles then Write('voyelle');
    WriteLn;
  end;
end.
