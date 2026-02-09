{ ============================================================================
  Section 5.8 : Types ensemble (Set)
  Description : Validation de mot de passe avec analyse de types de caracteres
  Fichier source : 08-types-ensemble-set.md
  ============================================================================ }
{$mode objfpc}{$H+}
program ValidationMotDePasse;
type
  TTypeCaractere = (Minuscule, Majuscule, Chiffre, Special);
  TTypesCaracteres = set of TTypeCaractere;

function AnalyserMotDePasse(mdp: String): TTypesCaracteres;
var
  i: Integer;
  c: Char;
begin
  Result := [];  // Ensemble vide au départ

  for i := 1 to Length(mdp) do
  begin
    c := mdp[i];

    if (c >= 'a') and (c <= 'z') then
      Include(Result, Minuscule)
    else if (c >= 'A') and (c <= 'Z') then
      Include(Result, Majuscule)
    else if (c >= '0') and (c <= '9') then
      Include(Result, Chiffre)
    else
      Include(Result, Special);
  end;
end;

function MotDePasseValide(mdp: String): Boolean;
var
  types: TTypesCaracteres;
begin
  // Un mot de passe valide doit contenir au moins 3 types
  types := AnalyserMotDePasse(mdp);

  // Compter les types présents
  MotDePasseValide :=
    (Minuscule in types) and
    (Majuscule in types) and
    ((Chiffre in types) or (Special in types));
end;

var
  motDePasse: String;
  types: TTypesCaracteres;
begin
  Write('Entrez un mot de passe : ');
  ReadLn(motDePasse);

  types := AnalyserMotDePasse(motDePasse);

  WriteLn;
  WriteLn('Analyse :');
  if Minuscule in types then WriteLn('  Contient des minuscules');
  if Majuscule in types then WriteLn('  Contient des majuscules');
  if Chiffre in types then WriteLn('  Contient des chiffres');
  if Special in types then WriteLn('  Contient des caractères spéciaux');

  WriteLn;
  if MotDePasseValide(motDePasse) then
    WriteLn('Mot de passe VALIDE')
  else
    WriteLn('Mot de passe INVALIDE (doit contenir minuscules, majuscules et chiffres/spéciaux)');
end.
