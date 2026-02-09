{ ============================================================================
  Section 2.9 : Conventions de nommage
  Description : Calcul de moyenne avec appreciation - exemple de bonnes
                conventions de nommage (camelCase, SCREAMING_SNAKE_CASE, etc.)
  Fichier source : 09-conventions-nommage.md
  ============================================================================ }
{$mode objfpc}{$H+}
program CalculateurMoyenne;

const
  NOMBRE_NOTES = 3;
  NOTE_MINIMUM = 0.0;
  NOTE_MAXIMUM = 20.0;

var
  note1, note2, note3: real;
  moyenneGenerale: real;
  appreciation: string;

function CalculerMoyenne(n1, n2, n3: real): real;
begin
  { Result est la variable spéciale pour renvoyer la valeur d'une fonction }
  Result := (n1 + n2 + n3) / NOMBRE_NOTES;
end;

function ObtenirAppreciation(moyenne: real): string;
begin
  if moyenne >= 16 then
    Result := 'Très bien'
  else if moyenne >= 14 then
    Result := 'Bien'
  else if moyenne >= 12 then
    Result := 'Assez bien'
  else if moyenne >= 10 then
    Result := 'Passable'
  else
    Result := 'Insuffisant';
end;

procedure AfficherResultat(moyenne: real; appre: string);
begin
  WriteLn('Moyenne : ', moyenne:0:2, '/20');
  WriteLn('Appréciation : ', appre);
end;

begin
  WriteLn('=== CALCUL DE MOYENNE ===');

  Write('Note 1 : ');
  ReadLn(note1);

  Write('Note 2 : ');
  ReadLn(note2);

  Write('Note 3 : ');
  ReadLn(note3);

  moyenneGenerale := CalculerMoyenne(note1, note2, note3);
  appreciation := ObtenirAppreciation(moyenneGenerale);

  WriteLn;
  AfficherResultat(moyenneGenerale, appreciation);
end.
