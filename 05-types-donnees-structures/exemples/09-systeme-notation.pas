{ ============================================================================
  Section 5.9 : Types intervalle
  Description : Systeme de notation avec conversion entre echelles
  Fichier source : 09-types-intervalle.md
  ============================================================================ }
program SystemeNotation;
type
  TNoteSur20 = 0..20;
  TNoteSur100 = 0..100;
  TPourcentage = 0..100;

function ConvertirNote20Vers100(note: TNoteSur20): TNoteSur100;
begin
  ConvertirNote20Vers100 := note * 5;
end;

function ConvertirNote100Vers20(note: TNoteSur100): TNoteSur20;
begin
  ConvertirNote100Vers20 := note div 5;
end;

function CalculerPourcentage(note, total: Integer): TPourcentage;
begin
  if total > 0 then
    CalculerPourcentage := Round((note * 100) / total)
  else
    CalculerPourcentage := 0;
end;

var
  noteSur20: TNoteSur20;
  noteSur100: TNoteSur100;
  pourcentage: TPourcentage;
begin
  Write('Entrez une note sur 20 : ');
  ReadLn(noteSur20);

  noteSur100 := ConvertirNote20Vers100(noteSur20);
  WriteLn('Note sur 100 : ', noteSur100);

  pourcentage := CalculerPourcentage(noteSur20, 20);
  WriteLn('Pourcentage : ', pourcentage, '%');
end.
