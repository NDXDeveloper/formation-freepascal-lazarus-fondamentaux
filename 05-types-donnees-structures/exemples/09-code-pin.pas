{ ============================================================================
  Section 5.9 : Types intervalle
  Description : Code PIN a 4 chiffres avec type intervalle TChiffre 0..9
  Fichier source : 09-types-intervalle.md
  ============================================================================ }
program CodePIN;  
type  
  TChiffre = 0..9;
  TCodePIN = array[1..4] of TChiffre;

var
  code: TCodePIN;
  tentative: TCodePIN;
  i: Integer;
  correct: Boolean;

procedure SaisirCode(var c: TCodePIN);  
var  
  i: Integer;
begin
  WriteLn('Entrez un code à 4 chiffres :');
  for i := 1 to 4 do
  begin
    Write('  Chiffre ', i, ' (0-9) : ');
    ReadLn(c[i]);
  end;
end;

function CodesIdentiques(c1, c2: TCodePIN): Boolean;  
var  
  i: Integer;
begin
  CodesIdentiques := True;
  for i := 1 to 4 do
  begin
    if c1[i] <> c2[i] then
    begin
      CodesIdentiques := False;
      Break;
    end;
  end;
end;

begin
  WriteLn('=== CRÉATION DE CODE PIN ===');
  SaisirCode(code);

  WriteLn;
  WriteLn('=== VÉRIFICATION ===');
  SaisirCode(tentative);

  if CodesIdentiques(code, tentative) then
    WriteLn('✓ Code correct !')
  else
    WriteLn('✗ Code incorrect');
end.
