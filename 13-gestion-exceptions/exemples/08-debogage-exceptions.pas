{ ============================================================================
  Section 13.8 : Debogage avec exceptions
  Description : Debug conversion, Assert, test d'exceptions, backtrace
  Fichier source : 08-debogage-avec-exceptions.md
  ============================================================================ }
program DebogageExceptions;

{$mode objfpc}{$H+}
{$ASSERTIONS ON}

uses
  SysUtils;

{ --- Demonstration 1 : debug de conversion caractere par caractere --- }
procedure DebugConversion(const texte: String);  
var  
  nombre: Integer;
  i: Integer;
begin
  WriteLn('  Tentative de conversion de : "', texte, '"');
  WriteLn('  Longueur : ', Length(texte));

  if Length(texte) > 0 then
    WriteLn('  Premier caractere (ord) : ', Ord(texte[1]));

  try
    nombre := StrToInt(texte);
    WriteLn('  Conversion reussie : ', nombre);
  except
    on E: EConvertError do
    begin
      WriteLn('  Conversion echouee !');
      WriteLn('  La chaine contient :');
      for i := 1 to Length(texte) do
        WriteLn('    Position ', i, ' : "', texte[i], '" (code: ', Ord(texte[i]), ')');
    end;
  end;
end;

procedure DemoDebugConversion;  
begin  
  WriteLn('=== 1. Debug de conversion ===');
  DebugConversion('123');
  WriteLn;
  DebugConversion('12 3');    { Espace cache }
  WriteLn;
  DebugConversion('123abc');  { Caracteres non numeriques }
  WriteLn;
end;

{ --- Demonstration 2 : Assert pour verifier les preconditions --- }
procedure TraiterTableau(const tableau: array of Integer; index: Integer);  
begin  
  Assert(Length(tableau) > 0, 'Le tableau ne doit pas etre vide');
  Assert((index >= Low(tableau)) and (index <= High(tableau)),
         Format('Index %d hors limites [%d..%d]',
                [index, Low(tableau), High(tableau)]));

  WriteLn('  Valeur a l''index ', index, ' : ', tableau[index]);
end;

procedure DemoAssert;  
var  
  donnees: array[0..4] of Integer;
  i: Integer;
begin
  WriteLn('=== 2. Assert pour verifier les preconditions ===');

  for i := 0 to 4 do
    donnees[i] := (i + 1) * 10;

  { Acces valide }
  TraiterTableau(donnees, 2);

  { Acces invalide - Assert va lever EAssertionFailed }
  try
    TraiterTableau(donnees, 10);
  except
    on E: EAssertionFailed do
      WriteLn('  Assertion echouee : ', E.Message);
  end;

  WriteLn;
end;

{ --- Demonstration 3 : tester que les exceptions sont correctement levees --- }
procedure ValiderEmail(const email: String);  
begin  
  if Pos('@', email) = 0 then
    raise EConvertError.CreateFmt('"%s" n''est pas un email valide', [email]);
end;

procedure TestConversionInvalide;  
var  
  exceptionLevee: Boolean;
begin
  exceptionLevee := False;
  try
    StrToInt('abc');
  except
    on E: EConvertError do
      exceptionLevee := True;
  end;

  if exceptionLevee then
    WriteLn('  Test 1 REUSSI : EConvertError correctement levee pour "abc"')
  else
    WriteLn('  Test 1 ECHOUE : exception non levee');
end;

procedure TestEmailInvalide;  
var  
  exceptionLevee: Boolean;
begin
  exceptionLevee := False;
  try
    ValiderEmail('pas-un-email');
  except
    on E: EConvertError do
      exceptionLevee := True;
  end;

  if exceptionLevee then
    WriteLn('  Test 2 REUSSI : exception levee pour email invalide')
  else
    WriteLn('  Test 2 ECHOUE : exception non levee');
end;

procedure TestEmailValide;  
var  
  exceptionLevee: Boolean;
begin
  exceptionLevee := False;
  try
    ValiderEmail('user@example.com');
  except
    on E: EConvertError do
      exceptionLevee := True;
  end;

  if not exceptionLevee then
    WriteLn('  Test 3 REUSSI : pas d''exception pour email valide')
  else
    WriteLn('  Test 3 ECHOUE : exception levee pour email valide');
end;

procedure DemoTestExceptions;  
begin  
  WriteLn('=== 3. Tester que les exceptions sont levees ===');
  TestConversionInvalide;
  TestEmailInvalide;
  TestEmailValide;
  WriteLn;
end;

{ --- Demonstration 4 : backtrace d'exception --- }
procedure FonctionProfonde;  
begin  
  raise Exception.Create('Erreur dans la fonction profonde');
end;

procedure FonctionIntermediaire;  
begin  
  FonctionProfonde;
end;

procedure AfficherInfoException;  
var  
  i: Integer;
  frames: PPointer;
begin
  WriteLn('  Adresse exception : $', HexStr(ExceptAddr));
  frames := ExceptFrames;
  if ExceptFrameCount > 0 then
  begin
    WriteLn('  Pile d''appels (', ExceptFrameCount, ' frames) :');
    for i := 0 to ExceptFrameCount - 1 do
      WriteLn('    #', i, ' : $', HexStr(frames[i]));
  end
  else
    WriteLn('  (pas de frames disponibles - compiler avec -gl pour les avoir)');
end;

procedure DemoBacktrace;  
begin  
  WriteLn('=== 4. Backtrace d''exception ===');
  try
    FonctionIntermediaire;
  except
    on E: Exception do
    begin
      WriteLn('  Exception : ', E.ClassName, ' - ', E.Message);
      AfficherInfoException;
    end;
  end;
  WriteLn;
end;

{ --- Demonstration 5 : enrichissement avec contexte --- }
procedure TraiterLigne(numeroLigne: Integer; const contenu: String);  
begin  
  try
    StrToInt(contenu);
  except
    on E: Exception do
      raise Exception.CreateFmt(
        'Erreur ligne %d (%s) : %s',
        [numeroLigne, E.ClassName, E.Message]
      );
  end;
end;

procedure DemoContexte;  
begin  
  WriteLn('=== 5. Enrichissement avec contexte ===');
  try
    TraiterLigne(1, '42');
    WriteLn('  Ligne 1 : OK');
    TraiterLigne(2, '100');
    WriteLn('  Ligne 2 : OK');
    TraiterLigne(3, 'abc');
    WriteLn('  Ligne 3 : OK');
  except
    on E: Exception do
      WriteLn('  ', E.Message);
  end;
  WriteLn;
end;

{ === Programme principal === }
begin
  WriteLn('--- Chapitre 13.8 : Debogage avec exceptions ---');
  WriteLn;

  DemoDebugConversion;
  DemoAssert;
  DemoTestExceptions;
  DemoBacktrace;
  DemoContexte;

  WriteLn('--- Fin des demonstrations ---');
end.
