{ ============================================================================
  Section 13.4 : Hierarchie des exceptions
  Description : ClassName, Message, ordre de capture, types standards, test is
  Fichier source : 04-hierarchie-exceptions.md
  ============================================================================ }
program HierarchieExceptions;

{$mode objfpc}{$H+}

uses
  SysUtils;

{ --- Demonstration 1 : E.ClassName et E.Message --- }
procedure DemoClassNameMessage;  
begin  
  WriteLn('=== 1. E.ClassName et E.Message ===');

  try
    StrToInt('abc');
  except
    on E: Exception do
    begin
      WriteLn('  Classe : ', E.ClassName);
      WriteLn('  Message : ', E.Message);
    end;
  end;

  WriteLn;
end;

{ --- Demonstration 2 : ordre de capture (specifique -> general) --- }
procedure DemoOrdreCapture;  
var  
  x, zero: Integer;
begin
  WriteLn('=== 2. Ordre de capture (specifique -> general) ===');
  zero := 0;

  { EDivByZero est capture par le handler specifique }
  WriteLn('Test A : division par zero');
  try
    x := 10 div zero;
    WriteLn(x);  { jamais execute }
  except
    on E: EDivByZero do
      WriteLn('  Capture comme EDivByZero (specifique)');
    on E: EIntError do
      WriteLn('  Capture comme EIntError (parent)');
    on E: Exception do
      WriteLn('  Capture comme Exception (general)');
  end;

  { EConvertError n'est pas EDivByZero ni EIntError, tombe dans Exception }
  WriteLn('Test B : erreur de conversion');
  try
    x := StrToInt('xyz');
    WriteLn(x);
  except
    on E: EDivByZero do
      WriteLn('  Capture comme EDivByZero');
    on E: EIntError do
      WriteLn('  Capture comme EIntError');
    on E: EConvertError do
      WriteLn('  Capture comme EConvertError');
    on E: Exception do
      WriteLn('  Capture comme Exception (general)');
  end;

  WriteLn;
end;

{ --- Demonstration 3 : types standards (EDivByZero, EConvertError, ERangeError) --- }
procedure DemoTypesStandards;
{$R+}  { Activer les verifications de limites pour ERangeError }
var
  tableau: array[1..5] of Integer;
  i, x, zero: Integer;
begin
  WriteLn('=== 3. Types d''exceptions standards ===');
  zero := 0;

  { EDivByZero }
  WriteLn('Test A : EDivByZero');
  try
    x := 100 div zero;
    WriteLn(x);
  except
    on E: EDivByZero do
      WriteLn('  ', E.ClassName, ' : ', E.Message);
  end;

  { EConvertError }
  WriteLn('Test B : EConvertError');
  try
    x := StrToInt('abc');
    WriteLn(x);
  except
    on E: EConvertError do
      WriteLn('  ', E.ClassName, ' : ', E.Message);
  end;

  { ERangeError }
  WriteLn('Test C : ERangeError');
  try
    for i := 1 to 5 do
      tableau[i] := i * 10;
    i := 10;
    tableau[i] := 999;  { Hors limites ! }
  except
    on E: ERangeError do
      WriteLn('  ', E.ClassName, ' : index ', i, ' hors limites');
  end;

  WriteLn;
end;
{$R-}

{ --- Demonstration 4 : test is pour verification dynamique du type --- }
procedure DemoTestIs;

  procedure TesterException(provoqueur: Integer);
  var
    x, zero: Integer;
  begin
    zero := 0;
    try
      case provoqueur of
        1: x := StrToInt('xyz');
        2: x := 10 div zero;
        3: raise Exception.Create('Erreur generique');
      end;
      WriteLn(x);  { evite hint unused }
    except
      on E: Exception do
      begin
        Write('  ');
        if E is EDivByZero then
          WriteLn('C''est une division par zero')
        else if E is EConvertError then
          WriteLn('C''est une erreur de conversion')
        else if E is EIntError then
          WriteLn('C''est une erreur d''entier')
        else
          WriteLn('Autre erreur : ', E.ClassName, ' - ', E.Message);
      end;
    end;
  end;

begin
  WriteLn('=== 4. Test "is" pour verification dynamique ===');
  TesterException(1);  { EConvertError }
  TesterException(2);  { EDivByZero }
  TesterException(3);  { Exception generique }
  WriteLn;
end;

{ --- Demonstration 5 : heritage - EDivByZero est une EIntError --- }
procedure DemoHeritage;  
var  
  x, zero: Integer;
begin
  WriteLn('=== 5. Heritage des exceptions ===');
  zero := 0;

  { EDivByZero capture comme EIntError (son parent) }
  WriteLn('Test A : EDivByZero capture comme EIntError');
  try
    x := 10 div zero;
    WriteLn(x);
  except
    on E: EIntError do
      WriteLn('  Capture comme EIntError : ', E.ClassName);
  end;

  { EDivByZero capture comme Exception (ancetre) }
  WriteLn('Test B : EDivByZero capture comme Exception');
  try
    x := 10 div zero;
    WriteLn(x);
  except
    on E: Exception do
      WriteLn('  Capture comme Exception : ', E.ClassName);
  end;

  { EDivByZero ne peut PAS etre capture comme EConvertError }
  WriteLn('Test C : EDivByZero n''est PAS une EConvertError');
  try
    x := 10 div zero;
    WriteLn(x);
  except
    on E: EConvertError do
      WriteLn('  Capture comme EConvertError');
    on E: Exception do
      WriteLn('  Non capture comme EConvertError, tombe dans Exception : ', E.ClassName);
  end;

  WriteLn;
end;

{ === Programme principal === }
begin
  WriteLn('--- Chapitre 13.4 : Hierarchie des exceptions ---');
  WriteLn;

  DemoClassNameMessage;
  DemoOrdreCapture;
  DemoTypesStandards;
  DemoTestIs;
  DemoHeritage;

  WriteLn('--- Fin des demonstrations ---');
end.
