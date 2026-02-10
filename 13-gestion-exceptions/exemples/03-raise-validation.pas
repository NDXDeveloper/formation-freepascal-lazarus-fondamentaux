{ ============================================================================
  Section 13.3 : Raise et declenchement
  Description : raise Exception.Create, CreateFmt, re-lever, validation
  Fichier source : 03-raise-declenchement.md
  ============================================================================ }
program RaiseValidation;

{$mode objfpc}{$H+}

uses
  SysUtils;

{ --- Demonstration 1 : raise Exception.Create pour validation --- }
procedure DefinirAge(age: Integer);  
begin  
  if age < 0 then
    raise Exception.Create('L''age ne peut pas etre negatif');

  if age > 150 then
    raise Exception.Create('L''age semble irrealiste');

  WriteLn('  Age defini : ', age);
end;

procedure DemoRaiseBasique;  
begin  
  WriteLn('=== 1. Raise basique pour validation ===');

  try
    DefinirAge(25);
  except
    on E: Exception do
      WriteLn('  Erreur : ', E.Message);
  end;

  try
    DefinirAge(-5);
  except
    on E: Exception do
      WriteLn('  Erreur : ', E.Message);
  end;

  try
    DefinirAge(200);
  except
    on E: Exception do
      WriteLn('  Erreur : ', E.Message);
  end;

  WriteLn;
end;

{ --- Demonstration 2 : raise Exception.CreateFmt --- }
procedure VerifierAge(age: Integer);  
begin  
  if (age < 18) or (age > 65) then
    raise Exception.CreateFmt(
      'Age invalide : %d. Doit etre entre 18 et 65.',
      [age]
    );
  WriteLn('  Age accepte : ', age);
end;

procedure DemoCreateFmt;  
begin  
  WriteLn('=== 2. Raise avec CreateFmt ===');

  try
    VerifierAge(30);
  except
    on E: Exception do
      WriteLn('  Erreur : ', E.Message);
  end;

  try
    VerifierAge(12);
  except
    on E: Exception do
      WriteLn('  Erreur : ', E.Message);
  end;

  WriteLn;
end;

{ --- Demonstration 3 : re-lever une exception avec raise seul --- }
procedure TraiterDonnee(const valeur: String);  
begin  
  StrToInt(valeur);  { Peut lever EConvertError }
end;

procedure TraiterAvecLog(const valeur: String);  
begin  
  try
    TraiterDonnee(valeur);
  except
    on E: Exception do
    begin
      WriteLn('  [LOG] Erreur detectee : ', E.Message);
      raise;  { Re-lever la meme exception }
    end;
  end;
end;

procedure DemoReraise;  
begin  
  WriteLn('=== 3. Re-lever une exception (raise seul) ===');

  try
    TraiterAvecLog('abc');
  except
    on E: Exception do
      WriteLn('  [MAIN] Exception finale capturee : ', E.Message);
  end;

  WriteLn;
end;

{ --- Demonstration 4 : cascade d'exceptions a travers les niveaux --- }
procedure NiveauProfond;  
begin  
  raise Exception.Create('Erreur au niveau profond');
end;

procedure NiveauIntermediaire;  
begin  
  WriteLn('  Avant appel niveau profond');
  NiveauProfond;
  WriteLn('  Apres appel - jamais execute');
end;

procedure NiveauSuperieur;  
begin  
  try
    WriteLn('  Debut du traitement');
    NiveauIntermediaire;
    WriteLn('  Fin du traitement - jamais execute');
  except
    on E: Exception do
      WriteLn('  Exception capturee : ', E.Message);
  end;
  WriteLn('  Apres le try-except');
end;

procedure DemoCascade;  
begin  
  WriteLn('=== 4. Cascade d''exceptions ===');
  NiveauSuperieur;
  WriteLn;
end;

{ --- Demonstration 5 : validation complete d'un formulaire --- }
procedure ValiderFormulaireInscription(
  const nom, email: String;
  age: Integer
);
begin
  { Validation du nom }
  if Trim(nom) = '' then
    raise Exception.Create('Le nom ne peut pas etre vide');

  if Length(nom) < 2 then
    raise Exception.Create('Le nom doit contenir au moins 2 caracteres');

  { Validation de l'email }
  if Pos('@', email) = 0 then
    raise Exception.Create('L''adresse email est invalide');

  { Validation de l'age }
  if age < 18 then
    raise Exception.CreateFmt(
      'Vous devez avoir au moins 18 ans (age actuel : %d)',
      [age]
    );

  if age > 120 then
    raise Exception.CreateFmt(
      'L''age %d semble incorrect',
      [age]
    );

  WriteLn('  Inscription validee pour ', nom);
end;

procedure DemoValidationFormulaire;  
begin  
  WriteLn('=== 5. Validation de formulaire ===');

  { Test 1 : nom vide }
  try
    ValiderFormulaireInscription('', 'test@example.com', 25);
  except
    on E: Exception do
      WriteLn('  Erreur de validation : ', E.Message);
  end;

  { Test 2 : email invalide }
  try
    ValiderFormulaireInscription('Alice', 'pas-un-email', 25);
  except
    on E: Exception do
      WriteLn('  Erreur de validation : ', E.Message);
  end;

  { Test 3 : age trop jeune }
  try
    ValiderFormulaireInscription('Bob', 'bob@mail.com', 15);
  except
    on E: Exception do
      WriteLn('  Erreur de validation : ', E.Message);
  end;

  { Test 4 : tout valide }
  try
    ValiderFormulaireInscription('Charlie', 'charlie@mail.com', 30);
  except
    on E: Exception do
      WriteLn('  Erreur de validation : ', E.Message);
  end;

  WriteLn;
end;

{ === Programme principal === }
begin
  WriteLn('--- Chapitre 13.3 : Raise et validation ---');
  WriteLn;

  DemoRaiseBasique;
  DemoCreateFmt;
  DemoReraise;
  DemoCascade;
  DemoValidationFormulaire;

  WriteLn('--- Fin des demonstrations ---');
end.
