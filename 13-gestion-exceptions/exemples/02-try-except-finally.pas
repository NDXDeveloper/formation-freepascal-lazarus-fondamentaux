{ ============================================================================
  Section 13.2 : Try-Except-Finally
  Description : Try-except basique, types multiples, try-finally, propagation
  Fichier source : 02-try-except-finally.md
  ============================================================================ }
program TryExceptFinally;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

{ --- Demonstration 1 : try-except basique avec erreur de conversion --- }
procedure DemoTryExceptBasique;  
var  
  nombre: Integer;
  texte: String;
begin
  WriteLn('=== 1. Try-except basique ===');
  texte := 'abc';

  try
    nombre := StrToInt(texte);
    WriteLn('Le nombre est : ', nombre);
  except
    WriteLn('Erreur : impossible de convertir le texte en nombre');
  end;

  WriteLn('Le programme continue normalement');
  WriteLn;
end;

{ --- Demonstration 2 : gestion de plusieurs types d'exceptions --- }
procedure DemoTypesMultiples;  
var  
  nombre, diviseur, resultat: Integer;
begin
  WriteLn('=== 2. Plusieurs types d''exceptions ===');

  { Test avec EConvertError }
  WriteLn('Test A : conversion invalide');
  try
    nombre := StrToInt('douze');
    diviseur := 2;
    resultat := nombre div diviseur;
    WriteLn('Resultat : ', resultat);
  except
    on E: EConvertError do
      WriteLn('  Erreur de conversion : ', E.Message);
    on E: EDivByZero do
      WriteLn('  Erreur : division par zero impossible !');
    else
      WriteLn('  Une erreur inattendue s''est produite');
  end;

  { Test avec EDivByZero }
  WriteLn('Test B : division par zero');
  try
    nombre := 42;
    diviseur := 0;
    resultat := nombre div diviseur;
    WriteLn('Resultat : ', resultat);
  except
    on E: EConvertError do
      WriteLn('  Erreur de conversion : ', E.Message);
    on E: EDivByZero do
      WriteLn('  Erreur : division par zero impossible !');
    else
      WriteLn('  Une erreur inattendue s''est produite');
  end;

  WriteLn;
end;

{ --- Demonstration 3 : try-finally pour liberation de ressources --- }
procedure DemoTryFinally;  
var  
  liste: TStringList;
begin
  WriteLn('=== 3. Try-finally (liberation de ressources) ===');
  liste := TStringList.Create;
  try
    liste.Add('Ligne 1');
    liste.Add('Ligne 2');
    liste.Add('Ligne 3');
    WriteLn('  Liste creee avec ', liste.Count, ' elements');
    WriteLn('  Contenu : ', liste.CommaText);
  finally
    liste.Free;
    WriteLn('  Liste liberee (finally execute)');
  end;
  WriteLn;
end;

{ --- Demonstration 4 : finally s'execute meme avec Exit --- }
procedure DemoFinallyAvecExit;  
begin  
  WriteLn('=== 4. Finally s''execute meme avec Exit ===');
  WriteLn('  Debut');
  try
    WriteLn('  Dans try');
    Exit;
    WriteLn('  Apres Exit - jamais execute');
  finally
    WriteLn('  Dans finally - TOUJOURS execute');
  end;
  WriteLn('  Fin - jamais execute');
end;

{ --- Demonstration 5 : propagation des exceptions --- }
procedure NiveauBas;  
begin  
  raise Exception.Create('Erreur au niveau bas');
end;

procedure NiveauMoyen;  
begin  
  NiveauBas;
end;

procedure NiveauHaut;  
begin  
  try
    NiveauMoyen;
  except
    on E: Exception do
      WriteLn('  Exception capturee en haut : ', E.Message);
  end;
end;

procedure DemoPropagation;  
begin  
  WriteLn;
  WriteLn('=== 5. Propagation des exceptions ===');
  NiveauHaut;
  WriteLn;
end;

{ --- Demonstration 6 : combinaison try-except + try-finally --- }
procedure DemoCombinaison;  
var  
  liste: TStringList;
begin
  WriteLn('=== 6. Combinaison try-except + try-finally ===');
  try
    liste := TStringList.Create;
    try
      liste.Add('Element valide');
      WriteLn('  Element ajoute : ', liste[0]);
      { Provoquons une erreur }
      raise Exception.Create('Erreur simulee pendant le traitement');
    finally
      liste.Free;
      WriteLn('  Liste liberee dans finally');
    end;
  except
    on E: Exception do
      WriteLn('  Exception geree : ', E.Message);
  end;
  WriteLn;
end;

{ === Programme principal === }
begin
  WriteLn('--- Chapitre 13.2 : Try-Except-Finally ---');
  WriteLn;

  DemoTryExceptBasique;
  DemoTypesMultiples;
  DemoTryFinally;
  DemoFinallyAvecExit;
  DemoPropagation;
  DemoCombinaison;

  WriteLn('--- Fin des demonstrations ---');
end.
