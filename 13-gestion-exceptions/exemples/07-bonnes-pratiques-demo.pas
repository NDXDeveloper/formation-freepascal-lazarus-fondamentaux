{ ============================================================================
  Section 13.7 : Bonnes pratiques
  Description : Messages contextuels, validation precoce, ne pas ignorer, re-lever
  Fichier source : 07-bonnes-pratiques.md
  ============================================================================ }
program BonnesPratiquesDemo;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

{ === Types d'exceptions === }
type
  EValidationException = class(Exception);
  EBusinessException = class(Exception);

{ --- Demonstration 1 : messages d'exception avec contexte (CreateFmt) --- }
procedure ChargerConfiguration(const nomFichier: String);
begin
  if not FileExists(nomFichier) then
    raise Exception.CreateFmt(
      'Impossible d''ouvrir le fichier "%s" : le fichier n''existe pas. ' +
      'Verifiez que l''installation est complete.',
      [nomFichier]
    );
  WriteLn('  Configuration chargee depuis : ', nomFichier);
end;

procedure DemoMessagesContextuels;
begin
  WriteLn('=== 1. Messages d''exception avec contexte ===');

  { Message vague vs message contextuel }
  try
    ChargerConfiguration('config_manquant.xml');
  except
    on E: Exception do
      WriteLn('  ERREUR : ', E.Message);
  end;

  WriteLn;
end;

{ --- Demonstration 2 : validation precoce (verifier avant d'agir) --- }
procedure DemoValidationPrecoce;
var
  a, b, resultat: Integer;
begin
  WriteLn('=== 2. Validation precoce ===');

  { BONNE PRATIQUE : verifier avant }
  a := 42;
  b := 0;

  WriteLn('  Division de ', a, ' par ', b);
  if b <> 0 then
  begin
    resultat := a div b;
    WriteLn('  Resultat : ', resultat);
  end
  else
    WriteLn('  Division impossible : diviseur est zero (verification precoce)');

  { Avec un diviseur valide }
  b := 7;
  WriteLn('  Division de ', a, ' par ', b);
  if b <> 0 then
  begin
    resultat := a div b;
    WriteLn('  Resultat : ', resultat);
  end
  else
    WriteLn('  Division impossible');

  WriteLn;
end;

{ --- Demonstration 3 : ne pas ignorer les exceptions silencieusement --- }
procedure OperationRisquee;
begin
  raise Exception.Create('Quelque chose a mal tourne');
end;

procedure DemoNePasIgnorer;
begin
  WriteLn('=== 3. Ne pas ignorer les exceptions ===');

  { MAUVAISE PRATIQUE : avaler silencieusement (on montre ce qu'il ne faut PAS faire) }
  WriteLn('  Mauvaise pratique (exception avalee) :');
  try
    OperationRisquee;
  except
    { Rien ! L'erreur est ignoree }
  end;
  WriteLn('  ... aucune trace de l''erreur (dangereux !)');

  { BONNE PRATIQUE : au minimum logger }
  WriteLn('  Bonne pratique (exception loguee) :');
  try
    OperationRisquee;
  except
    on E: Exception do
      WriteLn('  [LOG] ', E.ClassName, ' : ', E.Message);
  end;

  WriteLn;
end;

{ --- Demonstration 4 : re-lever apres logging --- }
procedure TraiterDonnees(const source: String);
begin
  raise EBusinessException.CreateFmt('Donnees invalides dans "%s"', [source]);
end;

procedure TraiterAvecLog(const source: String);
begin
  try
    TraiterDonnees(source);
  except
    on E: Exception do
    begin
      WriteLn('  [LOG] Erreur dans TraiterAvecLog : ', E.Message);
      raise;  { Re-lever pour que l'appelant puisse aussi gerer }
    end;
  end;
end;

procedure DemoReleverApresLog;
begin
  WriteLn('=== 4. Re-lever apres logging ===');

  try
    TraiterAvecLog('fichier_donnees.csv');
  except
    on E: EBusinessException do
      WriteLn('  [MAIN] Erreur metier capturee : ', E.Message);
    on E: Exception do
      WriteLn('  [MAIN] Erreur generique : ', E.Message);
  end;

  WriteLn;
end;

{ --- Demonstration 5 : ordre specifique -> general --- }
procedure DemoOrdreCapture;
begin
  WriteLn('=== 5. Capture specifique -> general ===');

  { Test avec EConvertError }
  WriteLn('  Test A : erreur de conversion');
  try
    StrToInt('abc');
  except
    on E: EConvertError do
      WriteLn('  Capture specifique : EConvertError');
    on E: Exception do
      WriteLn('  Capture generale : Exception');
  end;

  { Test avec une exception generique }
  WriteLn('  Test B : exception generique');
  try
    raise Exception.Create('Test generique');
  except
    on E: EConvertError do
      WriteLn('  Capture specifique : EConvertError');
    on E: Exception do
      WriteLn('  Capture generale : Exception - ', E.Message);
  end;

  WriteLn;
end;

{ --- Demonstration 6 : liberation de ressources avec exception --- }
procedure DemoRessourcesCorrectement;
var
  liste: TStringList;
begin
  WriteLn('=== 6. Liberation correcte des ressources ===');

  { BONNE PRATIQUE : try-finally }
  liste := TStringList.Create;
  try
    liste.Add('Donnee importante');
    WriteLn('  Donnee ajoutee : ', liste[0]);
    raise Exception.Create('Erreur pendant le traitement');
  finally
    liste.Free;
    WriteLn('  Ressource liberee dans finally');
  end;
end;

procedure DemoRessources;
begin
  try
    DemoRessourcesCorrectement;
  except
    on E: Exception do
      WriteLn('  Exception geree au niveau superieur : ', E.Message);
  end;
  WriteLn;
end;

{ --- Demonstration 7 : enrichir le message d'erreur --- }
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

procedure DemoEnrichirMessage;
begin
  WriteLn('=== 7. Enrichir le message d''erreur ===');

  try
    TraiterLigne(42, 'abc');
  except
    on E: Exception do
      WriteLn('  ', E.Message);
  end;

  WriteLn;
end;

{ === Programme principal === }
begin
  WriteLn('--- Chapitre 13.7 : Bonnes pratiques ---');
  WriteLn;

  DemoMessagesContextuels;
  DemoValidationPrecoce;
  DemoNePasIgnorer;
  DemoReleverApresLog;
  DemoOrdreCapture;
  DemoRessources;
  DemoEnrichirMessage;

  WriteLn('--- Fin des demonstrations ---');
end.
