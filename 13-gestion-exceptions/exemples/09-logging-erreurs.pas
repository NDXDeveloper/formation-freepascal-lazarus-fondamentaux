{ ============================================================================
  Section 13.9 : Logging des erreurs
  Description : Logging simple dans fichier avec niveaux, LogException, workflow
  Fichier source : 09-logging-erreurs.md
  ============================================================================ }
program LoggingErreurs;

{$mode objfpc}{$H+}

uses
  SysUtils;

{ === Systeme de logging simple === }
type
  TNiveauLog = (nlInfo, nlWarn, nlError, nlFatal);

var
  FichierLog: TextFile;
  LogOuvert: Boolean;

function NiveauToString(niveau: TNiveauLog): String;
begin
  case niveau of
    nlInfo:  Result := 'INFO ';
    nlWarn:  Result := 'WARN ';
    nlError: Result := 'ERROR';
    nlFatal: Result := 'FATAL';
  end;
end;

procedure InitialiserLog(const nomFichier: String);
begin
  AssignFile(FichierLog, nomFichier);
  if FileExists(nomFichier) then
    Append(FichierLog)
  else
    Rewrite(FichierLog);
  LogOuvert := True;
end;

procedure Log(niveau: TNiveauLog; const msg: String);
var
  ligne: String;
begin
  ligne := Format('[%s] [%s] %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
     NiveauToString(niveau),
     msg]);

  { Ecrire dans le fichier }
  if LogOuvert then
  begin
    WriteLn(FichierLog, ligne);
    Flush(FichierLog);
  end;

  { Aussi afficher en console }
  WriteLn(ligne);
end;

procedure LogException(const contexte: String; E: Exception);
begin
  Log(nlError, Format('%s - Exception: [%s] %s',
    [contexte, E.ClassName, E.Message]));
end;

procedure FermerLog;
begin
  if LogOuvert then
  begin
    CloseFile(FichierLog);
    LogOuvert := False;
  end;
end;

{ === Fonctions metier pour la demonstration === }

procedure TraiterFichierDonnees(const nomFichier: String);
begin
  Log(nlInfo, 'Debut traitement fichier : ' + nomFichier);

  if not FileExists(nomFichier) then
  begin
    Log(nlError, 'Fichier introuvable : ' + nomFichier);
    raise Exception.Create('Fichier introuvable : ' + nomFichier);
  end;

  Log(nlInfo, 'Fichier traite avec succes');
end;

procedure EffectuerCalcul(a, b: Integer);
begin
  Log(nlInfo, Format('Calcul demande : %d / %d', [a, b]));

  if b = 0 then
  begin
    Log(nlWarn, 'Division par zero evitee');
    WriteLn('  Resultat : division impossible (b=0)');
    Exit;
  end;

  WriteLn('  Resultat : ', a div b);
  Log(nlInfo, Format('Calcul reussi : %d / %d = %d', [a, b, a div b]));
end;

procedure ConvertirDonnee(const valeur: String);
var
  nombre: Integer;
begin
  Log(nlInfo, 'Conversion de "' + valeur + '"');
  try
    nombre := StrToInt(valeur);
    Log(nlInfo, Format('Conversion reussie : %d', [nombre]));
    WriteLn('  Converti : ', nombre);
  except
    on E: EConvertError do
    begin
      LogException('ConvertirDonnee("' + valeur + '")', E);
      WriteLn('  Conversion echouee pour "', valeur, '"');
    end;
  end;
end;

{ === Programme principal === }
var
  nomLog: String;
  contenuLog: TextFile;
  ligne: String;
begin
  nomLog := 'demo_application.log';
  LogOuvert := False;

  WriteLn('--- Chapitre 13.9 : Logging des erreurs ---');
  WriteLn;

  { Initialiser le log }
  InitialiserLog(nomLog);
  try
    Log(nlInfo, '=== SESSION DEMARREE ===');
    WriteLn;

    { --- Demo 1 : operations normales --- }
    WriteLn('=== 1. Operations normales ===');
    EffectuerCalcul(100, 7);
    ConvertirDonnee('42');
    WriteLn;

    { --- Demo 2 : avertissements --- }
    WriteLn('=== 2. Avertissements ===');
    EffectuerCalcul(50, 0);
    WriteLn;

    { --- Demo 3 : erreurs de conversion --- }
    WriteLn('=== 3. Erreurs de conversion ===');
    ConvertirDonnee('abc');
    ConvertirDonnee('12.5');
    WriteLn;

    { --- Demo 4 : erreur fichier --- }
    WriteLn('=== 4. Erreur fichier ===');
    try
      TraiterFichierDonnees('fichier_inexistant.dat');
    except
      on E: Exception do
      begin
        LogException('TraiterFichierDonnees', E);
        Log(nlFatal, 'Erreur critique, operation annulee');
        WriteLn('  Operation annulee : ', E.Message);
      end;
    end;
    WriteLn;

    Log(nlInfo, '=== SESSION TERMINEE ===');
  finally
    FermerLog;
  end;

  { --- Afficher le contenu du fichier log --- }
  WriteLn('=== Contenu du fichier log ===');
  AssignFile(contenuLog, nomLog);
  Reset(contenuLog);
  try
    while not EOF(contenuLog) do
    begin
      ReadLn(contenuLog, ligne);
      WriteLn('  ', ligne);
    end;
  finally
    CloseFile(contenuLog);
  end;

  { Nettoyage }
  DeleteFile(nomLog);

  WriteLn;
  WriteLn('--- Fin des demonstrations ---');
end.
