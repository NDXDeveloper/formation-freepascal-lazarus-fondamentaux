{ ============================================================================
  Section 20.8 : Logging structure et niveaux de log
  Description : Unite de logging simple avec niveaux (Debug, Info, Warning, Error, Fatal)
  Fichier source : 08-logging-structure-niveaux-log.md
  ============================================================================ }
unit SimpleLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError, llFatal);

  TSimpleLogger = class
  private
    FLogFile: TextFile;
    FLogFileName: String;
    FMinLevel: TLogLevel;
    FFileOpen: Boolean;
    function LevelToString(Level: TLogLevel): String;
    function GetTimestamp: String;
  public
    constructor Create(const AFileName: String; AMinLevel: TLogLevel = llInfo);
    destructor Destroy; override;

    procedure Log(Level: TLogLevel; const Msg: String); overload;
    procedure Log(Level: TLogLevel; const Msg: String; const Args: array of const); overload;

    procedure Debug(const Msg: String); overload;
    procedure Debug(const Msg: String; const Args: array of const); overload;

    procedure Info(const Msg: String); overload;
    procedure Info(const Msg: String; const Args: array of const); overload;

    procedure Warning(const Msg: String); overload;
    procedure Warning(const Msg: String; const Args: array of const); overload;

    procedure Error(const Msg: String); overload;
    procedure Error(const Msg: String; const Args: array of const); overload;

    procedure Fatal(const Msg: String); overload;
    procedure Fatal(const Msg: String; const Args: array of const); overload;

    property MinLevel: TLogLevel read FMinLevel write FMinLevel;
  end;

var
  Logger: TSimpleLogger;

implementation

constructor TSimpleLogger.Create(const AFileName: String; AMinLevel: TLogLevel);
begin
  inherited Create;
  FLogFileName := AFileName;
  FMinLevel := AMinLevel;
  FFileOpen := False;

  try
    AssignFile(FLogFile, FLogFileName);
    if FileExists(FLogFileName) then
      Append(FLogFile)
    else
      Rewrite(FLogFile);
    FFileOpen := True;
  except
    on E: Exception do
      WriteLn('Erreur ouverture fichier log: ', E.Message);
  end;
end;

destructor TSimpleLogger.Destroy;
begin
  if FFileOpen then
    CloseFile(FLogFile);
  inherited Destroy;
end;

function TSimpleLogger.LevelToString(Level: TLogLevel): String;
begin
  case Level of
    llDebug:   Result := 'DEBUG';
    llInfo:    Result := 'INFO ';
    llWarning: Result := 'WARN ';
    llError:   Result := 'ERROR';
    llFatal:   Result := 'FATAL';
  end;
end;

function TSimpleLogger.GetTimestamp: String;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);
end;

procedure TSimpleLogger.Log(Level: TLogLevel; const Msg: String);
var
  LogLine: String;
begin
  if Level < FMinLevel then Exit;

  LogLine := Format('%s [%s] %s', [GetTimestamp, LevelToString(Level), Msg]);

  if FFileOpen then
  begin
    WriteLn(FLogFile, LogLine);
    Flush(FLogFile);  // Ecriture immediate
  end;

  WriteLn(LogLine);
end;

procedure TSimpleLogger.Log(Level: TLogLevel; const Msg: String; const Args: array of const);
begin
  Log(Level, Format(Msg, Args));
end;

procedure TSimpleLogger.Debug(const Msg: String);
begin
  Log(llDebug, Msg);
end;

procedure TSimpleLogger.Debug(const Msg: String; const Args: array of const);
begin
  Log(llDebug, Msg, Args);
end;

procedure TSimpleLogger.Info(const Msg: String);
begin
  Log(llInfo, Msg);
end;

procedure TSimpleLogger.Info(const Msg: String; const Args: array of const);
begin
  Log(llInfo, Msg, Args);
end;

procedure TSimpleLogger.Warning(const Msg: String);
begin
  Log(llWarning, Msg);
end;

procedure TSimpleLogger.Warning(const Msg: String; const Args: array of const);
begin
  Log(llWarning, Msg, Args);
end;

procedure TSimpleLogger.Error(const Msg: String);
begin
  Log(llError, Msg);
end;

procedure TSimpleLogger.Error(const Msg: String; const Args: array of const);
begin
  Log(llError, Msg, Args);
end;

procedure TSimpleLogger.Fatal(const Msg: String);
begin
  Log(llFatal, Msg);
end;

procedure TSimpleLogger.Fatal(const Msg: String; const Args: array of const);
begin
  Log(llFatal, Msg, Args);
end;

initialization
  Logger := TSimpleLogger.Create('application.log', llInfo);

finalization
  Flush(Output);
  FreeAndNil(Logger);

end.
