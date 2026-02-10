{ ============================================================================
  Section 11.8 : Inherited et appel au parent
  Description : Systeme de logging hierarchique avec inherited
  Fichier source : 08-inherited-appel-parent.md
  ============================================================================ }
program SystemeLogging;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Classe de base : Logger simple }
  TLogger = class
  protected
    FNomFichier: string;
  public
    constructor Create(ANomFichier: string);
    procedure Log(const Message: string); virtual;
    destructor Destroy; override;
  end;

  { Logger avec horodatage }
  TLoggerAvecDate = class(TLogger)
  public
    procedure Log(const Message: string); override;
  end;

  { Logger avec niveau de priorité }
  TLoggerAvecNiveau = class(TLoggerAvecDate)
  private
    FNiveau: string;
  public
    constructor Create(ANomFichier: string; ANiveau: string);
    procedure Log(const Message: string); override;
  end;

  { Logger complet avec contexte }
  TLoggerComplet = class(TLoggerAvecNiveau)
  private
    FContexte: string;
  public
    constructor Create(ANomFichier, ANiveau, AContexte: string);
    procedure Log(const Message: string); override;
  end;

{ === TLogger === }

constructor TLogger.Create(ANomFichier: string);  
begin  
  inherited Create;
  FNomFichier := ANomFichier;
  WriteLn('[TLogger] Fichier log : ', FNomFichier);
end;

procedure TLogger.Log(const Message: string);  
begin  
  WriteLn(Message);
end;

destructor TLogger.Destroy;  
begin  
  WriteLn('[TLogger] Fermeture du fichier log');
  inherited Destroy;
end;

{ === TLoggerAvecDate === }

procedure TLoggerAvecDate.Log(const Message: string);  
var  
  MessageAvecDate: string;
begin
  // Ajoute la date/heure
  MessageAvecDate := '[' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + '] ' + Message;

  // Appelle le logger parent
  inherited Log(MessageAvecDate);
end;

{ === TLoggerAvecNiveau === }

constructor TLoggerAvecNiveau.Create(ANomFichier: string; ANiveau: string);  
begin  
  inherited Create(ANomFichier);
  FNiveau := ANiveau;
  WriteLn('[TLoggerAvecNiveau] Niveau : ', FNiveau);
end;

procedure TLoggerAvecNiveau.Log(const Message: string);  
var  
  MessageAvecNiveau: string;
begin
  // Ajoute le niveau
  MessageAvecNiveau := '[' + FNiveau + '] ' + Message;

  // Appelle le logger parent (qui ajoutera la date)
  inherited Log(MessageAvecNiveau);
end;

{ === TLoggerComplet === }

constructor TLoggerComplet.Create(ANomFichier, ANiveau, AContexte: string);  
begin  
  inherited Create(ANomFichier, ANiveau);
  FContexte := AContexte;
  WriteLn('[TLoggerComplet] Contexte : ', FContexte);
end;

procedure TLoggerComplet.Log(const Message: string);  
var  
  MessageComplet: string;
begin
  // Ajoute le contexte
  MessageComplet := '[' + FContexte + '] ' + Message;

  // Appelle le logger parent (qui ajoutera niveau et date)
  inherited Log(MessageComplet);
end;

{ === Programme principal === }
var
  Logger1: TLogger;
  Logger2: TLoggerAvecDate;
  Logger3: TLoggerAvecNiveau;
  Logger4: TLoggerComplet;
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('   DEMONSTRATION DU LOGGING HIERARCHIQUE');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  WriteLn('--- LOGGER SIMPLE ---');
  Logger1 := TLogger.Create('app.log');
  Logger1.Log('Démarrage de l''application');
  WriteLn;

  WriteLn('--- LOGGER AVEC DATE ---');
  Logger2 := TLoggerAvecDate.Create('app.log');
  Logger2.Log('Connexion utilisateur');
  WriteLn;

  WriteLn('--- LOGGER AVEC NIVEAU ---');
  Logger3 := TLoggerAvecNiveau.Create('app.log', 'INFO');
  Logger3.Log('Opération réussie');
  WriteLn;

  WriteLn('--- LOGGER COMPLET ---');
  Logger4 := TLoggerComplet.Create('app.log', 'ERROR', 'Module-DB');
  Logger4.Log('Échec de connexion à la base de données');
  WriteLn;

  WriteLn('--- LIBERATION ---');
  Logger1.Free;
  Logger2.Free;
  Logger3.Free;
  Logger4.Free;

  WriteLn;
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
