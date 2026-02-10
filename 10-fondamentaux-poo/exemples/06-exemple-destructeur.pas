{ ============================================================================
  Section 10.6 : Destructeurs (Destroy, Free)
  Description : Classe TFichierLog avec destructeur pour fermer le fichier
  Fichier source : 06-destructeurs-destroy-free.md
  ============================================================================ }
program ExempleDestructeur;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  TFichierLog = class
  private
    FNomFichier: string;
    FFichier: TextFile;
    FEstOuvert: Boolean;

  public
    constructor Create(const NomFichier: string);
    destructor Destroy; override;

    procedure Ecrire(const Message: string);
    procedure EcrireAvecDate(const Message: string);
  end;

// === IMPLÉMENTATION ===

constructor TFichierLog.Create(const NomFichier: string);  
begin  
  inherited Create;
  FNomFichier := NomFichier;
  FEstOuvert := False;

  try
    AssignFile(FFichier, FNomFichier);

    // Créer ou ouvrir le fichier en ajout
    if FileExists(FNomFichier) then
      Append(FFichier)
    else
      Rewrite(FFichier);

    FEstOuvert := True;
    WriteLn('Fichier log ouvert : ', FNomFichier);
  except
    on E: Exception do
      WriteLn('Erreur lors de l''ouverture du fichier : ', E.Message);
  end;
end;

destructor TFichierLog.Destroy;  
begin  
  // Fermer le fichier s'il est ouvert
  if FEstOuvert then
  begin
    try
      CloseFile(FFichier);
      WriteLn('Fichier log fermé : ', FNomFichier);
    except
      on E: Exception do
        WriteLn('Erreur lors de la fermeture : ', E.Message);
    end;
  end;

  inherited Destroy;  // Toujours en dernier
end;

procedure TFichierLog.Ecrire(const Message: string);  
begin  
  if FEstOuvert then
  begin
    try
      WriteLn(FFichier, Message);
      Flush(FFichier);  // Forcer l'écriture
    except
      on E: Exception do
        WriteLn('Erreur d''écriture : ', E.Message);
    end;
  end;
end;

procedure TFichierLog.EcrireAvecDate(const Message: string);  
var  
  Ligne: string;
begin
  Ligne := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ' + Message;
  Ecrire(Ligne);
end;

// === PROGRAMME PRINCIPAL ===

var
  Log: TFichierLog;
begin
  Log := TFichierLog.Create('application.log');
  try
    Log.EcrireAvecDate('Démarrage de l''application');
    Log.EcrireAvecDate('Opération 1 effectuée');
    Log.EcrireAvecDate('Opération 2 effectuée');
    Log.EcrireAvecDate('Arrêt de l''application');
  finally
    Log.Free;  // Le fichier sera fermé proprement
  end;

  WriteLn('Programme terminé');
end.
