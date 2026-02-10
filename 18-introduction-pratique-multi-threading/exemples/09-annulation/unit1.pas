unit Unit1;  

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

type
  { Thread avec support d'annulation }
  TThreadTraitement = class(TThread)
  private
    FProgression: Integer;
    FStatus: string;
    FLogMessage: string;
    FFichierTemp: string;
    procedure MettreAJourUI;
    procedure AjouterLog;
  protected
    procedure Execute; override;
  public
    property FichierTemp: string read FFichierTemp;
  end;

  { TForm1 }
  TForm1 = class(TForm)
    ButtonStart: TButton;
    ButtonAnnuler: TButton;
    LabelStatut: TLabel;
    LabelDetail: TLabel;
    MemoLog: TMemo;
    ProgressBar1: TProgressBar;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonAnnulerClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FThread: TThreadTraitement;
    procedure ThreadTermine(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TThreadTraitement }

procedure TThreadTraitement.MettreAJourUI;  
begin  
  Form1.ProgressBar1.Position := FProgression;
  Form1.LabelStatut.Caption := FStatus;
end;

procedure TThreadTraitement.AjouterLog;  
begin  
  Form1.MemoLog.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' ' + FLogMessage);
end;

procedure TThreadTraitement.Execute;  
var  
  i: Integer;
  NbTotal: Integer;
  FichierOuvert: Boolean;
  F: TextFile;
begin
  NbTotal := 200;
  FichierOuvert := False;

  // Simuler un fichier temporaire
  FFichierTemp := GetTempDir + 'thread_demo_temp.txt';

  try
    // Ouvrir un fichier temporaire (ressource a nettoyer)
    AssignFile(F, FFichierTemp);
    Rewrite(F);
    FichierOuvert := True;

    FLogMessage := 'Fichier temporaire cree : ' + ExtractFileName(FFichierTemp);
    Queue(@AjouterLog);

    for i := 1 to NbTotal do
    begin
      // VERIFICATION CRUCIALE
      if Terminated then
      begin
        FStatus := 'Annulation en cours...';
        FLogMessage := Format('Annule a l''iteration %d / %d', [i, NbTotal]);
        Synchronize(@MettreAJourUI);
        Synchronize(@AjouterLog);
        Exit;  // Le finally s'occupera du nettoyage
      end;

      // Simuler le travail
      Sleep(50);
      WriteLn(F, Format('Ligne %d - %s', [i, FormatDateTime('hh:nn:ss.zzz', Now)]));

      // Mise a jour de l'interface
      FProgression := (i * 100) div NbTotal;
      FStatus := Format('Traitement en cours... %d%%', [FProgression]);

      if (i mod 10 = 0) or (i = NbTotal) then
        Queue(@MettreAJourUI);

      if i mod 50 = 0 then
      begin
        FLogMessage := Format('Progression : %d / %d (%d%%)', [i, NbTotal, FProgression]);
        Queue(@AjouterLog);
      end;
    end;

    // Traitement termine normalement
    FStatus := 'Traitement termine avec succes !';
    FProgression := 100;
    FLogMessage := 'Traitement termine normalement';
    Synchronize(@MettreAJourUI);
    Synchronize(@AjouterLog);

  finally
    // NETTOYAGE : toujours execute
    if FichierOuvert then
    begin
      CloseFile(F);

      if Terminated then
      begin
        // Si annule, supprimer le fichier incomplet
        DeleteFile(FFichierTemp);
        FLogMessage := 'Fichier temporaire supprime (annulation)';
      end
      else
        FLogMessage := 'Fichier temporaire conserve (succes)';

      Queue(@AjouterLog);
    end;
  end;
end;

{ TForm1 }

procedure TForm1.ThreadTermine(Sender: TObject);  
begin  
  // Appele via OnTerminate quand le thread se termine
  ButtonStart.Enabled := True;
  ButtonAnnuler.Enabled := False;

  if FThread.Terminated then
  begin
    LabelStatut.Caption := 'Operation annulee';
    LabelStatut.Font.Color := clMaroon;
    LabelDetail.Caption := 'Le fichier temporaire a ete supprime';
  end
  else
  begin
    LabelStatut.Font.Color := clGreen;
    LabelDetail.Caption := 'Fichier : ' + FThread.FichierTemp;
  end;

  FThread := nil;  // FreeOnTerminate = True, ne pas Free
end;

procedure TForm1.ButtonStartClick(Sender: TObject);  
begin  
  // Reinitialiser
  ButtonStart.Enabled := False;
  ButtonAnnuler.Enabled := True;
  ProgressBar1.Position := 0;
  LabelStatut.Caption := 'Demarrage...';
  LabelStatut.Font.Color := clDefault;
  LabelDetail.Caption := '';
  MemoLog.Clear;

  // Creer et lancer le thread
  FThread := TThreadTraitement.Create(True);
  FThread.FreeOnTerminate := True;
  FThread.OnTerminate := @ThreadTermine;
  FThread.Start;
end;

procedure TForm1.ButtonAnnulerClick(Sender: TObject);  
begin  
  if Assigned(FThread) then
  begin
    ButtonAnnuler.Enabled := False;
    LabelStatut.Caption := 'Annulation en cours...';
    FThread.Terminate;
    // Pas de WaitFor : on utilise OnTerminate pour etre notifie
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);  
begin  
  if Assigned(FThread) then
  begin
    // Arreter le thread proprement avant de fermer
    FThread.OnTerminate := nil;  // Desactiver le callback
    FThread.Terminate;
    FThread.WaitFor;
    // FreeOnTerminate = True donc pas de Free
    FThread := nil;
  end;
  CanClose := True;
end;

end.
