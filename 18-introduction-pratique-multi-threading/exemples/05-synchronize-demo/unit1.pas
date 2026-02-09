unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

type
  { Thread de traitement avec Synchronize }
  TThreadTraitement = class(TThread)
  private
    FProgression: Integer;
    FStatus: string;
    FLogMessage: string;
    procedure MettreAJourUI;
    procedure AjouterLog;
    procedure TraitementTermine;
  protected
    procedure Execute; override;
  end;

  { TForm1 }
  TForm1 = class(TForm)
    ButtonStart: TButton;
    LabelStatut: TLabel;
    MemoLog: TMemo;
    ProgressBar1: TProgressBar;
    procedure ButtonStartClick(Sender: TObject);
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
  Form1.MemoLog.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + FLogMessage);
end;

procedure TThreadTraitement.TraitementTermine;
begin
  Form1.ProgressBar1.Position := 100;
  Form1.LabelStatut.Caption := 'Traitement termine !';
  Form1.ButtonStart.Enabled := True;
  Form1.MemoLog.Lines.Add('');
  Form1.MemoLog.Lines.Add('=== Traitement termine avec succes ===');
end;

procedure TThreadTraitement.Execute;
var
  i: Integer;
begin
  for i := 1 to 100 do
  begin
    if Terminated then
      Break;

    // Simuler un traitement
    Sleep(50);

    // Preparer les donnees pour l'UI
    FProgression := i;
    FStatus := Format('Traitement en cours... %d%%', [i]);

    // Synchronize : le thread s'arrete et attend
    Synchronize(@MettreAJourUI);

    // Ajouter un log tous les 10%
    if i mod 10 = 0 then
    begin
      FLogMessage := Format('Etape %d/100 terminee', [i]);
      Synchronize(@AjouterLog);
    end;
  end;

  if not Terminated then
    Synchronize(@TraitementTermine);
end;

{ TForm1 }

procedure TForm1.ButtonStartClick(Sender: TObject);
var
  Thread: TThreadTraitement;
begin
  // Reinitialiser l'interface
  ProgressBar1.Position := 0;
  ButtonStart.Enabled := False;
  LabelStatut.Caption := 'Demarrage...';
  MemoLog.Clear;
  MemoLog.Lines.Add('=== Debut du traitement ===');
  MemoLog.Lines.Add('');

  // Creer et lancer le thread
  Thread := TThreadTraitement.Create(True);
  Thread.FreeOnTerminate := True;
  Thread.Start;
end;

end.
