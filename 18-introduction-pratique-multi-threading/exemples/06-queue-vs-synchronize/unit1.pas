unit Unit1;  

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { Thread utilisant Synchronize }
  TThreadSync = class(TThread)
  private
    FCompteur: Integer;
    FMessage: string;
    procedure AfficherMessage;
    procedure ReactiverBouton;
  protected
    procedure Execute; override;
  end;

  { Thread utilisant Queue }
  TThreadQueue = class(TThread)
  private
    FCompteur: Integer;
    FMessage: string;
    procedure AfficherMessage;
    procedure ReactiverBouton;
  protected
    procedure Execute; override;
  end;

  { TForm1 }
  TForm1 = class(TForm)
    ButtonTestSync: TButton;
    ButtonTestQueue: TButton;
    LabelSync: TLabel;
    LabelQueue: TLabel;
    LabelComparaison: TLabel;
    MemoSync: TMemo;
    MemoQueue: TMemo;
    procedure ButtonTestSyncClick(Sender: TObject);
    procedure ButtonTestQueueClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TThreadSync }

procedure TThreadSync.AfficherMessage;  
begin  
  Form1.MemoSync.Lines.Add(FMessage);
end;

procedure TThreadSync.ReactiverBouton;  
begin  
  Form1.ButtonTestSync.Enabled := True;
end;

procedure TThreadSync.Execute;  
var  
  i: Integer;
  Debut: TDateTime;
begin
  Debut := Now;

  for i := 1 to 10 do
  begin
    if Terminated then Break;

    Sleep(200);  // Simuler un traitement

    FCompteur := i;
    FMessage := Format('[%d] Compteur = %d', [
      Round((Now - Debut) * 24 * 60 * 60 * 1000), i]);
    Synchronize(@AfficherMessage);  // ATTEND la mise a jour

    FMessage := Format('  -> Thread continue apres %d', [i]);
    Synchronize(@AfficherMessage);
  end;

  FMessage := '--- Termine ! ---';
  Synchronize(@AfficherMessage);

  // Reactiver le bouton
  Synchronize(@ReactiverBouton);
end;

{ TThreadQueue }

procedure TThreadQueue.AfficherMessage;  
begin  
  Form1.MemoQueue.Lines.Add(FMessage);
end;

procedure TThreadQueue.ReactiverBouton;  
begin  
  Form1.ButtonTestQueue.Enabled := True;
end;

procedure TThreadQueue.Execute;  
var  
  i: Integer;
  Debut: TDateTime;
begin
  Debut := Now;

  for i := 1 to 10 do
  begin
    if Terminated then Break;

    Sleep(200);  // Simuler un traitement

    FCompteur := i;
    FMessage := Format('[%d] Compteur = %d', [
      Round((Now - Debut) * 24 * 60 * 60 * 1000), i]);
    Queue(@AfficherMessage);  // NE ATTEND PAS

    FMessage := Format('  -> Thread continue apres %d', [i]);
    Queue(@AfficherMessage);
  end;

  FMessage := '--- Termine ! ---';
  Queue(@AfficherMessage);

  // Reactiver le bouton
  Queue(@ReactiverBouton);
end;

{ TForm1 }

procedure TForm1.ButtonTestSyncClick(Sender: TObject);  
var  
  T: TThreadSync;
begin
  MemoSync.Clear;
  MemoSync.Lines.Add('=== Test Synchronize ===');
  MemoSync.Lines.Add('Le thread ATTEND apres chaque appel');
  MemoSync.Lines.Add('');
  ButtonTestSync.Enabled := False;

  T := TThreadSync.Create(True);
  T.FreeOnTerminate := True;
  T.Start;
end;

procedure TForm1.ButtonTestQueueClick(Sender: TObject);  
var  
  T: TThreadQueue;
begin
  MemoQueue.Clear;
  MemoQueue.Lines.Add('=== Test Queue ===');
  MemoQueue.Lines.Add('Le thread CONTINUE immediatement');
  MemoQueue.Lines.Add('');
  ButtonTestQueue.Enabled := False;

  T := TThreadQueue.Create(True);
  T.FreeOnTerminate := True;
  T.Start;
end;

end.
