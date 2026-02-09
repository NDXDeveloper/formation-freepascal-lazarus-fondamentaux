unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  LCLType;

type
  TForm1 = class(TForm)
    LabelInfo: TLabel;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    procedure MettreAJourStatusBar;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'Demo Événements TForm';
  LabelInfo.Caption :=
    'Événements TForm démontrés :' + LineEnding +
    LineEnding +
    '• OnCreate : initialise le formulaire (exécuté maintenant)' + LineEnding +
    '• OnResize : redimensionnez la fenêtre pour voir la StatusBar se mettre à jour' + LineEnding +
    '• OnKeyDown : appuyez sur Ctrl+Q pour quitter, Escape pour minimiser' + LineEnding +
    '• OnCloseQuery : confirmation avant fermeture' + LineEnding +
    LineEnding +
    'Propriétés configurées :' + LineEnding +
    '  Position = poScreenCenter' + LineEnding +
    '  KeyPreview = True (capture clavier sur le formulaire)' + LineEnding +
    '  Color = clWindow';
  MettreAJourStatusBar;
end;

procedure TForm1.MettreAJourStatusBar;
begin
  StatusBar1.Panels[0].Text := Format('Taille : %d x %d', [Width, Height]);
  StatusBar1.Panels[1].Text := Format('Client : %d x %d', [ClientWidth, ClientHeight]);
  StatusBar1.Panels[2].Text := Format('Position : %d, %d', [Left, Top]);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  MettreAJourStatusBar;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_Q) and (ssCtrl in Shift) then
    Close
  else if Key = VK_ESCAPE then
    WindowState := wsMinimized;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := MessageDlg('Confirmation',
    'Voulez-vous vraiment quitter ?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

end.
