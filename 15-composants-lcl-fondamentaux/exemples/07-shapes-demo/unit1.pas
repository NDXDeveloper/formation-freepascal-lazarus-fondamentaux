{ ==========================================================================
  Formation FreePascal/Lazarus - Chapitre 15
  Section : 15.7 Composants d'affichage (TImage, TShape)
  Description : Feu tricolore avec TShape et animation pulsante via TTimer
  Fichier source : 07-composants-affichage-timage-tshape.md
  ========================================================================== }
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    PanelFeu: TPanel;
    LabelFeu: TLabel;
    ShapeRouge: TShape;
    ShapeOrange: TShape;
    ShapeVert: TShape;
    BtnRouge: TButton;
    BtnOrange: TButton;
    BtnVert: TButton;
    PanelAnimation: TPanel;
    LabelAnimation: TLabel;
    ShapePulse: TShape;
    Timer1: TTimer;
    BtnToggleAnimation: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnRougeClick(Sender: TObject);
    procedure BtnOrangeClick(Sender: TObject);
    procedure BtnVertClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure BtnToggleAnimationClick(Sender: TObject);
  private
    FPulseGrowing: Boolean;
    FPulseSize: Integer;
    procedure AfficherStatut(ARouge, AOrange, AVert: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  { Configuration feu tricolore }
  ShapeRouge.Shape := stCircle;
  ShapeOrange.Shape := stCircle;
  ShapeVert.Shape := stCircle;
  AfficherStatut(False, False, True);

  { Configuration animation pulsante }
  FPulseGrowing := True;
  FPulseSize := 20;
  ShapePulse.Shape := stCircle;
  ShapePulse.Brush.Color := clBlue;
  Timer1.Interval := 50;
  Timer1.Enabled := True;
end;

procedure TForm1.AfficherStatut(ARouge, AOrange, AVert: Boolean);  
begin  
  if ARouge then
    ShapeRouge.Brush.Color := clRed
  else
    ShapeRouge.Brush.Color := clGray;

  if AOrange then
    ShapeOrange.Brush.Color := clYellow
  else
    ShapeOrange.Brush.Color := clGray;

  if AVert then
    ShapeVert.Brush.Color := clLime
  else
    ShapeVert.Brush.Color := clGray;
end;

procedure TForm1.BtnRougeClick(Sender: TObject);  
begin  
  AfficherStatut(True, False, False);
end;

procedure TForm1.BtnOrangeClick(Sender: TObject);  
begin  
  AfficherStatut(False, True, False);
end;

procedure TForm1.BtnVertClick(Sender: TObject);  
begin  
  AfficherStatut(False, False, True);
end;

procedure TForm1.Timer1Timer(Sender: TObject);  
begin  
  if FPulseGrowing then
    Inc(FPulseSize, 2)
  else
    Dec(FPulseSize, 2);

  if FPulseSize >= 60 then
    FPulseGrowing := False
  else if FPulseSize <= 20 then
    FPulseGrowing := True;

  ShapePulse.Width := FPulseSize;
  ShapePulse.Height := FPulseSize;
  ShapePulse.Left := PanelAnimation.Width div 2 - FPulseSize div 2;
  ShapePulse.Top := 40 + (60 - FPulseSize) div 2;
end;

procedure TForm1.BtnToggleAnimationClick(Sender: TObject);  
begin  
  Timer1.Enabled := not Timer1.Enabled;
  if Timer1.Enabled then
    BtnToggleAnimation.Caption := 'Arrêter animation'
  else
    BtnToggleAnimation.Caption := 'Démarrer animation';
end;

end.
