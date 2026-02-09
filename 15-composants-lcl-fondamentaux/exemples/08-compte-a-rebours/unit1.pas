{ ==========================================================================
  Formation FreePascal/Lazarus - Chapitre 15
  Section : 15.8 Timers et traitement asynchrone
  Description : Compte à rebours avec TTimer, TSpinEdit, démarrer/annuler
  Fichier source : 08-timers-traitement-asynchrone.md
  ========================================================================== }
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin;

type
  TForm1 = class(TForm)
    LabelTitre: TLabel;
    LabelTemps: TLabel;
    LabelMinutes: TLabel;
    SpinEditMinutes: TSpinEdit;
    BtnDemarrer: TButton;
    BtnAnnuler: TButton;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure BtnDemarrerClick(Sender: TObject);
    procedure BtnAnnulerClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FSecondesRestantes: Integer;
    FCompteAReboursActif: Boolean;
    procedure AfficherTemps;
    procedure TerminerCompteARebours;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Timer1.Interval := 1000;
  Timer1.Enabled := False;
  FCompteAReboursActif := False;

  SpinEditMinutes.MinValue := 1;
  SpinEditMinutes.MaxValue := 60;
  SpinEditMinutes.Value := 5;

  LabelTemps.Caption := '00:00';
  BtnAnnuler.Enabled := False;
end;

procedure TForm1.BtnDemarrerClick(Sender: TObject);
begin
  FSecondesRestantes := SpinEditMinutes.Value * 60;
  FCompteAReboursActif := True;

  AfficherTemps;

  Timer1.Enabled := True;
  BtnDemarrer.Enabled := False;
  BtnAnnuler.Enabled := True;
  SpinEditMinutes.Enabled := False;
end;

procedure TForm1.BtnAnnulerClick(Sender: TObject);
begin
  Timer1.Enabled := False;
  FCompteAReboursActif := False;

  BtnDemarrer.Enabled := True;
  BtnAnnuler.Enabled := False;
  SpinEditMinutes.Enabled := True;
  LabelTemps.Caption := '00:00';
  LabelTemps.Font.Color := clBlack;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if not FCompteAReboursActif then Exit;

  Dec(FSecondesRestantes);

  AfficherTemps;

  { Couleur rouge dans les dernières 10 secondes }
  if FSecondesRestantes <= 10 then
    LabelTemps.Font.Color := clRed
  else
    LabelTemps.Font.Color := clBlack;

  if FSecondesRestantes <= 0 then
    TerminerCompteARebours;
end;

procedure TForm1.AfficherTemps;
var
  Minutes, Secondes: Integer;
begin
  Minutes := FSecondesRestantes div 60;
  Secondes := FSecondesRestantes mod 60;
  LabelTemps.Caption := Format('%2.2d:%2.2d', [Minutes, Secondes]);
end;

procedure TForm1.TerminerCompteARebours;
begin
  Timer1.Enabled := False;
  FCompteAReboursActif := False;

  LabelTemps.Font.Color := clRed;
  LabelTemps.Caption := '00:00';

  Beep;
  ShowMessage('Temps écoulé !');

  BtnDemarrer.Enabled := True;
  BtnAnnuler.Enabled := False;
  SpinEditMinutes.Enabled := True;
  LabelTemps.Font.Color := clBlack;
end;

end.
