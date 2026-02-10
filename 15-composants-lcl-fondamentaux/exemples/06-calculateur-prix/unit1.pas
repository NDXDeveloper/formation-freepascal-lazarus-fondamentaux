{ ==========================================================================
  Formation FreePascal/Lazarus - Chapitre 15
  Section : 15.6 Composants de saisie avancés
  Description : Calculateur de prix avec TSpinEdit, TFloatSpinEdit,
                TTrackBar, TDateEdit et TTimeEdit
  Fichier source : 06-composants-saisie-avances.md
  ========================================================================== }
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ComCtrls, EditBtn, ExtCtrls;

type
  TForm1 = class(TForm)
    LabelQuantite: TLabel;
    SpinEditQuantite: TSpinEdit;
    LabelPrixUnitaire: TLabel;
    FloatSpinEditPrix: TFloatSpinEdit;
    LabelRemise: TLabel;
    TrackBarRemise: TTrackBar;
    LabelRemiseValeur: TLabel;
    LabelLivraison: TLabel;
    DateEditLivraison: TDateEdit;
    LabelHeure: TLabel;
    TimeEditLivraison: TTimeEdit;
    BtnCalculer: TButton;
    LabelPrixTotal: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TrackBarRemiseChange(Sender: TObject);
    procedure BtnCalculerClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  { Quantité }
  SpinEditQuantite.MinValue := 1;
  SpinEditQuantite.MaxValue := 999;
  SpinEditQuantite.Value := 1;

  { Prix unitaire }
  FloatSpinEditPrix.MinValue := 0.01;
  FloatSpinEditPrix.MaxValue := 9999.99;
  FloatSpinEditPrix.DecimalPlaces := 2;
  FloatSpinEditPrix.Value := 10.00;

  { Remise (0-50%) }
  TrackBarRemise.Min := 0;
  TrackBarRemise.Max := 50;
  TrackBarRemise.Position := 0;
  TrackBarRemise.Frequency := 5;

  { Livraison demain par défaut }
  DateEditLivraison.Date := Date + 1;
  TimeEditLivraison.Time := EncodeTime(10, 0, 0, 0);

  LabelRemiseValeur.Caption := 'Remise : 0%';
  LabelPrixTotal.Caption := '';
end;

procedure TForm1.TrackBarRemiseChange(Sender: TObject);  
begin  
  LabelRemiseValeur.Caption := 'Remise : ' + IntToStr(TrackBarRemise.Position) + '%';
end;

procedure TForm1.BtnCalculerClick(Sender: TObject);  
var  
  PrixBase, Remise, PrixFinal: Double;
  DateLivraison: string;
begin
  PrixBase := SpinEditQuantite.Value * FloatSpinEditPrix.Value;
  Remise := PrixBase * (TrackBarRemise.Position / 100);
  PrixFinal := PrixBase - Remise;

  DateLivraison := DateToStr(DateEditLivraison.Date) + ' à ' +
                   TimeToStr(TimeEditLivraison.Time);

  LabelPrixTotal.Caption := Format(
    'Prix de base : %.2f €' + LineEnding +
    'Remise (-%d%%) : -%.2f €' + LineEnding +
    'Prix final : %.2f €' + LineEnding +
    'Livraison prévue : %s',
    [PrixBase, TrackBarRemise.Position, Remise, PrixFinal, DateLivraison]
  );
end;

end.
