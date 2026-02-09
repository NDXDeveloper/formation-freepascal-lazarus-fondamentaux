unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { Thread de calcul }
  TThreadCalcul = class(TThread)
  private
    FResultat: Double;
    FStatut: string;
    procedure MettreAJourUI;
    procedure AfficherResultat;
  protected
    procedure Execute; override;
  end;

  { TForm1 }
  TForm1 = class(TForm)
    ButtonBloquant: TButton;
    ButtonThread: TButton;
    LabelTitre: TLabel;
    LabelStatut: TLabel;
    LabelResultat: TLabel;
    LabelReactivite: TLabel;
    procedure ButtonBloquantClick(Sender: TObject);
    procedure ButtonThreadClick(Sender: TObject);
    procedure LabelReactiviteClick(Sender: TObject);
  private
    FClicCompteur: Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  NB_ITERATIONS = 50000000;  // 50 millions d'iterations

{ Fonction de calcul intensive }
function CalculLong: Double;
var
  i: Integer;
  Total: Double;
begin
  Total := 0;
  for i := 1 to NB_ITERATIONS do
    Total := Total + Sqrt(i) * Sin(i);
  Result := Total;
end;

{ TThreadCalcul }

procedure TThreadCalcul.MettreAJourUI;
begin
  Form1.LabelStatut.Caption := FStatut;
end;

procedure TThreadCalcul.AfficherResultat;
begin
  Form1.LabelStatut.Caption := 'Termine !';
  Form1.LabelResultat.Caption := Format('Resultat = %.4f', [FResultat]);
  Form1.ButtonBloquant.Enabled := True;
  Form1.ButtonThread.Enabled := True;
end;

procedure TThreadCalcul.Execute;
begin
  FStatut := 'Calcul en thread...';
  Synchronize(@MettreAJourUI);

  FResultat := CalculLong;

  if not Terminated then
    Synchronize(@AfficherResultat);
end;

{ TForm1 }

procedure TForm1.ButtonBloquantClick(Sender: TObject);
var
  Resultat: Double;
begin
  ButtonBloquant.Enabled := False;
  ButtonThread.Enabled := False;
  LabelStatut.Caption := 'Calcul bloquant en cours... (interface gelee !)';
  LabelResultat.Caption := '';
  Application.ProcessMessages;  // Forcer l'affichage avant le gel

  // Ce calcul BLOQUE l'interface
  Resultat := CalculLong;

  LabelStatut.Caption := 'Termine !';
  LabelResultat.Caption := Format('Resultat = %.4f', [Resultat]);
  ButtonBloquant.Enabled := True;
  ButtonThread.Enabled := True;
end;

procedure TForm1.ButtonThreadClick(Sender: TObject);
var
  T: TThreadCalcul;
begin
  ButtonBloquant.Enabled := False;
  ButtonThread.Enabled := False;
  LabelStatut.Caption := 'Calcul en thread... (interface reactive !)';
  LabelResultat.Caption := '';

  T := TThreadCalcul.Create(True);
  T.FreeOnTerminate := True;
  T.Start;
end;

procedure TForm1.LabelReactiviteClick(Sender: TObject);
begin
  Inc(FClicCompteur);
  LabelReactivite.Caption := Format('Interface reactive ! Clic #%d', [FClicCompteur]);
end;

end.
