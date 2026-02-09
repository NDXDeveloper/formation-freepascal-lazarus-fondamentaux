unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    LabelNom: TLabel;
    EditNom: TEdit;
    LabelEmail: TLabel;
    EditEmail: TEdit;
    LabelAge: TLabel;
    EditAge: TEdit;
    ButtonValider: TButton;
    ButtonAnnuler: TButton;
    procedure FormCreate(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure ButtonValiderClick(Sender: TObject);
    procedure ButtonAnnulerClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  { Configuration des labels }
  LabelNom.Caption := '&Nom :';
  LabelNom.FocusControl := EditNom;

  LabelEmail.Caption := '&Email :';
  LabelEmail.FocusControl := EditEmail;

  LabelAge.Caption := '&Âge :';
  LabelAge.FocusControl := EditAge;

  { Configuration des edits }
  EditNom.MaxLength := 50;
  EditEmail.MaxLength := 100;
  EditAge.MaxLength := 3;

  { Configuration des boutons }
  ButtonValider.Caption := '&Valider';
  ButtonValider.Default := True;
  ButtonValider.Enabled := False;

  ButtonAnnuler.Caption := '&Annuler';
  ButtonAnnuler.Cancel := True;

  { Focus sur le premier champ (ActiveControl car le formulaire n'est pas encore visible) }
  ActiveControl := EditNom;
end;

procedure TForm1.EditChange(Sender: TObject);
begin
  ButtonValider.Enabled := (EditNom.Text <> '') and
                           (EditEmail.Text <> '') and
                           (EditAge.Text <> '');
end;

procedure TForm1.ButtonValiderClick(Sender: TObject);
var
  Age: Integer;
begin
  if not TryStrToInt(EditAge.Text, Age) then
  begin
    ShowMessage('L''âge doit être un nombre !');
    EditAge.SetFocus;
    Exit;
  end;

  if (Age < 0) or (Age > 120) then
  begin
    ShowMessage('L''âge doit être entre 0 et 120 !');
    EditAge.SetFocus;
    Exit;
  end;

  if Pos('@', EditEmail.Text) = 0 then
  begin
    ShowMessage('L''email doit contenir un @');
    EditEmail.SetFocus;
    Exit;
  end;

  ShowMessage('Formulaire validé !' + sLineBreak +
              'Nom : ' + EditNom.Text + sLineBreak +
              'Email : ' + EditEmail.Text + sLineBreak +
              'Âge : ' + EditAge.Text);
end;

procedure TForm1.ButtonAnnulerClick(Sender: TObject);
begin
  Close;
end;

end.
