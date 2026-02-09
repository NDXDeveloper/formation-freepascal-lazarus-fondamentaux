unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    GroupBoxShared: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    LabelResultat: TLabel;
    GroupBoxTag: TGroupBox;
    BtnRouge: TButton;
    BtnVert: TButton;
    BtnBleu: TButton;
    LabelCouleur: TLabel;
    GroupBoxDynamic: TGroupBox;
    CheckBoxActiver: TCheckBox;
    ButtonDynamic: TButton;
    LabelDynamic: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BoutonSharedClick(Sender: TObject);
    procedure BoutonCouleurClick(Sender: TObject);
    procedure CheckBoxActiverChange(Sender: TObject);
  private
    procedure HandlerActif(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  { Handler partagé : assigner le même handler à 3 boutons }
  Button1.OnClick := @BoutonSharedClick;
  Button2.OnClick := @BoutonSharedClick;
  Button3.OnClick := @BoutonSharedClick;

  { Tag pour identifier les couleurs }
  BtnRouge.Tag := Integer(clRed);
  BtnVert.Tag := Integer(clGreen);
  BtnBleu.Tag := Integer(clBlue);

  BtnRouge.OnClick := @BoutonCouleurClick;
  BtnVert.OnClick := @BoutonCouleurClick;
  BtnBleu.OnClick := @BoutonCouleurClick;
end;

procedure TForm1.BoutonSharedClick(Sender: TObject);
var
  Bouton: TButton;
begin
  { Identifier le Sender avec 'as' }
  Bouton := Sender as TButton;
  LabelResultat.Caption := 'Clic sur : ' + Bouton.Caption +
    ' (Name = ' + Bouton.Name + ')';
end;

procedure TForm1.BoutonCouleurClick(Sender: TObject);
var
  Bouton: TButton;
begin
  { Utiliser le Tag pour appliquer la couleur }
  Bouton := Sender as TButton;
  LabelCouleur.Font.Color := TColor(Bouton.Tag);
  LabelCouleur.Caption := 'Couleur appliquée par ' + Bouton.Caption;
end;

procedure TForm1.CheckBoxActiverChange(Sender: TObject);
begin
  { Assigner ou retirer un handler dynamiquement }
  if CheckBoxActiver.Checked then
  begin
    ButtonDynamic.OnClick := @HandlerActif;
    LabelDynamic.Caption := 'Handler assigné — cliquez le bouton';
  end
  else
  begin
    ButtonDynamic.OnClick := nil;
    LabelDynamic.Caption := 'Handler retiré — le bouton ne fait rien';
  end;
end;

procedure TForm1.HandlerActif(Sender: TObject);
begin
  LabelDynamic.Caption := 'Clic détecté ! (' +
    FormatDateTime('hh:nn:ss', Now) + ')';
end;

end.
