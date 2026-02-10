unit Unit1;  

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    LabelCompteur: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  Compteur: Integer = 0;  // Variable globale pour compter les clics

implementation

{$R *.lfm}  // Charge le fichier .lfm (description visuelle du formulaire)

procedure TForm1.Button1Click(Sender: TObject);  
begin  
  Compteur := Compteur + 1;
  LabelCompteur.Caption := 'Compteur : ' + IntToStr(Compteur);

  if Compteur = 1 then
    ShowMessage('Premier clic !')
  else if Compteur = 5 then
    ShowMessage('Bravo ! Vous avez cliqué 5 fois !')
  else if Compteur = 10 then
    ShowMessage('Champion ! 10 clics !');
end;

end.
