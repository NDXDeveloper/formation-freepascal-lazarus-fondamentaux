unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}  // Charge le fichier .lfm (description visuelle du formulaire)

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Bonjour ! Vous avez cliqué sur le bouton !');
end;

end.
