{
  Section 19.6 - Cross-compilation : théorie et pratique
  Description : Formulaire principal de CrossGUI
                Affiche la plateforme détectée, bouton ShowMessage
  Fichier source : 06-cross-compilation-theorie-pratique.md
}
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'Application Cross-Compilée';

  {$IFDEF WINDOWS}
  Label1.Caption := 'Exécutable Windows';
  {$ENDIF}

  {$IFDEF LINUX}
  Label1.Caption := 'Exécutable Linux';
  {$ENDIF}
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Bonjour depuis ' + {$I %FPCTARGETOS%} + ' !');
end;

end.
