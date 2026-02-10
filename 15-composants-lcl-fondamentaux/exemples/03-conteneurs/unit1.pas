{ ==========================================================================
  Formation FreePascal/Lazarus - Chapitre 15
  Section : 15.3 Conteneurs (TPanel, TGroupBox, TPageControl)
  Description : Interface typique avec panneau haut/bas, onglets et groupes
  Fichier source : 03-conteneurs-tpanel-tgroupbox-tpagecontrol.md
  ========================================================================== }
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls;

type
  TForm1 = class(TForm)
    PanelTop: TPanel;
    PanelBottom: TPanel;
    PanelClient: TPanel;
    PageControl1: TPageControl;
    TabGeneral: TTabSheet;
    TabAffichage: TTabSheet;
    TabAPropos: TTabSheet;
    GroupBoxOptions: TGroupBox;
    RadioOption1: TRadioButton;
    RadioOption2: TRadioButton;
    RadioOption3: TRadioButton;
    LabelInfo: TLabel;
    LabelAffichage: TLabel;
    LabelAPropos: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure RadioOptionClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  PanelTop.Caption := 'Barre d''outils';
  PanelBottom.Caption := 'Prêt';
  RadioOption1.Checked := True;
end;

procedure TForm1.RadioOptionClick(Sender: TObject);  
begin  
  if RadioOption1.Checked then
    LabelInfo.Caption := 'Option A sélectionnée'
  else if RadioOption2.Checked then
    LabelInfo.Caption := 'Option B sélectionnée'
  else if RadioOption3.Checked then
    LabelInfo.Caption := 'Option C sélectionnée';
end;

end.
