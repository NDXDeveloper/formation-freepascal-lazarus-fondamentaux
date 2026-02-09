unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  Clipbrd;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    MainMenu1: TMainMenu;
    MenuFichier: TMenuItem;
    MenuNouveau: TMenuItem;
    MenuQuitter: TMenuItem;
    MenuEdition: TMenuItem;
    MenuCouper: TMenuItem;
    MenuCopier: TMenuItem;
    MenuColler: TMenuItem;
    MenuSepEdition: TMenuItem;
    MenuSelectTout: TMenuItem;
    PopupMenu1: TPopupMenu;
    PopCouper: TMenuItem;
    PopCopier: TMenuItem;
    PopColler: TMenuItem;
    PopSeparateur: TMenuItem;
    PopSupprimer: TMenuItem;
    PopSelectTout: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure MenuNouveauClick(Sender: TObject);
    procedure MenuQuitterClick(Sender: TObject);
    procedure CouperClick(Sender: TObject);
    procedure CopierClick(Sender: TObject);
    procedure CollerClick(Sender: TObject);
    procedure SupprimerClick(Sender: TObject);
    procedure SelectToutClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Bienvenue dans la démo Menus !');
  Memo1.Lines.Add('');
  Memo1.Lines.Add('Testez le menu principal (Fichier, Édition)');
  Memo1.Lines.Add('et le menu contextuel (clic droit).');
  Memo1.Lines.Add('');
  Memo1.Lines.Add('Les raccourcis clavier fonctionnent aussi :');
  Memo1.Lines.Add('  Ctrl+X : Couper');
  Memo1.Lines.Add('  Ctrl+C : Copier');
  Memo1.Lines.Add('  Ctrl+V : Coller');
  Memo1.Lines.Add('  Ctrl+A : Tout sélectionner');
end;

procedure TForm1.MenuNouveauClick(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TForm1.MenuQuitterClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.CouperClick(Sender: TObject);
begin
  Memo1.CutToClipboard;
end;

procedure TForm1.CopierClick(Sender: TObject);
begin
  Memo1.CopyToClipboard;
end;

procedure TForm1.CollerClick(Sender: TObject);
begin
  Memo1.PasteFromClipboard;
end;

procedure TForm1.SupprimerClick(Sender: TObject);
begin
  Memo1.SelText := '';
end;

procedure TForm1.SelectToutClick(Sender: TObject);
begin
  Memo1.SelectAll;
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
var
  HasSelection: Boolean;
begin
  HasSelection := Memo1.SelLength > 0;
  PopCouper.Enabled := HasSelection;
  PopCopier.Enabled := HasSelection;
  PopColler.Enabled := Clipboard.HasFormat(CF_TEXT);
  PopSupprimer.Enabled := HasSelection;
end;

end.
