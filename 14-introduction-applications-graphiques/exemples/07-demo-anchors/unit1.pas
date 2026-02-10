unit Unit1;  

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    LabelTitre: TLabel;
    EditRecherche: TEdit;
    ButtonRechercher: TButton;
    MemoResultats: TMemo;
    ButtonOK: TButton;
    ButtonAnnuler: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonRechercherClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonAnnulerClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  { Les Anchors sont définis dans le .lfm mais on peut
    aussi les configurer en code comme dans le .md :
    LabelTitre.Anchors := [akLeft, akTop];
    EditRecherche.Anchors := [akLeft, akTop, akRight];
    ButtonRechercher.Anchors := [akRight, akTop];
    MemoResultats.Anchors := [akLeft, akTop, akRight, akBottom];
    ButtonOK.Anchors := [akRight, akBottom];
    ButtonAnnuler.Anchors := [akRight, akBottom]; }
  MemoResultats.Lines.Clear;
  MemoResultats.Lines.Add('Redimensionnez la fenêtre pour voir les Anchors en action :');
  MemoResultats.Lines.Add('');
  MemoResultats.Lines.Add('  • LabelTitre : ancré Left+Top (fixe)');
  MemoResultats.Lines.Add('  • EditRecherche : ancré Left+Top+Right (s''étire horizontalement)');
  MemoResultats.Lines.Add('  • ButtonRechercher : ancré Right+Top (suit le bord droit)');
  MemoResultats.Lines.Add('  • MemoResultats : ancré aux 4 côtés (s''étire dans toutes les directions)');
  MemoResultats.Lines.Add('  • ButtonOK/Annuler : ancrés Right+Bottom (suivent le coin bas-droit)');
end;

procedure TForm1.ButtonRechercherClick(Sender: TObject);  
var  
  Terme: String;
begin
  Terme := EditRecherche.Text;
  if Terme = '' then
  begin
    ShowMessage('Saisissez un terme de recherche.');
    EditRecherche.SetFocus;
    Exit;
  end;
  MemoResultats.Lines.Clear;
  MemoResultats.Lines.Add('Recherche de "' + Terme + '"...');
  MemoResultats.Lines.Add('');
  MemoResultats.Lines.Add('Résultat 1 : Document contenant "' + Terme + '"');
  MemoResultats.Lines.Add('Résultat 2 : Fichier ' + Terme + '.pas');
  MemoResultats.Lines.Add('Résultat 3 : Note sur ' + Terme);
end;

procedure TForm1.ButtonOKClick(Sender: TObject);  
begin  
  ShowMessage('Sélection confirmée.');
end;

procedure TForm1.ButtonAnnulerClick(Sender: TObject);  
begin  
  Close;
end;

end.
