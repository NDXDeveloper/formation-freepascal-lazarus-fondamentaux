{ ==========================================================================
  Formation FreePascal/Lazarus - Chapitre 15
  Section : 15.5 Grilles (TStringGrid, TDrawGrid)
  Description : Carnet d'adresses avec TStringGrid, ajout et suppression
  Fichier source : 05-grilles-tstringgrid-tdrawgrid.md
  ========================================================================== }
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  ExtCtrls;

type
  TForm1 = class(TForm)
    StringGrid1: TStringGrid;
    PanelSaisie: TPanel;
    LabelNom: TLabel;
    EditNom: TEdit;
    LabelPrenom: TLabel;
    EditPrenom: TEdit;
    LabelTelephone: TLabel;
    EditTelephone: TEdit;
    LabelEmail: TLabel;
    EditEmail: TEdit;
    BtnAjouter: TButton;
    BtnSupprimer: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnAjouterClick(Sender: TObject);
    procedure BtnSupprimerClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  StringGrid1.ColCount := 4;
  StringGrid1.RowCount := 2;
  StringGrid1.FixedRows := 1;
  StringGrid1.FixedCols := 0;

  StringGrid1.Cells[0, 0] := 'Nom';
  StringGrid1.Cells[1, 0] := 'Prénom';
  StringGrid1.Cells[2, 0] := 'Téléphone';
  StringGrid1.Cells[3, 0] := 'Email';

  StringGrid1.ColWidths[0] := 120;
  StringGrid1.ColWidths[1] := 120;
  StringGrid1.ColWidths[2] := 130;
  StringGrid1.ColWidths[3] := 200;

  StringGrid1.Options := StringGrid1.Options + [goRowSelect, goColSizing];
  StringGrid1.Options := StringGrid1.Options - [goEditing];
end;

procedure TForm1.BtnAjouterClick(Sender: TObject);  
var  
  Ligne: Integer;
begin
  if (EditNom.Text = '') or (EditPrenom.Text = '') then
  begin
    ShowMessage('Nom et prénom obligatoires');
    Exit;
  end;

  Ligne := StringGrid1.RowCount;
  StringGrid1.RowCount := Ligne + 1;

  StringGrid1.Cells[0, Ligne] := EditNom.Text;
  StringGrid1.Cells[1, Ligne] := EditPrenom.Text;
  StringGrid1.Cells[2, Ligne] := EditTelephone.Text;
  StringGrid1.Cells[3, Ligne] := EditEmail.Text;

  EditNom.Clear;
  EditPrenom.Clear;
  EditTelephone.Clear;
  EditEmail.Clear;
  EditNom.SetFocus;
end;

procedure TForm1.BtnSupprimerClick(Sender: TObject);  
begin  
  if StringGrid1.Row < StringGrid1.FixedRows then Exit;
  if StringGrid1.RowCount <= StringGrid1.FixedRows + 1 then Exit;

  StringGrid1.DeleteRow(StringGrid1.Row);
end;

end.
