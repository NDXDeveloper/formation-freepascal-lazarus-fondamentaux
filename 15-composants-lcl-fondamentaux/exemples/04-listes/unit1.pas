{ ==========================================================================
  Formation FreePascal/Lazarus - Chapitre 15
  Section : 15.4 Listes (TListBox, TComboBox, TTreeView)
  Description : Démo ListBox avec ajout/suppression, ComboBox cascade,
                TreeView explorateur de projet
  Fichier source : 04-listes-tlistbox-tcombobox-ttreeview.md
  ========================================================================== }
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabListBox: TTabSheet;
    TabComboBox: TTabSheet;
    TabTreeView: TTabSheet;
    ListBox1: TListBox;
    EditElement: TEdit;
    BtnAjouter: TButton;
    BtnSupprimer: TButton;
    BtnVider: TButton;
    LabelPays: TLabel;
    ComboBoxPays: TComboBox;
    LabelVille: TLabel;
    ComboBoxVille: TComboBox;
    BtnValider: TButton;
    TreeView1: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure BtnAjouterClick(Sender: TObject);
    procedure BtnSupprimerClick(Sender: TObject);
    procedure BtnViderClick(Sender: TObject);
    procedure ComboBoxPaysChange(Sender: TObject);
    procedure BtnValiderClick(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);  
var  
  NodeProjet, NodeSrc, NodeBin, NodeDoc: TTreeNode;
begin
  { Tab 1 : ListBox }
  ListBox1.Sorted := True;
  ListBox1.Items.Add('Apple');
  ListBox1.Items.Add('Banana');
  ListBox1.Items.Add('Cherry');
  ListBox1.Items.Add('Date');
  ListBox1.Items.Add('Elderberry');

  { Tab 2 : ComboBox cascade }
  ComboBoxPays.Style := csDropDownList;
  ComboBoxVille.Style := csDropDownList;
  ComboBoxPays.Items.Add('France');
  ComboBoxPays.Items.Add('Espagne');
  ComboBoxPays.Items.Add('Italie');
  ComboBoxPays.ItemIndex := 0;
  ComboBoxPaysChange(nil);

  { Tab 3 : TreeView }
  TreeView1.Items.Clear;
  NodeProjet := TreeView1.Items.Add(nil, 'MonProjet');
  NodeSrc := TreeView1.Items.AddChild(NodeProjet, 'src');
  TreeView1.Items.AddChild(NodeSrc, 'main.pas');
  TreeView1.Items.AddChild(NodeSrc, 'unit1.pas');
  TreeView1.Items.AddChild(NodeSrc, 'unit2.pas');
  NodeBin := TreeView1.Items.AddChild(NodeProjet, 'bin');
  TreeView1.Items.AddChild(NodeBin, 'MonProjet.exe');
  TreeView1.Items.AddChild(NodeBin, 'MonProjet.dll');
  NodeDoc := TreeView1.Items.AddChild(NodeProjet, 'docs');
  TreeView1.Items.AddChild(NodeDoc, 'README.md');
  TreeView1.Items.AddChild(NodeDoc, 'CHANGELOG.md');
  NodeProjet.Expand(True);
end;

procedure TForm1.BtnAjouterClick(Sender: TObject);  
begin  
  if EditElement.Text <> '' then
  begin
    ListBox1.Items.Add(EditElement.Text);
    EditElement.Clear;
    EditElement.SetFocus;
  end;
end;

procedure TForm1.BtnSupprimerClick(Sender: TObject);  
begin  
  if ListBox1.ItemIndex >= 0 then
    ListBox1.Items.Delete(ListBox1.ItemIndex);
end;

procedure TForm1.BtnViderClick(Sender: TObject);  
begin  
  if MessageDlg('Vider la liste ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    ListBox1.Items.Clear;
end;

procedure TForm1.ComboBoxPaysChange(Sender: TObject);  
begin  
  ComboBoxVille.Items.Clear;
  case ComboBoxPays.ItemIndex of
    0: begin
      ComboBoxVille.Items.Add('Paris');
      ComboBoxVille.Items.Add('Lyon');
      ComboBoxVille.Items.Add('Marseille');
    end;
    1: begin
      ComboBoxVille.Items.Add('Madrid');
      ComboBoxVille.Items.Add('Barcelone');
      ComboBoxVille.Items.Add('Valence');
    end;
    2: begin
      ComboBoxVille.Items.Add('Rome');
      ComboBoxVille.Items.Add('Milan');
      ComboBoxVille.Items.Add('Naples');
    end;
  end;
  if ComboBoxVille.Items.Count > 0 then
    ComboBoxVille.ItemIndex := 0;
end;

procedure TForm1.BtnValiderClick(Sender: TObject);  
begin  
  ShowMessage(Format('Pays : %s, Ville : %s',
    [ComboBoxPays.Text, ComboBoxVille.Text]));
end;

procedure TForm1.TreeView1DblClick(Sender: TObject);  
var  
  Node: TTreeNode;
begin
  Node := TreeView1.Selected;
  if Assigned(Node) then
  begin
    if Node.Count > 0 then
      Node.Expanded := not Node.Expanded
    else
      ShowMessage('Ouvrir : ' + Node.Text);
  end;
end;

end.
