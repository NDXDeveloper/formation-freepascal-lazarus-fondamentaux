unit Unit1;  

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  StrUtils;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    MainMenu1: TMainMenu;
    MenuFichier: TMenuItem;
    MenuNouveau: TMenuItem;
    MenuOuvrir: TMenuItem;
    MenuEnregistrer: TMenuItem;
    MenuEnregistrerSous: TMenuItem;
    MenuSepFichier: TMenuItem;
    MenuQuitter: TMenuItem;
    MenuFormat: TMenuItem;
    MenuCouleurFond: TMenuItem;
    MenuPolice: TMenuItem;
    MenuRecherche: TMenuItem;
    MenuRechercher: TMenuItem;
    MenuRemplacer: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ColorDialog1: TColorDialog;
    FontDialog1: TFontDialog;
    FindDialog1: TFindDialog;
    ReplaceDialog1: TReplaceDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MenuNouveauClick(Sender: TObject);
    procedure MenuOuvrirClick(Sender: TObject);
    procedure MenuEnregistrerClick(Sender: TObject);
    procedure MenuEnregistrerSousClick(Sender: TObject);
    procedure MenuQuitterClick(Sender: TObject);
    procedure MenuCouleurFondClick(Sender: TObject);
    procedure MenuPoliceClick(Sender: TObject);
    procedure MenuRechercherClick(Sender: TObject);
    procedure MenuRemplacerClick(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure ReplaceDialog1Replace(Sender: TObject);
  private
    FFichierCourant: String;
    procedure NouveauDocument;
    procedure OuvrirDocument(const Fichier: String);
    procedure EnregistrerDocument(const Fichier: String);
    function VerifierModifications: Boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  OpenDialog1.Filter :=
    'Fichiers texte (*.txt)|*.txt|Fichiers Pascal (*.pas)|*.pas|Tous les fichiers (*.*)|*.*';
  OpenDialog1.Options := OpenDialog1.Options + [ofFileMustExist, ofEnableSizing];

  SaveDialog1.Filter := OpenDialog1.Filter;
  SaveDialog1.Options := SaveDialog1.Options + [ofOverwritePrompt, ofEnableSizing];
  SaveDialog1.DefaultExt := 'txt';

  FontDialog1.Options := [fdEffects];

  NouveauDocument;
end;

procedure TForm1.NouveauDocument;  
begin  
  Memo1.Clear;
  Memo1.Modified := False;
  FFichierCourant := '';
  Caption := 'Éditeur - Sans titre';
end;

function TForm1.VerifierModifications: Boolean;  
begin  
  Result := True;
  if Memo1.Modified then
  begin
    case MessageDlg('Enregistrer les modifications ?',
                    mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        begin
          MenuEnregistrerClick(nil);
          Result := not Memo1.Modified;
        end;
      mrNo:
        Result := True;
      mrCancel:
        Result := False;
    end;
  end;
end;

procedure TForm1.MenuNouveauClick(Sender: TObject);  
begin  
  if VerifierModifications then
    NouveauDocument;
end;

procedure TForm1.MenuOuvrirClick(Sender: TObject);  
begin  
  if VerifierModifications then
    if OpenDialog1.Execute then
      OuvrirDocument(OpenDialog1.FileName);
end;

procedure TForm1.OuvrirDocument(const Fichier: String);  
begin  
  try
    Memo1.Lines.LoadFromFile(Fichier);
    Memo1.Modified := False;
    FFichierCourant := Fichier;
    Caption := 'Éditeur - ' + ExtractFileName(Fichier);
  except
    on E: Exception do
      MessageDlg('Impossible d''ouvrir le fichier :' + sLineBreak + E.Message,
                 mtError, [mbOK], 0);
  end;
end;

procedure TForm1.MenuEnregistrerClick(Sender: TObject);  
begin  
  if FFichierCourant = '' then
    MenuEnregistrerSousClick(Sender)
  else
    EnregistrerDocument(FFichierCourant);
end;

procedure TForm1.MenuEnregistrerSousClick(Sender: TObject);  
begin  
  if FFichierCourant <> '' then
    SaveDialog1.FileName := FFichierCourant;
  if SaveDialog1.Execute then
    EnregistrerDocument(SaveDialog1.FileName);
end;

procedure TForm1.EnregistrerDocument(const Fichier: String);  
begin  
  try
    Memo1.Lines.SaveToFile(Fichier);
    Memo1.Modified := False;
    FFichierCourant := Fichier;
    Caption := 'Éditeur - ' + ExtractFileName(Fichier);
  except
    on E: Exception do
      MessageDlg('Impossible d''enregistrer :' + sLineBreak + E.Message,
                 mtError, [mbOK], 0);
  end;
end;

procedure TForm1.MenuQuitterClick(Sender: TObject);  
begin  
  Close;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);  
begin  
  CanClose := VerifierModifications;
end;

procedure TForm1.MenuCouleurFondClick(Sender: TObject);  
begin  
  ColorDialog1.Color := Memo1.Color;
  if ColorDialog1.Execute then
    Memo1.Color := ColorDialog1.Color;
end;

procedure TForm1.MenuPoliceClick(Sender: TObject);  
begin  
  FontDialog1.Font := Memo1.Font;
  if FontDialog1.Execute then
    Memo1.Font := FontDialog1.Font;
end;

procedure TForm1.MenuRechercherClick(Sender: TObject);  
begin  
  FindDialog1.Execute;
end;

procedure TForm1.MenuRemplacerClick(Sender: TObject);  
begin  
  ReplaceDialog1.Execute;
end;

procedure TForm1.FindDialog1Find(Sender: TObject);  
var  
  FoundPos, StartPos: Integer;
  SearchText, MemoText: String;
begin
  SearchText := FindDialog1.FindText;
  MemoText := Memo1.Text;

  if not (frMatchCase in FindDialog1.Options) then
  begin
    SearchText := LowerCase(SearchText);
    MemoText := LowerCase(MemoText);
  end;

  StartPos := Memo1.SelStart + Memo1.SelLength + 1;
  FoundPos := PosEx(SearchText, MemoText, StartPos);

  { Si non trouvé, recommencer depuis le début }
  if (FoundPos = 0) and (StartPos > 1) then
    FoundPos := PosEx(SearchText, MemoText, 1);

  if FoundPos > 0 then
  begin
    Memo1.SelStart := FoundPos - 1;
    Memo1.SelLength := Length(FindDialog1.FindText);
    Memo1.SetFocus;
  end
  else
    MessageDlg('Texte introuvable : "' + FindDialog1.FindText + '"',
               mtInformation, [mbOK], 0);
end;

procedure TForm1.ReplaceDialog1Replace(Sender: TObject);  
var  
  FoundPos, StartPos: Integer;
  SearchText, MemoText: String;
begin
  SearchText := ReplaceDialog1.FindText;
  MemoText := Memo1.Text;

  if not (frMatchCase in ReplaceDialog1.Options) then
  begin
    SearchText := LowerCase(SearchText);
    MemoText := LowerCase(MemoText);
  end;

  if frReplaceAll in ReplaceDialog1.Options then
  begin
    { Remplacer tout }
    if frMatchCase in ReplaceDialog1.Options then
      Memo1.Text := StringReplace(Memo1.Text, ReplaceDialog1.FindText,
        ReplaceDialog1.ReplaceText, [rfReplaceAll])
    else
      Memo1.Text := StringReplace(Memo1.Text, ReplaceDialog1.FindText,
        ReplaceDialog1.ReplaceText, [rfReplaceAll, rfIgnoreCase]);
  end
  else
  begin
    { Remplacer l'occurrence courante puis chercher la suivante }
    if Memo1.SelLength > 0 then
      Memo1.SelText := ReplaceDialog1.ReplaceText;

    StartPos := Memo1.SelStart + Memo1.SelLength + 1;
    FoundPos := PosEx(SearchText, MemoText, StartPos);
    if FoundPos > 0 then
    begin
      Memo1.SelStart := FoundPos - 1;
      Memo1.SelLength := Length(ReplaceDialog1.FindText);
      Memo1.SetFocus;
    end
    else
      MessageDlg('Plus aucune occurrence trouvée.',
                 mtInformation, [mbOK], 0);
  end;
end;

end.
