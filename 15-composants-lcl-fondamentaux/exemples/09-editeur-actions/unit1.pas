{ ==========================================================================
  Formation FreePascal/Lazarus - Chapitre 15
  Section : 15.9 Actions et TActionList
  Description : Éditeur de texte simple avec TActionList, TMainMenu,
                actions Fichier/Édition et handlers OnUpdate
  Fichier source : 09-actions-tactionlist.md
  ========================================================================== }
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, Menus,
  ComCtrls, StdCtrls, Clipbrd;

type
  TForm1 = class(TForm)
    ActionList1: TActionList;
    ActNouveau: TAction;
    ActOuvrir: TAction;
    ActSauvegarder: TAction;
    ActQuitter: TAction;
    ActCouper: TAction;
    ActCopier: TAction;
    ActColler: TAction;
    ActSelectionnerTout: TAction;
    MainMenu1: TMainMenu;
    MenuFichier: TMenuItem;
    MenuNouveau: TMenuItem;
    MenuOuvrir: TMenuItem;
    MenuSauvegarder: TMenuItem;
    MenuSep1: TMenuItem;
    MenuQuitter: TMenuItem;
    MenuEdition: TMenuItem;
    MenuCouper: TMenuItem;
    MenuCopier: TMenuItem;
    MenuColler: TMenuItem;
    MenuSep2: TMenuItem;
    MenuSelectionnerTout: TMenuItem;
    Memo1: TMemo;
    StatusBar1: TStatusBar;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure ActNouveauExecute(Sender: TObject);
    procedure ActOuvrirExecute(Sender: TObject);
    procedure ActSauvegarderExecute(Sender: TObject);
    procedure ActQuitterExecute(Sender: TObject);
    procedure ActCouperExecute(Sender: TObject);
    procedure ActCopierExecute(Sender: TObject);
    procedure ActCollerExecute(Sender: TObject);
    procedure ActSelectionnerToutExecute(Sender: TObject);
    procedure ActCouperUpdate(Sender: TObject);
    procedure ActCopierUpdate(Sender: TObject);
    procedure ActCollerUpdate(Sender: TObject);
    procedure ActSauvegarderUpdate(Sender: TObject);
  private
    FFichierCourant: string;
    procedure MettreAJourTitre;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  FFichierCourant := '';
  Memo1.Clear;
  MettreAJourTitre;
end;

{ --- Actions Fichier --- }

procedure TForm1.ActNouveauExecute(Sender: TObject);  
begin  
  if Memo1.Modified then
  begin
    case MessageDlg('Sauvegarder les modifications ?',
                    mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: ActSauvegarder.Execute;
      mrCancel: Exit;
    end;
  end;

  Memo1.Clear;
  FFichierCourant := '';
  Memo1.Modified := False;
  MettreAJourTitre;
end;

procedure TForm1.ActOuvrirExecute(Sender: TObject);  
begin  
  if OpenDialog1.Execute then
  begin
    try
      Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
      FFichierCourant := OpenDialog1.FileName;
      Memo1.Modified := False;
      MettreAJourTitre;
    except
      on E: Exception do
        ShowMessage('Erreur lors de l''ouverture : ' + E.Message);
    end;
  end;
end;

procedure TForm1.ActSauvegarderExecute(Sender: TObject);  
begin  
  if FFichierCourant = '' then
  begin
    if SaveDialog1.Execute then
      FFichierCourant := SaveDialog1.FileName
    else
      Exit;
  end;

  try
    Memo1.Lines.SaveToFile(FFichierCourant);
    Memo1.Modified := False;
    StatusBar1.SimpleText := 'Fichier sauvegardé : ' + ExtractFileName(FFichierCourant);
    MettreAJourTitre;
  except
    on E: Exception do
      ShowMessage('Erreur lors de la sauvegarde : ' + E.Message);
  end;
end;

procedure TForm1.ActQuitterExecute(Sender: TObject);  
begin  
  Close;
end;

{ --- Actions Édition --- }

procedure TForm1.ActCouperExecute(Sender: TObject);  
begin  
  Memo1.CutToClipboard;
end;

procedure TForm1.ActCopierExecute(Sender: TObject);  
begin  
  Memo1.CopyToClipboard;
end;

procedure TForm1.ActCollerExecute(Sender: TObject);  
begin  
  Memo1.PasteFromClipboard;
end;

procedure TForm1.ActSelectionnerToutExecute(Sender: TObject);  
begin  
  Memo1.SelectAll;
end;

{ --- Mise à jour automatique --- }

procedure TForm1.ActCouperUpdate(Sender: TObject);  
begin  
  ActCouper.Enabled := Memo1.SelLength > 0;
end;

procedure TForm1.ActCopierUpdate(Sender: TObject);  
begin  
  ActCopier.Enabled := Memo1.SelLength > 0;
end;

procedure TForm1.ActCollerUpdate(Sender: TObject);  
begin  
  ActColler.Enabled := Clipboard.HasFormat(CF_TEXT);
end;

procedure TForm1.ActSauvegarderUpdate(Sender: TObject);  
begin  
  ActSauvegarder.Enabled := Memo1.Modified;
end;

{ --- Utilitaires --- }

procedure TForm1.MettreAJourTitre;  
begin  
  if FFichierCourant = '' then
    Caption := 'Éditeur - Sans titre'
  else
    Caption := 'Éditeur - ' + ExtractFileName(FFichierCourant);

  if Memo1.Modified then
    Caption := Caption + ' *';
end;

end.
