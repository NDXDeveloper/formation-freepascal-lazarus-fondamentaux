{
  Section 14.9 - Boîtes de dialogue standard
  Description : Éditeur de texte complet avec toutes les boîtes de dialogue
                TOpenDialog, TSaveDialog, TColorDialog, TFontDialog,
                TFindDialog, TReplaceDialog, MessageDlg
  Fichier source : 09-boites-dialogue-standard.md
}
program EditeurDialogues;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  Forms, Unit1;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
