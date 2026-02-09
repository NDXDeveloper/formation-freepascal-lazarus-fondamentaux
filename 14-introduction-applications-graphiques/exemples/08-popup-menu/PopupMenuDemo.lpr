{
  Section 14.8 - Menus et barres d'outils
  Description : TMemo avec menu contextuel (PopupMenu) et menu principal
                Opérations presse-papiers : Couper, Copier, Coller, Supprimer
  Fichier source : 08-menus-barres-outils.md
}
program PopupMenuDemo;

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
