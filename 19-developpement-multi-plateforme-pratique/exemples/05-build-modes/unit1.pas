{
  Section 19.5 - Configuration de projets multi-cibles dans Lazarus
  Description : Formulaire affichant les informations de compilation :
                OS détecté, architecture, mode Debug/Release, version FPC
  Fichier source : 05-configuration-projets-multi-cibles.md
}
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TFormBuildModes = class(TForm)
    LabelTitre: TLabel;
    LabelOS: TLabel;
    LabelArch: TLabel;
    LabelMode: TLabel;
    LabelFPC: TLabel;
    LabelWidget: TLabel;
    procedure FormCreate(Sender: TObject);
  end;

var
  FormBuildModes: TFormBuildModes;

implementation

{$R *.lfm}

procedure TFormBuildModes.FormCreate(Sender: TObject);
var
  OSName, ArchName, ModeName, WidgetName: string;
begin
  { Détection du système d'exploitation via macro compilateur }
  OSName := {$I %FPCTARGETOS%};

  { Détection de l'architecture CPU }
  ArchName := {$I %FPCTARGETCPU%};

  { Détection du mode Debug/Release via defines personnalisés }
  {$IFDEF DEBUG}
  ModeName := 'Debug';
  {$ELSE}
  ModeName := 'Release';
  {$ENDIF}

  { Type de widget LCL }
  {$IFDEF LCLgtk2}
  WidgetName := 'GTK2';
  {$ELSE}
    {$IFDEF LCLgtk3}
    WidgetName := 'GTK3';
    {$ELSE}
      {$IFDEF LCLqt5}
      WidgetName := 'Qt5';
      {$ELSE}
        {$IFDEF LCLwin32}
        WidgetName := 'Win32/Win64';
        {$ELSE}
          {$IFDEF LCLcocoa}
          WidgetName := 'Cocoa (macOS)';
          {$ELSE}
          WidgetName := 'Autre / Non détecté';
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  Caption := 'Build Modes - Informations de compilation';
  LabelTitre.Caption := 'Informations de compilation';
  LabelOS.Caption := 'OS cible : ' + OSName;
  LabelArch.Caption := 'Architecture : ' + ArchName;
  LabelMode.Caption := 'Mode : ' + ModeName;
  LabelFPC.Caption := 'Version FPC : ' + {$I %FPCVERSION%};
  LabelWidget.Caption := 'Widget LCL : ' + WidgetName;
end;

end.
