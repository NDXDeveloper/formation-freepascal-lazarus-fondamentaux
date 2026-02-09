{
  Section 19.5 - Configuration de projets multi-cibles dans Lazarus
  Description : Programme principal du projet BuildModes
                Illustre les build modes, macros compilateur et defines
  Fichier source : 05-configuration-projets-multi-cibles.md
}
program BuildModes;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, Forms, Unit1;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TFormBuildModes, FormBuildModes);
  Application.Run;
end.
