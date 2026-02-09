{ ============================================================================
  Section 19.6 : Cross-compilation : theorie et pratique
  Description : Application console simple avec detection plateforme/CPU
  Fichier source : 06-cross-compilation-theorie-pratique.md
  ============================================================================ }
program HelloCross;

{$mode objfpc}{$H+}

uses
  SysUtils;

begin
  WriteLn('Hello from cross-compilation!');
  WriteLn('Compiled for: ', {$I %FPCTARGETOS%});  // {$I %VAR%} : macro compilateur, insere une info comme chaine
  WriteLn('CPU: ', {$I %FPCTARGETCPU%});

  {$IFDEF WINDOWS}
  WriteLn('This is a Windows executable');
  {$ENDIF}

  {$IFDEF LINUX}
  WriteLn('This is a Linux executable');
  {$ENDIF}
end.
