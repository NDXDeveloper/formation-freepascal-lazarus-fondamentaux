{ ============================================================================
  Section 19.3 : Directives de compilation conditionnelle
  Description : Detection du systeme d'exploitation avec IFDEF
  Fichier source : 03-directives-compilation-conditionnelle.md
  ============================================================================ }
program DetectionOS;

{$mode objfpc}{$H+}

begin
  WriteLn('Systeme d''exploitation detecte :');

  {$IFDEF WINDOWS}
  WriteLn('  - Windows');
  {$ENDIF}

  {$IFDEF LINUX}
  WriteLn('  - Linux');
  {$ENDIF}

  {$IFDEF DARWIN}
  WriteLn('  - macOS');
  {$ENDIF}

  {$IFDEF UNIX}
  WriteLn('  - Systeme de type Unix');
  {$ENDIF}
end.
