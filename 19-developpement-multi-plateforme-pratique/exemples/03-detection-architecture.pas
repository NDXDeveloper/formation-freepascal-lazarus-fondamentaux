{ ============================================================================
  Section 19.3 : Directives de compilation conditionnelle
  Description : Detection de l'architecture CPU avec IFDEF
  Fichier source : 03-directives-compilation-conditionnelle.md
  ============================================================================ }
program DetectionArchitecture;

{$mode objfpc}{$H+}

begin
  {$IFDEF CPU64}
  WriteLn('Version 64 bits');
  {$ELSE}
  WriteLn('Version 32 bits');
  {$ENDIF}

  {$IFDEF CPUARM}
  WriteLn('Processeur ARM detecte (ex: Raspberry Pi)');
  {$ENDIF}
end.
