{ ============================================================================
  Section 19.3 : Directives de compilation conditionnelle
  Description : Conditions multiples avec IF DEFINED et ELSEIF
  Fichier source : 03-directives-compilation-conditionnelle.md
  ============================================================================ }
program ConditionsAvancees;

{$mode objfpc}{$H+}

begin
  {$IF DEFINED(WINDOWS) AND DEFINED(CPU64)}
  WriteLn('Windows 64 bits');
  {$ELSEIF DEFINED(WINDOWS) AND DEFINED(CPU32)}
  WriteLn('Windows 32 bits');
  {$ELSEIF DEFINED(LINUX)}
  WriteLn('Linux');
  {$ELSE}
  WriteLn('Autre systeme');
  {$ENDIF}
end.
