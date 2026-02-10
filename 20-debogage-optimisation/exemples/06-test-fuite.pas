{ ============================================================================
  Section 20.6 : Gestion efficace de la memoire
  Description : Detection de fuite memoire avec HeapTrc
  Fichier source : 06-gestion-efficace-memoire.md
  ============================================================================ }
program TestFuite;

{$mode objfpc}{$H+}

{$DEFINE HEAPTRC}

uses
  {$IFDEF HEAPTRC}
  heaptrc,
  {$ENDIF}
  Classes;

procedure CreerFuite;  
var  
  liste: TStringList;
begin
  liste := TStringList.Create;
  liste.Add('Element');
  // Pas de Free ! (fuite volontaire pour la demonstration)
end;

begin
  {$IFDEF HEAPTRC}
  SetHeapTraceOutput('heaptrc.log');
  {$ENDIF}

  CreerFuite;

  WriteLn('Programme termine');
  WriteLn('Verifiez heaptrc.log pour le rapport de fuites memoire');
end.
