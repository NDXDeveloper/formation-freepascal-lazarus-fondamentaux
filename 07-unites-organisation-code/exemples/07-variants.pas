{ ============================================================================
  Section 7.7 : Unites standard du RTL
  Description : Demonstration du type Variant
  Fichier source : 07-unites-standard-rtl.md
  ============================================================================ }
{$mode objfpc}{$H+}
program VariantsDemo;

uses
  Variants;

var
  v: Variant;

begin
  v := 10;              // Entier
  WriteLn(v);

  v := 'Hello';         // Chaine
  WriteLn(v);

  v := 3.14;            // Reel
  WriteLn(v);

  v := True;            // Booleen
  WriteLn(v);
end.
