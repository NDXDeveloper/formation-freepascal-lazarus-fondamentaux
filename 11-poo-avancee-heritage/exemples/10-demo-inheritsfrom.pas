{ ============================================================================
  Section 11.10 : La classe TObject et la hiérarchie Pascal
  Description : Demonstration de InheritsFrom pour verification d heritage
  Fichier source : 10-classe-tobject-hierarchie-pascal.md
  ============================================================================ }
program DemoInheritsFrom;

{$mode objfpc}{$H+}

type
  TAnimal = class
  end;

  TMammifere = class(TAnimal)
  end;

  TChien = class(TMammifere)
  end;

  TOiseau = class(TAnimal)
  end;

var
  Chien: TChien;
begin
  Chien := TChien.Create;

  WriteLn('TChien hérite de TChien : ', Chien.InheritsFrom(TChien));      // True
  WriteLn('TChien hérite de TMammifere : ', Chien.InheritsFrom(TMammifere)); // True
  WriteLn('TChien hérite de TAnimal : ', Chien.InheritsFrom(TAnimal));    // True
  WriteLn('TChien hérite de TObject : ', Chien.InheritsFrom(TObject));    // True
  WriteLn('TChien hérite de TOiseau : ', Chien.InheritsFrom(TOiseau));    // False

  Chien.Free;
end.
