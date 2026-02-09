{ ============================================================================
  Section 11.10 : La classe TObject et la hiérarchie Pascal
  Description : Demonstration de ClassName avec heritage
  Fichier source : 10-classe-tobject-hierarchie-pascal.md
  ============================================================================ }
program DemoClassName;

{$mode objfpc}{$H+}

type
  TAnimal = class
  end;

  TChien = class(TAnimal)
  end;

var
  Animal: TAnimal;
  Chien: TChien;
begin
  Animal := TAnimal.Create;
  Chien := TChien.Create;

  WriteLn('Nom classe Animal : ', Animal.ClassName);  // "TAnimal"
  WriteLn('Nom classe Chien : ', Chien.ClassName);    // "TChien"

  // Libération du premier objet avant réaffectation
  Animal.Free;

  // Utilisation polymorphe
  Animal := Chien;
  WriteLn('Animal pointe vers : ', Animal.ClassName); // "TChien"

  // Libération une seule fois (Animal et Chien pointent vers le même objet)
  Chien.Free;
end.
