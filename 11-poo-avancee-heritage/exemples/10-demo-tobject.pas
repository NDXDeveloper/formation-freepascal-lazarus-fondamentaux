{ ============================================================================
  Section 11.10 : La classe TObject et la hiérarchie Pascal
  Description : Exemple complet utilisant les methodes de TObject
  Fichier source : 10-classe-tobject-hierarchie-pascal.md
  ============================================================================ }
program DemoTObject;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Classe de base }
  TAnimal = class
  private
    FNom: string;
  public
    constructor Create(ANom: string);
    destructor Destroy; override;
    procedure Afficher; virtual;
    property Nom: string read FNom;
  end;

  { Classe dérivée }
  TChien = class(TAnimal)
  private
    FRace: string;
  public
    constructor Create(ANom, ARace: string);
    destructor Destroy; override;
    procedure Afficher; override;
  end;

{ === TAnimal === }

constructor TAnimal.Create(ANom: string);
begin
  inherited Create;  // Appelle TObject.Create
  FNom := ANom;
  WriteLn('[', ClassName, '.Create] Création de ', FNom);
end;

destructor TAnimal.Destroy;
begin
  WriteLn('[', ClassName, '.Destroy] Destruction de ', FNom);
  inherited Destroy;  // Appelle TObject.Destroy
end;

procedure TAnimal.Afficher;
begin
  WriteLn('Animal : ', FNom);
  WriteLn('Classe : ', ClassName);
  WriteLn('Taille : ', InstanceSize, ' octets');
end;

{ === TChien === }

constructor TChien.Create(ANom, ARace: string);
begin
  inherited Create(ANom);
  FRace := ARace;
  WriteLn('[', ClassName, '.Create] Race : ', FRace);
end;

destructor TChien.Destroy;
begin
  WriteLn('[', ClassName, '.Destroy] Nettoyage spécifique');
  inherited Destroy;
end;

procedure TChien.Afficher;
begin
  inherited Afficher;
  WriteLn('Race : ', FRace);
end;

{ === Fonctions de démonstration === }

procedure AfficherInfosClasse(Obj: TObject);
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('Informations sur l''objet');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('Nom de classe : ', Obj.ClassName);
  WriteLn('Taille instance : ', Obj.InstanceSize, ' octets');
  WriteLn('Type de classe : ', Obj.ClassType.ClassName);
  WriteLn;
end;

procedure TesterHeritage;
var
  Animal: TAnimal;
  Chien: TChien;
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('TEST D''HERITAGE');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  Animal := TAnimal.Create('Générique');
  Chien := TChien.Create('Rex', 'Berger Allemand');

  WriteLn('--- Tests InheritsFrom ---');
  WriteLn('TAnimal hérite de TObject : ', TAnimal.InheritsFrom(TObject));
  WriteLn('TChien hérite de TAnimal : ', TChien.InheritsFrom(TAnimal));
  WriteLn('TChien hérite de TObject : ', TChien.InheritsFrom(TObject));
  WriteLn('TAnimal hérite de TChien : ', TAnimal.InheritsFrom(TChien));
  WriteLn;

  WriteLn('--- Tests avec is ---');
  WriteLn('Animal is TObject : ', Animal is TObject);
  WriteLn('Animal is TAnimal : ', Animal is TAnimal);
  WriteLn('Animal is TChien : ', Animal is TChien);
  WriteLn;
  WriteLn('Chien is TObject : ', Chien is TObject);
  WriteLn('Chien is TAnimal : ', Chien is TAnimal);
  WriteLn('Chien is TChien : ', Chien is TChien);
  WriteLn;

  Animal.Free;
  Chien.Free;
end;

procedure TesterPolymorphisme;
var
  Animal: TAnimal;
  Chien: TChien;
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('TEST DE POLYMORPHISME');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  Chien := TChien.Create('Max', 'Labrador');
  Animal := Chien;  // Polymorphisme

  WriteLn('Variable Animal pointe vers un Chien');
  WriteLn('Animal.ClassName = ', Animal.ClassName);  // "TChien"
  WriteLn;

  AfficherInfosClasse(Animal);

  Animal.Afficher;  // Appelle TChien.Afficher (polymorphisme)

  Chien.Free;
end;

procedure DemoFreeAndNil;
var
  Obj1, Obj2: TAnimal;
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('DEMO FREE vs FREEANDNIL');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  // Test avec Free
  WriteLn('--- Test avec Free ---');
  Obj1 := TAnimal.Create('Test1');
  WriteLn('Avant Free : Obj1 = ', IntToHex(PtrUInt(Obj1), 8));
  Obj1.Free;
  WriteLn('Après Free : Obj1 = ', IntToHex(PtrUInt(Obj1), 8));
  WriteLn('⚠️  Obj1 pointe toujours vers une adresse !');
  WriteLn;

  // Test avec FreeAndNil
  WriteLn('--- Test avec FreeAndNil ---');
  Obj2 := TAnimal.Create('Test2');
  WriteLn('Avant FreeAndNil : Obj2 = ', IntToHex(PtrUInt(Obj2), 8));
  FreeAndNil(Obj2);
  WriteLn('Après FreeAndNil : Obj2 = ', IntToHex(PtrUInt(Obj2), 8));
  WriteLn('✓ Obj2 vaut maintenant nil');
  WriteLn;
end;

{ === Programme principal === }
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('   DEMONSTRATION DE TOBJECT');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  TesterHeritage;
  TesterPolymorphisme;
  DemoFreeAndNil;

  WriteLn('═══════════════════════════════════════════════');
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
