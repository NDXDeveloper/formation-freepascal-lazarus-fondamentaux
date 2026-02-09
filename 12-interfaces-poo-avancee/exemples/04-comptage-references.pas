{ ============================================================================
  Section 12.4 : IInterface / IUnknown
  Description : Demonstration du comptage de references avec RefCount
  Fichier source : 04-iinterface-iunknown.md
  ============================================================================ }
program ComptageReferences;

{$mode objfpc}{$H+}

type
  ITest = interface
    ['{12345678-1234-1234-1234-123456789012}']
    procedure Afficher;
  end;

  TTest = class(TInterfacedObject, ITest)
  private
    FNom: string;
  public
    constructor Create(const Nom: string);
    destructor Destroy; override;
    procedure Afficher;
  end;

constructor TTest.Create(const Nom: string);
begin
  inherited Create;
  FNom := Nom;
  WriteLn('✅ Objet "', FNom, '" créé - Compteur: ', RefCount);
end;

destructor TTest.Destroy;
begin
  WriteLn('❌ Objet "', FNom, '" détruit');
  inherited;
end;

procedure TTest.Afficher;
begin
  WriteLn('👋 Je suis "', FNom, '" - Compteur: ', RefCount);
end;

var
  Ref1, Ref2, Ref3: ITest;
begin
  WriteLn('=== Démonstration du comptage de références ===');
  WriteLn('');

  WriteLn('1. Création de l''objet et assignation à Ref1');
  Ref1 := TTest.Create('MonObjet');  // Compteur = 1
  Ref1.Afficher;
  WriteLn('');

  WriteLn('2. Assignation à Ref2 (même objet)');
  Ref2 := Ref1;                       // Compteur = 2
  Ref2.Afficher;
  WriteLn('');

  WriteLn('3. Assignation à Ref3 (toujours le même objet)');
  Ref3 := Ref1;                       // Compteur = 3
  Ref3.Afficher;
  WriteLn('');

  WriteLn('4. Libération de Ref1');
  Ref1 := nil;                        // Compteur = 2
  WriteLn('   Objet toujours vivant car Ref2 et Ref3 existent');
  WriteLn('');

  WriteLn('5. Libération de Ref2');
  Ref2 := nil;                        // Compteur = 1
  WriteLn('   Objet toujours vivant car Ref3 existe');
  WriteLn('');

  WriteLn('6. Libération de Ref3');
  Ref3 := nil;                        // Compteur = 0 → DESTRUCTION !
  WriteLn('');

  WriteLn('Fin du programme');
end.
