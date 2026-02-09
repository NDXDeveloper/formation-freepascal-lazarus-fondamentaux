{ ============================================================================
  Section 12.5 : Comptage de références
  Description : Cycle de vie complet d'un objet interface avec compteur
  Fichier source : 05-comptage-references.md
  ============================================================================ }
program CycleDeVie;

{$mode objfpc}{$H+}

type
  IMessage = interface
    ['{12345678-9ABC-DEF0-1234-567890ABCDEF}']
    procedure Dire(const Texte: string);
  end;

  TMessagerie = class(TInterfacedObject, IMessage)
  private
    FNom: string;
  public
    constructor Create(const Nom: string);
    destructor Destroy; override;
    procedure Dire(const Texte: string);
    procedure AfficherCompteur;
  end;

constructor TMessagerie.Create(const Nom: string);
begin
  inherited Create;
  FNom := Nom;
  WriteLn('╔════════════════════════════════════════╗');
  WriteLn('║ ✅ Création de "', FNom, '"');
  WriteLn('║ Compteur initial: ', RefCount);
  WriteLn('╚════════════════════════════════════════╝');
end;

destructor TMessagerie.Destroy;
begin
  WriteLn('╔════════════════════════════════════════╗');
  WriteLn('║ ❌ Destruction de "', FNom, '"');
  WriteLn('║ Le compteur a atteint 0');
  WriteLn('╚════════════════════════════════════════╝');
  inherited;
end;

procedure TMessagerie.Dire(const Texte: string);
begin
  WriteLn('💬 ', FNom, ' dit: "', Texte, '"');
end;

procedure TMessagerie.AfficherCompteur;
begin
  WriteLn('📊 Compteur actuel de "', FNom, '": ', RefCount);
end;

var
  Obj: TMessagerie;            // Variable objet : accès à AfficherCompteur (hors interface)
  Ref1, Ref2, Ref3: IMessage;  // Variables interface : participent au comptage de références
begin
  WriteLn('');
  WriteLn('═══════════════════════════════════════════════════');
  WriteLn('   DÉMONSTRATION DU COMPTAGE DE RÉFÉRENCES');
  WriteLn('═══════════════════════════════════════════════════');
  WriteLn('');

  WriteLn('▶ Étape 1: Création et assignation à Ref1');
  Obj := TMessagerie.Create('Assistant');
  Ref1 := Obj;
  Obj.AfficherCompteur;
  WriteLn('');

  WriteLn('▶ Étape 2: Assignation à Ref2 (même objet)');
  Ref2 := Ref1;
  Obj.AfficherCompteur;
  WriteLn('');

  WriteLn('▶ Étape 3: Assignation à Ref3 (toujours le même)');
  Ref3 := Ref1;
  Obj.AfficherCompteur;
  WriteLn('');

  WriteLn('▶ Étape 4: Utilisation via différentes références');
  Ref1.Dire('Bonjour depuis Ref1');
  Ref2.Dire('Bonjour depuis Ref2');
  Ref3.Dire('Bonjour depuis Ref3');
  WriteLn('   (C''est le MÊME objet qui parle 3 fois !)');
  WriteLn('');

  WriteLn('▶ Étape 5: Libération de Ref1');
  Ref1 := nil;
  WriteLn('   ℹ️ Objet toujours vivant (Ref2 et Ref3 existent)');
  Obj.AfficherCompteur;
  WriteLn('');

  WriteLn('▶ Étape 6: Libération de Ref2');
  Ref2 := nil;
  WriteLn('   ℹ️ Objet toujours vivant (Ref3 existe)');
  Obj.AfficherCompteur;
  WriteLn('');

  WriteLn('▶ Étape 7: Libération de Ref3 (dernière référence)');
  Ref3 := nil;
  WriteLn('   ℹ️ Le compteur atteint 0 → Destruction automatique !');
  WriteLn('');

  WriteLn('═══════════════════════════════════════════════════');
  WriteLn('   FIN DU PROGRAMME');
  WriteLn('═══════════════════════════════════════════════════');
end.
