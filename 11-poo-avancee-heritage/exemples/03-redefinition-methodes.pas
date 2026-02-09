{ ============================================================================
  Section 11.3 : Redefinition de methodes
  Description : Masquage vs redefinition avec TAnimal, TChien et TChat
  Fichier source : 03-redefinition-methodes.md
  ============================================================================ }
program RedefinitionMethodes;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Classe de base : Animal }
  TAnimal = class
  protected
    FNom: string;
  public
    constructor Create(ANom: string);

    // Méthode NON virtuelle (masquage possible)
    procedure SePresenter;

    // Méthode virtuelle (redéfinition possible)
    procedure FaireDuBruit; virtual;

    // Méthode virtuelle
    procedure Manger; virtual;
  end;

  { Classe dérivée : Chien }
  TChien = class(TAnimal)
  private
    FRace: string;
  public
    constructor Create(ANom, ARace: string);

    // Masquage de SePresenter (pas de override)
    procedure SePresenter;

    // Redéfinition de FaireDuBruit
    procedure FaireDuBruit; override;

    // Redéfinition de Manger
    procedure Manger; override;
  end;

  { Classe dérivée : Chat }
  TChat = class(TAnimal)
  private
    FCouleur: string;
  public
    constructor Create(ANom, ACouleur: string);

    // Masquage de SePresenter
    procedure SePresenter;

    // Redéfinition de FaireDuBruit
    procedure FaireDuBruit; override;

    // Redéfinition de Manger
    procedure Manger; override;
  end;

{ === Implémentation de TAnimal === }

constructor TAnimal.Create(ANom: string);
begin
  inherited Create;
  FNom := ANom;
end;

procedure TAnimal.SePresenter;
begin
  WriteLn('Je suis un animal qui s''appelle ', FNom);
end;

procedure TAnimal.FaireDuBruit;
begin
  WriteLn('[Animal générique fait du bruit]');
end;

procedure TAnimal.Manger;
begin
  WriteLn(FNom, ' mange de la nourriture générique.');
end;

{ === Implémentation de TChien === }

constructor TChien.Create(ANom, ARace: string);
begin
  inherited Create(ANom);
  FRace := ARace;
end;

procedure TChien.SePresenter;
begin
  WriteLn('Je suis un chien ', FRace, ' qui s''appelle ', FNom);
end;

procedure TChien.FaireDuBruit;
begin
  WriteLn(FNom, ' aboie : Wouaf wouaf !');
end;

procedure TChien.Manger;
begin
  WriteLn(FNom, ' le chien mange des croquettes.');
end;

{ === Implémentation de TChat === }

constructor TChat.Create(ANom, ACouleur: string);
begin
  inherited Create(ANom);
  FCouleur := ACouleur;
end;

procedure TChat.SePresenter;
begin
  WriteLn('Je suis un chat ', FCouleur, ' qui s''appelle ', FNom);
end;

procedure TChat.FaireDuBruit;
begin
  WriteLn(FNom, ' miaule : Miaou miaou !');
end;

procedure TChat.Manger;
begin
  WriteLn(FNom, ' le chat mange du poisson.');
end;

{ === Programme principal === }

procedure TesterAvecType(Animal: TAnimal);
begin
  WriteLn('--- Test avec variable de type TAnimal ---');

  // SePresenter n'est PAS virtuelle : masquage
  WriteLn('SePresenter (masquée) :');
  Animal.SePresenter;  // Appelle TOUJOURS TAnimal.SePresenter

  WriteLn;

  // FaireDuBruit EST virtuelle : redéfinition
  WriteLn('FaireDuBruit (redéfinie) :');
  Animal.FaireDuBruit;  // Appelle la version du type REEL

  WriteLn;

  // Manger EST virtuelle : redéfinition
  WriteLn('Manger (redéfinie) :');
  Animal.Manger;  // Appelle la version du type REEL

  WriteLn;
end;

var
  MonChien: TChien;
  MonChat: TChat;
  UnAnimal: TAnimal;
begin
  WriteLn('=== DEMONSTRATION : MASQUAGE vs REDEFINITION ===');
  WriteLn;

  MonChien := TChien.Create('Rex', 'Berger Allemand');
  MonChat := TChat.Create('Félix', 'Tigré');

  WriteLn('========================================');
  WriteLn('TEST 1 : Utilisation directe (type exact)');
  WriteLn('========================================');
  WriteLn;

  WriteLn('--- Appel direct sur MonChien (type TChien) ---');
  MonChien.SePresenter;    // Version TChien
  MonChien.FaireDuBruit;   // Version TChien
  MonChien.Manger;         // Version TChien
  WriteLn;

  WriteLn('--- Appel direct sur MonChat (type TChat) ---');
  MonChat.SePresenter;     // Version TChat
  MonChat.FaireDuBruit;    // Version TChat
  MonChat.Manger;          // Version TChat
  WriteLn;

  WriteLn('========================================');
  WriteLn('TEST 2 : Via variable de type TAnimal');
  WriteLn('========================================');
  WriteLn;

  WriteLn('***** Chien vu comme Animal *****');
  UnAnimal := MonChien;
  TesterAvecType(UnAnimal);

  WriteLn('***** Chat vu comme Animal *****');
  UnAnimal := MonChat;
  TesterAvecType(UnAnimal);

  WriteLn('========================================');
  WriteLn('CONCLUSION');
  WriteLn('========================================');
  WriteLn('• Méthode NON virtuelle (SePresenter) :');
  WriteLn('  → Masquage : appelle toujours la version du TYPE DE LA VARIABLE');
  WriteLn;
  WriteLn('• Méthode virtuelle/override (FaireDuBruit, Manger) :');
  WriteLn('  → Redéfinition : appelle la version du TYPE REEL de l''objet');
  WriteLn('  → C''est le POLYMORPHISME !');
  WriteLn;

  MonChien.Free;
  MonChat.Free;

  ReadLn;
end.
