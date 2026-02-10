{ ============================================================================
  Section 11.1 : Concept d'heritage
  Description : Heritage simple avec TAnimal, TChien et TChat
  Fichier source : 01-concept-heritage.md
  ============================================================================ }
program ExempleHeritage;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  // CLASSE PARENT : Animal
  TAnimal = class
  private
    FNom: string;
    FAge: Integer;
  public
    constructor Create(ANom: string; AAge: Integer);
    procedure SePresenter;
    procedure Manger;
  end;

  // CLASSE ENFANT : Chien hérite de Animal
  TChien = class(TAnimal)
  private
    FRace: string;
  public
    constructor Create(ANom: string; AAge: Integer; ARace: string);
    procedure Aboyer;
  end;

  // CLASSE ENFANT : Chat hérite de Animal
  TChat = class(TAnimal)
  private
    FCouleur: string;
  public
    constructor Create(ANom: string; AAge: Integer; ACouleur: string);
    procedure Miauler;
  end;

{ Implémentation de TAnimal }

constructor TAnimal.Create(ANom: string; AAge: Integer);  
begin  
  FNom := ANom;
  FAge := AAge;
end;

procedure TAnimal.SePresenter;  
begin  
  WriteLn('Je m''appelle ', FNom, ' et j''ai ', FAge, ' ans.');
end;

procedure TAnimal.Manger;  
begin  
  WriteLn(FNom, ' est en train de manger.');
end;

{ Implémentation de TChien }

constructor TChien.Create(ANom: string; AAge: Integer; ARace: string);  
begin  
  inherited Create(ANom, AAge);  // Appel du constructeur parent
  FRace := ARace;
end;

procedure TChien.Aboyer;  
begin  
  WriteLn(FNom, ' aboie : Wouaf wouaf !');
end;

{ Implémentation de TChat }

constructor TChat.Create(ANom: string; AAge: Integer; ACouleur: string);  
begin  
  inherited Create(ANom, AAge);  // Appel du constructeur parent
  FCouleur := ACouleur;
end;

procedure TChat.Miauler;  
begin  
  WriteLn(FNom, ' miaule : Miaou miaou !');
end;

{ Programme principal }
var
  MonChien: TChien;
  MonChat: TChat;
begin
  // Création d'un chien
  MonChien := TChien.Create('Rex', 5, 'Berger Allemand');

  // Le chien peut utiliser les méthodes héritées de TAnimal
  MonChien.SePresenter;    // Méthode héritée
  MonChien.Manger;         // Méthode héritée
  MonChien.Aboyer;         // Méthode propre à TChien

  WriteLn;

  // Création d'un chat
  MonChat := TChat.Create('Félix', 3, 'Tigré');

  // Le chat peut aussi utiliser les méthodes héritées
  MonChat.SePresenter;     // Méthode héritée
  MonChat.Manger;          // Méthode héritée
  MonChat.Miauler;         // Méthode propre à TChat

  // Libération de la mémoire
  MonChien.Free;
  MonChat.Free;

  ReadLn;
end.
