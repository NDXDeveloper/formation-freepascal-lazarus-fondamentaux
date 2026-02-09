{ ============================================================================
  Section 11.6 : Polymorphisme
  Description : Zoo polymorphe avec 6 types d'animaux
  Fichier source : 06-polymorphisme.md
  ============================================================================ }
program ZooPolymorphe;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Classe de base }
  TAnimal = class
  protected
    FNom: string;
    FAge: Integer;
    FEspece: string;
  public
    constructor Create(ANom: string; AAge: Integer);
    procedure Afficher; virtual;
    procedure FaireDuBruit; virtual;
    procedure SeDeplacer; virtual;
    procedure Manger; virtual;
    function GetInfo: string;
  end;

  { Mammifères }
  TChien = class(TAnimal)
  public
    constructor Create(ANom: string; AAge: Integer);
    procedure FaireDuBruit; override;
    procedure SeDeplacer; override;
    procedure Manger; override;
  end;

  TChat = class(TAnimal)
  public
    constructor Create(ANom: string; AAge: Integer);
    procedure FaireDuBruit; override;
    procedure SeDeplacer; override;
    procedure Manger; override;
  end;

  TLion = class(TAnimal)
  public
    constructor Create(ANom: string; AAge: Integer);
    procedure FaireDuBruit; override;
    procedure SeDeplacer; override;
    procedure Manger; override;
  end;

  { Oiseaux }
  TPerroquet = class(TAnimal)
  public
    constructor Create(ANom: string; AAge: Integer);
    procedure FaireDuBruit; override;
    procedure SeDeplacer; override;
    procedure Manger; override;
  end;

  TAigle = class(TAnimal)
  public
    constructor Create(ANom: string; AAge: Integer);
    procedure FaireDuBruit; override;
    procedure SeDeplacer; override;
    procedure Manger; override;
  end;

  { Reptiles }
  TSerpent = class(TAnimal)
  public
    constructor Create(ANom: string; AAge: Integer);
    procedure FaireDuBruit; override;
    procedure SeDeplacer; override;
    procedure Manger; override;
  end;

{ === TAnimal === }

constructor TAnimal.Create(ANom: string; AAge: Integer);
begin
  inherited Create;
  FNom := ANom;
  FAge := AAge;
  FEspece := 'Animal générique';
end;

procedure TAnimal.Afficher;
begin
  WriteLn('--- ', FEspece, ' ---');
  WriteLn('Nom : ', FNom);
  WriteLn('Age : ', FAge, ' ans');
end;

procedure TAnimal.FaireDuBruit;
begin
  WriteLn('[Son générique d''animal]');
end;

procedure TAnimal.SeDeplacer;
begin
  WriteLn('[Déplacement générique]');
end;

procedure TAnimal.Manger;
begin
  WriteLn(FNom, ' mange de la nourriture');
end;

function TAnimal.GetInfo: string;
begin
  Result := Format('%s (%s, %d ans)', [FNom, FEspece, FAge]);
end;

{ === TChien === }

constructor TChien.Create(ANom: string; AAge: Integer);
begin
  inherited Create(ANom, AAge);
  FEspece := 'Chien';
end;

procedure TChien.FaireDuBruit;
begin
  WriteLn('🐕 ', FNom, ' aboie : Wouaf wouaf !');
end;

procedure TChien.SeDeplacer;
begin
  WriteLn('🐕 ', FNom, ' court à quatre pattes en remuant la queue');
end;

procedure TChien.Manger;
begin
  WriteLn('🐕 ', FNom, ' mange des croquettes');
end;

{ === TChat === }

constructor TChat.Create(ANom: string; AAge: Integer);
begin
  inherited Create(ANom, AAge);
  FEspece := 'Chat';
end;

procedure TChat.FaireDuBruit;
begin
  WriteLn('🐈 ', FNom, ' miaule : Miaou miaou !');
end;

procedure TChat.SeDeplacer;
begin
  WriteLn('🐈 ', FNom, ' se déplace silencieusement');
end;

procedure TChat.Manger;
begin
  WriteLn('🐈 ', FNom, ' mange du poisson');
end;

{ === TLion === }

constructor TLion.Create(ANom: string; AAge: Integer);
begin
  inherited Create(ANom, AAge);
  FEspece := 'Lion';
end;

procedure TLion.FaireDuBruit;
begin
  WriteLn('🦁 ', FNom, ' rugit : ROOAAAR !');
end;

procedure TLion.SeDeplacer;
begin
  WriteLn('🦁 ', FNom, ' marche majestueusement');
end;

procedure TLion.Manger;
begin
  WriteLn('🦁 ', FNom, ' dévore de la viande');
end;

{ === TPerroquet === }

constructor TPerroquet.Create(ANom: string; AAge: Integer);
begin
  inherited Create(ANom, AAge);
  FEspece := 'Perroquet';
end;

procedure TPerroquet.FaireDuBruit;
begin
  WriteLn('🦜 ', FNom, ' parle : Bonjour ! Bonjour !');
end;

procedure TPerroquet.SeDeplacer;
begin
  WriteLn('🦜 ', FNom, ' vole de branche en branche');
end;

procedure TPerroquet.Manger;
begin
  WriteLn('🦜 ', FNom, ' grignote des graines');
end;

{ === TAigle === }

constructor TAigle.Create(ANom: string; AAge: Integer);
begin
  inherited Create(ANom, AAge);
  FEspece := 'Aigle';
end;

procedure TAigle.FaireDuBruit;
begin
  WriteLn('🦅 ', FNom, ' crie : Crii crii !');
end;

procedure TAigle.SeDeplacer;
begin
  WriteLn('🦅 ', FNom, ' plane majestueusement dans le ciel');
end;

procedure TAigle.Manger;
begin
  WriteLn('🦅 ', FNom, ' chasse de petits animaux');
end;

{ === TSerpent === }

constructor TSerpent.Create(ANom: string; AAge: Integer);
begin
  inherited Create(ANom, AAge);
  FEspece := 'Serpent';
end;

procedure TSerpent.FaireDuBruit;
begin
  WriteLn('🐍 ', FNom, ' siffle : Ssssss !');
end;

procedure TSerpent.SeDeplacer;
begin
  WriteLn('🐍 ', FNom, ' rampe en ondulant');
end;

procedure TSerpent.Manger;
begin
  WriteLn('🐍 ', FNom, ' avale sa proie en entier');
end;

{ === Fonctions polymorphes === }

procedure PresentationAnimal(Animal: TAnimal);
begin
  WriteLn('=================================');
  Animal.Afficher;
  WriteLn;
  Write('Bruit : ');
  Animal.FaireDuBruit;
  Write('Déplacement : ');
  Animal.SeDeplacer;
  Write('Repas : ');
  Animal.Manger;
  WriteLn('=================================');
  WriteLn;
end;

procedure NourrirTousLesAnimaux(Animaux: array of TAnimal);
var
  i: Integer;
begin
  WriteLn('🍽️  HEURE DU REPAS DANS LE ZOO !');
  WriteLn;
  for i := 0 to High(Animaux) do
  begin
    WriteLn('→ ', Animaux[i].GetInfo);
    Animaux[i].Manger;
    WriteLn;
  end;
end;

procedure ConcertAnimal(Animaux: array of TAnimal);
var
  i: Integer;
begin
  WriteLn('🎵 CONCERT DES ANIMAUX !');
  WriteLn;
  for i := 0 to High(Animaux) do
  begin
    Write('♪ ');
    Animaux[i].FaireDuBruit;
  end;
  WriteLn;
end;

procedure CourseAnimale(Animaux: array of TAnimal);
var
  i: Integer;
begin
  WriteLn('🏃 GRANDE COURSE DES ANIMAUX !');
  WriteLn;
  for i := 0 to High(Animaux) do
  begin
    WriteLn('Concurrent ', i + 1, ' : ', Animaux[i].GetInfo);
    Animaux[i].SeDeplacer;
    WriteLn;
  end;
end;

{ === Programme principal === }
var
  Rex: TChien;
  Felix: TChat;
  Simba: TLion;
  Coco: TPerroquet;
  Zeus: TAigle;
  Kaa: TSerpent;

  Zoo: array[0..5] of TAnimal;
begin
  WriteLn('===============================================');
  WriteLn('    BIENVENUE AU ZOO POLYMORPHE !');
  WriteLn('===============================================');
  WriteLn;

  // Création des animaux
  Rex := TChien.Create('Rex', 5);
  Felix := TChat.Create('Félix', 3);
  Simba := TLion.Create('Simba', 8);
  Coco := TPerroquet.Create('Coco', 12);
  Zeus := TAigle.Create('Zeus', 6);
  Kaa := TSerpent.Create('Kaa', 4);

  // Remplissage du tableau polymorphe
  Zoo[0] := Rex;
  Zoo[1] := Felix;
  Zoo[2] := Simba;
  Zoo[3] := Coco;
  Zoo[4] := Zeus;
  Zoo[5] := Kaa;

  WriteLn('📋 LISTE DES PENSIONNAIRES :');
  WriteLn('----------------------------');
  WriteLn('1. ', Zoo[0].GetInfo);
  WriteLn('2. ', Zoo[1].GetInfo);
  WriteLn('3. ', Zoo[2].GetInfo);
  WriteLn('4. ', Zoo[3].GetInfo);
  WriteLn('5. ', Zoo[4].GetInfo);
  WriteLn('6. ', Zoo[5].GetInfo);
  WriteLn;
  WriteLn;

  // Test 1 : Présentation individuelle
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('TEST 1 : PRESENTATION INDIVIDUELLE');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;
  PresentationAnimal(Rex);
  PresentationAnimal(Simba);

  // Test 2 : Heure du repas
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('TEST 2 : HEURE DU REPAS');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;
  NourrirTousLesAnimaux(Zoo);

  // Test 3 : Concert
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('TEST 3 : CONCERT DES ANIMAUX');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;
  ConcertAnimal(Zoo);

  // Test 4 : Course
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('TEST 4 : GRANDE COURSE');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;
  CourseAnimale(Zoo);

  WriteLn('═══════════════════════════════════════════════');
  WriteLn('    FIN DE LA JOURNEE AU ZOO');
  WriteLn('═══════════════════════════════════════════════');

  // Libération
  Rex.Free;
  Felix.Free;
  Simba.Free;
  Coco.Free;
  Zeus.Free;
  Kaa.Free;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
