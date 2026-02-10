{ ============================================================================
  Section 11.7 : Transtypage (as, is)
  Description : Zoo avec identification et transtypage securise
  Fichier source : 07-transtypage-as-is.md
  ============================================================================ }
program TranstypageZoo;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Hiérarchie de classes }
  TAnimal = class
  protected
    FNom: string;
    FAge: Integer;
  public
    constructor Create(ANom: string; AAge: Integer);
    procedure SePresenter; virtual;
    procedure FaireDuBruit; virtual;
  end;

  { Mammifères }
  TMammifere = class(TAnimal)
  private
    FPoils: Boolean;
  public
    constructor Create(ANom: string; AAge: Integer; APoils: Boolean);
    procedure Allaiter;
  end;

  TChien = class(TMammifere)
  private
    FRace: string;
  public
    constructor Create(ANom: string; AAge: Integer; ARace: string);
    procedure Aboyer;
    procedure Garder;
    property Race: string read FRace;
  end;

  TChat = class(TMammifere)
  private
    FCouleur: string;
  public
    constructor Create(ANom: string; AAge: Integer; ACouleur: string);
    procedure Miauler;
    procedure Ronronner;
    property Couleur: string read FCouleur;
  end;

  { Oiseaux }
  TOiseau = class(TAnimal)
  private
    FEnvergure: Real;
  public
    constructor Create(ANom: string; AAge: Integer; AEnvergure: Real);
    procedure Voler;
    procedure ChercherNourriture;
    property Envergure: Real read FEnvergure;
  end;

  TPerroquet = class(TOiseau)
  private
    FVocabulaire: Integer;  // Nombre de mots
  public
    constructor Create(ANom: string; AAge: Integer; AEnvergure: Real; AVocabulaire: Integer);
    procedure Parler(const Phrase: string);
    property Vocabulaire: Integer read FVocabulaire;
  end;

  TAigle = class(TOiseau)
  public
    procedure Chasser;
  end;

{ === TAnimal === }

constructor TAnimal.Create(ANom: string; AAge: Integer);  
begin  
  inherited Create;
  FNom := ANom;
  FAge := AAge;
end;

procedure TAnimal.SePresenter;  
begin  
  WriteLn('Je suis ', FNom, ', j''ai ', FAge, ' ans');
end;

procedure TAnimal.FaireDuBruit;  
begin  
  WriteLn('[Bruit d''animal générique]');
end;

{ === TMammifere === }

constructor TMammifere.Create(ANom: string; AAge: Integer; APoils: Boolean);  
begin  
  inherited Create(ANom, AAge);
  FPoils := APoils;
end;

procedure TMammifere.Allaiter;  
begin  
  WriteLn(FNom, ' allaite ses petits');
end;

{ === TChien === }

constructor TChien.Create(ANom: string; AAge: Integer; ARace: string);  
begin  
  inherited Create(ANom, AAge, True);
  FRace := ARace;
end;

procedure TChien.Aboyer;  
begin  
  WriteLn('🐕 ', FNom, ' aboie : Wouaf wouaf !');
end;

procedure TChien.Garder;  
begin  
  WriteLn('🐕 ', FNom, ' monte la garde');
end;

{ === TChat === }

constructor TChat.Create(ANom: string; AAge: Integer; ACouleur: string);  
begin  
  inherited Create(ANom, AAge, True);
  FCouleur := ACouleur;
end;

procedure TChat.Miauler;  
begin  
  WriteLn('🐈 ', FNom, ' miaule : Miaou !');
end;

procedure TChat.Ronronner;  
begin  
  WriteLn('🐈 ', FNom, ' ronronne : Rrrrrr...');
end;

{ === TOiseau === }

constructor TOiseau.Create(ANom: string; AAge: Integer; AEnvergure: Real);  
begin  
  inherited Create(ANom, AAge);
  FEnvergure := AEnvergure;
end;

procedure TOiseau.Voler;  
begin  
  WriteLn('🦅 ', FNom, ' vole avec ', FEnvergure:0:2, ' m d''envergure');
end;

procedure TOiseau.ChercherNourriture;  
begin  
  WriteLn('🦅 ', FNom, ' cherche de la nourriture');
end;

{ === TPerroquet === }

constructor TPerroquet.Create(ANom: string; AAge: Integer; AEnvergure: Real; AVocabulaire: Integer);  
begin  
  inherited Create(ANom, AAge, AEnvergure);
  FVocabulaire := AVocabulaire;
end;

procedure TPerroquet.Parler(const Phrase: string);  
begin  
  WriteLn('🦜 ', FNom, ' dit : "', Phrase, '"');
end;

{ === TAigle === }

procedure TAigle.Chasser;  
begin  
  WriteLn('🦅 ', FNom, ' chasse sa proie depuis le ciel');
end;

{ === Fonctions utilisant le transtypage === }

procedure IdentifierAnimal(Animal: TAnimal);  
begin  
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('IDENTIFICATION D''UN ANIMAL');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  // Présentation générale
  Animal.SePresenter;
  WriteLn;

  // Identification du type précis avec is
  WriteLn('→ Analyse du type...');

  if Animal is TChien then
    WriteLn('✓ Type identifié : CHIEN')
  else if Animal is TChat then
    WriteLn('✓ Type identifié : CHAT')
  else if Animal is TPerroquet then
    WriteLn('✓ Type identifié : PERROQUET')
  else if Animal is TAigle then
    WriteLn('✓ Type identifié : AIGLE')
  else if Animal is TOiseau then
    WriteLn('✓ Type identifié : OISEAU (type générique)')
  else if Animal is TMammifere then
    WriteLn('✓ Type identifié : MAMMIFERE (type générique)')
  else
    WriteLn('✓ Type identifié : ANIMAL (type de base)');

  WriteLn;

  // Accès aux fonctionnalités spécifiques via transtypage
  WriteLn('→ Fonctionnalités spécifiques :');
  WriteLn;

  if Animal is TChien then
  begin
    // Transtypage sécurisé vers TChien
    with Animal as TChien do
    begin
      WriteLn('  Race : ', Race);
      Aboyer;
      Garder;
    end;
  end
  else if Animal is TChat then
  begin
    // Transtypage sécurisé vers TChat
    with Animal as TChat do
    begin
      WriteLn('  Couleur : ', Couleur);
      Miauler;
      Ronronner;
    end;
  end
  else if Animal is TPerroquet then
  begin
    // Transtypage sécurisé vers TPerroquet
    with Animal as TPerroquet do
    begin
      WriteLn('  Vocabulaire : ', Vocabulaire, ' mots');
      WriteLn('  Envergure : ', Envergure:0:2, ' m');
      Parler('Bonjour ! Bonjour !');
      Voler;
    end;
  end
  else if Animal is TAigle then
  begin
    // Transtypage sécurisé vers TAigle
    with Animal as TAigle do
    begin
      WriteLn('  Envergure : ', Envergure:0:2, ' m');
      Chasser;
      Voler;
    end;
  end
  else if Animal is TOiseau then
  begin
    // Transtypage vers TOiseau (type parent des oiseaux)
    with Animal as TOiseau do
    begin
      WriteLn('  Envergure : ', Envergure:0:2, ' m');
      Voler;
      ChercherNourriture;
    end;
  end
  else if Animal is TMammifere then
  begin
    // Transtypage vers TMammifere (type parent des mammifères)
    (Animal as TMammifere).Allaiter;
  end;

  WriteLn;
end;

procedure ComparaisonHierarchique(Animal: TAnimal);  
begin  
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('ANALYSE HIERARCHIQUE');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;
  WriteLn('Animal : ', Animal.FNom);
  WriteLn;
  WriteLn('Tests de type dans la hiérarchie :');
  WriteLn('  TAnimal ?       ', Animal is TAnimal);
  WriteLn('  TMammifere ?    ', Animal is TMammifere);
  WriteLn('  TChien ?        ', Animal is TChien);
  WriteLn('  TChat ?         ', Animal is TChat);
  WriteLn('  TOiseau ?       ', Animal is TOiseau);
  WriteLn('  TPerroquet ?    ', Animal is TPerroquet);
  WriteLn('  TAigle ?        ', Animal is TAigle);
  WriteLn;
end;

procedure NourrirAnimaux(Animaux: array of TAnimal);  
var  
  i: Integer;
  Chien: TChien;
  Chat: TChat;
  Oiseau: TOiseau;
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('HEURE DU REPAS');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  for i := 0 to High(Animaux) do
  begin
    WriteLn('→ ', Animaux[i].FNom, ' :');

    // Adaptation du repas selon le type
    if Animaux[i] is TChien then
    begin
      Chien := Animaux[i] as TChien;
      WriteLn('  Croquettes spéciales pour chien ', Chien.Race);
    end
    else if Animaux[i] is TChat then
    begin
      Chat := Animaux[i] as TChat;
      WriteLn('  Pâtée pour chat ', Chat.Couleur);
    end
    else if Animaux[i] is TOiseau then
    begin
      Oiseau := Animaux[i] as TOiseau;
      WriteLn('  Graines adaptées aux oiseaux');
    end
    else
      WriteLn('  Nourriture générique');

    WriteLn;
  end;
end;

{ === Programme principal === }
var
  Rex: TChien;
  Felix: TChat;
  Coco: TPerroquet;
  Zeus: TAigle;

  MesAnimaux: array[0..3] of TAnimal;
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('   DEMONSTRATION DU TRANSTYPAGE (is / as)');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  // Création des animaux
  Rex := TChien.Create('Rex', 5, 'Berger Allemand');
  Felix := TChat.Create('Felix', 3, 'Tigré');
  Coco := TPerroquet.Create('Coco', 12, 0.8, 50);
  Zeus := TAigle.Create('Zeus', 6, 2.2);

  WriteLn;

  // Test 1 : Identification de chaque animal
  IdentifierAnimal(Rex);
  IdentifierAnimal(Felix);
  IdentifierAnimal(Coco);
  IdentifierAnimal(Zeus);

  // Test 2 : Comparaison hiérarchique
  ComparaisonHierarchique(Rex);
  ComparaisonHierarchique(Coco);

  // Test 3 : Traitement polymorphe avec adaptation par type
  MesAnimaux[0] := Rex;
  MesAnimaux[1] := Felix;
  MesAnimaux[2] := Coco;
  MesAnimaux[3] := Zeus;

  NourrirAnimaux(MesAnimaux);

  // Libération
  Rex.Free;
  Felix.Free;
  Coco.Free;
  Zeus.Free;

  WriteLn('═══════════════════════════════════════════════');
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
