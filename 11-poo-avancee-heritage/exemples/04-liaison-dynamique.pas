{ ============================================================================
  Section 11.4 : Methodes virtuelles et override
  Description : Liaison dynamique avec vehicules (TVoiture, TMoto, TCamion)
  Fichier source : 04-methodes-virtuelles-override.md
  ============================================================================ }
program LiaisonDynamique;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Classe de base }
  TVehicule = class
  protected
    FMarque: string;
    FModele: string;
  public
    constructor Create(AMarque, AModele: string);

    // Méthode NON virtuelle
    procedure AfficherType;

    // Méthodes virtuelles
    procedure Demarrer; virtual;
    procedure Accelerer; virtual;
    procedure Freiner; virtual;
    function GetVitesseMax: Integer; virtual;
  end;

  { Voiture }
  TVoiture = class(TVehicule)
  public
    procedure Demarrer; override;
    procedure Accelerer; override;
    procedure Freiner; override;
    function GetVitesseMax: Integer; override;
  end;

  { Moto }
  TMoto = class(TVehicule)
  public
    procedure Demarrer; override;
    procedure Accelerer; override;
    procedure Freiner; override;
    function GetVitesseMax: Integer; override;
  end;

  { Camion }
  TCamion = class(TVehicule)
  public
    procedure Demarrer; override;
    procedure Accelerer; override;
    procedure Freiner; override;
    function GetVitesseMax: Integer; override;
  end;

{ === TVehicule === }

constructor TVehicule.Create(AMarque, AModele: string);
begin
  inherited Create;
  FMarque := AMarque;
  FModele := AModele;
end;

procedure TVehicule.AfficherType;
begin
  WriteLn('Type : Véhicule générique');
end;

procedure TVehicule.Demarrer;
begin
  WriteLn('[Véhicule] Démarrage générique');
end;

procedure TVehicule.Accelerer;
begin
  WriteLn('[Véhicule] Accélération générique');
end;

procedure TVehicule.Freiner;
begin
  WriteLn('[Véhicule] Freinage générique');
end;

function TVehicule.GetVitesseMax: Integer;
begin
  Result := 100;
end;

{ === TVoiture === }

procedure TVoiture.Demarrer;
begin
  WriteLn('🚗 Tournez la clé, le moteur de la voiture ronronne');
end;

procedure TVoiture.Accelerer;
begin
  WriteLn('🚗 La voiture accélère en douceur');
end;

procedure TVoiture.Freiner;
begin
  WriteLn('🚗 Freinage progressif de la voiture');
end;

function TVoiture.GetVitesseMax: Integer;
begin
  Result := 180;
end;

{ === TMoto === }

procedure TMoto.Demarrer;
begin
  WriteLn('🏍️  Vrrooom ! La moto démarre en trombe');
end;

procedure TMoto.Accelerer;
begin
  WriteLn('🏍️  Accélération fulgurante de la moto !');
end;

procedure TMoto.Freiner;
begin
  WriteLn('🏍️  Freinage sportif de la moto');
end;

function TMoto.GetVitesseMax: Integer;
begin
  Result := 220;
end;

{ === TCamion === }

procedure TCamion.Demarrer;
begin
  WriteLn('🚚 Le camion démarre lentement avec un bruit sourd');
end;

procedure TCamion.Accelerer;
begin
  WriteLn('🚚 Le camion accélère péniblement');
end;

procedure TCamion.Freiner;
begin
  WriteLn('🚚 Long freinage du camion chargé');
end;

function TCamion.GetVitesseMax: Integer;
begin
  Result := 110;
end;

{ === Procédures de test === }

procedure TestVehicule(V: TVehicule);
begin
  WriteLn('--- Test du véhicule : ', V.FMarque, ' ', V.FModele, ' ---');
  WriteLn;

  // Méthode NON virtuelle : liaison statique
  WriteLn('Appel de AfficherType (NON virtuelle) :');
  V.AfficherType;  // Appelle toujours TVehicule.AfficherType
  WriteLn;

  // Méthodes virtuelles : liaison dynamique
  WriteLn('Appels des méthodes virtuelles :');
  V.Demarrer;      // Appelle la version du type réel
  V.Accelerer;     // Appelle la version du type réel
  V.Freiner;       // Appelle la version du type réel
  WriteLn('Vitesse max : ', V.GetVitesseMax, ' km/h');
  WriteLn;
end;

procedure ComparerVehicules(V1, V2: TVehicule);
begin
  WriteLn('=== COMPARAISON DE VEHICULES ===');
  WriteLn('Véhicule 1 : vitesse max = ', V1.GetVitesseMax, ' km/h');
  WriteLn('Véhicule 2 : vitesse max = ', V2.GetVitesseMax, ' km/h');

  if V1.GetVitesseMax > V2.GetVitesseMax then
    WriteLn('→ Le véhicule 1 est plus rapide')
  else if V1.GetVitesseMax < V2.GetVitesseMax then
    WriteLn('→ Le véhicule 2 est plus rapide')
  else
    WriteLn('→ Même vitesse maximale');
  WriteLn;
end;

{ === Programme principal === }
var
  MaVoiture: TVoiture;
  MaMoto: TMoto;
  MonCamion: TCamion;
  UnVehicule: TVehicule;
begin
  WriteLn('=== DEMONSTRATION DES METHODES VIRTUELLES ===');
  WriteLn;

  // Création des véhicules
  MaVoiture := TVoiture.Create('Peugeot', '308');
  MaMoto := TMoto.Create('Yamaha', 'MT-07');
  MonCamion := TCamion.Create('Renault', 'Master');

  WriteLn('========================================');
  WriteLn('TEST 1 : Polymorphisme en action');
  WriteLn('========================================');
  WriteLn;

  // Chaque véhicule vu comme un TVehicule
  TestVehicule(MaVoiture);
  TestVehicule(MaMoto);
  TestVehicule(MonCamion);

  WriteLn('========================================');
  WriteLn('TEST 2 : Comparaisons polymorphes');
  WriteLn('========================================');
  WriteLn;

  ComparerVehicules(MaVoiture, MaMoto);
  ComparerVehicules(MaMoto, MonCamion);
  ComparerVehicules(MaVoiture, MonCamion);

  WriteLn('========================================');
  WriteLn('TEST 3 : Changement dynamique de type');
  WriteLn('========================================');
  WriteLn;

  // Une seule variable qui change de type
  WriteLn('UnVehicule pointe maintenant vers la voiture :');
  UnVehicule := MaVoiture;
  UnVehicule.Demarrer;
  WriteLn;

  WriteLn('UnVehicule pointe maintenant vers la moto :');
  UnVehicule := MaMoto;
  UnVehicule.Demarrer;
  WriteLn;

  WriteLn('UnVehicule pointe maintenant vers le camion :');
  UnVehicule := MonCamion;
  UnVehicule.Demarrer;
  WriteLn;

  // Libération
  MaVoiture.Free;
  MaMoto.Free;
  MonCamion.Free;

  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
