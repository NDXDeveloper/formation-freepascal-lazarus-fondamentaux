{ ============================================================================
  Section 11.2 : Classes derivees
  Description : Gestion de personnes avec heritage TPersonne/TEmploye/TClient
  Fichier source : 02-classes-derivees.md
  ============================================================================ }
program GestionPersonnes;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils;

type
  { Classe de base : TPersonne }
  TPersonne = class
  private
    FNom: string;
    FPrenom: string;
    FDateNaissance: TDateTime;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime);
    destructor Destroy; override;
    procedure Afficher; virtual;
    function GetAge: Integer;
    function GetNomComplet: string;
  end;

  { Classe dérivée : TEmploye }
  TEmploye = class(TPersonne)
  private
    FNumeroEmploye: Integer;
    FSalaire: Real;
    FDateEmbauche: TDateTime;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime);
    procedure Afficher; override;
    function GetAnciennete: Integer;
    procedure AugmenterSalaire(Pourcentage: Real);
  end;

  { Classe dérivée : TClient }
  TClient = class(TPersonne)
  private
    FNumeroClient: string;
    FMontantAchats: Real;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumeroClient: string);
    procedure Afficher; override;
    procedure AjouterAchat(Montant: Real);
    function EstClientFidele: Boolean;
  end;

{ === Implémentation de TPersonne === }

constructor TPersonne.Create(ANom, APrenom: string; ADateNaissance: TDateTime);
begin
  inherited Create;  // Appelle le constructeur de TObject
  FNom := ANom;
  FPrenom := APrenom;
  FDateNaissance := ADateNaissance;
  WriteLn('[TPersonne] Création de ', GetNomComplet);
end;

destructor TPersonne.Destroy;
begin
  WriteLn('[TPersonne] Destruction de ', GetNomComplet);
  inherited Destroy;  // Appelle le destructeur de TObject
end;

procedure TPersonne.Afficher;
begin
  WriteLn('Nom complet : ', GetNomComplet);
  WriteLn('Age : ', GetAge, ' ans');
  WriteLn('Né(e) le : ', DateToStr(FDateNaissance));
end;

function TPersonne.GetAge: Integer;
begin
  Result := YearsBetween(Now, FDateNaissance);
end;

function TPersonne.GetNomComplet: string;
begin
  Result := FPrenom + ' ' + FNom;
end;

{ === Implémentation de TEmploye === }

constructor TEmploye.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                            ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime);
begin
  // D'abord, on initialise la partie TPersonne
  inherited Create(ANom, APrenom, ADateNaissance);

  // Ensuite, on initialise la partie spécifique à TEmploye
  FNumeroEmploye := ANumero;
  FSalaire := ASalaire;
  FDateEmbauche := ADateEmbauche;
  WriteLn('[TEmploye] Numéro employé : ', ANumero);
end;

procedure TEmploye.Afficher;
begin
  WriteLn('=== FICHE EMPLOYE ===');
  inherited Afficher;  // Affiche les infos de base (nom, âge, etc.)
  WriteLn('Numéro employé : ', FNumeroEmploye);
  WriteLn('Salaire : ', FSalaire:0:2, ' €');
  WriteLn('Date embauche : ', DateToStr(FDateEmbauche));
  WriteLn('Ancienneté : ', GetAnciennete, ' ans');
  WriteLn('=====================');
end;

function TEmploye.GetAnciennete: Integer;
begin
  Result := YearsBetween(Now, FDateEmbauche);
end;

procedure TEmploye.AugmenterSalaire(Pourcentage: Real);
begin
  WriteLn('Augmentation de ', Pourcentage:0:1, '% pour ', GetNomComplet);
  FSalaire := FSalaire * (1 + Pourcentage / 100);
  WriteLn('Nouveau salaire : ', FSalaire:0:2, ' €');
end;

{ === Implémentation de TClient === }

constructor TClient.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                           ANumeroClient: string);
begin
  inherited Create(ANom, APrenom, ADateNaissance);
  FNumeroClient := ANumeroClient;
  FMontantAchats := 0;
  WriteLn('[TClient] Numéro client : ', ANumeroClient);
end;

procedure TClient.Afficher;
begin
  WriteLn('=== FICHE CLIENT ===');
  inherited Afficher;
  WriteLn('Numéro client : ', FNumeroClient);
  WriteLn('Montant total achats : ', FMontantAchats:0:2, ' €');
  if EstClientFidele then
    WriteLn('Statut : CLIENT FIDELE ⭐')
  else
    WriteLn('Statut : Client standard');
  WriteLn('====================');
end;

procedure TClient.AjouterAchat(Montant: Real);
begin
  FMontantAchats := FMontantAchats + Montant;
  WriteLn('Achat de ', Montant:0:2, ' € ajouté pour ', GetNomComplet);
end;

function TClient.EstClientFidele: Boolean;
begin
  Result := FMontantAchats >= 1000;  // Fidèle si > 1000€ d'achats
end;

{ === Programme principal === }
var
  Employe1: TEmploye;
  Client1: TClient;
begin
  WriteLn('=== DEMONSTRATION DES CLASSES DERIVEES ===');
  WriteLn;

  // Création d'un employé
  WriteLn('--- Création d''un employé ---');
  Employe1 := TEmploye.Create('Dupont', 'Jean', EncodeDate(1985, 3, 15),
                               1001, 2500.00, EncodeDate(2015, 1, 10));
  WriteLn;

  // Affichage de l'employé
  Employe1.Afficher;
  WriteLn;

  // Augmentation de salaire
  Employe1.AugmenterSalaire(10);
  WriteLn;

  // Création d'un client
  WriteLn('--- Création d''un client ---');
  Client1 := TClient.Create('Martin', 'Sophie', EncodeDate(1990, 7, 22), 'CL-2024-001');
  WriteLn;

  // Ajout d'achats
  Client1.AjouterAchat(450.00);
  Client1.AjouterAchat(325.50);
  Client1.AjouterAchat(280.00);
  WriteLn;

  // Affichage du client
  Client1.Afficher;
  WriteLn;

  // Libération de la mémoire
  WriteLn('--- Libération des objets ---');
  Employe1.Free;
  Client1.Free;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
