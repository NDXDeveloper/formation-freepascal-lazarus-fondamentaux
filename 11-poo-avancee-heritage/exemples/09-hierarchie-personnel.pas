{ ============================================================================
  Section 11.9 : Hierarchies de classes
  Description : Systeme de gestion de personnel avec hierarchie a 4 niveaux
  Fichier source : 09-hierarchies-classes.md
  ============================================================================ }
program HierarchiePersonnel;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, StrUtils;

type
  {
    Hiérarchie :

    TPersonne (racine)
      ├── TEmploye
      │   ├── TEmployePermanent
      │   │   ├── TManager
      │   │   └── TDirecteur
      │   └── TEmployeTemporaire
      │       ├── TStagiaire
      │       └── TInterimaire
      └── TClient
          ├── TClientParticulier
          └── TClientEntreprise
  }

  { === NIVEAU 1 : Racine === }

  TPersonne = class
  protected
    FNom: string;
    FPrenom: string;
    FDateNaissance: TDateTime;
    FEmail: string;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime);
    procedure Afficher; virtual;
    function GetAge: Integer;
    function GetNomComplet: string;
    property Email: string read FEmail write FEmail;
  end;

  { === NIVEAU 2 : Branches principales === }

  TEmploye = class(TPersonne)
  protected
    FNumeroEmploye: Integer;
    FSalaire: Real;
    FDateEmbauche: TDateTime;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime);
    procedure Afficher; override;
    function GetAnciennete: Integer;
    procedure AugmenterSalaire(Pourcentage: Real); virtual;
    property NumeroEmploye: Integer read FNumeroEmploye;
    property Salaire: Real read FSalaire;
  end;

  TClient = class(TPersonne)
  protected
    FNumeroClient: string;
    FMontantAchats: Real;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumeroClient: string);
    procedure Afficher; override;
    procedure AjouterAchat(Montant: Real);
    function GetRemise: Real; virtual;
    property MontantAchats: Real read FMontantAchats;
  end;

  { === NIVEAU 3 : Spécialisations d'employés === }

  TEmployePermanent = class(TEmploye)
  protected
    FAvantages: string;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime;
                       AAvantages: string);
    procedure Afficher; override;
  end;

  TEmployeTemporaire = class(TEmploye)
  protected
    FDateFin: TDateTime;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumero: Integer; ASalaire: Real; ADateEmbauche, ADateFin: TDateTime);
    procedure Afficher; override;
    function EstEnCours: Boolean;
  end;

  { === NIVEAU 3 : Spécialisations de clients === }

  TClientParticulier = class(TClient)
  private
    FProgrammeFidelite: Boolean;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumeroClient: string; AFidelite: Boolean);
    procedure Afficher; override;
    function GetRemise: Real; override;
  end;

  TClientEntreprise = class(TClient)
  private
    FRaisonSociale: string;
    FSiret: string;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumeroClient, ARaisonSociale, ASiret: string);
    procedure Afficher; override;
    function GetRemise: Real; override;
  end;

  { === NIVEAU 4 : Spécialisations avancées === }

  TManager = class(TEmployePermanent)
  private
    FTailleEquipe: Integer;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime;
                       AAvantages: string; ATailleEquipe: Integer);
    procedure Afficher; override;
    procedure AugmenterSalaire(Pourcentage: Real); override;
  end;

  TDirecteur = class(TEmployePermanent)
  private
    FDepartement: string;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime;
                       AAvantages, ADepartement: string);
    procedure Afficher; override;
  end;

  TStagiaire = class(TEmployeTemporaire)
  private
    FEcole: string;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumero: Integer; ASalaire: Real; ADateEmbauche, ADateFin: TDateTime;
                       AEcole: string);
    procedure Afficher; override;
  end;

  TInterimaire = class(TEmployeTemporaire)
  private
    FAgence: string;
  public
    constructor Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                       ANumero: Integer; ASalaire: Real; ADateEmbauche, ADateFin: TDateTime;
                       AAgence: string);
    procedure Afficher; override;
  end;

{ === Implémentation TPersonne === }

constructor TPersonne.Create(ANom, APrenom: string; ADateNaissance: TDateTime);
begin
  inherited Create;
  FNom := ANom;
  FPrenom := APrenom;
  FDateNaissance := ADateNaissance;
end;

procedure TPersonne.Afficher;
begin
  WriteLn('╔════════════════════════════════════════════════');
  WriteLn('║ PERSONNE');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ Nom complet : ', GetNomComplet);
  WriteLn('║ Age : ', GetAge, ' ans');
  WriteLn('║ Date naissance : ', DateToStr(FDateNaissance));
  if FEmail <> '' then
    WriteLn('║ Email : ', FEmail);
end;

function TPersonne.GetAge: Integer;
begin
  Result := YearsBetween(Now, FDateNaissance);
end;

function TPersonne.GetNomComplet: string;
begin
  Result := FPrenom + ' ' + FNom;
end;

{ === Implémentation TEmploye === }

constructor TEmploye.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                            ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime);
begin
  inherited Create(ANom, APrenom, ADateNaissance);
  FNumeroEmploye := ANumero;
  FSalaire := ASalaire;
  FDateEmbauche := ADateEmbauche;
end;

procedure TEmploye.Afficher;
begin
  inherited Afficher;
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ EMPLOYE');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ Numéro : ', FNumeroEmploye);
  WriteLn('║ Salaire : ', FSalaire:0:2, ' €');
  WriteLn('║ Date embauche : ', DateToStr(FDateEmbauche));
  WriteLn('║ Ancienneté : ', GetAnciennete, ' ans');
end;

function TEmploye.GetAnciennete: Integer;
begin
  Result := YearsBetween(Now, FDateEmbauche);
end;

procedure TEmploye.AugmenterSalaire(Pourcentage: Real);
begin
  FSalaire := FSalaire * (1 + Pourcentage / 100);
end;

{ === Implémentation TClient === }

constructor TClient.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                           ANumeroClient: string);
begin
  inherited Create(ANom, APrenom, ADateNaissance);
  FNumeroClient := ANumeroClient;
  FMontantAchats := 0;
end;

procedure TClient.Afficher;
begin
  inherited Afficher;
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ CLIENT');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ Numéro : ', FNumeroClient);
  WriteLn('║ Achats totaux : ', FMontantAchats:0:2, ' €');
  WriteLn('║ Remise : ', GetRemise:0:1, '%');
end;

procedure TClient.AjouterAchat(Montant: Real);
begin
  FMontantAchats := FMontantAchats + Montant;
end;

function TClient.GetRemise: Real;
begin
  Result := 0;  // Pas de remise par défaut
end;

{ === Implémentation TEmployePermanent === }

constructor TEmployePermanent.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                                     ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime;
                                     AAvantages: string);
begin
  inherited Create(ANom, APrenom, ADateNaissance, ANumero, ASalaire, ADateEmbauche);
  FAvantages := AAvantages;
end;

procedure TEmployePermanent.Afficher;
begin
  inherited Afficher;
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ EMPLOYE PERMANENT');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ Avantages : ', FAvantages);
end;

{ === Implémentation TEmployeTemporaire === }

constructor TEmployeTemporaire.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                                      ANumero: Integer; ASalaire: Real;
                                      ADateEmbauche, ADateFin: TDateTime);
begin
  inherited Create(ANom, APrenom, ADateNaissance, ANumero, ASalaire, ADateEmbauche);
  FDateFin := ADateFin;
end;

procedure TEmployeTemporaire.Afficher;
begin
  inherited Afficher;
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ EMPLOYE TEMPORAIRE');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ Date fin : ', DateToStr(FDateFin));
  WriteLn('║ Statut : ', IfThen(EstEnCours, 'EN COURS', 'TERMINE'));
end;

function TEmployeTemporaire.EstEnCours: Boolean;
begin
  Result := Now < FDateFin;
end;

{ === Implémentation TClientParticulier === }

constructor TClientParticulier.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                                      ANumeroClient: string; AFidelite: Boolean);
begin
  inherited Create(ANom, APrenom, ADateNaissance, ANumeroClient);
  FProgrammeFidelite := AFidelite;
end;

procedure TClientParticulier.Afficher;
begin
  inherited Afficher;
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ CLIENT PARTICULIER');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ Programme fidélité : ', IfThen(FProgrammeFidelite, 'OUI', 'NON'));
end;

function TClientParticulier.GetRemise: Real;
begin
  if FProgrammeFidelite then
    Result := 5.0  // 5% de remise
  else
    Result := 0;
end;

{ === Implémentation TClientEntreprise === }

constructor TClientEntreprise.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                                     ANumeroClient, ARaisonSociale, ASiret: string);
begin
  inherited Create(ANom, APrenom, ADateNaissance, ANumeroClient);
  FRaisonSociale := ARaisonSociale;
  FSiret := ASiret;
end;

procedure TClientEntreprise.Afficher;
begin
  inherited Afficher;
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ CLIENT ENTREPRISE');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ Raison sociale : ', FRaisonSociale);
  WriteLn('║ SIRET : ', FSiret);
end;

function TClientEntreprise.GetRemise: Real;
begin
  if FMontantAchats > 10000 then
    Result := 15.0  // 15% si > 10k€
  else
    Result := 10.0;  // 10% par défaut
end;

{ === Implémentation TManager === }

constructor TManager.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                            ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime;
                            AAvantages: string; ATailleEquipe: Integer);
begin
  inherited Create(ANom, APrenom, ADateNaissance, ANumero, ASalaire, ADateEmbauche, AAvantages);
  FTailleEquipe := ATailleEquipe;
end;

procedure TManager.Afficher;
begin
  inherited Afficher;
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ MANAGER');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ Taille équipe : ', FTailleEquipe, ' personnes');
  WriteLn('╚════════════════════════════════════════════════');
end;

procedure TManager.AugmenterSalaire(Pourcentage: Real);
begin
  // Bonus supplémentaire pour les managers
  inherited AugmenterSalaire(Pourcentage + 2);
end;

{ === Implémentation TDirecteur === }

constructor TDirecteur.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                              ANumero: Integer; ASalaire: Real; ADateEmbauche: TDateTime;
                              AAvantages, ADepartement: string);
begin
  inherited Create(ANom, APrenom, ADateNaissance, ANumero, ASalaire, ADateEmbauche, AAvantages);
  FDepartement := ADepartement;
end;

procedure TDirecteur.Afficher;
begin
  inherited Afficher;
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ DIRECTEUR');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ Département : ', FDepartement);
  WriteLn('╚════════════════════════════════════════════════');
end;

{ === Implémentation TStagiaire === }

constructor TStagiaire.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                              ANumero: Integer; ASalaire: Real;
                              ADateEmbauche, ADateFin: TDateTime; AEcole: string);
begin
  inherited Create(ANom, APrenom, ADateNaissance, ANumero, ASalaire, ADateEmbauche, ADateFin);
  FEcole := AEcole;
end;

procedure TStagiaire.Afficher;
begin
  inherited Afficher;
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ STAGIAIRE');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ École : ', FEcole);
  WriteLn('╚════════════════════════════════════════════════');
end;

{ === Implémentation TInterimaire === }

constructor TInterimaire.Create(ANom, APrenom: string; ADateNaissance: TDateTime;
                                ANumero: Integer; ASalaire: Real;
                                ADateEmbauche, ADateFin: TDateTime; AAgence: string);
begin
  inherited Create(ANom, APrenom, ADateNaissance, ANumero, ASalaire, ADateEmbauche, ADateFin);
  FAgence := AAgence;
end;

procedure TInterimaire.Afficher;
begin
  inherited Afficher;
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ INTERIMAIRE');
  WriteLn('╠════════════════════════════════════════════════');
  WriteLn('║ Agence : ', FAgence);
  WriteLn('╚════════════════════════════════════════════════');
end;

{ === Fonctions utilitaires === }

procedure AfficherHierarchie;
begin
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('               HIERARCHIE DES CLASSES');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;
  WriteLn('TPersonne (racine)');
  WriteLn('  ├── TEmploye');
  WriteLn('  │   ├── TEmployePermanent');
  WriteLn('  │   │   ├── TManager');
  WriteLn('  │   │   └── TDirecteur');
  WriteLn('  │   └── TEmployeTemporaire');
  WriteLn('  │       ├── TStagiaire');
  WriteLn('  │       └── TInterimaire');
  WriteLn('  └── TClient');
  WriteLn('      ├── TClientParticulier');
  WriteLn('      └── TClientEntreprise');
  WriteLn;
end;

procedure AnalyserPersonnes(Personnes: array of TPersonne);
var
  i: Integer;
  NbEmployes, NbClients, NbManagers: Integer;
begin
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('               ANALYSE DE LA COLLECTION');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  NbEmployes := 0;
  NbClients := 0;
  NbManagers := 0;

  for i := 0 to High(Personnes) do
  begin
    if Personnes[i] is TManager then
      Inc(NbManagers);

    if Personnes[i] is TEmploye then
      Inc(NbEmployes);

    if Personnes[i] is TClient then
      Inc(NbClients);
  end;

  WriteLn('Total personnes : ', Length(Personnes));
  WriteLn('  - Employés : ', NbEmployes);
  WriteLn('    dont Managers : ', NbManagers);
  WriteLn('  - Clients : ', NbClients);
  WriteLn;
end;

{ === Programme principal === }
var
  Manager: TManager;
  Directeur: TDirecteur;
  Stagiaire: TStagiaire;
  Interimaire: TInterimaire;
  ClientPart: TClientParticulier;
  ClientEntr: TClientEntreprise;

  Personnes: array[0..5] of TPersonne;
begin
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('        DEMONSTRATION DES HIERARCHIES DE CLASSES');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  AfficherHierarchie;

  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('               CREATION DES OBJETS');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  Manager := TManager.Create('Dupont', 'Marie', EncodeDate(1985, 3, 15),
                             1001, 4500, EncodeDate(2015, 1, 10),
                             'Voiture, Tickets resto', 8);

  Directeur := TDirecteur.Create('Martin', 'Pierre', EncodeDate(1975, 7, 22),
                                 2001, 7000, EncodeDate(2010, 5, 1),
                                 'Voiture, Tickets resto, Stock-options',
                                 'Informatique');

  Stagiaire := TStagiaire.Create('Durand', 'Sophie', EncodeDate(2002, 11, 8),
                                 3001, 600, EncodeDate(2024, 9, 1),
                                 EncodeDate(2025, 2, 28), 'ENSIMAG');

  Interimaire := TInterimaire.Create('Lefebvre', 'Thomas', EncodeDate(1990, 4, 12),
                                     4001, 1800, EncodeDate(2024, 10, 1),
                                     EncodeDate(2024, 12, 31), 'Manpower');

  ClientPart := TClientParticulier.Create('Bernard', 'Julie', EncodeDate(1988, 6, 5),
                                          'CP-001', True);
  ClientPart.AjouterAchat(1500);

  ClientEntr := TClientEntreprise.Create('Rousseau', 'Marc', EncodeDate(1970, 2, 18),
                                         'CE-001', 'TechCorp SAS', '12345678901234');
  ClientEntr.AjouterAchat(15000);

  WriteLn('✓ Tous les objets créés');
  WriteLn;

  // Stockage polymorphe
  Personnes[0] := Manager;
  Personnes[1] := Directeur;
  Personnes[2] := Stagiaire;
  Personnes[3] := Interimaire;
  Personnes[4] := ClientPart;
  Personnes[5] := ClientEntr;

  // Analyse
  AnalyserPersonnes(Personnes);

  // Affichage détaillé de chaque personne
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('               DETAILS DE CHAQUE PERSONNE');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  Manager.Afficher;
  WriteLn;

  Directeur.Afficher;
  WriteLn;

  Stagiaire.Afficher;
  WriteLn;

  Interimaire.Afficher;
  WriteLn;

  ClientPart.Afficher;
  WriteLn;

  ClientEntr.Afficher;
  WriteLn;

  // Test d'augmentation de salaire
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('               TEST AUGMENTATION SALAIRE');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  WriteLn('Manager (avant) : ', Manager.Salaire:0:2, ' €');
  Manager.AugmenterSalaire(10);  // +2% bonus = 12% au total
  WriteLn('Manager (après) : ', Manager.Salaire:0:2, ' €');
  WriteLn;

  // Libération
  Manager.Free;
  Directeur.Free;
  Stagiaire.Free;
  Interimaire.Free;
  ClientPart.Free;
  ClientEntr.Free;

  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
