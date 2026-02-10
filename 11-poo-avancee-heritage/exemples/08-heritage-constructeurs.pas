{ ============================================================================
  Section 11.8 : Inherited et appel au parent
  Description : Demonstration de inherited avec TPersonne/TEmploye/TManager
  Fichier source : 08-inherited-appel-parent.md
  ============================================================================ }
program HeritageConstructeurs;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Classe de base : Personne }
  TPersonne = class
  private
    FNom: string;
    FPrenom: string;
    FAge: Integer;
  public
    constructor Create(ANom, APrenom: string; AAge: Integer);
    destructor Destroy; override;
    procedure Afficher; virtual;
  end;

  { Classe dérivée : Employé }
  TEmploye = class(TPersonne)
  private
    FNumeroEmploye: Integer;
    FSalaire: Real;
  public
    constructor Create(ANom, APrenom: string; AAge: Integer;
                       ANumero: Integer; ASalaire: Real);
    destructor Destroy; override;
    procedure Afficher; override;
    procedure AugmenterSalaire(Pourcentage: Real);
  end;

  { Classe dérivée de niveau 2 : Manager }
  TManager = class(TEmploye)
  private
    FEquipe: Integer;  // Nombre de personnes dans l'équipe
  public
    constructor Create(ANom, APrenom: string; AAge: Integer;
                       ANumero: Integer; ASalaire: Real; AEquipe: Integer);
    destructor Destroy; override;
    procedure Afficher; override;
  end;

{ === TPersonne === }

constructor TPersonne.Create(ANom, APrenom: string; AAge: Integer);  
begin  
  inherited Create;  // Appelle TObject.Create
  WriteLn('[TPersonne.Create] Début');

  FNom := ANom;
  FPrenom := APrenom;
  FAge := AAge;

  WriteLn('[TPersonne.Create] ', APrenom, ' ', ANom, ' créé(e)');
end;

destructor TPersonne.Destroy;  
begin  
  WriteLn('[TPersonne.Destroy] ', FPrenom, ' ', FNom, ' détruit(e)');
  inherited Destroy;  // Appelle TObject.Destroy
end;

procedure TPersonne.Afficher;  
begin  
  WriteLn('=== PERSONNE ===');
  WriteLn('Nom : ', FNom);
  WriteLn('Prénom : ', FPrenom);
  WriteLn('Age : ', FAge, ' ans');
end;

{ === TEmploye === }

constructor TEmploye.Create(ANom, APrenom: string; AAge: Integer;
                            ANumero: Integer; ASalaire: Real);
begin
  WriteLn('[TEmploye.Create] Début');

  // TOUJOURS appeler inherited en premier !
  inherited Create(ANom, APrenom, AAge);

  // Maintenant, initialiser la partie TEmploye
  FNumeroEmploye := ANumero;
  FSalaire := ASalaire;

  WriteLn('[TEmploye.Create] Employé #', ANumero, ' créé avec salaire ', ASalaire:0:2);
end;

destructor TEmploye.Destroy;  
begin  
  WriteLn('[TEmploye.Destroy] Employé #', FNumeroEmploye);

  // Dans le destructeur, le code spécifique EN PREMIER
  // Puis appel au parent
  inherited Destroy;
end;

procedure TEmploye.Afficher;  
begin  
  // Appelle la version parent pour afficher les infos de base
  inherited Afficher;

  // Ajoute les infos spécifiques
  WriteLn('Numéro employé : ', FNumeroEmploye);
  WriteLn('Salaire : ', FSalaire:0:2, ' €');
end;

procedure TEmploye.AugmenterSalaire(Pourcentage: Real);  
begin  
  FSalaire := FSalaire * (1 + Pourcentage / 100);
  WriteLn('Salaire augmenté de ', Pourcentage:0:1, '% → ', FSalaire:0:2, ' €');
end;

{ === TManager === }

constructor TManager.Create(ANom, APrenom: string; AAge: Integer;
                            ANumero: Integer; ASalaire: Real; AEquipe: Integer);
begin
  WriteLn('[TManager.Create] Début');

  // Appelle TEmploye.Create qui lui-même appelle TPersonne.Create
  inherited Create(ANom, APrenom, AAge, ANumero, ASalaire);

  FEquipe := AEquipe;

  WriteLn('[TManager.Create] Manager créé avec équipe de ', AEquipe, ' personnes');
end;

destructor TManager.Destroy;  
begin  
  WriteLn('[TManager.Destroy] Manager avec équipe de ', FEquipe);
  inherited Destroy;
end;

procedure TManager.Afficher;  
begin  
  // Appelle TEmploye.Afficher qui appelle TPersonne.Afficher
  inherited Afficher;

  WriteLn('Taille équipe : ', FEquipe, ' personnes');
  WriteLn('Rôle : MANAGER');
end;

{ === Programme principal === }
var
  Manager: TManager;
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('   DEMONSTRATION DE INHERITED');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  WriteLn('--- CREATION D''UN MANAGER ---');
  WriteLn;
  Manager := TManager.Create('Dupont', 'Marie', 35, 1001, 4500.00, 8);
  WriteLn;

  WriteLn('--- AFFICHAGE ---');
  WriteLn;
  Manager.Afficher;
  WriteLn;

  WriteLn('--- AUGMENTATION DE SALAIRE ---');
  WriteLn;
  Manager.AugmenterSalaire(10);
  WriteLn;

  WriteLn('--- DESTRUCTION ---');
  WriteLn;
  Manager.Free;
  WriteLn;

  WriteLn('═══════════════════════════════════════════════');
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
