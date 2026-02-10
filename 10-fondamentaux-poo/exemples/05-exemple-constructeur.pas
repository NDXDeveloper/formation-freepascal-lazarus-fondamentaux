{ ============================================================================
  Section 10.5 : Constructeurs (Create)
  Description : Classe TArticle avec surcharge de constructeurs
  Fichier source : 05-constructeurs-create.md
  ============================================================================ }
program ExempleConstructeur;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TArticle = class
  private
    FCode: string;
    FLibelle: string;
    FPrixHT: Real;
    FQuantiteStock: Integer;
    FTauxTVA: Real;

    // Méthode privée de validation
    function PrixValide(Prix: Real): Boolean;

  public
    // Plusieurs constructeurs
    constructor Create; overload;
    constructor Create(const Code, Libelle: string); overload;
    constructor Create(const Code, Libelle: string; PrixHT: Real; Quantite: Integer); overload;

    // Autres méthodes
    function CalculerPrixTTC: Real;
    procedure Afficher;
  end;

// === IMPLÉMENTATION ===

function TArticle.PrixValide(Prix: Real): Boolean;  
begin  
  Result := Prix >= 0;
end;

// Constructeur par défaut
constructor TArticle.Create;  
begin  
  inherited Create;
  FCode := 'ART000';
  FLibelle := 'Article sans nom';
  FPrixHT := 0;
  FQuantiteStock := 0;
  FTauxTVA := 0.20;  // 20% par défaut
  WriteLn('Article créé avec valeurs par défaut');
end;

// Constructeur avec code et libellé
constructor TArticle.Create(const Code, Libelle: string);  
begin  
  inherited Create;
  FCode := Code;
  FLibelle := Libelle;
  FPrixHT := 0;
  FQuantiteStock := 0;
  FTauxTVA := 0.20;
  WriteLn('Article créé : ', Code);
end;

// Constructeur complet
constructor TArticle.Create(const Code, Libelle: string; PrixHT: Real; Quantite: Integer);  
begin  
  inherited Create;
  FCode := Code;
  FLibelle := Libelle;

  // Validation du prix
  if PrixValide(PrixHT) then
    FPrixHT := PrixHT
  else
  begin
    WriteLn('Attention : prix invalide, initialisé à 0');
    FPrixHT := 0;
  end;

  // Validation de la quantité
  if Quantite >= 0 then
    FQuantiteStock := Quantite
  else
  begin
    WriteLn('Attention : quantité invalide, initialisée à 0');
    FQuantiteStock := 0;
  end;

  FTauxTVA := 0.20;
  WriteLn('Article créé : ', Code, ' - ', Libelle);
end;

function TArticle.CalculerPrixTTC: Real;  
begin  
  Result := FPrixHT * (1 + FTauxTVA);
end;

procedure TArticle.Afficher;  
begin  
  WriteLn('=== Article ===');
  WriteLn('Code        : ', FCode);
  WriteLn('Libellé     : ', FLibelle);
  WriteLn('Prix HT     : ', FPrixHT:0:2, ' €');
  WriteLn('Prix TTC    : ', CalculerPrixTTC:0:2, ' €');
  WriteLn('Stock       : ', FQuantiteStock);
  WriteLn('===============');
end;

// === PROGRAMME PRINCIPAL ===

var
  Article1, Article2, Article3: TArticle;
begin
  WriteLn('--- Création avec constructeur par défaut ---');
  Article1 := TArticle.Create;
  Article1.Afficher;
  WriteLn;

  WriteLn('--- Création avec code et libellé ---');
  Article2 := TArticle.Create('ART001', 'Clavier mécanique');
  Article2.Afficher;
  WriteLn;

  WriteLn('--- Création complète ---');
  Article3 := TArticle.Create('ART002', 'Souris sans fil', 29.99, 50);
  Article3.Afficher;
  WriteLn;

  // Libération de la mémoire
  Article1.Free;
  Article2.Free;
  Article3.Free;

  WriteLn('Programme terminé');
end.
