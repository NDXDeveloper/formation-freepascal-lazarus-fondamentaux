{ ============================================================================
  Section 11.3 : Redefinition de methodes
  Description : Formes geometriques avec polymorphisme (TRectangle, TCercle, TTriangle)
  Fichier source : 03-redefinition-methodes.md
  ============================================================================ }
program FormesGeometriques;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Classe de base abstraite }
  TForme = class
  protected
    FCouleur: string;
    FNom: string;
  public
    constructor Create(ACouleur: string);

    // Méthodes virtuelles à redéfinir
    function CalculerAire: Real; virtual;
    function CalculerPerimetre: Real; virtual;
    procedure Afficher; virtual;

    property Nom: string read FNom;
    property Couleur: string read FCouleur write FCouleur;
  end;

  { Rectangle }
  TRectangle = class(TForme)
  private
    FLargeur, FHauteur: Real;
  public
    constructor Create(ACouleur: string; ALargeur, AHauteur: Real);
    function CalculerAire: Real; override;
    function CalculerPerimetre: Real; override;
    procedure Afficher; override;
  end;

  { Cercle }
  TCercle = class(TForme)
  private
    FRayon: Real;
  public
    constructor Create(ACouleur: string; ARayon: Real);
    function CalculerAire: Real; override;
    function CalculerPerimetre: Real; override;
    procedure Afficher; override;
  end;

  { Triangle }
  TTriangle = class(TForme)
  private
    FCote1, FCote2, FCote3: Real;
  public
    constructor Create(ACouleur: string; ACote1, ACote2, ACote3: Real);
    function CalculerAire: Real; override;
    function CalculerPerimetre: Real; override;
    procedure Afficher; override;
  end;

{ === TForme === }

constructor TForme.Create(ACouleur: string);  
begin  
  inherited Create;
  FCouleur := ACouleur;
  FNom := 'Forme générique';
end;

function TForme.CalculerAire: Real;  
begin  
  Result := 0;
  WriteLn('Aire non définie pour une forme générique');
end;

function TForme.CalculerPerimetre: Real;  
begin  
  Result := 0;
  WriteLn('Périmètre non défini pour une forme générique');
end;

procedure TForme.Afficher;  
begin  
  WriteLn('Forme de couleur ', FCouleur);
end;

{ === TRectangle === }

constructor TRectangle.Create(ACouleur: string; ALargeur, AHauteur: Real);  
begin  
  inherited Create(ACouleur);
  FLargeur := ALargeur;
  FHauteur := AHauteur;
  FNom := 'Rectangle';
end;

function TRectangle.CalculerAire: Real;  
begin  
  Result := FLargeur * FHauteur;
end;

function TRectangle.CalculerPerimetre: Real;  
begin  
  Result := 2 * (FLargeur + FHauteur);
end;

procedure TRectangle.Afficher;  
begin  
  WriteLn('=== RECTANGLE ===');
  WriteLn('Couleur : ', FCouleur);
  WriteLn('Dimensions : ', FLargeur:0:2, ' x ', FHauteur:0:2);
  WriteLn('Aire : ', CalculerAire:0:2);
  WriteLn('Périmètre : ', CalculerPerimetre:0:2);
  WriteLn;
end;

{ === TCercle === }

constructor TCercle.Create(ACouleur: string; ARayon: Real);  
begin  
  inherited Create(ACouleur);
  FRayon := ARayon;
  FNom := 'Cercle';
end;

function TCercle.CalculerAire: Real;  
begin  
  Result := Pi * Sqr(FRayon);
end;

function TCercle.CalculerPerimetre: Real;  
begin  
  Result := 2 * Pi * FRayon;
end;

procedure TCercle.Afficher;  
begin  
  WriteLn('=== CERCLE ===');
  WriteLn('Couleur : ', FCouleur);
  WriteLn('Rayon : ', FRayon:0:2);
  WriteLn('Aire : ', CalculerAire:0:2);
  WriteLn('Périmètre : ', CalculerPerimetre:0:2);
  WriteLn;
end;

{ === TTriangle === }

constructor TTriangle.Create(ACouleur: string; ACote1, ACote2, ACote3: Real);  
begin  
  inherited Create(ACouleur);
  FCote1 := ACote1;
  FCote2 := ACote2;
  FCote3 := ACote3;
  FNom := 'Triangle';
end;

function TTriangle.CalculerAire: Real;  
var  
  S: Real;  // Demi-périmètre
begin
  // Formule de Héron
  S := CalculerPerimetre / 2;
  Result := Sqrt(S * (S - FCote1) * (S - FCote2) * (S - FCote3));
end;

function TTriangle.CalculerPerimetre: Real;  
begin  
  Result := FCote1 + FCote2 + FCote3;
end;

procedure TTriangle.Afficher;  
begin  
  WriteLn('=== TRIANGLE ===');
  WriteLn('Couleur : ', FCouleur);
  WriteLn('Côtés : ', FCote1:0:2, ', ', FCote2:0:2, ', ', FCote3:0:2);
  WriteLn('Aire : ', CalculerAire:0:2);
  WriteLn('Périmètre : ', CalculerPerimetre:0:2);
  WriteLn;
end;

{ === Programme principal === }

procedure AfficherInfosForme(Forme: TForme);  
begin  
  // Grâce au polymorphisme, la bonne méthode Afficher est appelée
  Forme.Afficher;
end;

procedure CalculerAireTotale(Formes: array of TForme);  
var  
  AireTotal: Real;
  i: Integer;
begin
  AireTotal := 0;
  for i := 0 to High(Formes) do
    AireTotal := AireTotal + Formes[i].CalculerAire;

  WriteLn('Aire totale de toutes les formes : ', AireTotal:0:2);
end;

var
  Rectangle: TRectangle;
  Cercle: TCercle;
  Triangle: TTriangle;
  Formes: array[0..2] of TForme;
begin
  WriteLn('=== DEMONSTRATION DES FORMES GEOMETRIQUES ===');
  WriteLn;

  // Création des formes
  Rectangle := TRectangle.Create('Rouge', 5.0, 3.0);
  Cercle := TCercle.Create('Bleu', 4.0);
  Triangle := TTriangle.Create('Vert', 3.0, 4.0, 5.0);

  // Affichage direct
  WriteLn('--- Affichage direct ---');
  Rectangle.Afficher;
  Cercle.Afficher;
  Triangle.Afficher;

  // Démonstration du polymorphisme
  WriteLn('--- Via polymorphisme (type TForme) ---');
  Formes[0] := Rectangle;
  Formes[1] := Cercle;
  Formes[2] := Triangle;

  AfficherInfosForme(Formes[0]);
  AfficherInfosForme(Formes[1]);
  AfficherInfosForme(Formes[2]);

  CalculerAireTotale(Formes);

  // Libération
  Rectangle.Free;
  Cercle.Free;
  Triangle.Free;

  ReadLn;
end.
