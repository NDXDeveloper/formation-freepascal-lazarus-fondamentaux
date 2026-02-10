{ ============================================================================
  Section 11.5 : Methodes abstraites et classes abstraites
  Description : Hierarchie de formes geometriques avec classe abstraite TForme
  Fichier source : 05-methodes-abstraites-classes-abstraites.md
  ============================================================================ }
program FormesAbstraites;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Classe abstraite : Forme }
  TForme = class
  protected
    FCouleur: string;
  public
    constructor Create(ACouleur: string);

    // Méthodes abstraites : DOIVENT être implémentées
    function CalculerAire: Real; virtual; abstract;
    function CalculerPerimetre: Real; virtual; abstract;
    procedure Dessiner; virtual; abstract;

    // Méthode concrète : peut être utilisée telle quelle
    procedure AfficherInfos; virtual;

    property Couleur: string read FCouleur write FCouleur;
  end;

  { Rectangle : implémentation concrète }
  TRectangle = class(TForme)
  private
    FLargeur: Real;
    FHauteur: Real;
  public
    constructor Create(ACouleur: string; ALargeur, AHauteur: Real);

    // Implémentation OBLIGATOIRE des méthodes abstraites
    function CalculerAire: Real; override;
    function CalculerPerimetre: Real; override;
    procedure Dessiner; override;

    property Largeur: Real read FLargeur;
    property Hauteur: Real read FHauteur;
  end;

  { Cercle : implémentation concrète }
  TCercle = class(TForme)
  private
    FRayon: Real;
  public
    constructor Create(ACouleur: string; ARayon: Real);

    // Implémentation OBLIGATOIRE des méthodes abstraites
    function CalculerAire: Real; override;
    function CalculerPerimetre: Real; override;
    procedure Dessiner; override;

    property Rayon: Real read FRayon;
  end;

  { Triangle : implémentation concrète }
  TTriangle = class(TForme)
  private
    FCote1, FCote2, FCote3: Real;
  public
    constructor Create(ACouleur: string; ACote1, ACote2, ACote3: Real);

    // Implémentation OBLIGATOIRE des méthodes abstraites
    function CalculerAire: Real; override;
    function CalculerPerimetre: Real; override;
    procedure Dessiner; override;

    function EstValide: Boolean;
  end;

{ === TForme === }

constructor TForme.Create(ACouleur: string);  
begin  
  inherited Create;
  FCouleur := ACouleur;
  WriteLn('[TForme] Création d''une forme ', ACouleur);
end;

procedure TForme.AfficherInfos;  
begin  
  WriteLn('╔═══════════════════════════════════');
  WriteLn('║ Informations sur la forme');
  WriteLn('╠═══════════════════════════════════');
  WriteLn('║ Couleur : ', FCouleur);
  WriteLn('║ Aire : ', CalculerAire:0:2);  // Appel polymorphe
  WriteLn('║ Périmètre : ', CalculerPerimetre:0:2);  // Appel polymorphe
  WriteLn('╚═══════════════════════════════════');
end;

{ === TRectangle === }

constructor TRectangle.Create(ACouleur: string; ALargeur, AHauteur: Real);  
begin  
  inherited Create(ACouleur);
  FLargeur := ALargeur;
  FHauteur := AHauteur;
  WriteLn('[TRectangle] Dimensions : ', ALargeur:0:2, ' x ', AHauteur:0:2);
end;

function TRectangle.CalculerAire: Real;  
begin  
  Result := FLargeur * FHauteur;
end;

function TRectangle.CalculerPerimetre: Real;  
begin  
  Result := 2 * (FLargeur + FHauteur);
end;

procedure TRectangle.Dessiner;  
begin  
  WriteLn('Dessin d''un rectangle ', FCouleur);
  WriteLn('┌────────────┐');
  WriteLn('│            │');
  WriteLn('│            │');
  WriteLn('└────────────┘');
end;

{ === TCercle === }

constructor TCercle.Create(ACouleur: string; ARayon: Real);  
begin  
  inherited Create(ACouleur);
  FRayon := ARayon;
  WriteLn('[TCercle] Rayon : ', ARayon:0:2);
end;

function TCercle.CalculerAire: Real;  
begin  
  Result := Pi * Sqr(FRayon);
end;

function TCercle.CalculerPerimetre: Real;  
begin  
  Result := 2 * Pi * FRayon;
end;

procedure TCercle.Dessiner;  
begin  
  WriteLn('Dessin d''un cercle ', FCouleur);
  WriteLn('    ****    ');
  WriteLn('  *      *  ');
  WriteLn(' *        * ');
  WriteLn('  *      *  ');
  WriteLn('    ****    ');
end;

{ === TTriangle === }

constructor TTriangle.Create(ACouleur: string; ACote1, ACote2, ACote3: Real);  
begin  
  inherited Create(ACouleur);
  FCote1 := ACote1;
  FCote2 := ACote2;
  FCote3 := ACote3;
  WriteLn('[TTriangle] Côtés : ', ACote1:0:2, ', ', ACote2:0:2, ', ', ACote3:0:2);

  if not EstValide then
    WriteLn('  ⚠️  Attention : triangle invalide (inégalité triangulaire non respectée)');
end;

function TTriangle.EstValide: Boolean;  
begin  
  // Vérification de l'inégalité triangulaire
  Result := (FCote1 + FCote2 > FCote3) and
            (FCote1 + FCote3 > FCote2) and
            (FCote2 + FCote3 > FCote1);
end;

function TTriangle.CalculerAire: Real;  
var  
  S: Real;  // Demi-périmètre
begin
  if not EstValide then
  begin
    Result := 0;
    Exit;
  end;

  // Formule de Héron
  S := CalculerPerimetre / 2;
  Result := Sqrt(S * (S - FCote1) * (S - FCote2) * (S - FCote3));
end;

function TTriangle.CalculerPerimetre: Real;  
begin  
  Result := FCote1 + FCote2 + FCote3;
end;

procedure TTriangle.Dessiner;  
begin  
  WriteLn('Dessin d''un triangle ', FCouleur);
  WriteLn('      /\      ');
  WriteLn('     /  \     ');
  WriteLn('    /    \    ');
  WriteLn('   /______\   ');
end;

{ === Fonctions de traitement polymorphes === }

procedure AfficherDetailsFormes(Formes: array of TForme);  
var  
  i: Integer;
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('   AFFICHAGE DES FORMES');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  for i := 0 to High(Formes) do
  begin
    WriteLn('--- Forme ', i + 1, ' ---');
    Formes[i].Dessiner;
    WriteLn;
    Formes[i].AfficherInfos;
    WriteLn;
  end;
end;

function CalculerAireTotale(Formes: array of TForme): Real;  
var  
  i: Integer;
  Total: Real;
begin
  Total := 0;
  for i := 0 to High(Formes) do
    Total := Total + Formes[i].CalculerAire;  // Appel polymorphe

  Result := Total;
end;

function TrouverPlusGrandeForme(Formes: array of TForme): Integer;  
var  
  i, IndexMax: Integer;
  AireMax, Aire: Real;
begin
  if Length(Formes) = 0 then
  begin
    Result := -1;
    Exit;
  end;

  IndexMax := 0;
  AireMax := Formes[0].CalculerAire;

  for i := 1 to High(Formes) do
  begin
    Aire := Formes[i].CalculerAire;
    if Aire > AireMax then
    begin
      AireMax := Aire;
      IndexMax := i;
    end;
  end;

  Result := IndexMax;
end;

{ === Programme principal === }
var
  Rectangle: TRectangle;
  Cercle: TCercle;
  Triangle: TTriangle;

  MesFormes: array[0..2] of TForme;
  AireTotal: Real;
  IndexMax: Integer;

  // FormeAbstraite: TForme;  // Pour démonstration
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('   DEMONSTRATION DES CLASSES ABSTRAITES');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  // Tentative d'instanciation d'une classe abstraite (commenté car erreur)
  {
  WriteLn('Tentative de création d''une TForme abstraite :');
  FormeAbstraite := TForme.Create('Gris');
  // ❌ ERREUR : "Cannot create instance of abstract class"
  WriteLn;
  }

  WriteLn('Création des formes concrètes :');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  Rectangle := TRectangle.Create('Rouge', 10.0, 5.0);
  WriteLn;

  Cercle := TCercle.Create('Bleu', 7.0);
  WriteLn;

  Triangle := TTriangle.Create('Vert', 6.0, 8.0, 10.0);
  WriteLn;

  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  // Stockage dans un tableau polymorphe
  MesFormes[0] := Rectangle;
  MesFormes[1] := Cercle;
  MesFormes[2] := Triangle;

  // Affichage de toutes les formes
  AfficherDetailsFormes(MesFormes);

  // Calculs sur l'ensemble des formes
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('   STATISTIQUES');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  AireTotal := CalculerAireTotale(MesFormes);
  WriteLn('Aire totale de toutes les formes : ', AireTotal:0:2);
  WriteLn;

  IndexMax := TrouverPlusGrandeForme(MesFormes);
  WriteLn('La plus grande forme est la forme #', IndexMax + 1);
  WriteLn('avec une aire de ', MesFormes[IndexMax].CalculerAire:0:2);
  WriteLn;

  // Libération
  Rectangle.Free;
  Cercle.Free;
  Triangle.Free;

  WriteLn('═══════════════════════════════════════════════');
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
