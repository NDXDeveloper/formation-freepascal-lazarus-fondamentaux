{ ============================================================================
  Section 10.3 : Declaration de classes
  Description : Classe TRectangle avec bonnes pratiques de declaration
  Fichier source : 03-declaration-classes.md
  ============================================================================ }
program ExempleRectangle;

{$mode objfpc}{$H+}

type
  TRectangle = class
  private
    // Attributs privés
    FLargeur: Real;
    FHauteur: Real;

    // Méthode privée (helper interne)
    function ValiderDimension(Valeur: Real): Boolean;

  public
    // Méthodes d'accès (setters)
    procedure DefinirLargeur(Valeur: Real);
    procedure DefinirHauteur(Valeur: Real);

    // Méthodes d'accès (getters)
    function ObtenirLargeur: Real;
    function ObtenirHauteur: Real;

    // Méthodes de calcul
    function CalculerSurface: Real;
    function CalculerPerimetre: Real;

    // Méthodes utilitaires
    function EstCarre: Boolean;
    procedure Afficher;
    procedure Redimensionner(FacteurEchelle: Real);
  end;

// === IMPLÉMENTATION ===

function TRectangle.ValiderDimension(Valeur: Real): Boolean;  
begin  
  Result := Valeur > 0;
end;

procedure TRectangle.DefinirLargeur(Valeur: Real);  
begin  
  if ValiderDimension(Valeur) then
    FLargeur := Valeur
  else
    WriteLn('Erreur : largeur invalide');
end;

procedure TRectangle.DefinirHauteur(Valeur: Real);  
begin  
  if ValiderDimension(Valeur) then
    FHauteur := Valeur
  else
    WriteLn('Erreur : hauteur invalide');
end;

function TRectangle.ObtenirLargeur: Real;  
begin  
  Result := FLargeur;
end;

function TRectangle.ObtenirHauteur: Real;  
begin  
  Result := FHauteur;
end;

function TRectangle.CalculerSurface: Real;  
begin  
  Result := FLargeur * FHauteur;
end;

function TRectangle.CalculerPerimetre: Real;  
begin  
  Result := 2 * (FLargeur + FHauteur);
end;

function TRectangle.EstCarre: Boolean;  
begin  
  Result := FLargeur = FHauteur;
end;

procedure TRectangle.Afficher;  
begin  
  WriteLn('=== Rectangle ===');
  WriteLn('Largeur : ', FLargeur:0:2, ' cm');
  WriteLn('Hauteur : ', FHauteur:0:2, ' cm');
  WriteLn('Surface : ', CalculerSurface:0:2, ' cm²');
  WriteLn('Périmètre : ', CalculerPerimetre:0:2, ' cm');
  if EstCarre then
    WriteLn('C''est un carré !');
  WriteLn('================');
end;

procedure TRectangle.Redimensionner(FacteurEchelle: Real);  
begin  
  if ValiderDimension(FacteurEchelle) then
  begin
    FLargeur := FLargeur * FacteurEchelle;
    FHauteur := FHauteur * FacteurEchelle;
  end;
end;

// === PROGRAMME PRINCIPAL ===

var
  MonRectangle: TRectangle;
begin
  MonRectangle := TRectangle.Create;
  try
    MonRectangle.DefinirLargeur(10);
    MonRectangle.DefinirHauteur(5);
    MonRectangle.Afficher;

    WriteLn;
    WriteLn('Redimensionnement x2...');
    MonRectangle.Redimensionner(2);
    MonRectangle.Afficher;
  finally
    MonRectangle.Free;
  end;
end.
