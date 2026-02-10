{ ============================================================================
  Section 7.2 : Structure d'une unite (interface/implementation)
  Description : Unite de calculs sur les cercles (perimetre, surface)
  Fichier source : 02-structure-unite-interface-implementation.md
  ============================================================================ }
{$mode objfpc}{$H+}
unit UniteCercles;

interface

const
  PI_APPROX = 3.14159;

// Déclarations publiques
function CalculerPerimetre(rayon: Real): Real;  
function CalculerSurface(rayon: Real): Real;  
function EstRayonValide(rayon: Real): Boolean;  

implementation

// Implémentation des fonctions publiques
function CalculerPerimetre(rayon: Real): Real;  
begin  
  Result := 2 * Pi * rayon;
end;

function CalculerSurface(rayon: Real): Real;  
begin  
  Result := Pi * rayon * rayon;
end;

function EstRayonValide(rayon: Real): Boolean;  
begin  
  Result := rayon > 0;
end;

// Fonction privée (non déclarée dans interface)
function MessageErreur: String;  
begin  
  Result := 'Le rayon doit être positif !';
end;

end.
