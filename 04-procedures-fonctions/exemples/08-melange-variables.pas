{ ============================================================================
  Section 4.8 : Variables locales vs globales
  Description : Melange de variables locales et globales
  Fichier source : 08-variables-locales-vs-globales.md
  ============================================================================ }
program MelangeVariables;

var
  total: Integer;  // Globale

procedure AjouterDix;  
var  
  valeur: Integer;  // Locale
begin
  valeur := 10;
  total := total + valeur;
end;

procedure AjouterVingt;  
var  
  valeur: Integer;  // Locale (différente de celle de AjouterDix)
begin
  valeur := 20;
  total := total + valeur;
end;

begin
  total := 0;
  WriteLn('Total initial : ', total);      // 0

  AjouterDix;
  WriteLn('Après AjouterDix : ', total);   // 10

  AjouterVingt;
  WriteLn('Après AjouterVingt : ', total); // 30
end.
