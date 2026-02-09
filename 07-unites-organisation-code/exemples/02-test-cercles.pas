{ ============================================================================
  Section 7.2 : Structure d'une unite (interface/implementation)
  Description : Programme utilisant l'unite UniteCercles
  Fichier source : 02-structure-unite-interface-implementation.md
  ============================================================================ }
program TestCercles;

uses
  UniteCercles;

var
  r, perimetre, surface: Real;

begin
  WriteLn('Entrez le rayon du cercle :');
  ReadLn(r);

  if EstRayonValide(r) then
  begin
    perimetre := CalculerPerimetre(r);
    surface := CalculerSurface(r);

    WriteLn('Périmètre : ', perimetre:0:2);
    WriteLn('Surface : ', surface:0:2);
  end
  else
    WriteLn('Rayon invalide !');
end.
