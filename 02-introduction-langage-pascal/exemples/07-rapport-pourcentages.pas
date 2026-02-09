{ ============================================================================
  Section 2.7 : Formatage de sortie
  Description : Rapport de ventes par region avec pourcentages
  Fichier source : 07-formatage-sortie.md
  ============================================================================ }
program RapportPourcentages;
var
  totalVentes, ventes1, ventes2, ventes3: real;
  pct1, pct2, pct3: real;
begin
  ventes1 := 15000;
  ventes2 := 25000;
  ventes3 := 10000;
  totalVentes := ventes1 + ventes2 + ventes3;

  pct1 := (ventes1 / totalVentes) * 100;
  pct2 := (ventes2 / totalVentes) * 100;
  pct3 := (ventes3 / totalVentes) * 100;

  WriteLn;
  WriteLn('========================================');
  WriteLn('        RAPPORT DES VENTES');
  WriteLn('========================================');
  WriteLn;
  WriteLn('Région':-15, 'Ventes':15, 'Part':10);
  WriteLn('--------------':-15, '--------------':15, '---------':10);
  WriteLn('Nord':-15, ventes1:15:2, pct1:9:1, '%');
  WriteLn('Sud':-15, ventes2:15:2, pct2:9:1, '%');
  WriteLn('Est':-15, ventes3:15:2, pct3:9:1, '%');
  WriteLn('--------------':-15, '--------------':15, '---------':10);
  WriteLn('TOTAL':-15, totalVentes:15:2, 100.0:9:1, '%');
  WriteLn('========================================');
end.
