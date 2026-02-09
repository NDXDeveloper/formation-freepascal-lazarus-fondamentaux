{ ============================================================================
  Section 2.7 : Formatage de sortie
  Description : Facture avec sous-totaux et ligne de separation
  Fichier source : 07-formatage-sortie.md
  ============================================================================ }
program TableauTotaux;
const
  Separateur = '================================';
var
  prix1, prix2, prix3, total: real;
begin
  prix1 := 25.99;
  prix2 := 15.50;
  prix3 := 199.99;
  total := prix1 + prix2 + prix3;

  WriteLn(Separateur);
  WriteLn('        FACTURE');
  WriteLn(Separateur);
  WriteLn('Article':20, 'Prix':12);
  WriteLn('-------':20, '----':12);
  WriteLn('Clavier':20, prix1:12:2);
  WriteLn('Souris':20, prix2:12:2);
  WriteLn('Écran':20, prix3:12:2);
  WriteLn('-------':20, '--------':12);
  WriteLn('TOTAL':20, total:12:2);
  WriteLn(Separateur);
end.
