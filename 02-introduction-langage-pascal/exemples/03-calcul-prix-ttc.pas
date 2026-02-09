{ ============================================================================
  Section 2.3 : Variables et constantes
  Description : Calcul du prix TTC a partir du prix HT et du taux de TVA
  Fichier source : 03-variables-constantes.md
  ============================================================================ }
program CalculPrixTTC;

const
  TauxTVA = 20.0;  // En pourcentage

var
  prixHT: real;
  montantTVA: real;
  prixTTC: real;

begin
  // Saisie du prix HT
  prixHT := 100.0;

  // Calculs
  montantTVA := prixHT * TauxTVA / 100;
  prixTTC := prixHT + montantTVA;

  // Affichage des résultats
  writeln('Prix HT : ', prixHT:0:2, ' €');
  // :0:0 = 0 decimales (affiche le reel comme un entier)
  writeln('TVA (', TauxTVA:0:0, '%) : ', montantTVA:0:2, ' €');
  writeln('Prix TTC : ', prixTTC:0:2, ' €');
end.
