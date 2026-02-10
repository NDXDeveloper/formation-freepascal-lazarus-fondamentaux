{ ============================================================================
  Section 2.5 : Operateurs arithmetiques et logiques
  Description : Calcul de remise selon le montant d'achat avec des seuils
  Fichier source : 05-operateurs-arithmetiques-logiques.md
  ============================================================================ }
program CalculRemise;  
const  
  SeuilRemise1 = 50.0;
  SeuilRemise2 = 100.0;
  TauxRemise1 = 5.0;   // 5%
  TauxRemise2 = 10.0;  // 10%
var
  montantAchat: real;
  remise: real;
  montantFinal: real;
  aRemise: boolean;
begin
  montantAchat := 75.0;

  // Calcul de la remise selon le montant
  if montantAchat >= SeuilRemise2 then
    remise := montantAchat * TauxRemise2 / 100
  else if montantAchat >= SeuilRemise1 then
    remise := montantAchat * TauxRemise1 / 100
  else
    remise := 0;

  montantFinal := montantAchat - remise;
  aRemise := remise > 0;

  writeln('=== CALCUL DE REMISE ===');
  writeln('Montant d''achat : ', montantAchat:0:2, ' €');
  writeln('Remise : ', remise:0:2, ' €');
  writeln('Montant final : ', montantFinal:0:2, ' €');
  writeln('Remise appliquée : ', aRemise);
end.
