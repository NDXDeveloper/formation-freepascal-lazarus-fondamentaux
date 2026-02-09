{ ============================================================================
  Section 4.5 : Parametres constants (const)
  Description : Comparaison des trois types de parametres (valeur, var, const)
  Fichier source : 05-parametres-constants-const.md
  ============================================================================ }
program ComparaisonParametres;

// 1. Par valeur (copie)
procedure ParValeur(texte: String);
begin
  WriteLn('Par valeur : ', texte);
  texte := 'Modifié';  // Possible mais n'affecte pas l'original
end;

// 2. Par référence (var)
procedure ParReference(var texte: String);
begin
  WriteLn('Par référence : ', texte);
  texte := 'Modifié';  // Possible et affecte l'original
end;

// 3. Constant (const)
procedure ParConstant(const texte: String);
begin
  WriteLn('Par constant : ', texte);
  // texte := 'Modifié';  // ERREUR de compilation !
end;

var
  message: String;
begin
  message := 'Original';

  ParValeur(message);
  WriteLn('Après ParValeur : ', message);      // Original

  ParReference(message);
  WriteLn('Après ParReference : ', message);   // Modifié

  message := 'Original';  // Réinitialisation
  ParConstant(message);
  WriteLn('Après ParConstant : ', message);    // Original
end.
