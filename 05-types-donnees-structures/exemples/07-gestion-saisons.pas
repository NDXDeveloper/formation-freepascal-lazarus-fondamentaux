{ ============================================================================
  Section 5.7 : Types enumeres
  Description : Gestion des saisons avec types enumeres TMois et TSaison
  Fichier source : 07-types-enumeres.md
  ============================================================================ }
program GestionSaisons;  
type  
  TSaison = (Printemps, Ete, Automne, Hiver);
  TMois = (Janvier, Fevrier, Mars, Avril, Mai, Juin,
           Juillet, Aout, Septembre, Octobre, Novembre, Decembre);

function ObtenirSaison(mois: TMois): TSaison;  
begin  
  case mois of
    Mars, Avril, Mai:
      ObtenirSaison := Printemps;
    Juin, Juillet, Aout:
      ObtenirSaison := Ete;
    Septembre, Octobre, Novembre:
      ObtenirSaison := Automne;
    Decembre, Janvier, Fevrier:
      ObtenirSaison := Hiver;
  end;
end;

procedure DecrireSaison(s: TSaison);  
begin  
  case s of
    Printemps:
      WriteLn('C''est le printemps : les fleurs éclosent');
    Ete:
      WriteLn('C''est l''été : il fait chaud');
    Automne:
      WriteLn('C''est l''automne : les feuilles tombent');
    Hiver:
      WriteLn('C''est l''hiver : il fait froid');
  end;
end;

var
  mois: TMois;
  saison: TSaison;
begin
  mois := Juillet;
  saison := ObtenirSaison(mois);

  WriteLn('Nous sommes en juillet');
  DecrireSaison(saison);
end.
