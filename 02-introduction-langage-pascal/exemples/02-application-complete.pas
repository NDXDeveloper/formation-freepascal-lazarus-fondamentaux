{ ============================================================================
  Section 2.2 : Structure d'un programme Pascal
  Description : Structure complete avec constantes, variables, procedure
                et programme principal
  Fichier source : 02-structure-programme-pascal.md
  ============================================================================ }
program ApplicationComplete;

const
  NomApplication = 'Ma Super App';
  Version = '1.0';

var
  choix: integer;
  continuer: boolean;

procedure AfficherMenu;  
begin  
  writeln('=== ', NomApplication, ' v', Version, ' ===');
  writeln('1. Option 1');
  writeln('2. Option 2');
  writeln('0. Quitter');
end;

begin
  // Programme principal
  continuer := true;

  AfficherMenu;
  write('Votre choix : ');
  readln(choix);

  // En Pascal, on double l'apostrophe ('') pour en afficher une dans un texte
  writeln('Vous avez choisi l''option : ', choix);

  writeln('Fin du programme');
end.
