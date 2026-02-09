{ ============================================================================
  Section 12.1 : Concept d'interface
  Description : Exemple simple d'interface IVolant avec TOiseau et TAvion
  Fichier source : 01-concept-interface.md
  ============================================================================ }
program ExempleInterface;

{$mode objfpc}{$H+}

type
  // Déclaration de l'interface
  IVolant = interface
    procedure Voler;
    procedure Atterrir;
  end;

  // Un oiseau qui vole
  // TInterfacedObject gère le comptage de références (pas besoin de Free)
  TOiseau = class(TInterfacedObject, IVolant)
    procedure Voler;
    procedure Atterrir;
  end;

  // Un avion qui vole aussi
  TAvion = class(TInterfacedObject, IVolant)
    procedure Voler;
    procedure Atterrir;
  end;

// Implémentation de TOiseau
procedure TOiseau.Voler;
begin
  WriteLn('L''oiseau bat des ailes et s''envole gracieusement');
end;

procedure TOiseau.Atterrir;
begin
  WriteLn('L''oiseau se pose sur une branche');
end;

// Implémentation de TAvion
procedure TAvion.Voler;
begin
  WriteLn('L''avion décolle avec ses réacteurs');
end;

procedure TAvion.Atterrir;
begin
  WriteLn('L''avion se pose sur la piste');
end;

// Procédure qui utilise n'importe quel objet volant
procedure FaireVoler(UnVolant: IVolant);
begin
  WriteLn('--- Démarrage du vol ---');
  UnVolant.Voler;
  UnVolant.Atterrir;
  WriteLn('');
end;

var
  MonOiseau: IVolant;  // Type interface : active le comptage de références
  MonAvion: IVolant;
begin
  MonOiseau := TOiseau.Create;
  MonAvion := TAvion.Create;

  // La magie : on utilise la même procédure pour les deux !
  FaireVoler(MonOiseau);
  FaireVoler(MonAvion);

  // Pas besoin de Free : gestion automatique via les interfaces !
end.
