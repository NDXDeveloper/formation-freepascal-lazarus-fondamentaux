{ ============================================================================
  Section 6.2 : Declaration et Utilisation de Pointeurs
  Description : Partage de donnees entre fonctions via pointeurs (configuration)
  Fichier source : 02-declaration-utilisation-pointeurs.md
  ============================================================================ }
program PartageDonnees;  
type  
  TConfiguration = record
    serveur: String;
    port: Integer;
    timeout: Integer;
  end;
  PConfiguration = ^TConfiguration;

procedure AfficherConfig(cfg: PConfiguration);  
begin  
  if cfg <> nil then
  begin
    WriteLn('Serveur : ', cfg^.serveur);
    WriteLn('Port : ', cfg^.port);
  end;
end;

procedure ModifierPort(cfg: PConfiguration; nouveauPort: Integer);  
begin  
  if cfg <> nil then
    cfg^.port := nouveauPort;
end;

var
  config: TConfiguration;
begin
  config.serveur := 'localhost';
  config.port := 8080;

  AfficherConfig(@config);
  ModifierPort(@config, 9090);
  AfficherConfig(@config);
end.
