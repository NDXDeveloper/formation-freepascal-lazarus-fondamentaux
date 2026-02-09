{ ============================================================================
  Section 11.6 : Polymorphisme
  Description : Systeme de fichiers polymorphe (TFichier, TDossier)
  Fichier source : 06-polymorphisme.md
  ============================================================================ }
program SystemeFichiers;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  { Classe de base : Élément du système de fichiers }
  TElementFS = class
  protected
    FNom: string;
    FTaille: Int64;  // en octets
    FDateCreation: TDateTime;
  public
    constructor Create(ANom: string);
    function GetNom: string;
    function GetTaille: Int64; virtual;  // Polymorphe !
    function GetTailleFormatee: string;
    procedure Afficher; virtual;
    procedure Renommer(NouveauNom: string);
  end;

  { Fichier }
  TFichier = class(TElementFS)
  private
    FExtension: string;
  public
    constructor Create(ANom: string; ATaille: Int64);
    function GetTaille: Int64; override;
    procedure Afficher; override;
  end;

  { Dossier }
  TDossier = class(TElementFS)
  private
    FContenu: array of TElementFS;
  public
    constructor Create(ANom: string);
    destructor Destroy; override;
    procedure Ajouter(Element: TElementFS);
    function GetTaille: Int64; override;  // Taille = somme du contenu
    procedure Afficher; override;
    procedure ListerContenu;
  end;

{ === TElementFS === }

constructor TElementFS.Create(ANom: string);
begin
  inherited Create;
  FNom := ANom;
  FTaille := 0;
  FDateCreation := Now;
end;

function TElementFS.GetNom: string;
begin
  Result := FNom;
end;

function TElementFS.GetTaille: Int64;
begin
  Result := FTaille;
end;

function TElementFS.GetTailleFormatee: string;
var
  Taille: Int64;
begin
  Taille := GetTaille;  // Appel polymorphe !

  if Taille < 1024 then
    Result := Format('%d o', [Taille])
  else if Taille < 1024 * 1024 then
    Result := Format('%.2f Ko', [Taille / 1024])
  else if Taille < 1024 * 1024 * 1024 then
    Result := Format('%.2f Mo', [Taille / (1024 * 1024)])
  else
    Result := Format('%.2f Go', [Taille / (1024 * 1024 * 1024)]);
end;

procedure TElementFS.Afficher;
begin
  WriteLn(FNom, ' - ', GetTailleFormatee);
end;

procedure TElementFS.Renommer(NouveauNom: string);
begin
  WriteLn('Renommage : "', FNom, '" → "', NouveauNom, '"');
  FNom := NouveauNom;
end;

{ === TFichier === }

constructor TFichier.Create(ANom: string; ATaille: Int64);
var
  PosPoint: Integer;
begin
  inherited Create(ANom);
  FTaille := ATaille;

  // Extraire l'extension
  PosPoint := Pos('.', ANom);
  if PosPoint > 0 then
    FExtension := Copy(ANom, PosPoint + 1, Length(ANom))
  else
    FExtension := '';
end;

function TFichier.GetTaille: Int64;
begin
  Result := FTaille;  // Taille fixe pour un fichier
end;

procedure TFichier.Afficher;
begin
  Write('📄 ');
  inherited Afficher;
  if FExtension <> '' then
    WriteLn('   Type : Fichier .', FExtension);
end;

{ === TDossier === }

constructor TDossier.Create(ANom: string);
begin
  inherited Create(ANom);
  SetLength(FContenu, 0);
end;

destructor TDossier.Destroy;
var
  i: Integer;
begin
  // Libérer tous les éléments contenus
  for i := 0 to High(FContenu) do
    FContenu[i].Free;

  inherited Destroy;
end;

procedure TDossier.Ajouter(Element: TElementFS);
var
  Longueur: Integer;
begin
  Longueur := Length(FContenu);
  SetLength(FContenu, Longueur + 1);
  FContenu[Longueur] := Element;
end;

function TDossier.GetTaille: Int64;
var
  i: Integer;
  Total: Int64;
begin
  Total := 0;

  // Calcul polymorphe : la taille d'un dossier = somme de son contenu
  for i := 0 to High(FContenu) do
    Total := Total + FContenu[i].GetTaille;  // Appel polymorphe !

  Result := Total;
end;

procedure TDossier.Afficher;
begin
  Write('📁 ');
  inherited Afficher;
  WriteLn('   Contient : ', Length(FContenu), ' élément(s)');
end;

procedure TDossier.ListerContenu;
var
  i: Integer;
begin
  WriteLn;
  WriteLn('=== Contenu de "', FNom, '" ===');
  if Length(FContenu) = 0 then
  begin
    WriteLn('  (vide)');
  end
  else
  begin
    for i := 0 to High(FContenu) do
    begin
      Write('  ');
      FContenu[i].Afficher;  // Appel polymorphe !
    end;
  end;
  WriteLn('==============================');
end;

{ === Fonctions polymorphes === }

procedure AfficherInfosElement(Element: TElementFS);
begin
  WriteLn('╔══════════════════════════════════');
  WriteLn('║ Informations sur l''élément');
  WriteLn('╠══════════════════════════════════');
  WriteLn('║ Nom : ', Element.GetNom);
  WriteLn('║ Taille : ', Element.GetTailleFormatee);
  WriteLn('║ Créé le : ', DateTimeToStr(Element.FDateCreation));
  WriteLn('╚══════════════════════════════════');
  WriteLn;
end;

function CalculerTailleTotal(Elements: array of TElementFS): Int64;
var
  i: Integer;
  Total: Int64;
begin
  Total := 0;
  for i := 0 to High(Elements) do
    Total := Total + Elements[i].GetTaille;  // Polymorphe !

  Result := Total;
end;

{ === Programme principal === }
var
  RacineDossier: TDossier;
  DossierDocuments: TDossier;
  DossierImages: TDossier;

  Fichier1, Fichier2, Fichier3, Fichier4: TFichier;

  Tous: array[0..3] of TElementFS;
  TailleTotal: Int64;
begin
  WriteLn('═══════════════════════════════════════════');
  WriteLn('   SYSTEME DE FICHIERS POLYMORPHE');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;

  // Création de la structure
  RacineDossier := TDossier.Create('Racine');
  DossierDocuments := TDossier.Create('Documents');
  DossierImages := TDossier.Create('Images');

  Fichier1 := TFichier.Create('rapport.pdf', 2048000);      // 2 Mo
  Fichier2 := TFichier.Create('presentation.pptx', 5242880); // 5 Mo
  Fichier3 := TFichier.Create('photo1.jpg', 3145728);       // 3 Mo
  Fichier4 := TFichier.Create('photo2.jpg', 4194304);       // 4 Mo

  // Construction de l'arborescence
  DossierDocuments.Ajouter(Fichier1);
  DossierDocuments.Ajouter(Fichier2);

  DossierImages.Ajouter(Fichier3);
  DossierImages.Ajouter(Fichier4);

  RacineDossier.Ajouter(DossierDocuments);
  RacineDossier.Ajouter(DossierImages);

  // Affichage polymorphe
  WriteLn('--- Structure du système de fichiers ---');
  WriteLn;
  RacineDossier.Afficher;
  RacineDossier.ListerContenu;
  WriteLn;

  DossierDocuments.ListerContenu;
  WriteLn;

  DossierImages.ListerContenu;
  WriteLn;

  // Test des fonctions polymorphes
  WriteLn('--- Test des fonctions polymorphes ---');
  WriteLn;

  AfficherInfosElement(RacineDossier);    // Dossier
  AfficherInfosElement(Fichier1);         // Fichier
  AfficherInfosElement(DossierImages);    // Dossier

  // Calcul de taille avec tableau hétérogène
  Tous[0] := RacineDossier;
  Tous[1] := DossierDocuments;
  Tous[2] := Fichier1;
  Tous[3] := DossierImages;

  TailleTotal := CalculerTailleTotal(Tous);
  WriteLn('Taille totale calculée : ', TailleTotal, ' octets');
  WriteLn;

  // Libération (les fichiers seront libérés par les dossiers)
  RacineDossier.Free;

  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
