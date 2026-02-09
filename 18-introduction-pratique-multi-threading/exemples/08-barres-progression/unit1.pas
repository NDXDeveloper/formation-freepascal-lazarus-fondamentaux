unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

type
  { Thread multi-etapes avec progression }
  TThreadMultiEtapes = class(TThread)
  private
    FEtapeActuelle: Integer;
    FEtapesTotal: Integer;
    FProgressionEtape: Integer;
    FNomEtape: string;
    FDetailEtape: string;
    FLogMessage: string;
    FHeureDebut: TDateTime;
    procedure MettreAJourUI;
    procedure AjouterLog;
    procedure Terminer;
    function CalculerTempsRestant(ItemsTraites, ItemsTotal: Integer): string;
    procedure ExecuterEtape(NumEtape: Integer; const Nom: string; NbItems: Integer);
  protected
    procedure Execute; override;
  end;

  { TForm1 }
  TForm1 = class(TForm)
    ButtonStart: TButton;
    LabelEtape: TLabel;
    LabelDetail: TLabel;
    LabelGlobale: TLabel;
    LabelEtapeBar: TLabel;
    MemoLog: TMemo;
    ProgressBarGlobal: TProgressBar;
    ProgressBarEtape: TProgressBar;
    procedure ButtonStartClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TThreadMultiEtapes }

procedure TThreadMultiEtapes.MettreAJourUI;
var
  ProgressionGlobale: Integer;
begin
  // Progression globale : basee sur l'etape + avancement dans l'etape
  ProgressionGlobale := ((FEtapeActuelle - 1) * 100 + FProgressionEtape) div FEtapesTotal;
  Form1.ProgressBarGlobal.Position := ProgressionGlobale;
  Form1.ProgressBarEtape.Position := FProgressionEtape;

  Form1.LabelEtape.Caption := Format('Etape %d / %d : %s',
    [FEtapeActuelle, FEtapesTotal, FNomEtape]);
  Form1.LabelDetail.Caption := FDetailEtape;
end;

procedure TThreadMultiEtapes.AjouterLog;
begin
  Form1.MemoLog.Lines.Add(FLogMessage);
end;

procedure TThreadMultiEtapes.Terminer;
begin
  Form1.ProgressBarGlobal.Position := 100;
  Form1.ProgressBarEtape.Position := 100;
  Form1.LabelEtape.Caption := 'Traitement termine !';
  Form1.LabelDetail.Caption := Format('Duree totale : %s',
    [FormatDateTime('nn:ss', Now - FHeureDebut)]);
  Form1.ButtonStart.Enabled := True;
end;

function TThreadMultiEtapes.CalculerTempsRestant(ItemsTraites, ItemsTotal: Integer): string;
var
  TempsEcoule, TempsRestant: Double;
  Secondes: Integer;
begin
  if ItemsTraites = 0 then
  begin
    Result := '...';
    Exit;
  end;

  TempsEcoule := (Now - FHeureDebut) * 24 * 60 * 60;  // en secondes
  TempsRestant := (TempsEcoule / ItemsTraites) * (ItemsTotal - ItemsTraites);
  Secondes := Round(TempsRestant);

  if Secondes < 60 then
    Result := Format('%ds restantes', [Secondes])
  else
    Result := Format('%dm %ds restantes', [Secondes div 60, Secondes mod 60]);
end;

procedure TThreadMultiEtapes.ExecuterEtape(NumEtape: Integer;
  const Nom: string; NbItems: Integer);
var
  i: Integer;
begin
  FEtapeActuelle := NumEtape;
  FNomEtape := Nom;
  FProgressionEtape := 0;

  FLogMessage := Format('[%s] Etape %d : %s (%d items)',
    [FormatDateTime('hh:nn:ss', Now), NumEtape, Nom, NbItems]);
  Queue(@AjouterLog);

  for i := 1 to NbItems do
  begin
    if Terminated then Exit;

    // Simuler le travail
    Sleep(30);

    FProgressionEtape := (i * 100) div NbItems;
    FDetailEtape := Format('%d / %d items (%d%%) - %s',
      [i, NbItems, FProgressionEtape,
       CalculerTempsRestant(
         ((NumEtape - 1) * NbItems) + i,
         FEtapesTotal * NbItems)]);

    // Mise a jour frequente avec Queue (ne bloque pas)
    if (i mod 5 = 0) or (i = NbItems) then
      Queue(@MettreAJourUI);
  end;
end;

procedure TThreadMultiEtapes.Execute;
begin
  FEtapesTotal := 3;
  FHeureDebut := Now;

  // Etape 1 : Lecture
  ExecuterEtape(1, 'Lecture des donnees', 50);
  if Terminated then Exit;

  // Etape 2 : Traitement
  ExecuterEtape(2, 'Traitement des donnees', 80);
  if Terminated then Exit;

  // Etape 3 : Ecriture
  ExecuterEtape(3, 'Ecriture des resultats', 40);
  if Terminated then Exit;

  // Fin
  if not Terminated then
    Synchronize(@Terminer);
end;

{ TForm1 }

procedure TForm1.ButtonStartClick(Sender: TObject);
var
  Thread: TThreadMultiEtapes;
begin
  // Reinitialiser
  ButtonStart.Enabled := False;
  ProgressBarGlobal.Position := 0;
  ProgressBarEtape.Position := 0;
  LabelEtape.Caption := 'Demarrage...';
  LabelDetail.Caption := '';
  MemoLog.Clear;

  // Lancer le thread
  Thread := TThreadMultiEtapes.Create(True);
  Thread.FreeOnTerminate := True;
  Thread.Start;
end;

end.
