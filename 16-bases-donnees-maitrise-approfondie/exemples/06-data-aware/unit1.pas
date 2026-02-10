{
  Section 16.6 — Composants data-aware
  Description : Formulaire avec TDBGrid, TDBNavigator, TDBEdit,
                TDataSource, TDBMemo, validation BeforePost, OnNewRecord
  Fichier source : 06-composants-data-aware.md
}
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs,
  StdCtrls, DBGrids, DBCtrls,
  sqldb, sqlite3conn;

type
  TForm1 = class(TForm)
    { Composants non visuels }
    SQLite3Connection1: TSQLite3Connection;
    SQLTransaction1: TSQLTransaction;
    SQLQuery1: TSQLQuery;
    DataSource1: TDataSource;
    { Composants visuels }
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    LabelNom: TLabel;
    LabelPrenom: TLabel;
    LabelEmail: TLabel;
    LabelNotes: TLabel;
    LabelStatut: TLabel;
    DBEditNom: TDBEdit;
    DBEditPrenom: TDBEdit;
    DBEditEmail: TDBEdit;
    DBMemoNotes: TDBMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SQLQuery1BeforePost(DataSet: TDataSet);
    procedure SQLQuery1AfterPost(DataSet: TDataSet);
    procedure SQLQuery1AfterDelete(DataSet: TDataSet);
    procedure SQLQuery1NewRecord(DataSet: TDataSet);
  private
    procedure InitialiserBase;
    procedure InsererDonnees;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  try
    InitialiserBase;
    InsererDonnees;

    { Charger les donnees }
    SQLQuery1.SQL.Text := 'SELECT * FROM Contacts ORDER BY nom';
    SQLQuery1.Open;

    LabelStatut.Caption := Format('%d contact(s)',
      [SQLQuery1.RecordCount]);
  except
    on E: Exception do
    begin
      ShowMessage('Erreur d''initialisation : ' + E.Message);
      Application.Terminate;
    end;
  end;
end;

procedure TForm1.InitialiserBase;  
var  
  DBPath: string;
begin
  { Base de donnees dans le dossier de l'executable }
  DBPath := ExtractFilePath(Application.ExeName) + 'contacts_demo.db';

  SQLite3Connection1.DatabaseName := DBPath;
  SQLite3Connection1.CharSet := 'UTF8';
  SQLite3Connection1.Transaction := SQLTransaction1;
  SQLite3Connection1.Params.Add('foreign_keys=ON');

  SQLTransaction1.Database := SQLite3Connection1;
  SQLQuery1.Database := SQLite3Connection1;
  SQLQuery1.Transaction := SQLTransaction1;
  DataSource1.DataSet := SQLQuery1;

  SQLite3Connection1.Open;

  { Creer la table — VARCHAR pour que FPC mappe en ftString
    (TEXT serait mappe en ftMemo → affiche "(MEMO)" dans TDBGrid/TDBEdit) }
  SQLite3Connection1.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS Contacts (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  nom VARCHAR(100) NOT NULL,' +
    '  prenom VARCHAR(100),' +
    '  email VARCHAR(200),' +
    '  notes TEXT,' +
    '  date_creation VARCHAR(30) DEFAULT (datetime(''now''))' +
    ')');
  SQLTransaction1.Commit;
end;

procedure TForm1.InsererDonnees;  
var  
  QueryCheck: TSQLQuery;
begin
  { Verifier si la table est vide }
  QueryCheck := TSQLQuery.Create(nil);
  try
    QueryCheck.Database := SQLite3Connection1;
    QueryCheck.Transaction := SQLTransaction1;
    QueryCheck.SQL.Text := 'SELECT COUNT(*) AS nb FROM Contacts';
    QueryCheck.Open;

    if QueryCheck.FieldByName('nb').AsInteger = 0 then
    begin
      { Inserer des donnees de demonstration }
      SQLite3Connection1.ExecuteDirect(
        'INSERT INTO Contacts (nom, prenom, email, notes) VALUES ' +
        '(''Dupont'', ''Pierre'', ''pierre.dupont@email.fr'', ''Client fidele'')');
      SQLite3Connection1.ExecuteDirect(
        'INSERT INTO Contacts (nom, prenom, email, notes) VALUES ' +
        '(''Martin'', ''Marie'', ''marie.martin@email.fr'', ''Nouveau contact'')');
      SQLite3Connection1.ExecuteDirect(
        'INSERT INTO Contacts (nom, prenom, email, notes) VALUES ' +
        '(''Bernard'', ''Jean'', ''jean.bernard@email.fr'', ''Fournisseur'')');
      SQLite3Connection1.ExecuteDirect(
        'INSERT INTO Contacts (nom, prenom, email, notes) VALUES ' +
        '(''Dubois'', ''Sophie'', ''sophie.dubois@email.fr'', '''')');
      SQLite3Connection1.ExecuteDirect(
        'INSERT INTO Contacts (nom, prenom, email, notes) VALUES ' +
        '(''Thomas'', ''Paul'', ''paul.thomas@email.fr'', ''Partenaire'')');
      SQLTransaction1.Commit;
    end;

    QueryCheck.Close;
  finally
    QueryCheck.Free;
  end;
end;

procedure TForm1.SQLQuery1BeforePost(DataSet: TDataSet);  
var  
  Nom, Email: string;
begin
  { Validation du nom }
  Nom := Trim(DataSet.FieldByName('nom').AsString);
  if Nom = '' then
  begin
    ShowMessage('Le nom est obligatoire');
    DBEditNom.SetFocus;
    Abort;
  end;

  if Length(Nom) < 2 then
  begin
    ShowMessage('Le nom doit contenir au moins 2 caracteres');
    DBEditNom.SetFocus;
    Abort;
  end;

  { Validation email }
  Email := Trim(DataSet.FieldByName('email').AsString);
  if (Email <> '') and (Pos('@', Email) = 0) then
  begin
    ShowMessage('Format d''email invalide (doit contenir @)');
    DBEditEmail.SetFocus;
    Abort;
  end;

  { Normalisation }
  DataSet.FieldByName('nom').AsString := Trim(DataSet.FieldByName('nom').AsString);
  DataSet.FieldByName('prenom').AsString := Trim(DataSet.FieldByName('prenom').AsString);
end;

procedure TForm1.SQLQuery1AfterPost(DataSet: TDataSet);  
begin  
  try
    { ApplyUpdates envoie le SQL (INSERT/UPDATE) a la base,
      CommitRetaining valide sans fermer le dataset }
    SQLQuery1.ApplyUpdates;
    SQLTransaction1.CommitRetaining;
    LabelStatut.Caption := Format('%d contact(s) - Enregistre',
      [SQLQuery1.RecordCount]);
  except
    on E: Exception do
    begin
      SQLTransaction1.RollbackRetaining;
      ShowMessage('Erreur d''enregistrement : ' + E.Message);
    end;
  end;
end;

procedure TForm1.SQLQuery1AfterDelete(DataSet: TDataSet);  
begin  
  try
    SQLQuery1.ApplyUpdates;
    SQLTransaction1.CommitRetaining;
    LabelStatut.Caption := Format('%d contact(s) - Supprime',
      [SQLQuery1.RecordCount]);
  except
    on E: Exception do
    begin
      SQLTransaction1.RollbackRetaining;
      ShowMessage('Erreur de suppression : ' + E.Message);
    end;
  end;
end;

procedure TForm1.SQLQuery1NewRecord(DataSet: TDataSet);  
begin  
  { Valeurs par defaut pour un nouveau contact }
  DataSet.FieldByName('notes').AsString := '';
  LabelStatut.Caption := 'Nouveau contact...';
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);  
begin  
  if SQLite3Connection1.Connected then
  begin
    try
      if SQLQuery1.State in [dsEdit, dsInsert] then
        SQLQuery1.Cancel;
      if SQLTransaction1.Active then
        SQLTransaction1.Commit;
    except
      if SQLTransaction1.Active then
        SQLTransaction1.Rollback;
    end;
    SQLite3Connection1.Close;
  end;
end;

end.
