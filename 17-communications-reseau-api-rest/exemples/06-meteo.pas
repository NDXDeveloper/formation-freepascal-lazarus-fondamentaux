{ ============================================================================
  Section 17.6 : Consommation d'API publiques
  Description : Application meteo avec OpenWeatherMap (necessite cle API)
  Fichier source : 06-consommation-api-publiques.md
  ============================================================================ }
program WeatherApp;

{$mode objfpc}{$H+}

uses
  fphttpclient, opensslsockets, SysUtils;

const
  API_KEY = 'VOTRE_CLE_API_ICI';  // Remplacer par votre vraie clé
  BASE_URL = 'https://api.openweathermap.org/data/2.5';

procedure GetCurrentWeather(const City: String);  
var  
  Client: TFPHttpClient;
  Response: String;
  URL: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    try
      // Construire l'URL avec les paramètres
      URL := Format('%s/weather?q=%s&appid=%s&units=metric&lang=fr',
                    [BASE_URL, City, API_KEY]);

      WriteLn('Récupération de la météo pour ', City, '...');
      Response := Client.Get(URL);

      if Client.ResponseStatusCode = 200 then
      begin
        WriteLn('=== Météo à ', City, ' ===');
        WriteLn(Response);
        WriteLn;
        WriteLn('Note : Les données sont en JSON. ');
        WriteLn('Dans la prochaine section, nous apprendrons à parser ce JSON !');
      end
      else if Client.ResponseStatusCode = 404 then
        WriteLn('Ville introuvable')
      else if Client.ResponseStatusCode = 401 then
        WriteLn('Clé API invalide')
      else
        WriteLn('Erreur : Code ', Client.ResponseStatusCode);

    except
      on E: Exception do
        WriteLn('Erreur : ', E.Message);
    end;
  finally
    Client.Free;
  end;
end;

begin
  GetCurrentWeather('Paris');
  WriteLn;
  GetCurrentWeather('London');
  WriteLn;
  GetCurrentWeather('Tokyo');
end.
