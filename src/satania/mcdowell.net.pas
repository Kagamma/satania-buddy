unit mcdowell.net;

{$I configs.inc}

interface

uses
  Classes, SysUtils, fphttpclient;

type
  TSataniaHttpGetThread = class(TThread)
  protected
    procedure SendToHer;
  public
    Data,
    URL: String;
    procedure Execute; override;
  end;

  TSataniaHttpPostThread = class(TThread)
  protected
    procedure SendToHer;
  public
    FormData: TStrings;
    FieldName,
    FileName,
    Data,
    URL: String;
    constructor Create(CreateSuspend: Boolean);
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

uses
  globals, mcdowell;

procedure TSataniaHttpGetThread.SendToHer;
begin
  RunList.Delete(RunList.IndexOf(URL));
  RunResultList.Add(URL, Data);
end;

procedure TSataniaHttpGetThread.Execute;
begin
  try
    try
      Data := TFPHTTPClient.SimpleGet(URL);
      Synchronize(@SendToHer);
    except
      on E: Exception do
        Satania.Talk(E.Message);
    end;
  finally
    Terminate;
  end;
end;

procedure TSataniaHttpPostThread.SendToHer;
begin
  RunList.Delete(RunList.IndexOf(URL));
  RunResultList.Add(URL, Data);
end;

procedure TSataniaHttpPostThread.Execute;
var
  HTTP: TFPHTTPClient;
  Response: TStringStream;
begin
  HTTP := TFPHTTPClient.Create(nil);
  try
    try
      if (FieldName <> '') and (FileName <> '') then
      begin
        Response := TStringStream.Create('');
        try
          HTTP.FileFormPost(URL, FormData, FieldName, FileName, Response);
          Data := Response.DataString;
        finally
          Response.Free;
        end;
      end else
        Data := HTTP.FormPost(URL, FormData);
      Synchronize(@SendToHer);
    except
      on E: Exception do
        Satania.Talk(E.Message);
    end;
  finally
    HTTP.Free;
    Terminate;
  end;
end;

constructor TSataniaHttpPostThread.Create(CreateSuspend: Boolean);
begin
  inherited Create(CreateSuspend);
  FormData := TStringList.Create;
end;

destructor TSataniaHttpPostThread.Destroy;
begin
  FormData.Free;
  inherited;
end;

end.

