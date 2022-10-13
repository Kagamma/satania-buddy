unit mcdowell.net;

{$I configs.inc}

interface

uses
  Classes, SysUtils, fphttpclient;

type
  TSataniaHttpGetThread = class(TThread)
  protected
    ErrorMessage: String;
    procedure SendToHer;
    procedure ResetToDefault;
  public
    HTTP: TFPHTTPClient;
    Data,
    URL: String;   
    constructor Create(CreateSuspend: Boolean);
    procedure Execute; override;
    destructor Destroy; override;
  end;

  TSataniaHttpPostThread = class(TThread)
  protected
    ErrorMessage: String;
    procedure SendToHer;
    procedure ResetToDefault;
  public
    HTTP: TFPHTTPClient;
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
  RunResultList.AddOrSetValue(URL, Data);
end;

procedure TSataniaHttpGetThread.ResetToDefault;
begin
  Satania.TalkReset(ErrorMessage);
end;

constructor TSataniaHttpGetThread.Create(CreateSuspend: Boolean);
begin
  inherited Create(CreateSuspend);  
  Self.HTTP := TFPHTTPClient.Create(nil);
end;

procedure TSataniaHttpGetThread.Execute;
begin
  try
    try
      Data := HTTP.Get(URL);
      Synchronize(@SendToHer);
    except
      on E: Exception do
      begin
        ErrorMessage := E.Message;
        Synchronize(@Self.ResetToDefault);
      end;
    end;
  finally
    Terminate;
  end;
end; 

destructor TSataniaHttpGetThread.Destroy;
begin
  Self.HTTP.Free;
  inherited;
end;

procedure TSataniaHttpPostThread.SendToHer;
begin
  RunList.Delete(RunList.IndexOf(URL));
  RunResultList.AddOrSetValue(URL, Data);
end;

procedure TSataniaHttpPostThread.ResetToDefault;
begin
  Satania.TalkReset(ErrorMessage);
end;

procedure TSataniaHttpPostThread.Execute;
var
  Response: TStringStream;
begin
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
        Data := HTTP.FormPost(URL, FormData.Text);
      Synchronize(@SendToHer);
    except
      on E: Exception do
      begin
        ErrorMessage := E.Message;
        Synchronize(@Self.ResetToDefault);
      end;
    end;
  finally
    Terminate;
  end;
end;

constructor TSataniaHttpPostThread.Create(CreateSuspend: Boolean);
begin
  inherited Create(CreateSuspend);
  FormData := TStringList.Create;
  Self.HTTP := TFPHTTPClient.Create(nil);
end;

destructor TSataniaHttpPostThread.Destroy;
begin
  Self.FormData.Free;
  Self.HTTP.Free;
  inherited;
end;

end.

