unit mcdowell.net;

{$I configs.inc}

interface

uses
  Classes, SysUtils, fphttpclient, globals;

type
  TSataniaHttpGetThread = class(TThread)
  protected
    ErrorMessage: String;
    procedure SendToHer;
    procedure ResetToDefault;
  public
    Method: String;
    HTTP: TFPHTTPClient;
    FormData: TStrings;
    HttpResponse: THttpResponseRec;
    Key,
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
    HttpResponse: THttpResponseRec;
    Key,
    FieldName,
    FileName,
    URL: String;
    constructor Create(CreateSuspend: Boolean);
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

uses
  mcdowell;

procedure TSataniaHttpGetThread.SendToHer;
begin
  RunList.Delete(RunList.IndexOf(Key));
  RunHttpResultList.AddOrSetValue(Key, HttpResponse);
end;

procedure TSataniaHttpGetThread.ResetToDefault;
begin
  Satania.TalkReset(ErrorMessage);
end;

constructor TSataniaHttpGetThread.Create(CreateSuspend: Boolean);
begin
  inherited Create(CreateSuspend);
  FormData := TStringList.Create;
  Self.HTTP := TFPHTTPClient.Create(nil);
end;

procedure TSataniaHttpGetThread.Execute;
var
  S: String;
  I: Integer;
begin
  try
    try
      HTTP.RequestBody := TRawByteStringStream.Create(FormData.Text);
      case Method of
        'GET':
          HttpResponse.Data := HTTP.Get(URL);
        'PUT':
          HttpResponse.Data := HTTP.Put(URL);
        'DELETE':
          HttpResponse.Data := HTTP.Delete(URL);
        'PATCH':
          HttpResponse.Data := HTTP.Patch(URL);
        'OPTIONS':
          HttpResponse.Data := HTTP.Options(URL);
      end;
      HttpResponse.Status := HTTP.ResponseStatusCode;
      HttpResponse.IsBinary := False;
      SetLength(HttpResponse.HeaderKeys, HTTP.ResponseHeaders.Count);    
      SetLength(HttpResponse.HeaderValues, HTTP.ResponseHeaders.Count);
      for I := 0 to HTTP.ResponseHeaders.Count - 1 do
      begin
        if LowerCase(HTTP.ResponseHeaders.Names[I]) = 'content-type' then
        begin
          S := LowerCase(HTTP.ResponseHeaders.Values[HTTP.ResponseHeaders.Names[I]]);
          if S.IndexOf('image/') >= 0 then
          begin
            HttpResponse.IsBinary := True;
          end else
          begin
            case S of
              'application/x-binary',
              'application/octet-stream':
                HttpResponse.IsBinary := True;
            end;
          end;
        end;
        HttpResponse.HeaderKeys[I] := HTTP.ResponseHeaders.Names[I];
        HttpResponse.HeaderValues[I] := HTTP.ResponseHeaders.Values[HTTP.ResponseHeaders.Names[I]];
      end;
      Synchronize(@SendToHer);
    except
      on E: Exception do
      begin
        HttpResponse.Status:= HTTP.ResponseStatusCode;
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
  Self.FormData.Free;
  if HTTP.RequestBody <> nil then
    HTTP.RequestBody.Free;
  Self.HTTP.Free;
  inherited;
end;

procedure TSataniaHttpPostThread.SendToHer;
begin
  RunList.Delete(RunList.IndexOf(Key));
  RunHttpResultList.AddOrSetValue(Key, HttpResponse);
end;

procedure TSataniaHttpPostThread.ResetToDefault;
begin
  Satania.TalkReset(ErrorMessage);
end;

procedure TSataniaHttpPostThread.Execute;
var
  Response: TStringStream;
  I: Integer;
  S: String;
begin
  try
    try
      if (FieldName <> '') and (FileName <> '') then
      begin
        Response := TStringStream.Create('');
        try
          HTTP.FileFormPost(URL, FormData, FieldName, FileName, Response);
          HttpResponse.Data := Response.DataString;
        finally
          Response.Free;
        end;
      end else
      begin
        HTTP.RequestBody := TRawByteStringStream.Create(FormData.Text);
        HttpResponse.Data := HTTP.Post(URL);
      end;
      HttpResponse.Status := HTTP.ResponseStatusCode;
      HttpResponse.IsBinary := False;
      SetLength(HttpResponse.HeaderKeys, HTTP.ResponseHeaders.Count);
      SetLength(HttpResponse.HeaderValues, HTTP.ResponseHeaders.Count);
      for I := 0 to HTTP.ResponseHeaders.Count - 1 do
      begin
        if LowerCase(HTTP.ResponseHeaders.Names[I]) = 'content-type' then
        begin
          S := LowerCase(HTTP.ResponseHeaders.Values[HTTP.ResponseHeaders.Names[I]]);
          if S.IndexOf('image/') >= 0 then
          begin
            HttpResponse.IsBinary := True;
          end else
          begin
            case S of
              'application/x-binary',
              'application/octet-stream':
                HttpResponse.IsBinary := True;
            end;
          end;
        end;
        HttpResponse.HeaderKeys[I] := HTTP.ResponseHeaders.Names[I];
        HttpResponse.HeaderValues[I] := HTTP.ResponseHeaders.Values[HTTP.ResponseHeaders.Names[I]];
      end;
      Synchronize(@SendToHer);
    except
      on E: Exception do
      begin
        HttpResponse.Status := HTTP.ResponseStatusCode;
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
  if HTTP.RequestBody <> nil then
    HTTP.RequestBody.Free;
  Self.HTTP.Free;
  inherited;
end;

end.

