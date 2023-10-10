unit mcdowell.net;

{$I configs.inc}

interface

uses
  Classes, SysUtils, fphttpclient, globals;

type
  TSataniaHttpThread = class(TThread)
  protected
    ErrorMessage: String;
    procedure SendToHer;
    procedure ResetToDefault;
    procedure HandleDataReceived(Sender: TObject; const ContentLength, CurrentPos: Int64);
  public
    Method: String;
    HTTP: TFPHTTPClient;
    FormData: TStrings;
    HttpResponse: THttpResponseRec;
    Key,
    FieldName,
    FileName,
    URL: String;
    CurrentPos,
    ContentLength: QWord;
    constructor Create(CreateSuspend: Boolean; AKey: String);
    procedure Execute; override;
    destructor Destroy; override;
  end;

implementation

uses
  mcdowell;

procedure TSataniaHttpThread.SendToHer;
begin
  RunHttpResultList.AddOrSetValue(Key, HttpResponse);
end;

procedure TSataniaHttpThread.ResetToDefault;
begin
  Writeln(ErrorMessage);
end;

constructor TSataniaHttpThread.Create(CreateSuspend: Boolean; AKey: String);
begin
  FormData := TStringList.Create;
  Self.HTTP := TFPHTTPClient.Create(nil);
  Self.Key := AKey;
  ThreadDict.AddOrSetValue(Self.Key, Self);
  inherited Create(CreateSuspend);
end;

procedure TSataniaHttpThread.HandleDataReceived(Sender: TObject; const ContentLength, CurrentPos: Int64);
begin
  Self.ContentLength := ContentLength;
  Self.CurrentPos := CurrentPos;
end;

procedure TSataniaHttpThread.Execute;
var
  S: String;
  I: Integer;
  Response: TStringStream;
  SS: TRawByteStringStream;
begin
  HttpResponse.IsBinary := False;
  try
    try
      HTTP.AllowRedirect := True;
      HTTP.RequestBody := TRawByteStringStream.Create(FormData.Text);
      HTTP.OnDataReceived := @Self.HandleDataReceived;
      case Method of
        'HEAD':
          HTTP.HTTPMethod('HEAD', URL, nil, []);
        'GET':
          begin
            SS := TRawByteStringStream.Create('');
            try
              HTTP.HTTPMethod('GET', URL, SS, []);
              HttpResponse.Data := SS.DataString;
            finally
              SS.Free;
            end;
          end;
        'POST':
          begin
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
          end;
        'PUT':
          HttpResponse.Data := HTTP.Put(URL);
        'DELETE':
          HttpResponse.Data := HTTP.Delete(URL);
        'PATCH':
          HttpResponse.Data := HTTP.Patch(URL);
        'OPTIONS':
          HttpResponse.Data := HTTP.Options(URL);
        else
          raise Exception.Create('Invalid request method');
      end;
      HttpResponse.Status := HTTP.ResponseStatusCode;
      SetLength(HttpResponse.HeaderKeys, HTTP.ResponseHeaders.Count);
      SetLength(HttpResponse.HeaderValues, HTTP.ResponseHeaders.Count);
      for I := 0 to HTTP.ResponseHeaders.Count - 1 do
      begin
        if LowerCase(HTTP.ResponseHeaders.Names[I]) = 'content-type' then
        begin
          S := Trim(LowerCase(HTTP.ResponseHeaders.Values[HTTP.ResponseHeaders.Names[I]]));
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
        HttpResponse.HeaderKeys[I] := LowerCase(HTTP.ResponseHeaders.Names[I]);
        HttpResponse.HeaderValues[I] := HTTP.ResponseHeaders.Values[HTTP.ResponseHeaders.Names[I]];
      end;
      Synchronize(@SendToHer);
    except
      on E: Exception do
      begin
        HttpResponse.Status:= HTTP.ResponseStatusCode;
        if HttpResponse.Status = 0 then
          HttpResponse.Status := $FFFF;
        ErrorMessage := E.Message;
        HttpResponse.Data := ErrorMessage;
        Synchronize(@SendToHer);
        Synchronize(@Self.ResetToDefault);
      end;
    end;
  finally
    // TODO: Dont know why, but it will cause exception if free at destructor
    // So we free HTTP here
    if HTTP.RequestBody <> nil then
      HTTP.RequestBody.Free;
    Self.HTTP.Free;
    Terminate;
  end;
end;

destructor TSataniaHttpThread.Destroy;
begin
  Self.FormData.Free;
  ThreadDict.Remove(Self.Key);
  inherited;
end;

end.

