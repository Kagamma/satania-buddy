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
  Satania.TalkReset(ErrorMessage);
end;

constructor TSataniaHttpThread.Create(CreateSuspend: Boolean; AKey: String);
begin
  inherited Create(CreateSuspend);
  FormData := TStringList.Create;
  Self.HTTP := TFPHTTPClient.Create(nil);
  Self.Key := AKey;
  ThreadDict.AddOrSetValue(Self.Key, Self);
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
  OwnedHeaders: Boolean = True;
  Headers: TStrings = nil;
begin
  try
    try
      HTTP.AllowRedirect := True;
      HTTP.RequestBody := TRawByteStringStream.Create(FormData.Text);
      HTTP.OnDataReceived := @Self.HandleDataReceived;
      case Method of
        'HEAD':
          TFPHTTPClient.Head(URL, Headers);
        'GET':
          HttpResponse.Data := HTTP.Get(URL);
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
      end;
      HttpResponse.Status := HTTP.ResponseStatusCode;
      HttpResponse.IsBinary := False;
      if Headers = nil then
      begin
        OwnedHeaders := False;
        Headers := HTTP.ResponseHeaders;
      end;
      SetLength(HttpResponse.HeaderKeys, Headers.Count);
      SetLength(HttpResponse.HeaderValues, Headers.Count);
      for I := 0 to Headers.Count - 1 do
      begin
        if LowerCase(Headers.Names[I]) = 'content-type' then
        begin
          S := LowerCase(Headers.Values[Headers.Names[I]]);
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
        HttpResponse.HeaderKeys[I] := Headers.Names[I];
        HttpResponse.HeaderValues[I] := Headers.Values[Headers.Names[I]];
      end;
      if not OwnedHeaders then
        Headers.Free;
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

destructor TSataniaHttpThread.Destroy;
begin
  Self.FormData.Free;
  if HTTP.RequestBody <> nil then
    HTTP.RequestBody.Free;
  Self.HTTP.Free;
  ThreadDict.Remove(Self.Key);
  inherited;
end;

end.

