unit mcdowell.net;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient;

type
  TSataniaNetThread = class(TThread)
  protected
    procedure SendToHer;
  public
    Data,
    URL: String;
    procedure Execute; override;
  end;

implementation

uses
  globals, mcdowell;

procedure TSataniaNetThread.SendToHer;
begin
  RunList.Delete(RunList.IndexOf(URL));
  RunResultList.Add(URL, Data);
end;

procedure TSataniaNetThread.Execute;
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

end.

