unit UMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, JvComponentBase, JvNTEventLog,
  StdCtrls, ExtCtrls, Vcl.Samples.Spin;

type
  TfrmMain = class(TForm)
    MLog: TMemo;
    CLogfile: TRadioGroup;
    BtnLog: TButton;
    JvNTEventLog1: TJvNTEventLog;
    btnISAM: TSpinButton;
    procedure BtnLogClick(Sender: TObject);
    procedure btnISAMUpClick(Sender: TObject);
    procedure btnISAMDownClick(Sender: TObject);
    procedure CLogfileClick(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure DumpEvent;
  public
    { Public-Deklarationen }
  end;

var
  frmMain: TfrmMain;

implementation uses StrUtils;

{$R *.dfm}

procedure TfrmMain.btnISAMDownClick(Sender: TObject);
begin
  JvNTEventLog1.Next;

  DumpEvent;
end;

procedure TfrmMain.btnISAMUpClick(Sender: TObject);
begin
  JvNTEventLog1.Prior;

  DumpEvent;
end;

procedure TfrmMain.BtnLogClick(Sender: TObject);
begin
  JvNTEventLog1.Last;

  DumpEvent;
end;

procedure TfrmMain.DumpEvent;
var i: integer;
begin
  with MLog.Lines do begin
    BeginUpdate;
    try
      Add('--');
      Add('RecordNumber: ' + IntToStr(JvNTEventLog1.EventRecord.RecordNumber));
      Add('DateTime: ' + DateTimeToStr(JvNTEventLog1.EventRecord.DateTime));
      Add('EventType: ' + JvNTEventLog1.EventRecord.EventType);
      Add('EventID: ' + IntToStr(JvNTEventLog1.EventRecord.ID) + ' == ' +IntToHex(JvNTEventLog1.EventRecord.ID, 8));
      Add(Format('    Code: %d Facility %d  %s  %s-defined', [
            JvNTEventLog1.EventRecord.Code, JvNTEventLog1.EventRecord.Facility,
            JvNTEventLog1.EventRecord.SeverityAsText,
            IfThen( JvNTEventLog1.EventRecord.CustomCode, 'Application', 'Windows')]));
      Add('Category: ' + IntToStr(JvNTEventLog1.EventRecord.Category));

      i := JvNTEventLog1.EventRecord.StringCount;
      Add('Has '+IntToStr(i)+' string'+IfThen(i<>1,'s')+IfThen(i>0, ':', '.'));
      for i := 1 to i do
          Add(Format('%8d: %s', [i, JvNTEventLog1.EventRecord.EventString[i-1]]));
      Add('MessageText: ' + JvNTEventLog1.EventRecord.MessageText);
      // How the get the content of the message, this one doesn't work!
      Add('UserName: ' + JvNTEventLog1.EventRecord.UserName); // Doesn't work ?!
      Add('ComputerName: ' + JvNTEventLog1.EventRecord.Computer);
      Add('Application: ' + JvNTEventLog1.EventRecord.Source);
    finally
      EndUpdate;
      Add('');   // after .EU - to make TMemo scroll down
    end;
  end;
end;



procedure TfrmMain.CLogfileClick(Sender: TObject);
begin
  // JvNTEventLog1.Server := 'localhost'; // or leave blank
  JvNTEventLog1.Source := CLogfile.Items[CLogFile.ItemIndex];

  JvNTEventLog1.Open;

  btnISAM.Enabled := JvNTEventLog1.Active;
  BtnLog.Enabled  := JvNTEventLog1.Active;
end;

end.
