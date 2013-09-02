{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not Use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvEventLog.PAS, released on 2002-09-02.

The Initial Developer of the Original Code is Fernando Silva [fernando dott silva att myrealbox dott com]
Portions created by Fernando Silva are Copyright (C) 2002 Fernando Silva.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvNTEventLog;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils,
  JvComponentBase, JvTypes;

type
  TNotifyChangeEventLog = class;
  TJvNTEventLogRecord = class;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvNTEventLog = class(TJvComponent)
  private
    FLogHandle: THandle;
    FLog: string;
    FServer: string;
    FSource: string;
    FActive: Boolean;
    FLastError: Cardinal;
    FOnChange: TNotifyEvent;
    FNotifyThread: TNotifyChangeEventLog;
    FEventRecord: TJvNTEventLogRecord;
    procedure SetActive(Value: Boolean);
    procedure SetServer(const Value: string);
    procedure SetSource(const Value: string);
    procedure SetLog(const Value: string);
    function GetEventCount: Cardinal;
    function DoReadRecord(const Direction: ShortInt; const RecNo: Cardinal = 0): boolean;
    procedure SeekRecord(N: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    procedure First;
    procedure Last;
    function Eof: Boolean;
    procedure Next;
    procedure Prior;
    procedure Seek(N: Cardinal);
    procedure ReadEventLogs(AStrings: TStrings);
    property EventCount: Cardinal read GetEventCount;
    property EventRecord: TJvNTEventLogRecord read FEventRecord;
  published
    property Server: string read FServer write SetServer;
    property Source: string read FSource write SetSource;
    property Log: string read FLog write SetLog; // Remove ? Seems to be a proper name for Source but never been actually used
    property Active: Boolean read FActive write SetActive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TNotifyChangeEventLog = class(TJvCustomThread)
  private
    FEventLog: TJvNTEventLog;
    FEventHandle: THandle;
    procedure DoChange;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TComponent);
  end;

  TEventSeverity = (evsSuccess, evsInfo, evsWarning, evsError );

  TJvNTEventLogRecord = class(TObject)
  private
    FEventLog: TJvNTEventLog;
    FCurrentRecord: Pointer;
    FOwner: TComponent;
    FRecordBytesSize: Cardinal;
    FNonParsed: boolean;
    FEventArgs: array of string;
    FComputerName, FEventSource: string;
    function GetRecordNumber: Cardinal;  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetDateTime: TDateTime;
    function GetRawID: DWORD;            {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetType: string;
    function GetStringCount: DWORD;      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetCategory: Cardinal;      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetSource: string;
    function GetComputer: string;
    function GetSID: PSID;
    function GetString(Index: Cardinal): string;
    function GetMessageText: string;
    function GetUsername: string;
    procedure SetRecordBytesSize(const Value: Cardinal);
    property RecordBytesSize: Cardinal read FRecordBytesSize write SetRecordBytesSize;
    function TryParseRecord(BytesRead: Cardinal = 0): boolean;
  public
    constructor Create(AOwner: TComponent);
    property RecordNumber: Cardinal read GetRecordNumber;
    property DateTime: TDateTime read GetDateTime;
    property EventType: string read GetType;
    property Category: Cardinal read GetCategory;
    property Source: string read GetSource;
    property Computer: string read GetComputer;
    property ID: DWORD read GetRawID;
    function Code: Word;                {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function CustomCode: boolean;       {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function Facility: Word;
    function Severity: TEventSeverity;
    function SeverityAsText: string;
    property StringCount: DWORD read GetStringCount;
    property SID: PSID read GetSID;
    property EventString[Index: Cardinal]: string read GetString;
    property MessageText: string read GetMessageText;
    property UserName: string read GetUsername;
    property Owner: TComponent read FOwner;
  end;

  PEventLogRecord = ^TEventLogRecord;
  TEventLogRecord = packed record
    Length: DWORD; // Length of full record
    Reserved: DWORD; // Used by the service
    RecordNumber: DWORD; // Absolute record number
    TimeGenerated: DWORD; // Seconds since 1-1-1970
    TimeWritten: DWORD; // Seconds since 1-1-1970
    EventID: DWORD;
    EventType: WORD;
    NumStrings: WORD;
    EventCategory: WORD;
    ReservedFlags: WORD; // For Use with paired events (auditing)
    ClosingRecordNumber: DWORD; // For Use with paired events (auditing)
    StringOffset: DWORD; // Offset from beginning of record
    UserSidLength: DWORD;
    UserSidOffset: DWORD;
    DataLength: DWORD;
    DataOffset: DWORD; // Offset from beginning of record
  end;


{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  Registry,
  JclStringLists,
  JvResources;

const
  EVENTLOG_SEQUENTIAL_READ = $0001;
  EVENTLOG_SEEK_READ = $0002;
  EVENTLOG_FORWARDS_READ = $0004;
  EVENTLOG_BACKWARDS_READ = $0008;

  cEventLogBaseKey = 'SYSTEM\CurrentControlSet\Services\EventLog';

//=== { TJvNTEventLog } ======================================================

constructor TJvNTEventLog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLog := '';
  FSource := '';
  FOnChange := nil;
  FNotifyThread := nil;
  FEventRecord := TJvNTEventLogRecord.Create(Self);
end;

destructor TJvNTEventLog.Destroy;
begin
  Close;
  FEventRecord.Free;
  inherited Destroy;
end;


procedure TJvNTEventLog.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    if csDesigning in ComponentState then
      FActive := Value
    else
    if Value then
      Open
    else
      Close;
  end;
end;

procedure TJvNTEventLog.SetServer(const Value: string);
var
  OldActive: Boolean;
begin
  if FServer <> Value then
  begin
    OldActive := Active;
    Active := False;
    FServer := Value;
    Active := OldActive;
  end
end;

procedure TJvNTEventLog.SetSource(const Value: string);
var
  OldActive: Boolean;
begin
  if FSource <> Value then
  begin
    OldActive := Active;
    Active := False;
    FSource := Value;
    Active := OldActive;
  end
end;

procedure TJvNTEventLog.SetLog(const Value: string);
var
  OldActive: Boolean;
begin
  if FLog <> Value then
  begin
    OldActive := Active;
    Active := False;
    FLog := Value;
    Active := OldActive;
  end
end;

function TJvNTEventLog.GetEventCount: Cardinal;
begin
  if Active then
    Win32Check( GetNumberOfEventLogRecords(FLogHandle, Result) )
  else
    Result := 0;
end;

procedure TJvNTEventLog.Open;
begin
  if Source <> '' then
  begin
    Close;
    FLogHandle := OpenEventLog(PChar(Server), PChar(Source));
    if FLogHandle = 0 then
      RaiseLastOSError;
    FNotifyThread := TNotifyChangeEventLog.Create(Self);
    FActive := True;
  end;
end;

procedure TJvNTEventLog.Close;
begin
  if FLogHandle <> 0 then
  begin
    if FNotifyThread <> nil then
      FNotifyThread.Terminate;
    CloseEventLog(FLogHandle);
    FLogHandle := 0;
    FreeAndNil(FNotifyThread);
  end;
  FreeMem(FEventRecord.FCurrentRecord);
  FEventRecord.FCurrentRecord := nil;
  FActive := False;
end;

procedure TJvNTEventLog.First;
begin
  SeekRecord(0);
end;

procedure TJvNTEventLog.Last;
begin
  SeekRecord(GetEventCount - 1);
end;

function TJvNTEventLog.Eof: Boolean;
begin
  Result := (EventRecord.FCurrentRecord = nil) or (EventRecord.RecordNumber = GetEventCount) or
    (FLastError = ERROR_HANDLE_EOF);
end;

function TJvNTEventLog.DoReadRecord(const Direction: ShortInt;
  const RecNo: Cardinal): boolean;
var
  Flags: DWORD;
  BytesRead, BytesNeeded: Cardinal;
  Dummy: Char;
begin
  if Direction = 0
     then Flags := EVENTLOG_SEEK_READ
     else Flags := EVENTLOG_SEQUENTIAL_READ;
  if Direction < 0
     then Flags := Flags or EVENTLOG_BACKWARDS_READ
     else Flags := Flags or EVENTLOG_FORWARDS_READ;

  Result := False;
  ReadEventLog(FLogHandle, Flags, RecNo, @Dummy, 0, BytesRead, BytesNeeded);
  FLastError := GetLastError;
  if FLastError = ERROR_INSUFFICIENT_BUFFER then
  begin
    FEventRecord.RecordBytesSize := BytesNeeded;
    if not ReadEventLog(FLogHandle, Flags, RecNo, FEventRecord.FCurrentRecord, BytesNeeded, BytesRead, BytesNeeded) then
      RaiseLastOSError;
    Result := FEventRecord.TryParseRecord(BytesRead);
//    if not FEventRecord.IsRecordValid(BytesRead) then
//      Raise EIntError.Create(ClassName + ': Invalid WinNT Event structure.');
  end
  else
  if FLastError <> ERROR_HANDLE_EOF then
    RaiseLastOSError;
end;

procedure TJvNTEventLog.Next;
begin
  DoReadRecord(+1);
end;

procedure TJvNTEventLog.Prior;
begin
  DoReadRecord(-1);
end;

procedure TJvNTEventLog.SeekRecord(N: Cardinal);
var
  Offset: DWORD;
  RecNo: Cardinal;
begin
  Win32Check( GetOldestEventLogRecord(FLogHandle, Offset) );
  RecNo := N + Offset;

  DoReadRecord(0, RecNo);
end;

procedure TJvNTEventLog.Seek(N: Cardinal);
begin
  if N <> FEventRecord.RecordNumber then
    SeekRecord(N);
end;

procedure TJvNTEventLog.ReadEventLogs(AStrings: TStrings);
begin
  with TRegistry.Create do
  begin
    AStrings.BeginUpdate;
    try
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey(cEventLogBaseKey, False);
      GetKeyNames(AStrings);
    finally
      Free;
      AStrings.EndUpdate;
    end;
  end;
end;

//=== { TNotifyChangeEventLog } ==============================================

constructor TNotifyChangeEventLog.Create(AOwner: TComponent);
begin
  inherited Create(False);

  // initialize system events
  FEventLog := TJvNTEventLog(AOwner);
  FEventHandle := CreateEvent(nil, True, False, nil);
  NotifyChangeEventLog(FEventLog.FLogHandle, FEventHandle);
  ThreadName := Format('%s: %s',[ClassName, AOwner.Name]);
end;

procedure TNotifyChangeEventLog.DoChange;
begin
  if Assigned(FEventLog.FOnChange) then
    FEventLog.FOnChange(FEventLog);
end;

procedure TNotifyChangeEventLog.Execute;
var
  LResult: DWORD;
begin
  // (rom) secure thread against exceptions
  NameThread(ThreadName);
  LResult := WAIT_OBJECT_0;
  try
    while not Terminated do
    begin
      // reset event signal if we're here for any other reason than a
      // timeout, so we can get it again
      if LResult <> WAIT_TIMEOUT then
        ResetEvent(FEventHandle);
      // wait for event to happen
      LResult := WaitForSingleObject(FEventHandle, 100);
      if LResult = WAIT_OBJECT_0 then
        Synchronize(DoChange);
    end;
  except
  end;
end;

//=== { TJvNTEventLogRecord } ================================================

constructor TJvNTEventLogRecord.Create(AOwner: TComponent);
begin
  // (rom) added inherited Create
  inherited Create;
  FEventLog := TJvNTEventLog(AOwner);
  FCurrentRecord := nil;
  FOwner := AOwner;
end;

function TJvNTEventLogRecord.TryParseRecord(BytesRead: Cardinal): boolean;
Var CR: PEventLogRecord;
    PLen2: PCardinal; LastDataOffs: Cardinal;
    i: integer;

  ParsingStringPtr: PChar;
  function ParseString(const InitOffset: Cardinal = 0): string;
  begin
    if InitOffset <> 0 then
       Cardinal(ParsingStringPtr) := Cardinal(CR) + InitOffset;

    Result := ParsingStringPtr;
    Inc(ParsingStringPtr, Length(Result) + 1);
  end;
begin
  if BytesRead = 0 then BytesRead := RecordBytesSize;

  Result := False;
  if BytesRead < RecordBytesSize then exit;
  if FCurrentRecord = nil then exit;

  CR := FCurrentRecord;
  if RecordBytesSize < SizeOf(CR^) then exit;
  if RecordBytesSize < CR^.Length then exit;

  LastDataOffs := RecordBytesSize - SizeOf(PLen2^);
  Cardinal(PLen2) := Cardinal(CR) + LastDataOffs;
  if PLen2^ <> CR^.Length then exit;

  if CR^.Reserved <> $654c664c then exit;

  if (CR^.UserSidOffset <> 0) and (CR^.UserSidLength <> 0) then
     if CR^.UserSidOffset + CR^.UserSidLength > LastDataOffs then exit;

  if (CR^.DataOffset <> 0) and (CR^.DataLength <> 0) then
     if CR^.DataOffset + CR^.DataLength > LastDataOffs then exit;

  if CR^.StringOffset > LastDataOffs then exit;

  FEventSource := ParseString( SizeOf(TEventLogRecord) );
  FComputerName := ParseString();

  SetLength(FEventArgs, CR^.NumStrings);
  if Length(FEventArgs) > 0 then begin
     i := 0;
     FEventArgs[i] := ParseString(CR^.StringOffset);
     While True do begin
        Inc(i);
        if i > High(FEventArgs) then break;
        FEventArgs[i] := ParseString();
     end;
  end;

  Result := True;
  FNonParsed := False;
end;

procedure TJvNTEventLogRecord.SetRecordBytesSize(const Value: Cardinal);
begin
  ReallocMem(FCurrentRecord, Value);
  FRecordBytesSize := Value;
  FNonParsed := True;
end;

function TJvNTEventLogRecord.GetRecordNumber: Cardinal;
begin
  Result := PEventLogRecord(FCurrentRecord)^.RecordNumber;
end;

function TJvNTEventLogRecord.GetMessageText: string;
var
  MessagePath: string;
  {Count,} I: Integer;
//  P: PChar;
  Args: array of PChar;
//  iArgs: Integer;
  St: string;
  reg: TRegistry;
  jsl: IJclStringList;

  function FormatMessageFrom(const DllName: string): Boolean;
  var
    DllModule: THandle;
    Buffer: array [0..2047] of Char;
    FullDLLName: array [0..MAX_PATH] of Char;
  begin
    Result := False;

    DllModule := 0;
    if DllName > '' then begin
       ExpandEnvironmentStrings(PChar(DllName), FullDLLName, MAX_PATH);
       DllModule := LoadLibraryEx(FullDLLName, 0, LOAD_LIBRARY_AS_DATAFILE);
    end;
    if DllModule <> 0 then
    try
      if FormatMessage(
        FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_ARGUMENT_ARRAY,
        Pointer(DllModule), ID, 0, Buffer, Length(Buffer), @Args[Low(Args)]) > 0 then
      begin
        Buffer[StrLen(Buffer) - 2] := #0;
        St := Buffer;
        Result := True;
      end else
        RaiseLastOSError;
    finally
      FreeLibrary(DllModule);
    end
  end;

begin
  Result := '';

  if FNonParsed then
     if not TryParseRecord() then
        Exit;

  SetLength(Args, Length(FEventArgs));
  for I := Low(Args) to High(Args) do
      Args[I] := PChar( FEventArgs[I] );

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKeyReadOnly(Format('%s\%s\%s', [cEventLogBaseKey, FEventLog.Source, Self.Source]));
    if reg.ReadString('ParameterMessageFile') <> '' then begin
       Result := RsLogNotImplParamsFile; // http://delphimaster.net/view/4-65276
       exit;
    end;
    MessagePath := reg.ReadString('EventMessageFile');
    if (MessagePath = '') and (reg.ReadString('ProviderGuid') <> '') then begin
       Result := RsLogNotImplVista;
       exit;  // Vista+ Evt***  new API - someone's welcome to implement
    end;
  finally
    reg.Free;
  end;

  St := '';
  jsl := JclStringList.Split(MessagePath,';').Trim;

  if jsl.Count > 0 then begin
    I := 0;
    while (I < jsl.Count) and (not FormatMessageFrom(jsl[I]))
      do Inc(I);
  end else
    FormatMessageFrom(''); // last resort: try OS-wide message tables

  Result := St;
end;

function TJvNTEventLogRecord.GetUsername: string;
var
  UserName: array [0..512] of Char;
  UserNameLen: Cardinal;
  DomainName: array [0..512] of Char;
  DomainNameLen: Cardinal;
  Use: SID_NAME_USE;
  UserSID: PSID;

begin
  Result := '';

  UserSID := SID;
  if Assigned(UserSID) then
  begin
    UserNameLen := SizeOf(UserName);
    DomainNameLen := SizeOf(DomainName);
    if LookupAccountSID(nil, UserSID, UserName, UserNameLen, DomainName, DomainNameLen, Use) then
      Result := string(DomainName) + '\' + string(UserName);
  end
  else
  begin
    Result := RsLogUserSIDNotFound;
  end;
end;

function TJvNTEventLogRecord.GetType: string;
begin
  case PEventLogRecord(FCurrentRecord)^.EventType of
    EVENTLOG_ERROR_TYPE:
      Result := RsLogError;
    EVENTLOG_WARNING_TYPE:
      Result := RsLogWarning;
    EVENTLOG_INFORMATION_TYPE:
      Result := RsLogInformation;
    EVENTLOG_AUDIT_SUCCESS:
      Result := RsLogSuccessAudit;
    EVENTLOG_AUDIT_FAILURE:
      Result := RsLogFailureAudit;
  else
    Result := '';
  end;
end;

function TJvNTEventLogRecord.GetSource: string;
begin
  Result := '';
  if FNonParsed then
     if not TryParseRecord() then
        Exit;

  Result := FEventSource;
end;

function TJvNTEventLogRecord.GetComputer: string;
begin
  Result := '';
  if FNonParsed then
     if not TryParseRecord() then
        Exit;

  Result := FComputerName;
end;

function TJvNTEventLogRecord.GetRawID: DWORD;
begin
  // Raw Event ID, see MSDN for its internal format
  Result := PEventLogRecord(FCurrentRecord)^.EventID;
end;

function TJvNTEventLogRecord.Code: Word;
begin
  Result := ID and $0000FFFF;
end;

function TJvNTEventLogRecord.Facility: Word;
begin
  Result := (ID shr 16) and $00000FFF;
end;

function TJvNTEventLogRecord.CustomCode: boolean;
begin
  Result := (ID and $20000000 ) <> 0;
end;

function TJvNTEventLogRecord.Severity: TEventSeverity;
begin
  Byte(Result) := (ID shr 30) and $03;
end;

const SeverityDescriptions: array [ TEventSeverity] of string =
 ( RsLogSuccess, RsLogInformation, RsLogWarning, RsLogERROR);

function TJvNTEventLogRecord.SeverityAsText: string;
begin
  Result := SeverityDescriptions[Severity];
end;


function TJvNTEventLogRecord.GetStringCount: DWORD;
begin
  Result := PEventLogRecord(FCurrentRecord)^.NumStrings;
end;

function TJvNTEventLogRecord.GetCategory: Cardinal;
begin
  Result := PEventLogRecord(FCurrentRecord)^.EventCategory;
end;

function TJvNTEventLogRecord.GetSID: PSID;
begin
  Result := nil;
  if PEventLogRecord(FCurrentRecord)^.UserSidLength > 0 then
    Result := PSID(PChar(FCurrentRecord) + PEventLogRecord(FCurrentRecord)^.UserSidOffset);
end;

function TJvNTEventLogRecord.GetString(Index: Cardinal): string;
begin
  Result := '';

  if FNonParsed then
     if not TryParseRecord() then
        Exit;

  Result := FEventArgs[Index];
end;

function TJvNTEventLogRecord.GetDateTime: TDateTime;
const
  StartPoint: TDateTime = 25569.0; // January 1, 1970 00:00:00
begin
  // Result := IncSecond(StartPoint, PEventLogRecord(FCurrentRecord)^.TimeGenerated);
//  Result := IncSecond(StartPoint, PEventLogRecord(FCurrentRecord)^.TimeWritten);
  Result := ((StartPoint * 86400.0) + PEventLogRecord(FCurrentRecord)^.TimeWritten) / 86400.0;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
