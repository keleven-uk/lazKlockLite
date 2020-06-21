unit ulazKlockLite;

{
# lazKlockLite

A light version of Klock, only contains a LED klock.

But, also contains a countdown to a given event i.e. retirement.
The events are held in a text file, events.txt.
The entry should be in the form  - event, date, time.
If no date and time are given the event test is just displayed.

The text can be made to scroll, this is done by doubling the text line
and moving a sliding windows over the text.

Also, contains the code to simulate the pressing of F15 - to keep monitors awake.


The softwate is issued under the GNU General Public License v3 (GPL-3).

You may copy, distribute and modify the software as long as you
track changes/dates in source files. Any modifications to or software
including via compiler) GPL-licensed code must also be made available
under the GPL along with build & install instructions.

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ECSpinCtrls,
  strUtils, dateUtils, VpLEDLabel, Windows, LMessages, LCLType, typinfo,
  MouseAndKeyInput, LCLVersion, uOptions, formAbout, formLicence;

type

  { TfrmlazKlockLite }

  TfrmlazKlockLite = class(TForm)
    MenuItem1        : TMenuItem;
    mnItmLicence     : TMenuItem;
    mnItmAbout       : TMenuItem;
    mnItmReload      : TMenuItem;
    mnItmMonitorAwake: TMenuItem;
    mnItmScroll      : TMenuItem;
    mnItmClose       : TMenuItem;
    PopupMenu1       : TPopupMenu;
    tmrKlock         : TECTimer;
    ledKlock         : TVpLEDLabel;

    procedure FormDestroy(Sender: TObject);
    procedure mnItmAboutClick(Sender: TObject);
    procedure mnItmCloseClick(Sender: TObject);
    procedure mnItmLicenceClick(Sender: TObject);
    procedure mnItmReloadClick(Sender: TObject);
    procedure tmrKlockTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    WindowDragMousePos: TPoint;
    WindowDragTopLeft : TPoint;
    WindowDragStarted : Boolean;

    procedure loadEvents;
    procedure loadBankHolidays;
    procedure MouseHook(Sender: TObject; Msg: Cardinal);
    procedure pressF15;
    procedure setDisplay;
    function everyMinute(myNow: TdateTime; mins: integer): Boolean;
    function calcWorkingDays(futureDate: TdateTime): integer;
    function notBankHoliday(CurrDate: TdateTime): boolean;
  public

  end;

CONST
  NO_OF_COLUMNS = 60;
var
  frmlazKlockLite: TfrmlazKlockLite;
  userOptions    : Options;
  appStartTime   : int64;          //  used by formAbout to determine how long the app has been running.
  lines          : TStringList;
  bankHolidaysStr: TStringList;
  bankHolidays   : array of TDateTime; // Dynamic Array
  scrollPos      : integer;
  debug          : Boolean;
  debugFle       : text;

implementation

{$R *.lfm}

{ TfrmlazKlockLite }

procedure TfrmlazKlockLite.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  tmrKlock.Enabled := false;
  lines.Free;

  if debug then begin
    writeLn(debugFle, format ('%s : Debug log Closed', [timeToStr(now)]));
    CloseFile(debugFle);
  end;

  UserOptions.formTop  := frmlazKlockLite.Top;
  UserOptions.formLeft := frmlazKlockLite.Left;

  userOptions.writeCurrentOptions;  // write out options file.
  userOptions.Free;

  CloseAction:= caFree;
end;

procedure TfrmlazKlockLite.FormDestroy(Sender: TObject);
begin
  // To prevent possible system resource leaks
  Application.RemoveOnUserInputHandler(@MouseHook);
end;

procedure TfrmlazKlockLite.FormCreate(Sender: TObject);
VAR
  DebugFleName : String;
begin
  debug := false ;

  if debug then begin
    DebugFleName := 'debug.log';
    assignfile(debugFle, DebugFleName);
    rewrite(debugFle);
    writeLn(debugFle, format ('%s : %s Created', [timeToStr(now), DebugFleName]));
  end;

  Application.AddOnUserInputHandler(@MouseHook);

  appStartTime := GetTickCount64;  //  tick count when application starts.
  userOptions  := Options.Create;  // create options file as c:\Users\<user>\AppData\Local\Stub\Options.xml

  frmlazKlockLite.Top  := UserOptions.formTop;
  frmlazKlockLite.Left := UserOptions.formLeft;
  lines                := TStringList.create;
  bankHolidaysStr      := TStringList.create;
  tmrKlock.Enabled     := true;
  ledKlock.Caption     := FormatDateTime('hh:nn', now);
  scrollPos            := 1;

  loadEvents;
  loadBankHolidays;
  setDisplay;
  tmrKlockTimer(Sender);
end;

procedure TfrmlazKlockLite.MouseHook(Sender: TObject; Msg: Cardinal);
{  Implements a draggable window.  Because the control fills the complete window
   We cant just catch the forms mouse events - so we use a global hook and
   filter out just the mouse movements.
}
begin
  if (Msg <> LM_MOUSEMOVE) and (Msg <> LM_LBUTTONUP) and (Msg <> LM_LBUTTONDOWN) then Exit;

  { MouseMove - Code to drag the main window using the mouse}
  if msg = LM_MOUSEMOVE then
  begin
    if WindowDragStarted then
      begin
        Left := WindowDragTopLeft.X + (Mouse.CursorPos.X - WindowDragMousePos.X);
        Top  := WindowDragTopLeft.Y + (Mouse.CursorPos.Y - WindowDragMousePos.Y);
      end;
  end;

  { MouseUp - Code to drag the main window using the mouse }
  if msg = LM_LBUTTONUP then
  begin
    WindowDragStarted := False;
  end;

  { MouseDown - Code to drag the main window using the mouse}
  if msg = LM_LBUTTONDOWN then
  begin
    WindowDragStarted   := True;
    WindowDragMousePos  := Mouse.CursorPos;
    WindowDragTopLeft.X := Left;
    WindowDragTopLeft.Y := Top;
  end;
end;

procedure TfrmlazKlockLite.loadEvents;
{  Loads the events from a text file, if present.
   The events file should be called events.txt and reside in the
   same directory has the executable.

   The format of the file -

   name of event, date of event, time of event, only show working days

   First first argument is mandtory, the remaing are optional.
}
begin
  try
    lines.LoadFromFile('events.txt');
  except
    //  no events file - complain silently.
  end;
end;

procedure TfrmlazKlockLite.loadBankHolidays;
{  Loads the BankHolidays from a text file, if present.
   The BankHolidays file should be called BankHolidays.txt and reside in the
   same directory has the executable.

   This needs to be a list of bank holidat dates, from not to the latest date used.
   The dates need to be in the format dd/mm/yyyy.  One date per line.
}
VAR
  count       : integer;
  bankHoliday : string;
begin
  count := 0;
  try
    bankHolidaysStr.LoadFromFile('BankHolidays.txt');

    setLength(bankHolidays, bankHolidaysStr.count);  // size the dynamic array correctly.

    for bankHoliday in bankHolidaysStr do
    begin
      bankHolidays[count] := StrToDate(bankHoliday);
      inc(count);
    end;
  except
    if debug then
      writeLn(debugFle, 'Error loading holiday file')
    //  no BankHolidays file - complain silently.
  end;
end;

//
//............................. timer ..........................................
//
procedure TfrmlazKlockLite.tmrKlockTimer(Sender: TObject);
var
  split       : TStringArray;
  display     : string;
  sTime       : string;
  line        : string;
  eName       : string;
  eWorking    : string;
  workingDays : integer;
  oLength     : integer;
  flag        : boolean;
  eDate       : TDateTime;
  timediff    : TDateTime;
begin
  setDisplay;

  if mnItmScroll.Checked then
    sTime   := FormatDateTime('hh:nn:ss', now)
  else
    sTime   := PadRight(FormatDateTime('hh:nn:ss', now), 10);

  display := format('%s : %s', [sTime, FormatDateTime('ddd dd MMM YYYY', now)]);

  if lines.Count <> 0 then
  begin
    for line in lines do
    begin
      split := line.Split(',');

      try
        eDate    := StrToDate(split[1]) + StrToTime(split[2]);
        eWorking := split[3];
        flag     := true;
      except  //  no date or time set.
        flag := false;
      end;

      if mnItmScroll.Checked then
      begin
        eName := split[0];
        display += '    ';
      end
      else
      begin
        eName := PadRight(split[0], 10);
        display += lineEnding;
      end;

      if flag then
         if eWorking = ' True' then  //  eWorking set to True - ignore working days.
         begin
           workingDays := calcWorkingDays(eDate);
           timeDiff    := eDate - Now;
           display     += Format('%s : %d Working Days %s',
                              [eName,
                               workingDays,
                               FormatDateTime('h" hrs "n" mins "s" secs"', timediff)])
         end
         else                      //  eWorking is either set to False or blank
         begin
           timeDiff := eDate - Now;
           display  += Format('%s : %d days %s',
                              [eName,
                               trunc(timediff),
                               FormatDateTime('h" hrs "n" mins "s" secs"', timediff)])
         end  //  if eWorking = ' True' then
      else    //  no date or time set - so just use name.  else of if flag then
        display  += eName;

    end;  //  for line in lines do
  end;    //  if lines.Count <> 0 then

  if mnItmScroll.Checked then
  begin
    oLength := length(display) + 4;  //  We add 4 for the 4 spaces on the next line.
    display := display + '    ' + display;
    ledKlock.Caption := Copy(display, scrollPos, NO_OF_COLUMNS);

    //if debug then
    //  writeLn(debugFle, format('scrollPos = %d :: %d :: %s ', [scrollPos, oLength, ledKlock.Caption]));

    inc(scrollPos);
    if scrollPos > oLength then scrollPos := 1;
  end
  else
    ledKlock.Caption := display;

  {  if system is idle, then keep monitor awake if required.    }
  if mnItmMonitorAwake.Checked and everyMinute(Now, 8) then pressF15;
end;
//
//.................................... Menu ....................................
//
procedure TfrmlazKlockLite.mnItmCloseClick(Sender: TObject);
begin
  close;
end;

procedure TfrmlazKlockLite.mnItmLicenceClick(Sender: TObject);
begin
  frmLicence := TfrmLicence.Create(Nil);  //frmLicence is created
  frmLicence.ShowModal;                   //frmLicence is displayed
  FreeAndNil(frmLicence);                 //frmLicence is release
end;

procedure TfrmlazKlockLite.mnItmReloadClick(Sender: TObject);
{  Reload the events text file.    }
begin
  loadEvents;
end;

procedure TfrmlazKlockLite.mnItmAboutClick(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Nil);  //frmAbout is created
  frmAbout.ShowModal;                 //frmAbout is displayed
  FreeAndNil(frmAbout);               //frmAbout is release
end;

//
//..................................... Helper Routines ........................
//
function TfrmlazKlockLite.everyMinute(myNow: TdateTime; mins: integer): Boolean;
{  Returns true if the current minutes is a multiple of the supplied minute.
   i.e. true every n minutes.
}
Var
  hour, minute, second, milliSeconds: word;
begin
  DecodeTime(myNow, hour, minute, second, milliSeconds);
  result := (minute mod mins = 0) and (second = 0);
end;

procedure TfrmlazKlockLite.pressF15;
{  This simulates the pressing of the <CTRL F15> key.
   This is used to keep the monitor awake i.e. not going into sleep mode.

   <CTRL F15> should be recognised by most systems, but is rarely used in applications.
                                                 and does not appear on most keyboards.
}
begin
  KeyInput.Apply([ssCtrl]);
  KeyInput.Press(VK_F15);                // This will simulate press of <CTRL F15> key.
  KeyInput.Unapply([ssCtrl]);
end;

procedure TfrmlazKlockLite.setDisplay;
{  Sets up the display depending weather the message will be scrolling
   or multi-line, this is set by a menu item.
}
begin
  if mnItmScroll.Checked then
    ledKlock.Rows := 1
  else
    ledKlock.Rows := lines.Count + 1;

  ledKlock.Columns := NO_OF_COLUMNS;

  frmlazKlockLite.Width  := ledKlock.Width;
  frmlazKlockLite.Height := ledKlock.Height;
end;

function TfrmlazKlockLite.calcWorkingDays(futureDate: TdateTime): integer;
{   Calculates to number or working days between two date.
    i.e. ignores weekends.

    Expects futureDate to be a date in the future.
}
var
  CurrDate : TDateTime;

begin
  CurrDate := Now;
  Result   := 0;
  while (CurrDate <= futureDate) do
  begin
    // DayOfTheWeek returns 1-5 for Mon-Fri, so 6 and 7 are weekends
    if (DayOfTheWeek(CurrDate) < 6) and (notBankHoliday(CurrDate)) then
      Inc(Result);
    CurrDate := CurrDate + 1;
  end;
end;

function TfrmlazKlockLite.notBankHoliday(CurrDate: TdateTime): boolean;
{  Checks Currdate against a list of known bank holidays.
   Returns true if a match, else false.
}
VAR
  bankHoliday : TDateTime;

begin
  result := True;

  if Length(bankHolidays) <> 0 then  //  if list of known bank holidats does not exist.
  begin
    for bankHoliday in bankHolidays do
    begin
      if CompareDate(bankHoliday, CurrDate) = 0 then
        result := False;
    end;
  end;
end;

end.

