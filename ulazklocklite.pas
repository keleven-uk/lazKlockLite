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
    procedure MouseHook(Sender: TObject; Msg: Cardinal);
    procedure pressF15;
    procedure setDisplay;
    function everyMinute(myNow: TdateTime; mins: integer): Boolean;
  public

  end;

CONST
  NO_OF_COLUMNS = 50;
var
  frmlazKlockLite: TfrmlazKlockLite;
  userOptions    : Options;
  appStartTime   : int64;          //  used by formAbout to determine how long the app has been running.
  lines          : TStringList;
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
    writeLn(debugFle, format ('%s : log file Closed', [timeToStr(now)]));
    CloseFile(debugFle);
  end;
end;

procedure TfrmlazKlockLite.FormDestroy(Sender: TObject);
begin
  // To prevent possible system resource leaks
  Application.RemoveOnUserInputHandler(@MouseHook);

  UserOptions.formTop  := frmlazKlockLite.Top;
  UserOptions.formLeft := frmlazKlockLite.Left;

  userOptions.writeCurrentOptions;  // write out options file.
  userOptions.Free;
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
  tmrKlock.Enabled     := true;
  ledKlock.Caption     := FormatDateTime('hh:nn', now);
  scrollPos            := 1;

  loadEvents;
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
   The events file shpuld be called events.txt and reside in the
   same directory has the executable.
}
begin
  try
    lines.LoadFromFile('events.txt');
  except
    //  no events file - complain silently
  end;

  setDisplay;
end;
//
//............................. timer ..........................................
//
procedure TfrmlazKlockLite.tmrKlockTimer(Sender: TObject);
var
  split   : TStringArray;
  display : string;
  sTime   : string;
  line    : string;
  eName   : string;
  oLength : integer;
  flag    : boolean;
  eDate   : TDateTime;
  timediff: TDateTime;
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
        timeDiff := eDate - Now;
        flag     := true;
      except  //  no date on time set.
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
        display  += Format('%s : %d days %s',
                         [eName,
                          trunc(timediff),
                          FormatDateTime('h" hrs "n" mins "s" secs"', timediff)])
      else  //  no date or time set - so just use name.
        display  += eName;

    end;  //  for line in lines do
  end;    //  if lines.Count <> 0 then

  if mnItmScroll.Checked then
  begin
    oLength := length(display) + 4;  //  We add 4 for the 4 spaces on the next line.
    display := display + '    ' + display;
    ledKlock.Caption := Copy(display, scrollPos, NO_OF_COLUMNS);

    if debug then
      writeLn(debugFle, format('scrollPos = %d :: %d :: %s ', [scrollPos, oLength, ledKlock.Caption]));

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

   <CTRL F15> should be reconised by most systems, but is rarely used in applications.
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
    ledKlock.Rows    := 1
  else
    ledKlock.Rows    := lines.Count + 1;

  ledKlock.Columns := NO_OF_COLUMNS;

  frmlazKlockLite.Width  := ledKlock.Width;
  frmlazKlockLite.Height := ledKlock.Height;
end;

end.

