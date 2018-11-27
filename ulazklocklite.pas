unit ulazKlockLite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ECSpinCtrls,
  strUtils, dateUtils, VpLEDLabel, Windows, LMessages, LCLType, typinfo,
  MouseAndKeyInput, LCLVersion;

type

  { TfrmlazKlockLite }

  TfrmlazKlockLite = class(TForm)
    mnItmClose: TMenuItem;
    PopupMenu1: TPopupMenu;
    tmrKlock: TECTimer;
    ledKlock: TVpLEDLabel;
    procedure FormDestroy(Sender: TObject);
    procedure mnItmCloseClick(Sender: TObject);
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
    function everyMinute(myNow: TdateTime; mins: integer): Boolean;
  public

  end;

var
  frmlazKlockLite: TfrmlazKlockLite;
  lines          : TStringList;
implementation

{$R *.lfm}

{ TfrmlazKlockLite }

procedure TfrmlazKlockLite.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  tmrKlock.Enabled := false;
  lines.Free;
end;

procedure TfrmlazKlockLite.FormDestroy(Sender: TObject);
begin
  // To prevent possible system resource leaks
  Application.RemoveOnUserInputHandler(@MouseHook)
end;

procedure TfrmlazKlockLite.FormCreate(Sender: TObject);
begin
  Application.AddOnUserInputHandler(@MouseHook);

  lines            := TStringList.create;
  tmrKlock.Enabled := true;
  ledKlock.Caption := FormatDateTime('hh:nn', now);
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
begin
  try
    lines.LoadFromFile('events.txt');
  except
    //  no events file - complain silently
  end;

  ledKlock.Rows    := lines.Count + 1;
  ledKlock.Columns := 50;

  frmlazKlockLite.Width  := ledKlock.Width;
  frmlazKlockLite.Height := ledKlock.Height;
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
  eDate   : TDateTime;
  timediff: TDateTime;
begin
  sTime   := PadRight(FormatDateTime('hh:nn:ss', now), 10);
  display := format('%s : %s', [sTime, FormatDateTime('ddd dd MMM YYYY', now)]);

  if lines.Count <> 0 then
  begin
    for line in lines do
    begin
      split := line.Split(',');
      eName := PadRight(split[0], 10);
      eDate := StrToDate(split[1]) + StrToTime(split[2]);

      timeDiff := eDate - Now;
      display  += lineEnding + Format('%s : %d days %s',
                         [eName,
                          trunc(timediff),
                          FormatDateTime('h" hrs "n" min "s" secs"', timediff)]);
    end;
  end;

  ledKlock.Caption := display;

  {  if system is idle, then keep monitor awake if required.    }
  if everyMinute(Now, 8) then pressF15;
end;
//
//.................................... Menu ....................................
//
procedure TfrmlazKlockLite.mnItmCloseClick(Sender: TObject);
begin
  close;
end;
//
//
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
end.

