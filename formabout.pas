unit formAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLVersion, ExtCtrls, ShellApi, ulazklockliteuntils;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnAboutExit         : TButton;
    btnAboutMSInfo       : TButton;
    Image1               : TImage;
    lblProgramName       : TLabel;
    lblProgrammer        : TLabel;
    lblSysUpTime         : TLabel;
    lblAppUpTime         : TLabel;
    lblSystemUpTime      : TLabel;
    lblApplicationUpTime : TLabel;
    LstBxInfo            : TListBox;
    LstBxDiscSpace       : TListBox;
    Panel1               : TPanel;
    Panel2               : TPanel;
    Panel3               : TPanel;
    Panel4               : TPanel;
    tmrUpTime            : TTimer;

    procedure btnAboutExitClick(Sender: TObject);
    procedure btnAboutMSInfoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure tmrUpTimeTimer(Sender: TObject);
    function FileSizeToHumanReadableString(fileSize: Int64): string;
  public
    { public declarations }
  end; 

const
  OneKB = Int64(1024);
  OneMB = Int64(1024) * OneKB;
  OneGB = Int64(1024) * OneMB;
  OneTB = Int64(1024) * OneGB;
  OnePB = Int64(1024) * OneTB;
  fmt   = '#.###';

var
  frmAbout: TfrmAbout;

implementation

uses
  ulazKlockLite;

{$R *.lfm}

{ TfrmAbout }


procedure TfrmAbout.btnAboutExitClick(Sender: TObject);
{  Close About form.  }
begin
  tmrUpTime.Enabled := false;
  Close;
end;

procedure TfrmAbout.btnAboutMSinfoClick(Sender: TObject);
{  Run the external application MSinfo.
   If ShellExecute returns 32 or less this indicates an error, just inform the user.
}
begin
  if ShellExecute(0, nil, PChar('"msinfo32.exe"'), nil, nil, 1) < 33 then
    ShowMessage('ERROR : running MSinfo');
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
var
  dskSize  : string;
  dskFree  : string;
  message  : string;
  cmpDate  : string;
  cmpUkDate: string;
  i        : integer;
begin
  tmrUpTime.Enabled      := True;
  lblAppUpTime.Caption   := getUpTime('Application');
  lblSysUpTime.Caption   := getUpTime('System');
  lblProgrammer.Caption  := userOptions.legalCopyright;
  lblProgramName.Caption := userOptions.productName;

  //  {$I %DATE%} returns the compile date, but in american [ignores local date format]
  //  So, we string slice it to give good old english date format.
  cmpDate               := {$I %DATE%};
  cmpUkDate             := format('%s/%s/%s', [copy(cmpDate, 9, 2),
                                               copy(cmpDate, 6, 2),
                                               copy(cmpDate, 1, 4)]);

  lstBxInfo.Items.add(userOptions.fileDescription);
  lstBxInfo.Items.add('');
  lstBxInfo.Items.add(format('%s Build   :: %s', [userOptions.productName, userOptions.productVersion]));
  lstBxInfo.Items.add(format('%s Version :: %s', [userOptions.productName, userOptions.fileVersion]));
  lstBxInfo.Items.add(format('%s Built   :: %s', [userOptions.productName, cmpUkDate + ' @ ' + {$I %TIME%}]));

  {$ifdef WIN32}
    lstBxInfo.Items.add(format('Built with 32 bit Lazarus Version :: %s', [lcl_version]));
  {$else}
    lstBxInfo.Items.add(format('Built with 64 bit Lazarus Version :: %s', [lcl_version]));
  {$endif}
  lstBxInfo.Items.add('');
  lstBxInfo.Items.add(getWindowsVersion);
  lstBxInfo.Items.add('');
  lstBxInfo.Items.add(userOptions.CompanyName);
  lstBxInfo.Items.add('');
  lstBxInfo.Items.add(userOptions.Comments);
  lstBxInfo.Items.add('');
  lstBxInfo.Items.add('App Dir : ' + ExtractFilePath(Application.ExeName));

  // Display the free space on drives B, C, D, E, F, where present
  for i := 2 to 10 do
  begin
    dskFree := FileSizeToHumanReadableString(DiskFree(i));
    dskSize := FileSizeToHumanReadableString(DiskSize(i));

    if DiskSize(i) >= 0 then
    begin
      message := format(' Disk %s : Free / Size :: %s / %s Gbytes', [Chr(i + 64), dskFree, dskSize]);
      LstBxDiscSpace.Items.Add(message);
    end;
  end;

end;

function TfrmAbout.FileSizeToHumanReadableString(fileSize: Int64): string;
{  Returns filesize in a human readable form.
   Does not use ther silly ISO standard unit of Pib, TiB, GiB, MiB & KiB.
   Used the good old fashion units of Pib, TB, GB, MB & KB.
}

begin
  if fileSize > OnePB then
    result := FormatFloat(fmt + 'PB', fileSize / OnePB)
  else
    if fileSize > OneTB then
      result := FormatFloat(fmt + 'TB', fileSize / OneTB)
    else
      if fileSize > OneGB then
        result := FormatFloat(fmt + 'GB', fileSize / OneGB)
      else
        if fileSize > OneMB then
          result := FormatFloat(fmt + 'MB', fileSize / OneMB)
        else
          if fileSize > OneKB then
            result := FormatFloat(fmt + 'KB', fileSize / OneKB)
          else
            if fileSize > 0 then
              result := FormatFloat(fmt + 'bytes', fileSize)
            else
              result := ''

end;

procedure TfrmAbout.tmrUpTimeTimer(Sender: TObject);
{  Update the labels in real time.    }
begin
  lblAppUpTime.Caption := getUpTime('Application');
  lblSysUpTime.Caption := getUpTime('System');
end;

end.

