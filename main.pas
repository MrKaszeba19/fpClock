program main;

{$APPTYPE CONSOLE}
{$mode objfpc}{$H+}

uses SysUtils, Classes, CustApp,
    {$IFDEF UNIX}
    {$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}
    unix,
    {$ENDIF} 
    {$IFDEF MSWINDOWS}
    Process,
    {$ENDIF}
    MathUtils, StopWatch;

const NANOSEC = 0.01;
const TICK = 1;
const MICROSEC = 10.0;
const MILLISEC = 10000.0;
const SEC = 10000000.0;
const MIN = SEC*60;
const HOUR = MIN*60;
const DAY = HOUR*24;
const WEEK = DAY*7;
    
function string_fromC(x : String) : String;
begin
	x := StringReplace(x, '\a', #7, [rfReplaceAll]);
	x := StringReplace(x, '\b', #8, [rfReplaceAll]);
	x := StringReplace(x, '\e', #27, [rfReplaceAll]);
	x := StringReplace(x, '\f', #12, [rfReplaceAll]);
	x := StringReplace(x, '\n', #10, [rfReplaceAll]);
	x := StringReplace(x, '\r', #13, [rfReplaceAll]);
	x := StringReplace(x, '\t', #9, [rfReplaceAll]);
	x := StringReplace(x, '\v', #11, [rfReplaceAll]);
	x := StringReplace(x, '\\', '\', [rfReplaceAll]);
	x := StringReplace(x, '\''', #39, [rfReplaceAll]);
	x := StringReplace(x, '\"', #34, [rfReplaceAll]);
	x := StringReplace(x, '\?', '?', [rfReplaceAll]);
	Result := x;
end;

procedure printClocked(input : Extended; Precision : ShortInt);
var
    h,m,s : Extended;
begin
    h := ffloor(input/3600.0);
    m := ffloor(fmod(input,3600.0)/60.0);
    s := fmod(input,60.0);
    if (h < 10.0)
        then write('0', h:1:0, ':')
        else write(h:2:0, ':');
    if (m < 10.0)
        then write('0', m:1:0, ':')
        else write(m:2:0, ':');
    if (s < 10.0)
        then write('0', s:1:(Precision))
        else write(s:2:(Precision));
end;

type
    FPClock = class(TCustomApplication)
    private
        FeedLine  : Boolean;
        Precision : Integer;
        Units     : Extended;
        CString   : Boolean;
        Clocked   : Boolean; // print execution time in clock format
        {$IFDEF MSWINDOWS}
        Wait      : Integer;
        function GetLatestPowershell : String;
        {$ENDIF}
        function MeasureExecTime(input : String; ExecPath : String = '') : Int64;
    protected
        procedure DoRun; override;
    public
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;
        procedure WriteHelp; virtual;
end;

{$IFDEF MSWINDOWS}
function FPClock.GetLatestPowershell : String;
var
    a, b  : ShortInt;
    Found : Boolean = False;
begin
    Result := 'C:\Windows\System32\WindowsPowershell\v1.0\powershell.exe';
    for a := 8 downto 1 do
    begin
        for b := 1 downto 0 do
        begin
            if FileExists('C:\Windows\System32\WindowsPowershell\v'+IntToStr(a)+'.'+IntToStr(b)+'\powershell.exe') then
            begin
                Result := 'C:\Windows\System32\WindowsPowershell\v'+IntToStr(a)+'.'+IntToStr(b)+'\powershell.exe';
                Found := True;
            end;
            if Found then Break;   
        end;
        if Found then Break;
    end;
end;
{$ENDIF}

function FPClock.MeasureExecTime(input : String; ExecPath : String = '') : Int64;
var
    Clock    : TStopWatch;
    {$IFDEF UNIX}
    Status   : LongInt;
    {$ENDIF}
begin
    Clock := TStopwatch.Create;
    if (input = '') then
    begin
        Clock.Start();
        Clock.Stop();
    end else begin
        Clock.Start();
        {$IFDEF MSWINDOWS}
        SysUtils.ExecuteProcess(utf8toansi(ExecPath + ' /c "' + input + '"'), '', []);
        {$ENDIF}
        {$IFDEF UNIX}
        Status := fpSystem(input);
        {$ENDIF}
        Clock.Stop();
    end;
    Result := Clock.ElapsedTicks;
end;

procedure FPClock.DoRun;
var
    input    : String;
    Clock    : TStopWatch;
    Elapsed  : Int64;
    {$IFDEF MSWINDOWS}
    ExecPath : String;
    {$ENDIF}
begin
    if HasOption('h', 'help') then begin
        WriteHelp();
        Halt();
    end;
    if HasOption('n', 'no-feed-line') then FeedLine := False;
    if HasOption('c', 'cstring') then CString := True;
    if HasOption('p', 'prec') then
    begin
        if not (TryStrToInt(getOptionValue('p', 'prec'), Precision)) or (Precision < 0) 
            then begin
                writeln(StdErr, 'Error: Invalid precision parameter value.');
                Halt(1);
            end;
    end;

    {$IFDEF MSWINDOWS}
    if HasOption('w', 'wait') then
    begin
        if not (TryStrToInt(getOptionValue('w', 'wait'), Wait)) or (Wait < 0) 
            then begin
                Wait := -1;
            end;
    end;
    {$ENDIF}

    if HasOption('u', 'units') then
    begin
        case getOptionValue('u', 'units') of
            't', 
            'ticks' : Units := TICK;
            'n',
            'ns', 
            'nano',
            'nanoseconds' : Units := NANOSEC;
            'u',
            'us',
            'mus',
            'μs',
            'micro',
            'microseconds' : Units := MICROSEC;
            'ms', 
            'milli',
            'milliseconds' : Units := MILLISEC;
            's',
            'sec',
            'secs',
            'seconds' : Units := SEC;
            'm',
            'min',
            'mins',
            'minutes' : Units := MIN;
            'h', 
            'hr',
            'hrs',
            'hours' : Units := HOUR;
            'd',
            'days' : Units := DAY;
            'w',
            'weeks' : Units := WEEk;
            'c',
            'clock' : Clocked := True;
            else begin
                writeln(StdErr, 'Error: Invalid units parameter value');
                Halt(1);
            end;
        end;
    end;

    if not (HasOption('P', 'prompt')) then
    begin
        if (CString) 
            then input := string_fromC(ParamStr(1))
            else input := ParamStr(1);
    end else begin
        read(input);
    end;
    
    {$IFDEF MSWINDOWS}
    ExecPath := 'c:\windows\system32\cmd.exe';
    if HasOption('e', 'env') then
    begin
        case getOptionValue('e', 'env') of
            'cmd' : ExecPath := 'c:\windows\system32\cmd.exe';
            'powershell'  : ExecPath := GetLatestPowershell();
            'powershell1' : ExecPath := 'C:\Windows\System32\WindowsPowershell\v1.0\powershell.exe';
            'powershell2' : ExecPath := 'C:\Windows\System32\WindowsPowershell\v2.0\powershell.exe';
            'powershell3' : ExecPath := 'C:\Windows\System32\WindowsPowershell\v3.0\powershell.exe';
            'powershell4' : ExecPath := 'C:\Windows\System32\WindowsPowershell\v4.0\powershell.exe';
            'powershell5' : ExecPath := 'C:\Windows\System32\WindowsPowershell\v5.0\powershell.exe';
            'powershell5.1' : ExecPath := 'C:\Windows\System32\WindowsPowershell\v5.1\powershell.exe';
            'powershell6' : ExecPath := 'C:\Windows\System32\WindowsPowershell\v6.0\powershell.exe';
            'powershell7' : ExecPath := 'C:\Windows\System32\WindowsPowershell\v7.0\powershell.exe';
            'powershell8' : ExecPath := 'C:\Windows\System32\WindowsPowershell\v8.0\powershell.exe';
            'ps'  : ExecPath := GetLatestPowershell();
            'ps1' : ExecPath := 'C:\Windows\System32\WindowsPowershell\v1.0\powershell.exe';
            'ps2' : ExecPath := 'C:\Windows\System32\WindowsPowershell\v2.0\powershell.exe';
            'ps3' : ExecPath := 'C:\Windows\System32\WindowsPowershell\v3.0\powershell.exe';
            'ps4' : ExecPath := 'C:\Windows\System32\WindowsPowershell\v4.0\powershell.exe';
            'ps5' : ExecPath := 'C:\Windows\System32\WindowsPowershell\v5.0\powershell.exe';
            'ps5.1' : ExecPath := 'C:\Windows\System32\WindowsPowershell\v5.1\powershell.exe';
            'ps6' : ExecPath := 'C:\Windows\System32\WindowsPowershell\v6.0\powershell.exe';
            'ps7' : ExecPath := 'C:\Windows\System32\WindowsPowershell\v7.0\powershell.exe';
            'ps8' : ExecPath := 'C:\Windows\System32\WindowsPowershell\v8.0\powershell.exe';
            else ExecPath := getOptionValue('e', 'env');
        end;
    end;
    Elapsed := MeasureExecTime(input, ExecPath);
    {$ENDIF}
    {$IFDEF UNIX}
    Elapsed := MeasureExecTime(input);
    {$ENDIF UNIX}

    if (Clocked) 
        then printClocked(Elapsed / Units, Precision)
        else write((Elapsed / Units):1:(Precision));
    if (FeedLine) then writeln();

    {$IFDEF MSWINDOWS}
    if (Wait > 0) 
        then Sleep(Wait)
        else if (Wait = -1) then readln();
    {$ENDIF}

    Terminate;
end;

constructor FPClock.Create(TheOwner: TComponent);
begin
    inherited Create(TheOwner);
    StopOnException:=True;
    FeedLine := True;
    CString := False;
    Clocked := False;
    Precision := 4;
    Units := SEC;
    {$IFDEF MSWINDOWS}
    Wait := 0;
    {$ENDIF}
end;

destructor FPClock.Destroy;
begin
    inherited Destroy;
end;

procedure FPClock.WriteHelp;
begin
    writeln('Usage: fpclock ''COMMAND'' flags');
    writeln();
    writeln('Available flags: ');
    writeln('    (no flag)           : Show execution time of COMMAND in seconds');
    writeln('                          with precision of 4 digits');
    writeln('                          and feed the line afterwards');
    writeln('   -c  , --cstring      : Input command is C-like formatted');
    {$IFDEF MSWINDOWS}
    writeln('   -e E, --env=E        : Choose the environment (Windows only)');
    writeln('                          – either `cmd` (default) or `powershell`/`ps`');
    {$ENDIF}
    writeln('   -h  , --help         : Print help');
    writeln('   -n  , --no-feed-line : Do not feed the line after having shown output ');
    writeln('   -p N, --prec=N       : Set precision to N digits (default N=4)');
    writeln('   -P  , --prompt       : Prompt for a command from standard input');
    writeln('   -u U, --units=U      : Set measurement unit to U');
    writeln('                          (U in [ticks, clock, ns, mus, ms, s, m, h, d, w])');
    {$IFDEF MSWINDOWS}
    writeln('   -w  , --wait         : Pause after measuring time (Windows only)');
    writeln('   -w N, --wait=N       : Wait N milliseconds after measuring time ');
    writeln('                          (Windows only, default N=0)');
    {$ENDIF}
    writeln();
    writeln('Examples');
    writeln('    - fpclock ''ls -l''');
    writeln('    - fpclock ''cp ./foo/ ./bar -r'' -n');
    writeln('    - fpclock --prompt');
    writeln();
    writeln('More info at https://github.com/MrKaszeba19/fpClock');
end;

var App : FPClock;

//{$R *.res}

begin
    App := FPClock.Create(nil);
    App.Title := 'fpclock';
    App.Run;
    App.Free;
end.
