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

function string_fromC(dupa : String) : String;
begin
	dupa := StringReplace(dupa, '\a', #7, [rfReplaceAll]);
	dupa := StringReplace(dupa, '\b', #8, [rfReplaceAll]);
	dupa := StringReplace(dupa, '\e', #27, [rfReplaceAll]);
	dupa := StringReplace(dupa, '\f', #12, [rfReplaceAll]);
	dupa := StringReplace(dupa, '\n', #10, [rfReplaceAll]);
	dupa := StringReplace(dupa, '\r', #13, [rfReplaceAll]);
	dupa := StringReplace(dupa, '\t', #9, [rfReplaceAll]);
	dupa := StringReplace(dupa, '\v', #11, [rfReplaceAll]);
	dupa := StringReplace(dupa, '\\', '\', [rfReplaceAll]);
	dupa := StringReplace(dupa, '\''', #39, [rfReplaceAll]);
	dupa := StringReplace(dupa, '\"', #34, [rfReplaceAll]);
	dupa := StringReplace(dupa, '\?', '?', [rfReplaceAll]);
	Result := dupa;
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
        then write('0', s:2:(Precision))
        else write(s:2:(Precision));
end;

type
    FPClock = class(TCustomApplication)
    private
        FeedLine  : Boolean;
        Precision : Integer;
        Units     : Extended;
        CString   : Boolean;
        Clocked   : Boolean;
        {$IFDEF MSWINDOWS}
        Wait      : Integer;
        function GetLatestPowershell : String;
        {$ENDIF}
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

procedure FPClock.DoRun;
var
    input    : String;
    Clock    : TStopWatch;
    {$IFDEF MSWINDOWS}
    //Result   : String;
    ExecPath : String;
    {$ENDIF}
    {$IFDEF UNIX}
    Status   : LongInt;
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
            'ticks' : Units := TICK;
            'ns' : Units := NANOSEC;
            'nano' : Units := NANOSEC;
            'nanoseconds' : Units := NANOSEC;
            'mus' : Units := MICROSEC;
            'μs' : Units := MICROSEC;
            'micro' : Units := MICROSEC;
            'microseconds' : Units := MICROSEC;
            'ms' : Units := MILLISEC;
            'milli' : Units := MILLISEC;
            'milliseconds' : Units := MILLISEC;
            's' : Units := SEC;
            'secs' : Units := SEC;
            'seconds' : Units := SEC;
            'm' : Units := MIN;
            'mins' : Units := MIN;
            'minutes' : Units := MIN;
            'h' : Units := HOUR;
            'hrs' : Units := HOUR;
            'hours' : Units := HOUR;
            'd' : Units := DAY;
            'days' : Units := DAY;
            'c' : Clocked := True;
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
    {$ENDIF}

    Clock := TStopwatch.Create;
    Clock.Start();
    {$IFDEF MSWINDOWS}
    SysUtils.ExecuteProcess(utf8toansi(ExecPath + ' /c "' + input + '"'), '', []);
    {$ENDIF}
    {$IFDEF UNIX}
    Status := fpSystem(input);
    {$ENDIF}
    Clock.Stop();

    if (Clocked) 
        then printClocked(Clock.ElapsedTicks / Units, Precision)
        else write((Clock.ElapsedTicks / Units):2:(Precision));
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
    writeln('                          (U in [ticks, clock, ns, mus, μs, ms, s, m, h, d])');
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
    writeln('More info at https://github.com/RooiGevaar19/fpClock');
end;

var App : FPClock;

//{$R *.res}

begin
    App := FPClock.Create(nil);
    App.Title := 'fpclock';
    App.Run;
    App.Free;
    //{$IFDEF MSWINDOWS}
    //Sleep(500);
    //{$ENDIF}
    //readln();
end.
