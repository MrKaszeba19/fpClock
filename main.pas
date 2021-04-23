program main;

{$APPTYPE CONSOLE}
{$mode objfpc}{$H+}

uses SysUtils, Classes, CustApp,
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}
    unix,
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
    protected
        procedure DoRun; override;
    public
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;
        procedure WriteHelp; virtual;
end;

procedure FPClock.DoRun;
var
    input  : String;
    Status : LongInt;
    Clock  : TStopWatch;
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

    if (CString) 
        then input := string_fromC(ParamStr(1))
        else input := ParamStr(1);
    Clock := TStopwatch.Create;
    Clock.Start();
    Status := fpSystem(input);
    Clock.Stop();
    if (Clocked) 
        then printClocked(Clock.ElapsedTicks / Units, Precision)
        else write((Clock.ElapsedTicks / Units):2:(Precision));
    if (FeedLine) then writeln();
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
    writeln('   -h  , --help         : Print help');
    writeln('   -n  , --no-feed-line : Do not feed the line after having shown output ');
    writeln('   -p N, --prec=N       : Set precision to N digits (default N=4)');
    writeln('   -u U, --units=U      : Set measurement unit to U');
    writeln('                          (U in [ticks, clock, ns, mus, μs, ms, s, m, h, d])');
    writeln();
    writeln('Examples');
    writeln('    - fpclock ''ls -l''');
    writeln('    - fpclock ''cp ./foo/ ./bar -r'' -n');
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
