program main;

{$APPTYPE CONSOLE}
{$mode objfpc}{$H+}

uses SysUtils, Classes, CustApp,
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}
    unix,
    {$ENDIF} 
    StopWatch;

type
    FPClock = class(TCustomApplication)
    private
        FeedLine  : Boolean;
        Precision : Integer;
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
    if HasOption('p', 'prec') then
    begin
        if not (TryStrToInt(getOptionValue('p', 'prec'), Precision)) or (Precision < 0) 
            then begin
                writeln('Error: invalid precision value.');
                Halt(1);
            end;
    end;

    input := ParamStr(1);
    Clock := TStopwatch.Create;
    Clock.Start();
    Status := fpSystem(input);
    Clock.Stop();
    write((Clock.ElapsedTicks / 10000.0):2:(Precision));
    if (FeedLine) then writeln();
    Terminate;
end;

constructor FPClock.Create(TheOwner: TComponent);
begin
    inherited Create(TheOwner);
    StopOnException:=True;
    FeedLine := True;
    Precision := 4;
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
    writeln('    (no flag)           : Show execution time of COMMAND in milliseconds with precision of 4 digits');
    writeln('   -h  , --help         : Print help');
    writeln('   -n  , --no-feed-line : Do not feed line after having shown output ');
    writeln('   -p N, --prec=N       : Set precision to N digits (default N=4)');
    writeln();
    writeln('Examples');
    writeln('    - fpclock ''ls -l''');
    writeln('    - fpclock ''cp ./foo/ ./bar -r'' -n');
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
