program main;

{$APPTYPE CONSOLE}
{$mode objfpc}{$H+}

uses Classes, CustApp,
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}
    unix,
    {$ENDIF} 
    StopWatch;

type
    FPClock = class(TCustomApplication)
    private
        FeedLine : Boolean;
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
        Terminate;
        //Halt();
    end;
    if HasOption('n', 'no-feed-line') then begin
        FeedLine := False;
    end;

    input := ParamStr(1);
    Clock := TStopwatch.Create;
    Clock.Start();
    Status := fpSystem(input);
    Clock.Stop();
    write((Clock.ElapsedTicks / 10000.0):2:6);
    if (FeedLine) then writeln();
    Terminate;
end;

constructor FPClock.Create(TheOwner: TComponent);
begin
    inherited Create(TheOwner);
    StopOnException:=True;
    FeedLine := True;
end;

destructor FPClock.Destroy;
begin
    inherited Destroy;
end;

procedure FPClock.WriteHelp;
begin
    writeln('Usage: fpclock ''COMMAND'' -flags');
    writeln();
    writeln('Available flags: ');
    writeln('    (no flag)         : Show execution time of COMMAND in milliseconds');
    writeln('   -h, --help         : Print help');
    writeln('   -n, --no-feed-line : Do not feed line after having shown output ');
    writeln();
    writeln('Examples');
    writeln('    - fpclock ''ls -l''');
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
