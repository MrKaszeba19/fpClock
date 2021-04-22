program main;

uses Unix, StopWatch;

var
    input  : String;
    Status : LongInt;
    Clock  : TStopWatch;
begin
    input := ParamStr(1);
    Clock := TStopwatch.Create;
    Clock.Start();
    Status := fpSystem(input);
    Clock.Stop();
    write((Clock.ElapsedTicks / 10000.0):2:6);
end.
