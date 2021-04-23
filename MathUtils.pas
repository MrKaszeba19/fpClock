unit MathUtils;

{$mode objfpc}{$H+}

interface

function isInteger(x : Extended) : Boolean;

function fmod(x, y : Extended) : Extended;
function fdiv(x, y : Extended) : Extended;

function ftrunc(x : Extended) : Extended;
function ffrac(x : Extended) : Extended;
function fround(x : Extended) : Extended;
function ffloor(x : Extended) : Extended;
function fceiling(x : Extended) : Extended;

implementation

uses Math;

function isInteger(x : Extended) : Boolean;
begin
    Result := (x = int(x));
end;

function fmod(x,y:Extended):Extended;
begin
    Result := x - y * Int(x/y);
end;

function fdiv(x, y : Extended) : Extended;
begin
    Result := Int(x/y);
end;

function ftrunc(x : Extended) : Extended;
begin
    //{$IFDEF cpu32} writeln( 'cpu32' ); 
    if x <= High(LongInt) 
        then Result := trunc(x)
        else Result := fdiv(x,1);
    //{$ENDIF}
end;

function ffrac(x : Extended) : Extended;
begin
    //{$IFDEF cpu32} writeln( 'cpu32' ); 
    if x <= High(LongInt) 
        then Result := frac(x)
        else Result := fmod(x,1);
    //{$ENDIF}
end;

function fround(x : Extended) : Extended;
begin
    if abs(x) <= High(LongInt)
        then 
            //Result := Trunc(x+0.5)
            if (x <= 0) 
                then Result := Trunc(x-0.5)
                else Result := Trunc(x+0.5)
        else 
            if (x <= 0) 
                then Result := fdiv(x-0.5,1)
                else Result := fdiv(x+0.5,1);
end;

function ffloor(x : Extended) : Extended;
begin
    if abs(x) <= High(LongInt)
        then Result := Floor(x)
        else if (isInteger(x)) 
            then Result := fdiv(x,1)
	        else if (x < 0) 
                then Result := fdiv(x,1)-1 
                else Result := fdiv(x,1);
end;
function fceiling(x : Extended) : Extended;
begin
    if abs(x) <= High(LongInt)
        then Result := Ceil(x)
        else if (isInteger(x)) 
            then Result := fdiv(x,1)
	        else if (x < 0) 
                then Result := fdiv(x,1) 
                else Result := fdiv(x,1)+1;
end;


end.
