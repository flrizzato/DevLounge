program NameOfTest;

{$APPTYPE CONSOLE}
uses
  System.SysUtils;

type
  TRec = record
    type
      TNested = record
        F: Integer;
      end;
    var
      N: TNested;
  end;

  TBase = class
    BData: Integer;
  end;
  TFoo<T> = class(TBase)
    R: TRec;
  end;

procedure Test1;
var
  F: TFoo<Integer>;
begin
  F := TFoo<Integer>.Create;

  WriteLn(NameOf(F)); // F
  WriteLn(NameOf(TRec)); // TRec
  WriteLn(NameOf(TBase)); // TBase
  WriteLn(NameOf(TFoo<Integer>)); // TFoo
  WriteLn(NameOf(TRec.TNested)); // TNested
  WriteLn(NameOf(Exception)); // Exception
  WriteLn(NameOf(System.SysUtils.Exception)); // Exception
  WriteLn(NameOf(Test1)); // Test1
  WriteLn(NameOf(Length)); // Length
  WriteLn(NameOf(WriteLn)); // Writeln
  WriteLn(NameOf(System.SysUtils.Format)); // Format

  ReadLN;

  F.Free;
end;

begin
  Test1;
end.
