with Ada.Text_IO; use Ada.Text_IO;
procedure Main4 is
  type Int_Array is array (Natural range <>) of Integer;
  A : Int_Array(0 .. 999) := (others => 0);
  function Dangerous(V1, V2 : Integer) return Integer is
  begin return V1 / V2; end Dangerous;
  function Foo(A : Int_Array; Counter : Integer) return Integer is
  begin
    if Counter = 0 then return Dangerous(A(0), Counter);
    else return Foo(A, Counter - 1); end if;
  end Foo;
begin
  Put_Line(Integer'Image(Foo(A, 6)));
end Main4;

