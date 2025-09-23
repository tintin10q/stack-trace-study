with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
   type Int_Array is array (Natural range <>) of Integer;
   A : Int_Array(0 .. 999) := (others => 0);
   function Dangerous(A : Int_Array; Index : Integer) return Integer is
   begin
      return A(Index + 2);
   end Dangerous;
   function Foo(A : Int_Array; Index : Integer) return Integer is
   begin
      return Dangerous(A, Index);
   end Foo;
   function Foo1(A : Int_Array; Index : Integer) return Integer is
   begin
      return Foo(A, Index * 3);
   end Foo1;
   function Foo2(A : Int_Array; Index : Integer) return Integer is
   begin
      return Foo1(A, Index + 137);
   end Foo2;
   function Foo3(A : Int_Array; Index : Integer) return Integer is
   begin
      return Foo2(A, Index - 1);
   end Foo3;
   function Foo4(A : Int_Array; Index : Integer) return Integer is
   begin
      return Foo3(A, Index * 137);
   end Foo4;
   function Foo5(A : Int_Array; Index : Integer) return Integer is
   begin
      return Foo4(A, Index + 20);
   end Foo5;
   function Foo6(A : Int_Array; Index : Integer) return Integer is
   begin
      return Foo5(A, Index / 3);
   end Foo6;
begin
   Put_Line(Integer'Image(Foo6(A, 50)));
end Main;

