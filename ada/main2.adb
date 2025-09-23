with Ada.Text_IO; use Ada.Text_IO;
procedure Main2 is
   type Int_Array is array (Integer range 0 .. 999) of Integer;
   function Dangerous(A : Int_Array; Index : Integer) return Integer is
   begin
      return A(Index); -- will raise Constraint_Error when Index=9137
   end Dangerous;

   function Foo(A : Int_Array; Counter : Integer) return Integer is
   begin
      if Counter = 0 then
         return Dangerous(A, Counter + 9137);
      else
         return Foo(A, Counter - 1);
      end if;
   end Foo;

   A : Int_Array := (others => 0);
   R : Integer;
begin
   R := Foo(A, 6);
   Put_Line("The result is " & Integer'Image(R));
end Main2;
