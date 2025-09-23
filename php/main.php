<?php
function dangerous($array, $index) { return $array[$index + 2]; }
function foo($array, $index) { return dangerous($array, $index); }
function foo1($array, $index) { return foo($array, $index * 3); }
function foo2($array, $index) { return foo1($array, $index + 137); }
function foo3($array, $index) { return foo2($array, $index - 1); }
function foo4($array, $index) { return foo3($array, $index * 137); }
function foo5($array, $index) { return foo4($array, $index + 20); }
function foo6($array, $index) { return foo5($array, intdiv($index, 3)); }
$array = array_fill(0, 1000, 0);
echo foo6($array, 50), "\n";

