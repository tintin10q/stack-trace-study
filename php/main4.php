<?php
function dangerous($v1,$v2){ return intdiv($v1,$v2); }
function foo($array,$counter){
  if($counter===0){ return dangerous($array[0], $counter); }
  return foo($array, $counter-1);
}
$array = array_fill(0, 1000, 0);
echo "The result is ".foo($array, 6).PHP_EOL;

