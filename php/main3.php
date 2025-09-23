<?php
function dangerous($array, $index) {
    return $array[$index] + 1; // will be null+1 -> TypeError in PHP 8
}
function foo($array, $counter) {
    if ($counter === 0) return dangerous($array, $counter + 9137);
    return foo($array, $counter - 1);
}
$array = array_fill(0, 1000, 0);
$result = foo($array, 900);
echo "The result is $result\n";
