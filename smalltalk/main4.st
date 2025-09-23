Object subclass: Chain2 [
  Chain2 class >> dangerous: v1 v2: v2 [ ^v1 // v2 ]
  Chain2 class >> foo: array counter: c [
    c = 0 ifTrue: [ ^self dangerous: (array at: 1) v2: c ].
    ^self foo: array counter: c - 1
  ]
]
| array |
array := (Array new: 1000) atAllPut: 0; yourself.
Transcript show: 'The result is ', (Chain2 foo: array counter: 6) printString; cr.

