Object subclass: Chain [
  Chain class >> dangerous: array index: index [ ^array at: index + 3 ] "1-based"
  Chain class >> foo: array index: index [ ^self dangerous: array index: index ]
  Chain class >> foo1: array index: index [ ^self foo: array index: index * 3 ]
  Chain class >> foo2: array index: index [ ^self foo1: array index: index + 137 ]
  Chain class >> foo3: array index: index [ ^self foo2: array index: index - 1 ]
  Chain class >> foo4: array index: index [ ^self foo3: array index: index * 137 ]
  Chain class >> foo5: array index: index [ ^self foo4: array index: index + 20 ]
  Chain class >> foo6: array index: index [ ^self foo5: array index: index // 3 ]
]
| array |
array := (Array new: 1000) atAllPut: 0; yourself.
Transcript show: (Chain foo6: array index: 50); cr.

