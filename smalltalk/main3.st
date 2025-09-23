"Run with: gst smalltalk/main3.st"
Object subclass: Main3 [
    Main3 class >> dangerous: array index: idx [
        ^ array at: idx. "Out of bounds raises an error"
    ]

    Main3 class >> foo: array counter: counter [
        counter = 0
            ifTrue: [ ^ self dangerous: array index: counter + 9137 ].
        ^ self foo: array counter: counter - 1.
    ]

    Main3 class >> main [
        | a r |
        a := Array new: 1000 withAll: 0.
        r := self foo: a counter: 900.
        Transcript show: 'The result is ', r printString; cr.
    ]
].

Main3 main.
