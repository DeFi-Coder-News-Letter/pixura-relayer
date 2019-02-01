module Test (test, Person(..)) where

type Person = {
    age :: Number,
    name :: String
}

test :: Person
test = {
    age: 1.0,
    name: "Charlie"
}
