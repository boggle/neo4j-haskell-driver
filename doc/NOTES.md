= Serialization and Value Types

[source]
----
             (Atomic a)------------[ atomize ]---------->(Atom)------------[put]----------\
             (Atomic a)<-----------[construct]-----------(Atom)------------[get]<---------|
                        \                                |                                |
 PRIVATE [v #encodeAtomic\#decodeAtomic v]   [^ #unEncode|#Encode v] PRIVATE              |
                          \                              |                                |
                           +-----------------------------|                                |
                                                         |                                |
                             ^^^^^^^^ INTERNAL ^^^^^^^^  | vvvvvvvv PUBLIC vvvvvvvv       |
                                                         |                                |
                           +-----------------------------(EncodedValue)                   |
                          /                                                               |
           [^ encodeValue/decoveValue v]                                                  (Binary)
                       /                                                                  |
            (Valued a)------[ toValue ]----->(Value)-----[toAnyValue  ]----->(AnyValue)   |
            (Valued a)<-----[fromValue]------(Value)-----[fromAnyValue]------(AnyValue)   |
                      |                                                                   |
          [v packValue|unPackValue ^]                                                     |
                      |                                                                   |
        (PackedValue a)-------------------------------[put]-------------------------------|
        (PackedValue a)-------------------------------[get]-------------------------------/
---

= Modules

Top-Level Namespace is Database.Neo4j

o Value: All user relevant types

o Internal.Packstream: Pack stream serialization
o Internal.Utl: Utility classes
