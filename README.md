vinyl-json
==========

Automatic json instances for Data.Vinyl


Take a look!

    example_json = "{\"name\": \"jon\", \"job\":\"Code\", \"age\":42, \"things\":[1,2,3] }"

    parsed_example :: Maybe (PlainRec '[("name" ::: Text),
                                        ("job"  ::: Text),
                                        ("age"  ::: Int),
                                        ("things" ::: [Int])])
    parsed_example = decode example_json



At the moment, it only works for `PlainRec`s but more might happen in the future.
