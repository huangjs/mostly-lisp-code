[
([],    [],     [],     [],             "start"),
([],    ["a"],  [],     [],             "a=1;"),
([],    ["b"],  [],     [],             "b=2;"),
([],    [],     ["a"],  ["L4","L6"],    "if (a==1) {"),
("L4",  ["a"],  [],     [],             "  a=3;"),
([],    ["c"],  ["a"],  ["L10"],        "  c=a;} else {"),
("L6",  [],     ["a"],  ["L7","L8"],    "  if (a==2)"),
("L7",  ["c"],  [],     ["L9"],         "    c=1;"),
("L8",  ["c"],  ["a"],  ["L9"],         "    else c=a+2;"),
("L9",  ["c"],  ["a"],  [],             "  c=a+1;}"),
("L10", [],     ["c"],  [],             "return c;")
]