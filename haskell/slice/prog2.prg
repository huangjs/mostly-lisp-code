[
([],    [],     [],     [],             "start"),
([],    ["inw"],[],     [],             "inw = FALSE;"),
([],    ["nl"], [],     [],             "nl = 0;"),
([],    ["nw"], [],     [],             "nw = 0;"),
([],    ["nc"], [],     [],             "nc = 0;"),
([],    ["c"],  [],     [],             "c = read();"),
("L6",  [],     ["c"],  ["L7","L16"],   "while (c != EOF) {"),
("L7",  ["nc"], ["nc"], [],             "  nc++;"),
([],    [],     ["c"],  ["L9","L10"],   "  if (c == '\\n')"),
("L9",  ["nl"], ["nl"], [],             "    nl++;"),
("L10", [],     ["c"],  ["L11","L12"],  "  if (c==' '||c=='\\n'||c=='\\t')"),
("L11", ["inw"],[],     [],             "    inw = FALSE;"),
("L12", [],     ["inw"],["L13","L15"],  "  else if (inw == FALSE) {"),
("L13", ["inw"],[],     [],             "    inw = TRUE;"),
([],    ["nw"], ["nw"], [],             "    nw++; }"),
("L15", ["c"],  [],     ["L6"],         "  c = read(); }"),
("L16", [],     ["nl"], [],             "write(nl);"),
([],    [],     ["nw"], [],             "write(nw);"),
([],    [],     ["nc"], [],             "write(nc);")
]
