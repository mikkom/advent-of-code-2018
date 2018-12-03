open Utils;

let toChars = Js.String.split("");

let str = Node_fs.readFileSync("./input2.txt", `utf8);

let ids = str |> Js.String.split("\n") |> Js.Array.filter(s => s != "");

let countChars = str => {
  open Belt.HashMap.String;
  let chars = toChars(str);
  let counts = make(~hintSize=Js.String.length(str));
  chars->Belt.Array.forEach(ch =>
    switch (counts->get(ch)) {
    | Some(count) => counts->set(ch, count + 1)
    | None => counts->set(ch, 1)
    }
  );
  counts;
};

let getChecksum = ids => {
  let charCounts =
    ids->Belt.Array.map(
      countChars >> Belt.HashMap.String.valuesToArray >> Belt.Set.Int.fromArray,
    );
  let twos =
    charCounts
    ->Belt.Array.keep(set => set->Belt.Set.Int.has(2))
    ->Belt.Array.length;
  let threes =
    charCounts
    ->Belt.Array.keep(set => set->Belt.Set.Int.has(3))
    ->Belt.Array.length;
  twos * threes;
};

let day2_1 = getChecksum(ids);

Js.Console.log("Day 2 pt 1 solution: " ++ string_of_int(day2_1));

let dropLetterAt = (i, str) =>
  Js.String.substring(~from=0, ~to_=i, str)
  ++ Js.String.substringToEnd(~from=i + 1, str);

let findDuplicateString = arr => {
  open Belt.Set.String;
  let len = Array.length(arr);
  let rec loop = (acc, i) =>
    if (i >= len) {
      None;
    } else {
      let str = arr[i];
      if (acc->has(str)) {
        Some(str);
      } else {
        loop(acc->add(str), i + 1);
      };
    };
  loop(empty, 0);
};

let findOffByOneString = arr => {
  open Belt.Array;
  let findStr = i => arr->map(dropLetterAt(i)) |> findDuplicateString;
  let len = Js.String.length(arr[0]);
  let rec loop = i =>
    if (i >= len) {
      None;
    } else {
      switch (findStr(i)) {
      | None => loop(i + 1)
      | x => x
      };
    };
  loop(0);
};

let day2_2 = findOffByOneString(ids);

Js.Console.log("Day 2 pt 2 solution: " ++ Belt.Option.getExn(day2_2));