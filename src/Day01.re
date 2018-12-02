open Utils;

let str = Node_fs.readFileSync("./input1.txt", `utf8);

let input = str |> Js.String.split("\n") |> Js.Array.filter(s => s != "");

let nums =
  input->Belt.Array.map(Js.String.replace("+", "") >> int_of_string);

let day1_1 = nums => nums->Belt.Array.reduce(0, (+));

Js.Console.log("Day 1 pt 1 solution: " ++ string_of_int(day1_1(nums)));

let day1_2 = nums => {
  let len = Belt.Array.length(nums);
  let rec loop = (seenFreqs, freq, i) =>
    if (seenFreqs->Belt.Set.Int.has(freq)) {
      freq;
    } else {
      loop(
        seenFreqs->Belt.Set.Int.add(freq),
        freq + nums[i],
        (i + 1) mod len,
      );
    };

  loop(Belt.Set.Int.empty, 0, 0);
};

Js.Console.log("Day 1 pt 2 solution: " ++ string_of_int(day1_2(nums)));