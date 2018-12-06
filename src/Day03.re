open Utils;

let str = Node_fs.readFileSync("./input3.txt", `utf8);

let input = str |> Js.String.split("\n") |> Js.Array.filter(s => s != "");

/* #1 @ 258,327: 19x22 */

type claim = {
  id: int,
  left: int,
  top: int,
  width: int,
  height: int,
};

let parseClaim = str => {
  let [|idStr, _, topLeft, dimensions|] = str |> Js.String.split(" ");
  let id = idStr->Js.String.substringToEnd(~from=1) |> int_of_string;
  let [|leftStr, topStr|] = topLeft |> Js.String.split(",");
  let left = leftStr |> int_of_string;
  let top = topStr |> Js.String.slice(~from=0, ~to_=-1) |> int_of_string;
  let [|width, height|] =
    dimensions |> Js.String.split("x") |> Js.Array.map(int_of_string);
  {id, left, top, width, height};
};

let claims = input->Belt.Array.map(parseClaim);

Js.Console.log(claims);

/* let foo = Bigarray.Array2.create(Int32, C_layout); */

let (fabricWidth, fabricHeight) =
  claims->Belt.Array.reduce((0, 0), ((x, y), claim) =>
    (max(x, claim.left + claim.width), max(y, claim.top + claim.height))
  );

Js.Console.log("fabric width: " ++ string_of_int(fabricWidth));
Js.Console.log("fabric height: " ++ string_of_int(fabricHeight));

let cleanClaims = Belt.MutableSet.Int.make();

let fabric = {
  let fabric = Array.make_matrix(fabricWidth, fabricHeight, []);
  Belt.Array.(
    claims->forEach(claim => {
      cleanClaims->Belt.MutableSet.Int.add(claim.id);
      range(claim.left, claim.left + claim.width - 1)
      ->forEach(x =>
          range(claim.top, claim.top + claim.height - 1)
          ->forEach(y => {
              let ids = [claim.id, ...fabric[x][y]];
              fabric[x][y] = ids;
              if (List.length(ids) > 1) {
                cleanClaims->Belt.MutableSet.Int.removeMany(
                  Array.of_list(ids),
                );
              };
            })
        );
    })
  );
  fabric;
};

let countOverlappingSqures = {
  let count = ref(0);
  for (x in 0 to fabricWidth - 1) {
    for (y in 0 to fabricHeight - 1) {
      if (List.length(fabric[x][y]) > 1) {
        count := count^ + 1;
      };
    };
  };
  count^;
};

Js.Console.log("count is " ++ string_of_int(countOverlappingSqures));

Js.Console.log(
  "non-overlapping claims: "
  ++ Js.Array.join(Belt.MutableSet.Int.toArray(cleanClaims)),
);