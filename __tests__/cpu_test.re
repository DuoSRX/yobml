open Jest

describe("Whatever", () => {
  open Expect;

  test("toBe", () =>
    expect(1 + 2) |> toBe(3));
});
