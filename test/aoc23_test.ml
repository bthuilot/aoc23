let () =
  Alcotest.run "Advent of Code 2023" [
      "Day 0", Day00_test.suite;
      "Day 1", Day01_test.suite
    ]