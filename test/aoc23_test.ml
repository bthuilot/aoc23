let () =
  Alcotest.run "Advent of Code 2023" [
      "Day 0", Day00_test.suite;
      "Day 1", Day01_test.suite;
      "Day 2", Day02_test.suite;
      "Day 3", Day03_test.suite
    ]