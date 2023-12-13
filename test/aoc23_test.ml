let () =
  Alcotest.run "Advent of Code 2023" [
      "Day 0", Day00_test.suite;
      "Day 1", Day01_test.suite;
      "Day 2", Day02_test.suite;
      "Day 3", Day03_test.suite;
      "Day 4", Day04_test.suite;
      "Day 5", Day05_test.suite;
      "Day 6", Day06_test.suite;
      "Day 7", Day07_test.suite;
      "Day 8", Day08_test.suite;
      "Day 9", Day09_test.suite;
      "Day 10", Day10_test.suite;
      "Day 11", Day11_test.suite;
      "Day 13", Day13_test.suite;
    ]
