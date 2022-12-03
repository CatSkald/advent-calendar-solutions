package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
)

func readLines(path string) ([]string) {
    file, _ := os.Open(path)
    defer file.Close()

    var lines []string
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        lines = append(lines, scanner.Text())
    }
    return lines
}

func getCalories(lines []string)(calories []int){
	current := 0
	for count, line := range lines {
		if line == "" {
			calories = append(calories, current)
			current = 0
		} else if count == len(lines) - 1{
			value, _ := strconv.Atoi(line)
			current = current + value
			calories = append(calories, current)
		} else {
			value, _ := strconv.Atoi(line)
			current = current + value
		}
	}
	return
}

func main() {
	input := "../input/1input.txt"
	lines := readLines(input)
 
	calories := getCalories(lines)
    sort.Sort(sort.Reverse(sort.IntSlice(calories)))

	fmt.Println("Top 1:", calories[0])
	fmt.Println("Top 3:", calories[0] + calories[1] + calories[2])
}