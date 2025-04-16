package main

import (
	"fmt"
	"mono/repl"
	"os"
	"os/user"
)

func main() {
	user, err := user.Current()
	if err != nil {
		panic(err)
	}

	fmt.Printf("Hello %s! This is the Mono lang\n", user.Username)
	repl.Start(os.Stdin, os.Stdout)
}
