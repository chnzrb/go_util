package main

import (
	"fmt"
	"os"
	"runtime"
	"path/filepath"
)

func checkerr(err error, pre string) {
	if err != nil {
		_, file, line, _ := runtime.Caller(1)
		fileBaseName := filepath.Base(file)
		fmt.Printf("[ERROR]%s:%d %s %v", fileBaseName, line, pre, err)
		os.Exit(-1)
	}
}
