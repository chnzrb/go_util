@echo off
protoc.exe --plugin=protoc-gen-go=%GOPATH%\bin\protoc-gen-go.exe  --go_out=./ ./debug.proto
pause
