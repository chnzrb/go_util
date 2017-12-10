package main

// 生成协议
import (
	"os"
	"fmt"
	"io"
	"strings"
	"path/filepath"
	"io/ioutil"
	"regexp"
	"strconv"
	"time"
	"os/exec"
	"runtime"
	"sync"
	"bytes"
	"log"
)

var includePath = "../include/"
var genSrcPath = "../src/gen/"
var protoPath = "../proto/"
var clientProtoPath = "E:/youwo_h5/trunk/resource/server_client/proto/"

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func checkFileIsExist(filename string) (bool) {
	var exist = true
	if _, err := os.Stat(filename); os.IsNotExist(err) {
		exist = false
	}
	return exist
}
func cmd(commandName string, params []string) string {
	cmd := exec.Command(commandName, params...)
	var out bytes.Buffer
	cmd.Stdout = &out
	err := cmd.Start()
	if err != nil {
		log.Fatal(err)
	}
	//log.Printf("Waiting for command to finish...")
	//fmt.Println(cmd.Args)
	err = cmd.Wait()
	if err != nil {
		log.Printf("Command finished with error: %v", err)
	}
	//fmt.Print(out.String())
	return out.String()
}

//func execCommand(commandName string, params []string) bool {
//	cmd := exec.Command(commandName, params...)
//
//	//显示运行的命令
//	//fmt.Println(cmd.Args)
//
//	stdout, err := cmd.StdoutPipe()
//
//	if err != nil {
//		fmt.Println(err)
//		return false
//	}
//
//	cmd.Start()
//
//	reader := bufio.NewReader(stdout)
//
//	//实时循环读取输出流中的一行内容
//	for {
//		line, err2 := reader.ReadString('\n')
//		if err2 != nil || io.EOF == err2 {
//			break
//		}
//		fmt.Println(line)
//	}
//
//	cmd.Wait()
//	return true
//}

func main() {
	isCreateClientProto := os.Args[1]
	t0 := time.Now()
	var socketRouterHead string

	enumMap := make(map[string]string)
	socketRouterHead += "-module(socket_router).\n"
	socketRouterHead += "-export([handle/2]).\n"
	socketRouterHead += "-include(\"common.hrl\").\n"
	socketRouterHead += "-include(\"prof.hrl\").\n\n"

	include := ""
	socketRouterBody := "handle(<<>>, State = #conn{player_id  = _PlayerId}) ->\n"
	socketRouterBody += "    State;\n"
	socketRouterBody += "handle(<<_IsZip:8, Method:32/unsigned, Data/binary>>, State = #conn{status = Status, player_id  = _PlayerId}) ->\n"

	socketRouterBody += "    %%Data1 = if IsZip == 1 -> zlib:uncompress(Data); true -> Data end,\n"
	socketRouterBody += "    ?START_PROF,\n"
	socketRouterBody += "    NewState = handle(Method, Data, State),\n"
	socketRouterBody += "    ?STOP_PROF(?MODULE, receive_proto, Method),\n"
	socketRouterBody += "    NewState.\n\n"
	messageProtoCode := ""
	messageProtoCode += fmt.Sprintf("//Auto Created :%s\n\n", time.Now().String())

	protoCodeHead := "-module(proto).\n"
	protoCodeHead += "-export([encode/1, decode/1]).\n\n"

	protoCodeEncodeBody := ""

	protoCodeDecodeBody := "decode(<<_IsZip:8, Method:32/unsigned, Data/binary>>) ->\n"
	protoCodeDecodeBody += "  decode(Method, Data).\n"

	files := getFileList(protoPath, ".proto")
	runtime.GOMAXPROCS(2)
	//c := make(chan int, 50)
	var wg sync.WaitGroup
	for _, file := range files {
		methodNameMap := make(map[string]string)

		baseName := filepath.Base(file)


		baseName = strings.TrimSuffix(baseName, ".proto")

		context, err := ReadAll(file)
		check(err)

		array := strings.Split(baseName, "_")
		if len(array) != 2 {
			//fmt.Println(" [ignore]")
			continue
		}
		fmt.Printf("\n\nDecode proto %s.proto%s", baseName, strings.Repeat(".", 45-len(baseName)))
		moduleNum, err := strconv.Atoi(array[0])
		check(err)
		moduleName := array[1]

		erlReadProtoFile := protoPath + moduleName + ".proto"
		filePutContext(erlReadProtoFile, context)
		a := []string{
			"proto.escript",
			"-type",
			"-W",
			"-Werror",
			"-msgtolower",
			"-modprefix",
			"p_",
			"-I",
			protoPath,
			"-o-erl",
			"../src/gen/",
			"-o-hrl",
			"../include/",
			"-v",
			"always",
			erlReadProtoFile,
		}
		wg.Add(1)
		go func(commandArgs [] string, file string ) {
			defer wg.Done()
			//execCommand("escript", commandArgs)
			out := cmd("escript", commandArgs)
			err = os.Remove(erlReadProtoFile)
			check(err)

			if out != "" {
				panic("\n" + file + "\n" + out)
			}
		}(a, file)

		context = regexp.MustCompile(`import[ \t\n\r]*"[ \t\n\r]*[\w]+.proto[ \t\n\r]*"[ \t\n\r]*;`).ReplaceAllString(context, "")
		include += fmt.Sprintf("-include(\"p_%s.hrl\").\n", moduleName)

		minMsgNum := moduleNum * 100
		maxMsgNum := minMsgNum + 100
		msgNum := minMsgNum

		messageProtoCode += fmt.Sprintf("\n/*************************************%s:[%d, %d]********************************************/\n", moduleName, minMsgNum, maxMsgNum)

		reg := regexp.MustCompile(`message[ \t\n\r]+m_([\w]+)_tos[ \t\n\r]+{`)
		matchArray := reg.FindAllStringSubmatch(context, -1)
		for _, msg := range matchArray {
			methodName := msg[1]

			msgNum += 1

			msgName := fmt.Sprintf("m_%s_tos", methodName)

			if methodNameMap[msgName] != "" {
				panic("\nmethodName repeated:" + msgName)
				os.Exit(1)
			}

			methodNameMap[msgName] = "0"

			clientMsgName := fmt.Sprintf("m_%s_%s_tos", moduleName, methodName)

			context = regexp.MustCompile(msgName).ReplaceAllString(context, clientMsgName)

			messageProtoCode += fmt.Sprintf("//<%s:%d>\n", clientMsgName, msgNum)

			protoCodeEncodeBody += fmt.Sprintf("encode(#%s{} = Msg) ->\n", msgName)

			protoCodeEncodeBody += fmt.Sprintf("    Bin = p_%s:encode_msg(Msg),\n", moduleName)

			protoCodeEncodeBody += fmt.Sprintf("    encode(%d, Bin);\n", msgNum)

			protoCodeDecodeBody += fmt.Sprintf("decode(%d, Bin) ->\n", msgNum)

			protoCodeDecodeBody += fmt.Sprintf("  p_%s:decode_msg(Bin, %s);\n", moduleName, msgName)

			if msgNum == 1 {
				socketRouterBody += fmt.Sprintf("handle(%d, Bin, State = #conn{player_id = _PlayerId}) ->\n", msgNum)
			} else if msgNum == 2 {
				socketRouterBody += fmt.Sprintf("handle(%d, Bin, State = #conn{status = ?CLIENT_STATE_WAIT_CREATE_ROLE, player_id = _PlayerId}) ->\n", msgNum)
			} else if msgNum == 3 {
				socketRouterBody += fmt.Sprintf("handle(%d, Bin, State = #conn{status = ?CLIENT_STATE_WAIT_ENTER_GAME, player_id = _PlayerId}) ->\n", msgNum)
			} else if msgNum >= 10000 && msgNum < 10100 {
				socketRouterBody += fmt.Sprintf("handle(%d, Bin, State = #conn{player_id = _PlayerId}) ->\n", msgNum)
				socketRouterBody += fmt.Sprintf("    ?ASSERT(?IS_DEBUG, {proto_no_debug, %d}),\n", msgNum)
			} else {
				socketRouterBody += fmt.Sprintf("handle(%d, Bin, State = #conn{status = ?CLIENT_STATE_ENTER_GAME, player_id = _PlayerId}) ->\n", msgNum)
			}

			socketRouterBody += fmt.Sprintf("    Msg = p_%s:decode_msg(Bin, %s),\n", moduleName, msgName)
			socketRouterBody += "    mod_log:write_player_receive_proto_log(_PlayerId, Msg),\n"

			socketRouterBody += fmt.Sprintf("    api_%s:%s(Msg, State);\n", moduleName, methodName)
		}
		reg = regexp.MustCompile(`message[ \t\n\r]+m_([\w]+)_toc[ \t\n\r]+{`)
		matchArray = reg.FindAllStringSubmatch(context, -1)
		for _, msg := range matchArray {
			methodName := msg[1]
			//fmt.Println(msg[1])

			msgNum += 1

			msgName := fmt.Sprintf("m_%s_toc", methodName)

			if methodNameMap[msgName] != "" {
				panic("methodName repeated:" + methodName)
				os.Exit(1)
			}

			methodNameMap[msgName] = "0"

			clientMsgName := fmt.Sprintf("m_%s_%s_toc", moduleName, methodName)

			context = regexp.MustCompile(msgName).ReplaceAllString(context, clientMsgName)

			messageProtoCode += fmt.Sprintf("//<%s:%d>\n", clientMsgName, msgNum)

			protoCodeEncodeBody += fmt.Sprintf("encode(#%s{} = Msg) ->\n", msgName)

			protoCodeEncodeBody += fmt.Sprintf("    Bin = p_%s:encode_msg(Msg),\n", moduleName)

			protoCodeEncodeBody += fmt.Sprintf("    encode(%d, Bin);\n", msgNum)

			protoCodeDecodeBody += fmt.Sprintf("decode(%d, Bin) ->\n", msgNum)

			protoCodeDecodeBody += fmt.Sprintf("  p_%s:decode_msg(Bin, %s);\n", moduleName, msgName)
		}
		messageProtoCode += context

		reg = regexp.MustCompile(`enum[ \t\n\r]+[\w]+[ \t\n\r]*{([^}]*)}`)
		matchArray = reg.FindAllStringSubmatch(context, -1)
		for _, msg := range matchArray {
			reg = regexp.MustCompile(`[ \t\n\r]*([\w]+)[ \t\n\r]*=[ \t\n\r]*[\d]+[ \t\n\r]*;`)
			for _, enumArray := range reg.FindAllStringSubmatch(msg[1], -1) {
				//fmt.Print(enumArray[1])
				enumMap[enumArray[1]] = "1"
			}
		}

		fmt.Println(" [ok]")

	}

	enumCode := ""
	for enum, _ := range enumMap {
		//fmt.Println(enum)
		enumCode += fmt.Sprintf("-define(P_%s, %s).\n", strings.ToUpper(enum), enum)
	}
	filePutContext(includePath+"p_enum.hrl", enumCode)

	protoCodeEncodeBody += "encode(Other) -> exit({unknow_msg, Other}).\n"

	socketRouterBody += "handle(MsgNum, _, #conn{status = Status}) ->\n"
	socketRouterBody += "    exit({unexpected_proto_num, MsgNum, Status}).\n"

	protoCodeDecodeBody += "decode(MsgNum, _) ->\n"
	protoCodeDecodeBody += "    exit({unexpected_proto_num, MsgNum}).\n"

	protoCodeEncodeBody += "\n\nencode(MethodNum, Bin) ->\n"
	protoCodeEncodeBody += "    Len = iolist_size(Bin),\n"
	protoCodeEncodeBody += "    if Len > 150000 ->\n"
	protoCodeEncodeBody += "        Bin1 = zlib:compress(Bin),\n"
	protoCodeEncodeBody += "        Len1 = iolist_size(Bin1),\n"
	protoCodeEncodeBody += "        if Len1 < Len ->\n"
	protoCodeEncodeBody += "            io:format(\"proto compress:~p~n\", [{MethodNum, Len, Len1}]),\n"
	protoCodeEncodeBody += "            websocket_util:pack_packet(<<1, MethodNum:32, Bin1/binary>>);\n"
	protoCodeEncodeBody += "            true ->\n"
	protoCodeEncodeBody += "                websocket_util:pack_packet(<<0, MethodNum:32, Bin/binary>>)\n"
	protoCodeEncodeBody += "        end;\n"
	protoCodeEncodeBody += "    true ->\n"
	protoCodeEncodeBody += "        websocket_util:pack_packet(<<0, MethodNum:32, Bin/binary>>)\n"
	protoCodeEncodeBody += "    end.\n"

	include += "\n\n"
	filePutContext(genSrcPath+"socket_router.erl", socketRouterHead+include+socketRouterBody)
	filePutContext(genSrcPath+"proto.erl", protoCodeHead+include+protoCodeEncodeBody+protoCodeDecodeBody)
	if isCreateClientProto == "true" {
		filePutContext(clientProtoPath+"message.proto", messageProtoCode)
	}
	wg.Wait()
	usedTime := time.Since(t0)
	fmt.Print("\n\n")
	fmt.Print("*************************************************************\n\n")
	fmt.Print("                      build proto success\n\n")
	fmt.Printf("                         %s second\n\n", usedTime.String())
	fmt.Print("*************************************************************\n\n")
}

func ReadAll(filePth string) (string, error) {
	f, err := os.Open(filePth)
	if err != nil {
		return "", err
	}

	context, err := ioutil.ReadAll(f)
	return string(context), err
}
func filePutContext(filename string, context string) {
	var f *os.File
	var err error
	if checkFileIsExist(filename) {
		//如果文件存在
		del := os.Remove(filename)
		if del != nil {
			fmt.Println(del)
		}
	}
	f, err = os.Create(filename) //创建文件
	check(err)
	_, err = io.WriteString(f, context)
	f.Close()
	check(err)
}

//func get_base_name(fileName string, ext string) string {
//	baseName := filepath.Base(fileName)
//	return strings.TrimSuffix(baseName, ext)
//}

func getFileList(path string, suffix string) (files []string) {
	files = make([]string, 0, 10)
	err := filepath.Walk(path, func(path string, f os.FileInfo, err error) error {
		if f == nil {
			return err
		}
		if f.IsDir() {
			return nil
		}
		if strings.HasSuffix(path, suffix) {
			files = append(files, path)
			//println(path)
		}
		//println(path)
		return nil
	})

	if err != nil {
		fmt.Printf("filepath.Walk() returned %v\n", err)
	}
	return files
}

//获取指定目录下的所有文件，不进入下一级目录搜索，可以匹配后缀过滤。
//func ListDir(dirPth string, suffix string) (files []string, err error) {
//	files = make([]string, 0, 10)
//	dir, err := ioutil.ReadDir(dirPth)
//	if err != nil {
//		return nil, err
//	}
//	PthSep := string(os.PathSeparator)
//	suffix = strings.ToUpper(suffix) //忽略后缀匹配的大小写
//	for _, fi := range dir {
//		if fi.IsDir() { // 忽略目录
//			continue
//		}
//		if strings.HasSuffix(strings.ToUpper(fi.Name()), suffix) { //匹配文件
//			files = append(files, dirPth+PthSep+fi.Name())
//		}
//	}
//	return files, nil
//}
//
////获取指定目录及所有子目录下的所有文件，可以匹配后缀过滤。
//func WalkDir(dirPth, suffix string) (files []string, err error) {
//	files = make([]string, 0, 30)
//	suffix = strings.ToUpper(suffix)                                                     //忽略后缀匹配的大小写
//	err = filepath.Walk(dirPth, func(filename string, fi os.FileInfo, err error) error { //遍历目录
//		//if err != nil { //忽略错误
//		// return err
//		//}
//		if fi.IsDir() { // 忽略目录
//			return nil
//		}
//		if strings.HasSuffix(strings.ToUpper(fi.Name()), suffix) {
//			files = append(files, filename)
//		}
//		return nil
//	})
//	return files, err
//}
