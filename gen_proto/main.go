package main

// 生成协议
import (
	"os"
	"fmt"
	//"io"
	"strings"
	"path/filepath"
	"io/ioutil"
	"regexp"
	"strconv"
	"time"
	"os/exec"
	//"runtime"
	//"sync"
	"bytes"
	"log"
	"runtime"
	"io"
)

var includePath = "../include/"
var genSrcPath = "../src/gen/"
var protoPath = "../proto/"
var tmpProtoPath = "../proto/"
var clientProtoPath = "../../server_client/proto/"

func check(err error, msg ...string) {
	//if e != nil {
	//	panic(e)
	//}
	if err != nil {
		_, file, line, _ := runtime.Caller(1)
		fileBaseName := filepath.Base(file)
		fmt.Printf("[ERROR]%s:%d %s %v", fileBaseName, line, msg, err)
		os.Exit(1)
	}
}

func checkFileIsExist(filename string) (bool) {
	var exist = true
	if _, err := os.Stat(filename); os.IsNotExist(err) {
		exist = false
	}
	return exist
}
func cmd(commandName string, params []string) (string, error) {
	cmd := exec.Command(commandName, params...)
	var out bytes.Buffer
	cmd.Stdout = &out
	err := cmd.Start()
	if err != nil {
		log.Fatal(err)
	}
	err = cmd.Wait()
	//check(err, out.String())
	//if err != nil {
	//	log.Printf("Command finished with error: %v \nout: %v", err, out.String())
	//	panic(err)
	//}
	//fmt.Print(out.String())
	return out.String(), err
}


func main() {
	isCreateClientProto := os.Args[1]
	t0 := time.Now()
	var socketRouterHead string

	enumMap := make(map[string]string, 200)
	socketRouterHead += "-module(socket_router).\n"
	socketRouterHead += "-export([handle/2]).\n"
	socketRouterHead += "-include(\"common.hrl\").\n"
	socketRouterHead += "-include(\"prof.hrl\").\n\n"
	socketRouterHead += "-include(\"p_message.hrl\").\n\n"
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
	messageProtoCode += "package proto;\n"
	gmContent := ""
	protoCodeHead := "-module(proto).\n"
	protoCodeHead += "-export([encode/1, decode/1]).\n"
	protoCodeHead += "-include(\"p_message.hrl\").\n\n"

	protoCodeHead += "-ifdef(debug).\n"
	protoCodeHead += "-define(ENCODE_OPTS, [{verify, true}]).\n"
	protoCodeHead += "-else.\n"
	protoCodeHead += "-define(ENCODE_OPTS, []).\n"
	protoCodeHead += "-endif.\n\n"


	protoCodeEncodeBody := ""

	protoCodeDecodeBody := "decode(<<_IsZip:8, Method:32/unsigned, Data/binary>>) ->\n"
	protoCodeDecodeBody += "  decode(Method, Data).\n"

	files := getFileList(protoPath, ".proto")
	runtime.GOMAXPROCS(2)

	//var wg sync.WaitGroup
	nameReg := regexp.MustCompile(`(\d+)_(\w+)`)
	for _, file := range files {
		methodNameMap := make(map[string]string, 15)

		baseName := filepath.Base(file)


		baseName = strings.TrimSuffix(baseName, ".proto")

		//array := strings.Split(baseName, "_")
		array := nameReg.FindAllStringSubmatch(baseName, 1)
		//fmt.Println("nameArray:", array[0])
		//if len(array) < 2 {
		//	continue
		//}
		fmt.Printf("\n\nDecode proto %s.proto%s", baseName, strings.Repeat(".", 45-len(baseName)))
		moduleNum, err := strconv.Atoi(array[0][1])
		check(err)
		moduleName := array[0][2]

		reg := regexp.MustCompile(`message\s+m_(\w+)_to([cs])\s*{`)
		context, err := ReadAll(file)
		check(err)
		noAnnotationContext := regexp.MustCompile(`//[^\n]*`).ReplaceAllString(context, "")


		regexp.MustCompile(`\s*is_platform\s*:\s*"\w"`).ReplaceAllString(context, "is_platform:\"s1\"")
		//erlReadProtoFile := tmpProtoPath + moduleName + ".proto"


		//wg.Add(1)

		go func() {
			//commandArgs := []string{
			//	"proto.escript",
			//	"-type",
			//	"-W",
			//	"-Werror",
			//	"-msgtolower",
			//	"-modprefix",
			//	"p_",
			//	"-I",
			//	tmpProtoPath,
			//	"-o-erl",
			//	genSrcPath,
			//	"-o-hrl",
			//	includePath,
			//	"-v",
			//	"always",
			//	erlReadProtoFile,
			//}
			//filePutContext(erlReadProtoFile, noAnnotationContext)
			//defer wg.Done()
			//defer func() {
			//	err = os.Remove(erlReadProtoFile)
			//	check(err)
			//}()
			//out, err :=cmd("escript", commandArgs)
			//check(err, erlReadProtoFile, out)
		}()

		context = regexp.MustCompile(`import\s*"\s*\w+.proto\s*"\s*;`).ReplaceAllString(context, "")
		//include += fmt.Sprintf("-include(\"p_%s.hrl\").\n", moduleName)


		minMsgNum := moduleNum * 100
		maxMsgNum := minMsgNum + 100
		msgNum := minMsgNum

		if moduleNum == 99 {
			gmContent += fmt.Sprintf("\n/*************************************%s:[%d, %d]********************************************/\n", moduleName, minMsgNum, maxMsgNum)
		} else {
			messageProtoCode += fmt.Sprintf("\n/*************************************%s:[%d, %d]********************************************/\n", moduleName, minMsgNum, maxMsgNum)
		}


		matchArray := reg.FindAllStringSubmatch(noAnnotationContext, -1)
		for _, msg := range matchArray {
			methodName := msg[1]
			msgType := msg[2]
			msgNum += 1

			if msgType == "s" {
				msgName := fmt.Sprintf("m_%s_tos", methodName)

				if methodNameMap[msgName] != "" {
					panic("\nmethodName repeated:" + msgName)
					os.Exit(1)
				}

				methodNameMap[msgName] = "0"

				clientMsgName := fmt.Sprintf("m_%s_%s_tos", moduleName, methodName)

				context = regexp.MustCompile(msgName).ReplaceAllString(context, clientMsgName)

				if moduleNum == 99 {
					gmContent += fmt.Sprintf("//<%s:%d>\n", clientMsgName, msgNum)
				} else {
					messageProtoCode += fmt.Sprintf("//<%s:%d>\n", clientMsgName, msgNum)
				}


				protoCodeEncodeBody += fmt.Sprintf("encode(#%s{} = Msg) ->\n", clientMsgName)

				protoCodeEncodeBody += fmt.Sprintf("    Bin = p_message:encode_msg(Msg, ?ENCODE_OPTS),\n")

				protoCodeEncodeBody += fmt.Sprintf("    encode(%d, Bin);\n", msgNum)

				protoCodeDecodeBody += fmt.Sprintf("decode(%d, Bin) ->\n", msgNum)

				protoCodeDecodeBody += fmt.Sprintf("  p_message:decode_msg(Bin, %s);\n", clientMsgName)

				if msgNum == 1 || (msgNum >= 9900 && msgNum < 10000) {
					socketRouterBody += fmt.Sprintf("handle(%d, Bin, State = #conn{player_id = _PlayerId}) ->\n", msgNum)
				} else if msgNum == 3 {
					socketRouterBody += fmt.Sprintf("handle(%d, Bin, State = #conn{status = ?CLIENT_STATE_WAIT_CREATE_ROLE, player_id = _PlayerId}) ->\n", msgNum)
				} else if msgNum == 5 {
					socketRouterBody += fmt.Sprintf("handle(%d, Bin, State = #conn{status = ?CLIENT_STATE_WAIT_ENTER_GAME, player_id = _PlayerId}) ->\n", msgNum)
				} else if msgNum >= 10000 && msgNum < 10100  {
					socketRouterBody += fmt.Sprintf("handle(%d, Bin, State = #conn{player_id = _PlayerId}) ->\n", msgNum)
					socketRouterBody += fmt.Sprintf("    ?ASSERT(?IS_DEBUG, {proto_no_debug, %d}),\n", msgNum)
				} else {
					socketRouterBody += fmt.Sprintf("handle(%d, Bin, State = #conn{status = ?CLIENT_STATE_ENTER_GAME, player_id = _PlayerId}) ->\n", msgNum)
				}

				socketRouterBody += fmt.Sprintf("    Msg = p_message:decode_msg(Bin, %s),\n", clientMsgName)
				socketRouterBody += "    mod_log:write_player_receive_proto_log(_PlayerId, Msg),\n"

				socketRouterBody += fmt.Sprintf("    api_%s:%s(Msg, State);\n", moduleName, methodName)
			}else {
				msgName := fmt.Sprintf("m_%s_toc", methodName)

				if methodNameMap[msgName] != "" {
					panic("methodName repeated:" + methodName)
					os.Exit(1)
				}

				methodNameMap[msgName] = "0"

				clientMsgName := fmt.Sprintf("m_%s_%s_toc", moduleName, methodName)

				context = regexp.MustCompile(msgName).ReplaceAllString(context, clientMsgName)



				if moduleNum == 99 {
					gmContent += fmt.Sprintf("//<%s:%d>\n", clientMsgName, msgNum)
				} else {
					messageProtoCode += fmt.Sprintf("//<%s:%d>\n", clientMsgName, msgNum)
				}

				protoCodeEncodeBody += fmt.Sprintf("encode(#%s{} = Msg) ->\n", clientMsgName)

				protoCodeEncodeBody += fmt.Sprintf("    Bin = p_message:encode_msg(Msg, ?ENCODE_OPTS),\n")

				protoCodeEncodeBody += fmt.Sprintf("    encode(%d, Bin);\n", msgNum)

				protoCodeDecodeBody += fmt.Sprintf("decode(%d, Bin) ->\n", msgNum)

				protoCodeDecodeBody += fmt.Sprintf("  p_message:decode_msg(Bin, %s);\n", clientMsgName)
			}
		}
		//reg = regexp.MustCompile(`message[ \t\n\r]+m_([\w]+)_toc[ \t\n\r]+{`)
		//matchArray = reg.FindAllStringSubmatch(context, -1)
		//for _, msg := range matchArray {
		//	methodName := msg[1]
		//	//fmt.Println(msg[1])
		//
		//	msgNum += 1
		//
		//	msgName := fmt.Sprintf("m_%s_toc", methodName)
		//
		//	if methodNameMap[msgName] != "" {
		//		panic("methodName repeated:" + methodName)
		//		os.Exit(1)
		//	}
		//
		//	methodNameMap[msgName] = "0"
		//
		//	clientMsgName := fmt.Sprintf("m_%s_%s_toc", moduleName, methodName)
		//
		//	context = regexp.MustCompile(msgName).ReplaceAllString(context, clientMsgName)
		//
		//	messageProtoCode += fmt.Sprintf("//<%s:%d>\n", clientMsgName, msgNum)
		//
		//	protoCodeEncodeBody += fmt.Sprintf("encode(#%s{} = Msg) ->\n", msgName)
		//
		//	protoCodeEncodeBody += fmt.Sprintf("    Bin = p_%s:encode_msg(Msg),\n", moduleName)
		//
		//	protoCodeEncodeBody += fmt.Sprintf("    encode(%d, Bin);\n", msgNum)
		//
		//	protoCodeDecodeBody += fmt.Sprintf("decode(%d, Bin) ->\n", msgNum)
		//
		//	protoCodeDecodeBody += fmt.Sprintf("  p_%s:decode_msg(Bin, %s);\n", moduleName, msgName)
		//}

		if moduleNum == 99 {
			gmContent += context
		} else {
			messageProtoCode += context
		}


		reg = regexp.MustCompile(`enum\s+\w+\s*{([^}]*)}`)
		matchArray = reg.FindAllStringSubmatch(noAnnotationContext, -1)
		for _, msg := range matchArray {
			reg = regexp.MustCompile(`\s*(\w+)\s*=\s*\d+\s*;`)
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

	// 去除压缩功能 20180725
	//protoCodeEncodeBody += "\n\nencode(MethodNum, Bin) ->\n"
	//protoCodeEncodeBody += "    Len = iolist_size(Bin),\n"
	//protoCodeEncodeBody += "    if Len > 150000 ->\n"
	//protoCodeEncodeBody += "        Bin1 = zlib:compress(Bin),\n"
	//protoCodeEncodeBody += "        Len1 = iolist_size(Bin1),\n"
	//protoCodeEncodeBody += "        if Len1 < Len ->\n"
	//protoCodeEncodeBody += "            io:format(\"proto compress:~p~n\", [{MethodNum, Len, Len1}]),\n"
	//protoCodeEncodeBody += "            util_websocket:pack_packet(<<1, MethodNum:32, Bin1/binary>>);\n"
	//protoCodeEncodeBody += "            true ->\n"
	//protoCodeEncodeBody += "                util_websocket:pack_packet(<<0, MethodNum:32, Bin/binary>>)\n"
	//protoCodeEncodeBody += "        end;\n"
	//protoCodeEncodeBody += "    true ->\n"
	//protoCodeEncodeBody += "        util_websocket:pack_packet(<<0, MethodNum:32, Bin/binary>>)\n"
	//protoCodeEncodeBody += "    end.\n"

	protoCodeEncodeBody += "\n\nencode(MethodNum, Bin) ->\n"
	protoCodeEncodeBody += "      util_websocket:pack_packet(<<0, MethodNum:32, Bin/binary>>).\n\n\n"

	include += "\n\n"
	filePutContext(genSrcPath+"socket_router.erl", socketRouterHead+include+socketRouterBody)
	filePutContext(genSrcPath+"proto.erl", protoCodeHead+include+protoCodeEncodeBody+protoCodeDecodeBody)
	if isCreateClientProto == "true" {
		filePutContext(clientProtoPath+"message.proto", messageProtoCode)
	}
	filePutContext(tmpProtoPath+"message.proto", messageProtoCode + gmContent)
	commandArgs := []string{
		"proto.escript",
		"-type",
		"-W",
		"-Werror",
		"-strbin",
		"-msgtolower",
		"-modprefix",
		"p_",
		"-I",
		tmpProtoPath,
		"-o-erl",
		genSrcPath,
		"-o-hrl",
		includePath,
		//"-v",
		//"never",
		"message.proto",
	}

	//filePutContext(erlReadProtoFile, noAnnotationContext)
	//defer wg.Done()
	//defer func() {
	//	err = os.Remove(erlReadProtoFile)
	//	check(err)
	//}()
	out, err :=cmd("escript", commandArgs)
	check(err, "", out)

	err = os.Remove(tmpProtoPath+"message.proto")
	check(err, tmpProtoPath+"message.proto")

	//wg.Wait()
	usedTime := time.Since(t0)
	fmt.Print("\n\n")
	fmt.Print("*************************************************************\n\n")
	fmt.Print("                      All finished\n\n")
	fmt.Printf("                    %s second\n\n", usedTime.String())
	fmt.Print("*************************************************************\n\n")
}

func ReadAll(filePth string) (string, error) {
	f, err := os.Open(filePth)
	defer f.Close()
	if err != nil {
		return "", err
	}
	context, err := ioutil.ReadAll(f)
	return string(context), err
}
func filePutContext(filename string, context string) {
	if checkFileIsExist(filename) {
		//如果文件存在
		//fmt.Println("文件存在", filename)
		del := os.Remove(filename)
		check(del)
	}
	f, err := os.Create(filename) //创建文件
	check(err)
	defer f.Close()
	_, err = io.WriteString(f, context)
	check(err)
}


func getFileList(path string, suffix string) (files []string) {
	files = make([]string, 0, 30)
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
