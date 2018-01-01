package controllers

import (
	"github.com/astaxie/beego"
	"log"
	"strings"
	"go_util/h5_back_end/proto"
	"github.com/golang/protobuf/proto"
	"fmt"
	"golang.org/x/net/websocket"

	"bytes"
	"encoding/binary"
	"compress/zlib"
	"io"
	//"regexp"
)

const wsUrl = "ws://192.168.31.100:5050"

type MainController struct {
	beego.Controller
}

func (c *MainController) Get() {
	c.TplName = "index.html"
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func (c *MainController) BuildProto() {
	do(c, 10003, &debug.MBuildProtoTos{}, &debug.MBuildProtoToc{})
}

func (c *MainController) BuildTable() {
	do(c, 10005, &debug.MBuildTableTos{}, &debug.MBuildTableToc{})
}

func (c *MainController) BuildMap() {
	do(c, 10007, &debug.MBuildMapTos{}, &debug.MBuildMapToc{})
}

func (c *MainController) BuildScene() {
	do(c, 10009, &debug.MBuildSceneTos{}, &debug.MBuildSceneToc{})
}

func (c *MainController) BuildProject() {
	do(c, 10011, &debug.MBuildProjectTos{}, &debug.MBuildProjectToc{})
}

func (c *MainController) Restart() {
	c.Data["redirect"] = "/"
	c.TplName = "message.html"
	result := ""
	ws, err := websocket.Dial(wsUrl, "", wsUrl)
	if err != nil {
		log.Fatal(err)
	}
	request := debug.MRestartTos{}
	mRequest, err := proto.Marshal(&request)
	if err != nil {
		log.Fatal(err)
	}
	_, err = ws.Write(Packet(10013, mRequest))
	fmt.Println(Packet(10013, mRequest))
	result = "请等待服务器重启！"
	c.Data["message"] = result
	ws.Close()
}

//进行zlib解压缩
func DoZlibUnCompress(compressSrc []byte) []byte {
	b := bytes.NewReader(compressSrc)
	var out bytes.Buffer
	r, _ := zlib.NewReader(b)
	io.Copy(&out, r)
	return out.Bytes()
}

func do(c *MainController, protoNum int, requestPb proto.Message, responsePb proto.Message) {
	c.Data["redirect"] = "/"
	c.TplName = "message.html"
	ws, err := websocket.Dial(wsUrl, "", wsUrl)
	check(err)

	request := requestPb
	mRequest, err := proto.Marshal(request)
	check(err)

	_, err = ws.Write(Packet(protoNum, mRequest))
	fmt.Println(Packet(protoNum, mRequest))

	var receive = make([]byte, 10240, 1024000)
	n, err := ws.Read(receive)
	check(err)
	respone := responsePb
	//f :=receive[1]
	//isZip := f >> 7 == 1
	//fmt.Printf("isZip:%v", isZip)
	data := receive[5:n]
	//if isZip{
	//	data = DoZlibUnCompress(data)
	//}
	err = proto.Unmarshal(data, respone)
	check(err)

	stringResult := respone.String()
	//fmt.Println("stringResult=", respone.ProtoMessage)
	//fmt.Println("stringResult=", stringResult)
	//fmt.Println(stringResult == "success")
	//fmt.Println(strings.Compare(stringResult,"success"))
	if strings.Contains(stringResult, "result:\"success\"") {
		stringResult = "恭喜, 你成功了！"
	} else {
		stringResult = strings.Replace(stringResult, " ", "&nbsp", -1)
		stringResult = strings.Replace(stringResult, "\\n", "<br>", -1)
		stringResult = "失败了!!!!!!!!!!!!!!!!!!\n"+stringResult
	}
	////fmt.Print(stringResult)
	//stringResult = strings.Replace(stringResult, " ", "&nbsp", -1)
	//stringResult = strings.Replace(stringResult, "\\n", "<br>", -1)
	////stringResult = regexp.MustCompile(`\\n`).ReplaceAllString(stringResult, "<br>")
	//if strings.Contains(stringResult, "success"){
	//	stringResult = "恭喜, 你成功了！"
	//} else{
	//	stringResult = "失败了!!!!!!!!!!!!!!!!!!\n"+stringResult
	//}
	c.Data["message"] = stringResult
	ws.Close()
}
//封包
func Packet(methodNum int, message []byte) []byte {
	return append(append([]byte{0}, IntToBytes(methodNum)...), message...)
}
//整形转换成字节
func IntToBytes(n int) []byte {
	x := int32(n)

	bytesBuffer := bytes.NewBuffer([]byte{})
	binary.Write(bytesBuffer, binary.BigEndian, x)
	return bytesBuffer.Bytes()
}








