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
)

const wsUrl = "ws://127.0.0.1:5050"

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

	var receive = make([]byte, 1024, 1024)
	n, err := ws.Read(receive)
	check(err)
	respone := responsePb
	err = proto.Unmarshal(receive[5:n], respone)
	check(err)

	stringResult := respone.String()
	if strings.Contains(stringResult, "success"){
		stringResult = "恭喜, 你成功了！"
	} else{
		stringResult = "失败了!!!!!!!!!!!!!!!!!!\n"
	}
	c.Data["message"] = stringResult
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








