package main

import (
	_ "go_util/h5_back_end/routers"
	"github.com/astaxie/beego"
	"net/http"
	"html/template"
)

func main() {
	beego.ErrorHandler("404", func(rw http.ResponseWriter, r *http.Request) {
		t, _ := template.New("404.html").ParseFiles(beego.BConfig.WebConfig.ViewsPath + "/404.html")
		data := make(map[string]interface{})
		data["content"] = "页面不存在"
		t.Execute(rw, data)
	})
	beego.SetStaticPath("/static", "static")
	beego.Run()
}
