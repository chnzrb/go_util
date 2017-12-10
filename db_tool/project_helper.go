package main

import (
	"fmt"
	"html/template"
	"io"
	"net/http"
	//"os"
//	"path/filepath"
	"regexp"
//	"strconv"
	"time"
	"os/exec"
)

var mux map[string]func(http.ResponseWriter, *http.Request)

type Myhandler struct{}
type home struct {
	Title string
}

const (
	Template_Dir = "./view/"
	//Upload_Dir   = "./upload/"
)

func main() {
	server := http.Server{
		Addr:        ":2016",
		Handler:     &Myhandler{},
		ReadTimeout: 10 * time.Second,
	}
	mux = make(map[string]func(http.ResponseWriter, *http.Request))
	mux["/"] = index
	mux["/build_database"] = build_database
	mux["/submit_json"] = submit_json
	mux["/build_map"] = build_map
	mux["/build_project"] = build_project
	server.ListenAndServe()
}

func (*Myhandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	if h, ok := mux[r.URL.String()]; ok {
		h(w, r)
		return
	}
	if ok, _ := regexp.MatchString("/css/", r.URL.String()); ok {
		http.StripPrefix("/css/", http.FileServer(http.Dir("./css/"))).ServeHTTP(w, r)
	} else {
		http.StripPrefix("/", http.FileServer(http.Dir("./upload/"))).ServeHTTP(w, r)
	}

}

//func DownloadProto(w http.ResponseWriter, r *http.Request) {
//	var s string
//	var filename = "Proto-" + time.Now().Format("2006-01-02_15:04:05") + ".rar"
//	s = fmt.Sprintf("attachment;filename=\"%s\"", filename)
//	w.Header().Add("Content-Type", "application/octet-stream")
//	w.Header().Add("Content-Disposition", s)
//
//	var in io.Reader
//	in, _ = os.OpenFile("client.rar", os.O_RDWR, 0666)
//	io.Copy(w, in)
//}

//func DownloadJson(w http.ResponseWriter, r *http.Request) {
//	var s string
//	var filename = "Data-" + time.Now().Format("2006-01-02_15:04:05") + ".rar"
//	s = fmt.Sprintf("attachment;filename=\"%s\"", filename)
//	w.Header().Add("Content-Type", "application/octet-stream")
//	w.Header().Add("Content-Disposition", s)
//
//	var in io.Reader
//	in, _ = os.OpenFile("data.rar", os.O_RDWR, 0666)
//	io.Copy(w, in)
//}

func build_database(w http.ResponseWriter, r *http.Request) {
	f, err := exec.Command("erl", "-noshell", "-pa", "../ebin", "-env_file", "../config/tool.config","-s", "build_db", "start","-s", "init","stop").Output()
	if err != nil {
		io.WriteString(w,string("编译数据库失败"))
		fmt.Println(err.Error())
		io.WriteString(w,string(f))
	} else {
		fmt.Println(w, string(f))
		io.WriteString(w,string(f))
	}

}

func build_project(w http.ResponseWriter, r *http.Request) {
	f, err := exec.Command("erl", "-noshell", "-pa", "../ebin", "-s", "qmake", "all","-s", "init","stop").Output()
	if err != nil {
		io.WriteString(w,string("编译项目失败"))
		fmt.Println(err.Error())
		io.WriteString(w,string(f))
	} else{
		fmt.Println(w, string(f))
		io.WriteString(w,string(f))
	}
}

func submit_json(w http.ResponseWriter, r *http.Request) {
	f, err := exec.Command("sh", "../script/do_release.sh", "4").Output()
	if err != nil {
		io.WriteString(w,string("提交json失败"))
		fmt.Println(err.Error())
		io.WriteString(w,string(f))
	} else{
		fmt.Println(w, string(f))
		io.WriteString(w,string(f))
	}
}

func build_map(w http.ResponseWriter, r *http.Request) {
	f, err := exec.Command("erl", "-noshell", "-pa", "../ebin", "-s", "build_map", "start","-s", "init","stop").Output()
	if err != nil {
		io.WriteString(w,string("编译地图失败"))
		fmt.Println(err.Error())
		io.WriteString(w,string(f))
	} else{
		fmt.Println(w, string(f))
		io.WriteString(w,string(f))
	}
}

func index(w http.ResponseWriter, r *http.Request) {
	title := home{Title: "首页"}
	t, _ := template.ParseFiles(Template_Dir + "index.html")
	t.Execute(w, title)
}

//func StaticServer(w http.ResponseWriter, r *http.Request) {
//	http.StripPrefix("/file", http.FileServer(http.Dir("./upload/"))).ServeHTTP(w, r)
//}

//func check(name string) bool {
//	ext := []string{".exe", ".js", ".png"}
//
//	for _, v := range ext {
//		if v == name {
//			return false
//		}
//	}
//	return true
//}
