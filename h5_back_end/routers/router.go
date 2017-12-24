package routers

import (
	"go_util/h5_back_end/controllers"
	"github.com/astaxie/beego"
)

func init() {
	beego.Router("/", &controllers.MainController{})
	beego.Router("/build_table", &controllers.MainController{}, "*:BuildTable")
	beego.Router("/build_proto", &controllers.MainController{}, "*:BuildProto")
	//beego.Router("/submit_json", &controllers.MainController{}, "*:SubmitJson")
	beego.Router("/build_map", &controllers.MainController{}, "*:BuildMap")
	beego.Router("/build_project", &controllers.MainController{}, "*:BuildProject")
	//beego.Router("/pack", &controllers.MainController{}, "*:Pack")
	beego.Router("/restart", &controllers.MainController{}, "*:Restart")
	beego.Router("/build_scene", &controllers.MainController{}, "*:BuildScene")

}
