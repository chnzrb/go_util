package routers

import (
	"h5_back_end/controllers"
	"github.com/astaxie/beego"
)

func init() {
	beego.Router("/", &controllers.MainController{})
	beego.Router("/build_database", &controllers.MainController{}, "*:BuildDatabase")
	//beego.Router("/submit_json", &controllers.MainController{}, "*:SubmitJson")
	beego.Router("/build_map", &controllers.MainController{}, "*:BuildMap")
	beego.Router("/build_project", &controllers.MainController{}, "*:BuildProject")
	//beego.Router("/pack", &controllers.MainController{}, "*:Pack")
	beego.Router("/restart", &controllers.MainController{}, "*:Restart")
	beego.Router("/build_scene", &controllers.MainController{}, "*:BuildScene")

}
