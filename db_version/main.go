package main

import (
	"fmt"
	"os"
	"strings"
	_ "github.com/go-sql-driver/mysql"
	"github.com/astaxie/beego/config"
	"database/sql"
	"path/filepath"
	"time"
	"io/ioutil"
	"strconv"
	//"sync"
	"runtime"
	"sort"
)

var db *sql.DB

const ext = ".sql"
var sqlDir = "./changes"


var database = ""
var action = ""

func init() {
	if len(os.Args) != 3 {
		fmt.Println("参数错误!!!")
		fmt.Println(".example db_version [update | drop | version] localhost")
		os.Exit(1)
	}
	action = os.Args[1]
	section := os.Args[2]
	config, err := config.NewConfig("ini", "./config.ini")
	checkerr(err, "配置读取失败")
	db_user := config.String(section + "::db_user")
	if db_user == "" {
		fmt.Printf( "[配置读取失败] %s::%s 不存在\n", section, "db_user")
		os.Exit(1)
	}
	db_passwd := config.String(section + "::db_passwd")
	db_name := config.String(section + "::db_name")
	if db_name == "" {
		fmt.Printf( "[配置读取失败] %s::%s 不存在\n", section, "db_name")
		os.Exit(1)
	}
	db_host := config.String(section + "::db_host")
	if db_host == "" {
		fmt.Printf( "[配置读取失败] %s::%s 不存在\n", section, "db_host")
		os.Exit(1)
	}
	db_port := config.String(section + "::db_port")
	if db_port == "" {
		fmt.Printf( "[配置读取失败] %s::%s 不存在\n", section, "db_port")
		os.Exit(1)
	}

	sqlDir = config.String("sql_dir")

	database = db_name

	fmt.Printf("数据库地址: %s@%s:%s/%s\n", db_user, db_host, db_port, db_name)

	dsn := fmt.Sprintf("%s:%s@tcp(%s:%s)/%s", db_user, db_passwd, db_host, db_port, "")

	db, err = sql.Open("mysql", dsn)
	checkerr(err, "数据库连接失败")
	err = db.Ping()
	checkerr(err, "数据库连接失败")
	if action == "update" {
		initDatabase(db_name)
	}

	db.SetMaxOpenConns(50)
	db.SetMaxIdleConns(20)
	//db.SetConnMaxLifetime()
}

func main() {
	t1 := time.Now()
	runtime.GOMAXPROCS(4)

	switch action {
	case "update":
		nowDbVersion := getDbVersion()
		fmt.Println("更新前数据库版本:", nowDbVersion)
		files, _ := getSQLFileList(sqlDir, ext)
		sort.Strings(files)
		for _, file := range files {
			update(file, nowDbVersion)
		}
		fmt.Println("更新后数据库版本:", getDbVersion())
	case "drop":
		drop(database)
	case "version":
		nowDbVersion := getDbVersion()
		fmt.Println("当前数据库版本:", nowDbVersion)
	default:
		fmt.Println("参数错误:", action)
		os.Exit(1)
	}
	cost := time.Since(t1)
	fmt.Println("耗时:", cost)
}

func drop(database string) {
	fmt.Println("删除数据库:", database)
	_, err := db.Exec("drop database `" + database + "`;")
	checkerr(err, "删除数据库失败")
}

func update(fileName string, nowDbVersion int) {
	thisSqlFileVersion := getDbVersionFromFileName(fileName)
	if thisSqlFileVersion <= nowDbVersion {
		return
	}

	baseName := filepath.Base(fileName)
	fmt.Print("apply: ", baseName, " ", strings.Repeat(".", 50-len(baseName)))
	file, err := os.Open(fileName)
	checkerr(err, "打开文件失败")
	context, err := ioutil.ReadAll(file)
	checkerr(err, "读取文件失败")
	sqlList := strings.Split(string(context), ";")
	for _, sql := range sqlList {
		if strings.Contains(sql, "TABLE") {
				_, err = db.Exec(sql)
				if err != nil {
					fmt.Println("\nSQL:", sql)
					checkerr(err, "SQL执行失败")
				}
		}
	}
	updateDbVersion(thisSqlFileVersion)
	fmt.Println(" [OK]")
}

func initDatabase(database string) {
	rows := db.QueryRow("SELECT `SCHEMA_NAME` FROM information_schema.SCHEMATA WHERE `SCHEMA_NAME` = '" + database + "';")
	name := ""
	rows.Scan(&name)
	needInit := name == ""

	if needInit == true {
		fmt.Println("创建数据库:", database)
		_, err := db.Exec("CREATE DATABASE `" + database + "` CHARACTER SET 'utf8' COLLATE 'utf8_general_ci';\n")
		checkerr(err, "创建数据库失败")
		_, err = db.Exec("use `" + database + "` ;")
		checkerr(err, "切换数据库失败")
		fmt.Println("创建表: db_version")
		_, err = db.Exec("CREATE TABLE `db_version` ( `version` INT, PRIMARY KEY ( `version`));\n")
		checkerr(err, "创建 db_version 失败")
		_, err = db.Exec("INSERT INTO `db_version` VALUES (0);\n")
		checkerr(err, "初始化 db_version 失败")
	} else {
		_, err := db.Exec("use `" + database + "` ;")
		checkerr(err, "切换数据库失败")
	}
}

func getDbVersionFromFileName(fimeName string) int {
	baseName := filepath.Base(fimeName)
	versionStr := strings.Split(baseName, ".")[0]
	versionInt, err := strconv.Atoi(versionStr)
	checkerr(err, "文件名转换版本号失败:" + fimeName)
	return versionInt
}
func getDbVersion() int {
	_, err := db.Exec("use `" + database + "` ;")
	checkerr(err, "切换数据库失败")
	rows := db.QueryRow("SELECT `version` FROM `db_version`")
	checkerr(err, "读取数据库版本失败")
	dbVersion := -1
	//for rows.Next() {
	rows.Scan(&dbVersion)
	//}
	return dbVersion
}

func updateDbVersion(version int) {
	_, err := db.Exec("use `" + database + "` ;")
	checkerr(err, "切换数据库失败")
	row, err := db.Query("UPDATE `db_version` SET `version` = " + strconv.Itoa(version))
	row.Close()
	checkerr(err, "更新数据库版本失败")
}

func getSQLFileList(dirPth, suffix string) (files []string, err error) {
	files = make([]string, 0, 50)
	suffix = strings.ToUpper(suffix)

	err = filepath.Walk(dirPth, func(filename string, fi os.FileInfo, err error) error {
		//遍历目录
		if fi.IsDir() {
			// 忽略目录
			return nil
		}
		if strings.HasSuffix(strings.ToUpper(fi.Name()), suffix) {
			baseName := filepath.Base(filename)
			if len(baseName) == 14{
				files = append(files, filename)
			}
		}
		return nil
	})
	return files, err
}

func checkerr(err error, desc string) {
	if err != nil {
		_, file, line, _ := runtime.Caller(1)
		fileBaseName := filepath.Base(file)
		fmt.Printf("[ERROR]%s:%d %s %v", fileBaseName, line, desc, err)
		os.Exit(1)
	}
}
