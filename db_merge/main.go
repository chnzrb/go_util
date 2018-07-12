package main

import (
	"fmt"
	_ "github.com/go-sql-driver/mysql"
	"encoding/json"
	"io/ioutil"
	"github.com/go-xorm/xorm"
	"time"
	"strings"
	"os"
)

type db struct {
	DbHost string       `json:"db_host"`
	DbName string       `json:"db_name"`
	DbUser string       `json:"db_user"`
	DbPort int          `json:"db_port"`
	DbPwd  string       `json:"db_pwd"`
	Db     *xorm.Engine `json:"-"`
}

type dbConfig struct {
	TargetDb     db     `json:"target_db"`
	SourceDbList [] *db `json:"source_db_list"`
}

type tableConfig struct {
	IgnoreList [] string `json:"ignore_list"`
	CleanList  [] string `json:"clean_list"`
}

func CheckError(err error, msg ... string) {
	if err != nil {
		fmt.Printf("[ERROR]%s %v\r\n", msg, err)
	}
}

func main() {
	t0 := time.Now()
	dbConfigFileData, err := ioutil.ReadFile("db_config.json")
	CheckError(err, "读取db_config.json失败")
	if err != nil {
		os.Exit(1)
	}
	dbConfig := &dbConfig{}
	err = json.Unmarshal(dbConfigFileData, dbConfig)

	CheckError(err, "解析db_config.json失败")
	if err != nil {
		os.Exit(1)
	}

	tableConfigFileData, err := ioutil.ReadFile("table_config.json")
	CheckError(err, "table_config.json失败")
	if err != nil {
		os.Exit(1)
	}
	tableConfig := &tableConfig{}
	err = json.Unmarshal(tableConfigFileData, tableConfig)

	CheckError(err, "table_config.json失败")
	if err != nil {
		os.Exit(1)
	}

	//fmt.Printf("dbConfig:%+v\n", dbConfig)
	//fmt.Printf("tableConfig:%+v\n", tableConfig)

	dsn := fmt.Sprintf("%s:%s@tcp(%s:%d)/%s", dbConfig.TargetDb.DbUser, dbConfig.TargetDb.DbPwd, dbConfig.TargetDb.DbHost, dbConfig.TargetDb.DbPort, dbConfig.TargetDb.DbName)

	fmt.Printf("目标数据库: %s:%s\n", dbConfig.TargetDb.DbHost, dbConfig.TargetDb.DbName)
	targetDb, err := xorm.NewEngine("mysql", dsn)
	CheckError(err, "连接目标数据库失败:"+dsn)
	if err != nil {
		os.Exit(1)
	}

	_, err = targetDb.Exec("SET NAMES utf8;")
	CheckError(err)
	if err != nil {
		os.Exit(1)
	}

	dbConfig.TargetDb.Db = targetDb
	if len(dbConfig.SourceDbList) == 0 {
		fmt.Print("[ERROR]:源数据库不能为空\n")
		os.Exit(1)
	}
	for i, e := range dbConfig.SourceDbList {
		dsn := fmt.Sprintf("%s:%s@tcp(%s:%d)/%s", e.DbUser, e.DbPwd, e.DbHost, e.DbPort, e.DbName)
		fmt.Printf("源数据库[%d]: %s:%s\n", i + 1, e.DbHost, e.DbName)
		db, err := xorm.NewEngine("mysql", dsn)
		CheckError(err, "连接源数据库失败:"+dsn)
		if err != nil {
			os.Exit(1)
		}

		_, err = db.Exec("SET NAMES utf8;")
		CheckError(err)
		if err != nil {
			os.Exit(1)
		}
		e.Db = db
	}

	fmt.Printf("开始合服:\n")
	doMerge(dbConfig, tableConfig)
	usedTime := time.Since(t0)
	fmt.Print("\n")
	fmt.Print("*****************************************************\n")
	fmt.Print("    合服成功.\n")
	fmt.Printf("    耗时 %s. \n", usedTime.String())
	fmt.Print("*****************************************************\n\n")
}

func doMerge(dbConfig *dbConfig, tableConfig *tableConfig) {
	r, err := dbConfig.TargetDb.Db.DBMetas()
	CheckError(err, "清空表数据失败:")
	if err != nil {
		os.Exit(1)
	}

	for _, e := range r {
		tableName := e.Name
		if inArray(tableName, tableConfig.IgnoreList) {
			fmt.Printf("%s %s [ignore]\n", tableName, strings.Repeat(".", 50-len(tableName)))
		} else if inArray(tableName, tableConfig.CleanList) {
			fmt.Printf("%s %s ", tableName, strings.Repeat(".", 50-len(tableName)))
			sql := fmt.Sprintf("delete from %s;\n", tableName)
			_, err := dbConfig.TargetDb.Db.Exec(sql)
			CheckError(err, "清空表数据失败:"+sql)
			if err != nil {
				os.Exit(1)
			}
			fmt.Printf("[clean]\n")
		} else {
			fmt.Printf("%s %s ", tableName, strings.Repeat(".", 50-len(tableName)))
			sql := fmt.Sprintf("SELECT * FROM %s;", tableName)
			for _, sourceDb := range dbConfig.SourceDbList {
				rows, err := sourceDb.Db.QueryString(sql)
				CheckError(err, "读取源表失败:"+sql)
				if err != nil {
					os.Exit(1)
				}
				if len(rows) > 0 {
					for _, row := range rows {
						//fmt.Printf("row:%+v\n", row)
						insertCols := make([] string , 0)
						for col, value := range row {
							insertCols = append(insertCols, fmt.Sprintf("`%s` = '%s'", col, value))
						}
						insertSql := fmt.Sprintf("INSERT INTO `%s` set %s", tableName, strings.Join(insertCols, ", "))
						//fmt.Printf("%s\n", insertSql)
						_, err = dbConfig.TargetDb.Db.Exec(insertSql)
						CheckError(err, "插入数据失败:" + insertSql)
						if err != nil {
							os.Exit(1)
						}
					}
				}
			}
			fmt.Printf("[merge]\n")
		}
	}
}

func inArray(v string, array [] string) bool {
	for _, e := range array {
		if e == v {
			return true
		}
	}
	return false
}

//func getTableList(database string, db *xorm.Engine) (error, [] string) {
//	sql := fmt.Sprintf("SELECT `table_name` FROM information_schema.tables WHERE table_schema='%s' and table_type='base table'", database)
//	//var data [] struct {
//	//	TableName string
//	//}
//	data, err := db.QueryString(sql)
//
//	//fmt.Printf("data:%+v\n", data)
//	if err != nil {
//		return err, nil
//	}
//	tables := make([] string, 0)
//	for _, e := range data {
//		tables = append(tables, e["table_name"])
//	}
//	return nil, tables
//}
