package main

import (
	"fmt"
	"os"
	"strings"
	_ "github.com/go-sql-driver/mysql"
	"github.com/astaxie/beego/config"
	"database/sql"
	. "database/sql"
	"path/filepath"
	"time"
	"encoding/csv"
)

var db *sql.DB
var dir string

const ext = ".csv"

const tablePre = "t_"

func init() {
	config, err := config.NewConfig("ini", "./config.ini")
	if err != nil {
		fmt.Println("配置读取失败!")
		os.Exit(-1)
	}
	dir, err = filepath.Abs(config.String("dir"))
	checkerr(err)
	db_user := config.String("db_user")
	checkerr(err)
	db_passwd := config.String("db_passwd")
	checkerr(err)
	db_name := config.String("db_name")
	checkerr(err)
	db_host := config.String("db_host")
	checkerr(err)
	db_port := config.String("db_port")
	checkerr(err)
	dsn := fmt.Sprintf("%s:%s@tcp(%s:%s)/%s", db_user, db_passwd, db_host, db_port, db_name)

	db, err = sql.Open("mysql", dsn)
	checkerr(err)
	fmt.Println("数据库连接成功!")
	db.SetMaxOpenConns(100)
	db.SetMaxIdleConns(100)
}

func main() {
	t1 := time.Now()
	err := ensure_dir(dir)
	checkerr(err)
	action := os.Args[1]
	switch action {
	case "input_all":
		files, _ := WalkDir(dir, "", ext)
		for _, file := range files {
			input2Mysql(file)
		}
		fmt.Println("\n\nTotal input", len(files), "tables.")
	case "output_all":
		tables := get_all_tables("template_db")
		for _, table := range tables {
			output_from_mysql(table, dir)
		}

		fmt.Println("\n\nTotal output", len(tables), "tables.")
	case "input_one":
		table := os.Args[2]
		input2Mysql(filepath.Join(dir, table + ext))
		fmt.Println("\n\nTotal input", 1, "tables.")

	case "output_one":
		table := os.Args[2]
		output_from_mysql( tablePre + table, dir)
		fmt.Println("\n\nTotal output", 1, "tables.")
	}
	cost := time.Since(t1)
	fmt.Println("Cost:", cost)
}

func input2Mysql(fileName string) {
	fmt.Print("导入: ", filepath.Base(fileName), strings.Repeat(".", 50 - len(filepath.Base(fileName))))
	file, err := os.Open(fileName)
	defer file.Close()
	checkerr(err)
	reader := csv.NewReader(file)
	rows, err := reader.ReadAll()
	checkerr(err)
	tableName := rows[0][0]
	fields := rows[2]
	stmt, err := db.Prepare("DELETE FROM `" + tableName + "`;")
	checkerr(err)
	res, err := stmt.Exec()
	checkerr(err)
	_, err = res.RowsAffected()
	checkerr(err)

	sign := make(chan string)
	rowsLen := len(rows)
	for i := 3; i < rowsLen; i++ {
		go func(i int) {
			row := rows[i]
			tx, err := db.Begin()
			defer tx.Rollback()
			checkerr(err)
			sql:= "INSERT INTO `" + tableName + "` SET "
			tmp := 0
			for key, field := range fields {
				if tmp == 0 {
					sql += "`" + field + "` = '" + row[key] + "'"
				} else {
					sql += ", `" + field + "` = '" + row[key] + "'"
				}
				tmp++
			}
			smt, err := tx.Prepare(sql + ";")
			checkerr(err)

			_, err = smt.Exec()
			checkerr(err)

			smt.Close()
			err = tx.Commit()
			checkerr(err)
			sign <- "success"
		}(i)
	}
	for i := 3; i < rowsLen; i++ {
		<-sign
	}
	fmt.Println(" [OK]")
}

func get_all_tables(dbName string) ([] string) {
	rows, err := db.Query(" SELECT `table_name` FROM information_schema.tables WHERE table_schema='" + dbName + "' and table_type='base table';")
	checkerr(err)

	columns, _ := rows.Columns()
	scanArgs := make([]interface{}, len(columns))
	values := make([]interface{}, len(columns))
	for i := range values {
		scanArgs[i] = &values[i]
	}
	var tables [] string
	for rows.Next() {
		err = rows.Scan(scanArgs...)
		for _, col := range values {
			if col != nil {

				s := string(col.([]byte))
				if strings.HasPrefix(s, tablePre) {
					tables = append(tables, string(col.([]byte)))
				}

			}
		}
	}
	return tables
}

func output_from_mysql(table string, dir string) {
	fileName := table + ext
	fileName = strings.TrimPrefix(fileName, tablePre)
	fmt.Print("导出: ", fileName, strings.Repeat(".", 50 - len(fileName)))
	file, err := os.OpenFile(filepath.Join(dir, fileName), os.O_WRONLY | os.O_CREATE | os.O_TRUNC, os.ModePerm)
	checkerr(err)
	defer file.Close()
	rows, err := db.Query("SHOW FULL FIELDS FROM `" + table + "`;")
	cols := get_rows(rows)

	defer rows.Close()
	w := csv.NewWriter(file)
	outRow := make([]string, len(cols))
	outRow[0] = table
	err = w.Write(outRow)
	checkerr(err)
	//fmt.Println(cols)
	//os.Exit(-1)
	i := 0
	for _, field := range cols {
		//IsPri := field[3]
		fieldType := ""
		if (strings.Contains(field[1], "int")){
			fieldType = "int"
		}else{
			fieldType = "str"
		}
		if (field[4] == "PRI"){
			outRow[i] = fieldType + "|key"
		}else{
			outRow[i] = fieldType
		}

		i++
	}
	err = w.Write(outRow)
	i = 0
	for _, field := range cols {
		//IsPri := field[3]
		//defaultValue := ""
		//if (field[5] == ""){
		//	outRow[i] = ""
		//}else{
		//	outRow[i] = fieldType
		//}
		outRow[i] = field[5]
		i++
	}
	err = w.Write(outRow)
	i = 0
	for _, field := range cols {
		outRow[i] = field[0]
		i++
	}
	err = w.Write(outRow)
	i = 0
	for _, field := range cols {
		outRow[i] = field[8]
		i++
	}
	err = w.Write(outRow)

	rows, err = db.Query("SELECT * FROM `" + table + "`;")
	cols = get_rows(rows)
	err = w.WriteAll(cols)
	checkerr(err)
	w.Flush()
	fmt.Println(" [OK]")
}
func get_rows(rows *Rows) [][]string {
	columns, err := rows.Columns()
	checkerr(err)
	colLength := len(columns)
	scanArgs := make([]interface{}, colLength)
	values := make([]interface{}, colLength)

	for i := range values {
		scanArgs[i] = &values[i]
	}
	var cols [][]string
	i := 0
	for rows.Next() {
		err = rows.Scan(scanArgs...)
		cols = append(cols, make([] string, 0))
		for _, col := range values {
			if col != nil {
				cols[i] = append(cols[i], string(col.([]byte)))
			} else {
				cols[i] = append(cols[i], "")
			}
		}
		i++
	}
	return cols
}
func WalkDir(dirPth, prefix string, suffix string) (files []string, err error) {
	files = make([]string, 0, 30)
	prefix = strings.ToUpper(prefix)
	suffix = strings.ToUpper(suffix)

	err = filepath.Walk(dirPth, func(filename string, fi os.FileInfo, err error) error {
		//遍历目录
		if fi.IsDir() {
			// 忽略目录
			return nil
		}
		if strings.HasSuffix(strings.ToUpper(fi.Name()), suffix) && strings.HasPrefix(strings.ToUpper(fi.Name()), prefix) {
			files = append(files, filename)
		}
		return nil
	})
	return files, err
}

func ensure_dir(dir string) error {
	_, err := os.Stat(dir)
	if err == nil {
		return nil
	}
	if os.IsNotExist(err) {
		err := os.MkdirAll(dir, 0777)
		return err
	} else {
		return err
	}
}
