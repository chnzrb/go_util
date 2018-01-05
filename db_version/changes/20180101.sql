        CREATE TABLE `player_online_log` (
            `id`                INT unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
            `player_id`         INT unsigned NOT NULL COMMENT '玩家ID',
            `login_time`        INT NOT NULL DEFAULT '0' COMMENT '登录时间',
            `offline_time`      INT NOT NULL DEFAULT '0' COMMENT '离线时间',
            `online_time`       INT NOT NULL DEFAULT '0' COMMENT '在线时长',
          PRIMARY KEY (`id`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家在线日志';


        CREATE TABLE `player_login_log` (
            `id`                INT unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
            `player_id`         INT unsigned NOT NULL COMMENT '玩家ID',
            `ip`                VARCHAR(128) NOT NULL COMMENT '登录ip',
            `timestamp`         INT NOT NULL DEFAULT '0' COMMENT '时间戳',
          PRIMARY KEY (`id`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家在线日志';

        CREATE TABLE `player_fcm` (
            `player_id`             INT unsigned NOT NULL COMMENT '玩家ID',
            `total_online_time`     INT NOT NULL DEFAULT '0' COMMENT '累计在线时间',
            `total_offline_time`    INT NOT NULL DEFAULT '0' COMMENT '累计离线时间',
            `update_time`           INT NOT NULL DEFAULT '0' COMMENT '刷新时间',
          PRIMARY KEY (`player_id`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家防沉迷';



        CREATE TABLE `player`
        (
            `id`                    INT unsigned NOT NULL COMMENT '玩家id',
            `acc_id`                VARCHAR(64) NOT NULL DEFAULT '' COMMENT '平台帐号',
            `server_id`             VARCHAR(64) NOT NULL COMMENT '服务器ID',
            `nickname`              VARCHAR(64) NOT NULL DEFAULT '' COMMENT '昵称',
            `sex`                   TINYINT NOT NULL DEFAULT '0' COMMENT '性别, 0:男 1:女',
            `disable_login`         INT NOT NULL DEFAULT '0' COMMENT '禁止登陆截止时间',
            `reg_time`              INT unsigned NOT NULL DEFAULT '0' COMMENT '注册时间',
            `last_login_time`       INT unsigned NOT NULL DEFAULT '0' COMMENT '最后登陆时间',
            `last_offline_time`     INT unsigned NOT NULL DEFAULT '0' COMMENT '最后离线时间',
            `last_login_ip`         VARCHAR(32)  NOT NULL DEFAULT '' COMMENT '最后登陆IP',
            `login_times`           INT NOT NULL DEFAULT '0' COMMENT '登录次数',
            `cumulative_day`        INT NOT NULL DEFAULT '0' COMMENT '累计登录天数',
            `total_recharge_ingot`  INT unsigned NOT NULL DEFAULT '0' COMMENT '充值总金额',
            `last_recharge_time`    INT unsigned NOT NULL DEFAULT '0' COMMENT '最后充值时间',
            `recharge_times`        INT unsigned NOT NULL DEFAULT '0' COMMENT '充值次数',
            `disable_chat_time`     INT NOT NULL DEFAULT '0' COMMENT '禁止聊天时间',
            `is_pass_fcm`           TINYINT NOT NULL DEFAULT '0' COMMENT '是否通过防沉迷[0:否 1:是]',
            `type`                  TINYINT NOT NULL DEFAULT '0' COMMENT '0:普通号 1:测试号 7:机器人',
            `is_online`             TINYINT NOT NULL DEFAULT '0' COMMENT '是否在线0:否',
            PRIMARY KEY (`id`)
        )
        COMMENT       = '玩家'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';

        CREATE TABLE `player_data`
        (
            `player_id`                 INT unsigned NOT NULL  COMMENT '玩家id',
            `vip_level`                 SMALLINT unsigned NOT NULL  DEFAULT '0' COMMENT 'vip等级',
            `exp`                       BIGINT unsigned NOT NULL  DEFAULT '0' COMMENT '经验',
            `level`                     SMALLINT unsigned NOT NULL  DEFAULT '0' COMMENT '等级',
            `title_id`                  INT NOT NULL DEFAULT '0' COMMENT '称号id',
            `honor_id`                  INT NOT NULL DEFAULT '0' COMMENT '头衔id',
            `faction_id`                INT NOT NULL DEFAULT '0' COMMENT '帮派id',
            `head_id`                   INT NOT NULL DEFAULT '1' COMMENT '头像',
            `anger`                     INT unsigned NOT NULL  DEFAULT '0' COMMENT '怒气值',
            `max_hp`                    INT unsigned NOT NULL  DEFAULT '0' COMMENT '最大血量',
            `hp`                        INT unsigned NOT NULL  DEFAULT '0' COMMENT '血量',
            `attack`                    INT unsigned NOT NULL  DEFAULT '0' COMMENT '攻击',
            `defense`                   INT unsigned NOT NULL  DEFAULT '0' COMMENT '防御',
            `hit`                       INT unsigned NOT NULL DEFAULT '0' COMMENT '命中',
            `dodge`                     INT unsigned NOT NULL DEFAULT '0' COMMENT '闪避',
            `tenacity`                  INT unsigned NOT NULL DEFAULT '0' COMMENT '韧性',
            `critical`                  INT unsigned NOT NULL DEFAULT '0' COMMENT '暴击',
            `power`                     INT unsigned NOT NULL  DEFAULT '0' COMMENT '战力',
            `speed`                     INT unsigned NOT NULL  DEFAULT '0' COMMENT '速度',
            `pk`                        INT unsigned NOT NULL  DEFAULT '0' COMMENT 'pk值',
            `last_world_scene_id`       INT NOT NULL DEFAULT '0' COMMENT '上次世界场景ID',
            `x`                         INT NOT NULL DEFAULT '0' COMMENT 'X',
            `y`                         INT NOT NULL DEFAULT '0' COMMENT 'Y',
            `fight_mode`                TINYINT NOT NULL DEFAULT '0' COMMENT '战斗模式',
            `mount_status`              TINYINT NOT NULL DEFAULT '0' COMMENT '坐骑状态',
            `game_event_id`             INT NOT NULL DEFAULT '0' COMMENT '事件id',
            PRIMARY KEY (`player_id`)
        )
        COMMENT       = '玩家数据'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';


        CREATE TABLE `c_game_server`
        (
            `platform_id`       INT NOT NULL COMMENT '平台id',
            `id`                VARCHAR(128) NOT NULL  COMMENT 'id',
            `desc`              VARCHAR(128) NOT NULL DEFAULT '' COMMENT '描述',
            `node`              VARCHAR(128) NOT NULL COMMENT '节点',
            PRIMARY KEY (`platform_id`, `id`)
        )
        COMMENT       = '游戏服列表'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';

        CREATE TABLE `c_server_node`
        (
            `node`              VARCHAR(128) NOT NULL COMMENT '节点',
            `ip`                VARCHAR(64) NOT NULL DEFAULT '' COMMENT 'IP地址',
            `port`              INT NOT NULL DEFAULT '0' COMMENT '端口',
            `type`              TINYINT NOT NULL DEFAULT '1' COMMENT '类型[0:中心服节点 1:游戏服节点 2:跨服节点 3:拍卖行节点 4:web节点 5.唯一id服务器]',
            `zone_node`         VARCHAR(128) NOT NULL DEFAULT '' COMMENT '跨服节点(游戏服有效)',
            `server_version`    VARCHAR(128) NOT NULL DEFAULT '' COMMENT '服务器版本',
            `client_version`    VARCHAR(128) NOT NULL DEFAULT '' COMMENT '客户端版本(游戏服有效)',
            `open_time`         INT NOT NULL DEFAULT '0' COMMENT '开服时间(游戏服有效)',
            `state`             TINYINT NOT NULL DEFAULT '0' COMMENT '状态[0:断开 1:运行]',
            `is_test`           TINYINT NOT NULL DEFAULT '0' COMMENT '是否测试服[0:正式服 1:测试服](游戏服有效)',
            `platform_id`       INT NOT NULL DEFAULT '0' COMMENT '平台id(游戏服有效)',
            PRIMARY KEY (`node`)
        )
        COMMENT       = '服务器节点'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';

        CREATE TABLE `server_state` (
          `time`                  INT NOT NULL COMMENT '时间戳',
          `create_count`         INT NOT NULL DEFAULT '0' COMMENT '创建角色次数',
          `login_count`         INT NOT NULL DEFAULT '0' COMMENT '登录次数',
          `online_count`         INT NOT NULL DEFAULT '0' COMMENT '最高在线人数',
          `error_count`         INT NOT NULL DEFAULT '0' COMMENT '累计服务器错误数',
          `db_error_count`         INT NOT NULL DEFAULT '0' COMMENT '累计数据库错误数',
          PRIMARY KEY (`time`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='服务器状态';

        CREATE TABLE `player_ingot_log` (
          `id`             INT NOT NULL AUTO_INCREMENT COMMENT 'ID',
          `player_id`      INT unsigned NOT NULL COMMENT '玩家ID',
          `op_type`        INT NOT NULL DEFAULT '0' COMMENT '操作类型',
          `op_time`        INT NOT NULL DEFAULT '0' COMMENT '操作时间',
          `change_value`   INT NOT NULL DEFAULT '0' COMMENT '变化值',
          `new_value`      INT NOT NULL DEFAULT '0' COMMENT '新数值',
          PRIMARY KEY (`id`),
         KEY `idx_player_ingot_log_1` (`player_id`),
          KEY `idx_player_ingot_log_2` (`player_id`, `op_type`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家元宝日志';


        CREATE TABLE `player_skill`
        (
            `player_id`         INT unsigned NOT NULL  COMMENT '玩家id',
            `skill_id`          INT NOT NULL  COMMENT '技能ID',
            `skill_type`        TINYINT NOT NULL DEFAULT '0' COMMENT '技能类型[0:主动技能 1:被动技能]',
            `function_id`       INT NOT NULL DEFAULT '0' COMMENT  '功能id',
            `level`             INT NOT NULL COMMENT '等级',
            `exp`               INT NOT NULL DEFAULT '0' COMMENT  '经验',
            `last_time`         INT NOT NULL DEFAULT '0' COMMENT '上次使用时间',
            PRIMARY KEY (`player_id`, `skill_type`, `skill_id`)
        )
        COMMENT       = '玩家技能'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';


        CREATE TABLE `player_task`
        (
            `player_id`         INT unsigned NOT NULL COMMENT '玩家id',
            `chapter`           SMALLINT NOT NULL DEFAULT '1' COMMENT '章节',
            `task_id`           INT NOT NULL COMMENT '任务id',
            `status`            TINYINT NOT NULL COMMENT '任务状态[0:未完成 1:等待交接 2:已完成]',
            `num`               INT NOT NULL DEFAULT '0' COMMENT '数量',
            `update_time`       INT NOT NULL DEFAULT '0' COMMENT '更新时间',
            PRIMARY KEY (`player_id`)
        )
        COMMENT       = '玩家任务'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';

        CREATE TABLE `player_times_data`
        (
            `player_id`         INT unsigned NOT NULL COMMENT '玩家id',
            `times_id`          INT NOT NULL COMMENT '次数id',
            `use_times`         INT NOT NULL COMMENT '今日使用次数',
            `left_times`        INT NOT NULL DEFAULT '0' COMMENT '剩余次数',
            `buy_times`         INT NOT NULL COMMENT '购买次数',
            `update_time`       INT NOT NULL COMMENT '更新时间',
            `recover_time`      INT NOT NULL COMMENT '上次恢复时间',
            PRIMARY KEY (`player_id`, `times_id`)
        )
        COMMENT       = '玩家次数数据'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';

        CREATE TABLE `timer_data`
        (
            `timer_id`          INT NOT NULL COMMENT '定时器id',
            `last_time`            INT NOT NULL COMMENT '最近执行时间',
            PRIMARY KEY (`timer_id`)
        )
        COMMENT       = '服务器定时器数据'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';

        CREATE TABLE `c_server_trace_log`
        (
            `node`              varchar(64) NOT NULL COMMENT '服务器节点',
            `time`                INT NOT NULL COMMENT '时间',
            `online_num`        INT NOT NULL COMMENT '在线人数',
            `connect_times`     INT NOT NULL COMMENT '连接数',
            `enter_create_role` INT NOT NULL COMMENT '进入创建角色界面次数',
            `one_level_player`  INT NOT NULL COMMENT '1级玩家数量',
            `valid_player`      INT NOT NULL COMMENT '有效玩家数量',
            PRIMARY KEY (`node`, `time`)
        )
        COMMENT       = '服务器追踪数据日志'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';

        CREATE TABLE `c_server_trace_daily_log`
        (
            `node`              varchar(64) NOT NULL COMMENT '服务器节点',
            `year`                INT NOT NULL COMMENT '年',
            `month`                INT NOT NULL COMMENT '月',
            `day`               INT NOT NULL COMMENT '日',
            `connect_times`     INT NOT NULL COMMENT '连接数',
            `enter_create_role` INT NOT NULL COMMENT '进入创建角色界面次数',
            `one_level_player`  INT NOT NULL COMMENT '新增一级玩家数量',
            `valid_player`      INT NOT NULL COMMENT '新增有效玩家数量',
            `login_num`         INT NOT NULL COMMENT '今日登录过的玩家数量',
            `total_online_time` INT NOT NULL COMMENT '今日玩家在线时长总和',
            PRIMARY KEY (`node`, `year`, `month`, `day`)
        )
        COMMENT       = '服务器每日追踪数据日志'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';

        CREATE TABLE `player_game_buff`
        (
            `player_id`         INT NOT NULL COMMENT '玩家id',
            `game_buff_id`      INT NOT NULL DEFAULT '0' COMMENT 'buff_id',
            `num`               INT NOT NULL DEFAULT '0' COMMENT '个数',
            `invalid_time`      INT NOT NULL DEFAULT '0' COMMENT '失效时间(s)',
            PRIMARY KEY (`player_id`, `game_buff_id`)
        )
        COMMENT       = '玩家游戏buff数据'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';


        CREATE TABLE `test` (
            `id`                INT unsigned NOT NULL AUTO_INCREMENT COMMENT 'id',
            `num`               INT unsigned NOT NULL COMMENT 'num',
            `str`               VARCHAR(128) NOT NULL COMMENT 'str',
          PRIMARY KEY (`id`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='测试表';


        CREATE TABLE `unique_id_data`
        (
            `type`              INT unsigned NOT NULL COMMENT '唯一id类型',
            `id`                INT unsigned NOT NULL COMMENT '数据',
            PRIMARY KEY (`type`)
        )
        COMMENT       = '唯一id数据'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';

        CREATE TABLE `server_data` (
          `id` 		int unsigned NOT NULL COMMENT 'id',
          `int_data` int unsigned NOT NULL DEFAULT 0 COMMENT '整型数据',
          `str_data` VARCHAR(128) NOT NULL DEFAULT '' COMMENT '字符串数据',
          PRIMARY KEY (`id`)
        )
        COMMENT       = '服务器数据'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';

        CREATE TABLE `player_game_data` (
          `player_id` int unsigned NOT NULL COMMENT 'player_id',
          `data_id` int unsigned NOT NULL COMMENT '数据id',
          `int_data` int unsigned NOT NULL DEFAULT 0 COMMENT '整型数据',
          `str_data` VARCHAR(128) NOT NULL DEFAULT '' COMMENT '字符串数据',
          PRIMARY KEY (`player_id`, `data_id`)
        )
        COMMENT       = '玩家游戏数据'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';

        CREATE TABLE `player_prop`
        (
            `player_id`         INTEGER NOT NULL DEFAULT 0 COMMENT '玩家id',
            `prop_type`         INTEGER NOT NULL DEFAULT 0 COMMENT '道具类型',
            `prop_id`           INTEGER NOT NULL DEFAULT 0 COMMENT '道具id',
            `num`               INTEGER unsigned NOT NULL DEFAULT 0 COMMENT '数量',
            PRIMARY KEY (`player_id`, `prop_type`, `prop_id`)
        )
        COMMENT       = '玩家道具'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';



        CREATE TABLE `player_equip_pos`
        (
            `player_id`                 INTEGER NOT NULL COMMENT '玩家id',
            `pos_id`                    INTEGER NOT NULL DEFAULT 0 COMMENT '部位id',
            `equip_id`                  INTEGER NOT NULL DEFAULT 0 COMMENT '装备id',
            `level`                     INTEGER NOT NULL DEFAULT 0 COMMENT '强化等级',
			`gem_level`                     INTEGER NOT NULL DEFAULT 0 COMMENT '宝石等级',
			`start_level`                     INTEGER NOT NULL DEFAULT 0 COMMENT '升星等级',
            primary key(`player_id`,`pos_id`)
        )
        COMMENT       = '玩家装备部位'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';


        CREATE TABLE `player_offline_apply`
        (
            `id`                INT unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
            `player_id`         INTEGER NOT NULL DEFAULT 0 COMMENT '玩家id',
            `module`            VARCHAR(128) NOT NULL COMMENT '模块名',
            `function`          VARCHAR(128) NOT NULL COMMENT '函数名',
            `args`              VARCHAR(512) NOT NULL COMMENT '参数',
            `timestamp`         INTEGER NOT NULL DEFAULT 0 COMMENT '时间戳',
            PRIMARY KEY (`id`)
        )
        COMMENT       = '离线操作'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';

        CREATE TABLE `player_function`
        (
            `player_id`         INT NOT NULL COMMENT '玩家id',
            `function_id`       INT NOT NULL DEFAULT 0 COMMENT '功能Id',
            `time`              INT NOT NULL DEFAULT 0 COMMENT '时间戳',
            primary key(`player_id`, `function_id`)
        )
        COMMENT       = '玩家功能'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';

        CREATE TABLE `player_mission_data`
        (
            `player_id`         INT NOT NULL COMMENT '玩家id',
            `mission_type`      INT NOT NULL COMMENT '副本类型',
            `mission_id`        INT NOT NULL DEFAULT 0 COMMENT '通关的副本id',
            primary key(`player_id`, `mission_type`)
        )
        COMMENT       = '玩家副本数据'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';


        CREATE TABLE `player_sys_attr`
        (
            `player_id`         INTEGER NOT NULL COMMENT '玩家id',
            `fun_id`            INTEGER NOT NULL DEFAULT 0 COMMENT '功能系统',
            `power`             INTEGER NOT NULL DEFAULT 0 COMMENT '当前系统总战力',
            `hp`                INTEGER NOT NULL DEFAULT 0 COMMENT '生命',
            `attack`            INTEGER NOT NULL DEFAULT 0 COMMENT '攻击',
            `defense`           INTEGER NOT NULL DEFAULT 0 COMMENT '防御',
            `hit`               INTEGER NOT NULL DEFAULT 0 COMMENT '命中',
            `dodge`             INTEGER NOT NULL DEFAULT 0 COMMENT '闪避',
            `critical`          INTEGER NOT NULL DEFAULT 0 COMMENT '暴击',
            `tenacity`          INTEGER NOT NULL DEFAULT 0 COMMENT '韧性',
            `speed`             INTEGER NOT NULL DEFAULT 0 COMMENT '速度',
            PRIMARY KEY (`player_id`, `fun_id`)
        )
        COMMENT       = '玩家各系统属性'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';

        CREATE TABLE `player_guide_data`
        (
            `player_id`         INT unsigned NOT NULL COMMENT '玩家id',
            `guide_id`          TINYINT NOT NULL COMMENT '引导id',
            PRIMARY KEY (`player_id`, `guide_id`)
        )
        COMMENT       = '玩家引导数据'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';

        CREATE TABLE `player_setting_data`
        (
            `player_id`         INT unsigned NOT NULL COMMENT '玩家id',
            `setting_id`        INT NOT NULL COMMENT '设置id',
            `value`             INT NOT NULL COMMENT '数值',
            PRIMARY KEY (`player_id`, `setting_id`)
        )
        COMMENT       = '玩家设置数据'
        ENGINE        = 'InnoDB'
        CHARACTER SET = 'utf8'
        COLLATE       = 'utf8_general_ci';



