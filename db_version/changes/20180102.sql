CREATE TABLE `player_login_log2` (
   `id`                INT unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
   `player_id`         INT unsigned NOT NULL COMMENT '玩家ID',
   `ip`                VARCHAR(128) NOT NULL COMMENT '登录ip',
   `timestamp`         INT NOT NULL DEFAULT '0' COMMENT '时间戳', -- nihaodff
   PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家在线日志';

CREATE TABLE `player_login_log3` (
  `id`                INT unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `player_id`         INT unsigned NOT NULL COMMENT '1',
  `ip`                VARCHAR(128) NOT NULL COMMENT '1',
  `timestamp`         INT NOT NULL DEFAULT '0' COMMENT '1', /* dfdfs */
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='1';




