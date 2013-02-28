---
title: POST /sessions/create
---

登录

## 路径

    http://jing.fm/api/v1/sessions/create

## 方法
    
    POST

## 参数

### email

- 含义: 登录邮箱

### pwd

- 含义: 登录密码

## 返回结果

### 成功

- HTTP Status Code

    `HTTP/1.1 200 OK`

- Response Headers

    * Jing-A-Token-Header
    * Jing-R-Token-Header

- 返回值 (JSON格式)

        {
            "msg": "操作成功",
            "result": {
                "newview": {
                    "t": [],
                    "m": [],
                    "k": []
                },
                "avbF": "",
                "cm": "10",
                "sts": {
                    "rltd": "true",
                    "thm": "dflt",
                    "rtCv": "true",
                    "frdlvd": "true",
                    "frdCntNtf": "true",
                    "tckNtf": "true",
                    "hbr": "true",
                    "rcmd": "true",
                    "rmdTone": "true",
                    "h5": "true",
                    "timedot": "true",
                    "tipNtf": "true",
                    "lgA": "false",
                    "autoSnc": "false"
                },
                "snstokens": {
                    "douban": ""
                },
                "usr": {
                    "id": *uid*,
                    "sid": "",
                    "newbie": 0,
                    "regip": "",
                    "nick": "",
                    "c": 1,
                    "avatar": ""
                },
                "release": null,
                "pld": {
                    "an": "Tempus Fugit",
                    "atn": "As the Stars Fall",
                    "cmbt": "postrock",
                    "ct": "14",
                    "d": "220",
                    "fid": "2012071712lEx.jpg",
                    "mid": "2012071712PgV.m4a",
                    "n": "From Another Time",
                    "tid": 1230929,
                    "uid": *uid*
                }
            },
            "success": true
        }

