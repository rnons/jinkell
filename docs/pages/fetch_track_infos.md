---
title: POST /music/fetch_track_infos
---

获取歌曲信息

## 路径

    http://jing.fm/api/v1/music/fetch_track_infos

## 方法
    
    POST

## 参数

### uid

- 含义: user id

### tid

- 含义: track id

## 返回结果

### 成功

- HTTP Status Code

    `HTTP/1.1 200 OK`

- 返回值 (JSON格式)

        {
            "msg": "操作成功",
            "result": {
                "lvd": "n",
                "tid": 1180187,
                "vsn": [],
                "ply_info": null,
                "cmps_info": {
                    "singer": "惘闻"
                }
            },
            "success": true
        }
