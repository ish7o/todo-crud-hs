# Crud TODO in HASKELL with SCOTTY and PERSISTENT
<3
some curl snippets for myself

```sh
curl 'localhost:3000/api/tasks?user_id=1'
```

```sh
curl -X POST 'localhost:3000/api/tasks' -d '{
 "taskName": "cute name",
 "taskUserId": 1,
 "taskCreatedAt": "2025-10-12T12:54:24.521255Z",
 "taskStatus": "Pending",
 "taskDueDate": "2025-11-10",
 "taskPriority": "Low" }' | jq '.id'
```
getting fancy with the jq and stuff :3

```sh
curl -X PATCH 'localhost:3000/api/tasks/3/complete'
```

```sh
curl -X PUT 'localhost:3000/api/tasks/:id' -d '{
 "taskName": "cute name 2",
 "taskUserId": 1,
 "taskCreatedAt": "2025-09-12T12:54:24.521255Z",
 "taskStatus": "InProgress",
 "taskDueDate": "2025-11-10",
 "taskPriority": "High" }'
```

```sh
curl -X DELETE 'localhost:3000/api/tasks/3'
```

