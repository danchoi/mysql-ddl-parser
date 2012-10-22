dropdb mackey
createdb mackey
mysqldump -uroot -d mackey_development | runghc parse.hs | psql mackey
runghc convert.hs
