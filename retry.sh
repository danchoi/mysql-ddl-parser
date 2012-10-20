dropdb mackey
createdb mackey
runghc parse.hs < mackey.mysql.schema | psql mackey
runghc convert.hs
