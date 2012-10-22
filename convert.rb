require 'sequel'

mysql = Sequel.connect database: "mackey_development", username: 'root', adapter: 'mysql2'
pg = Sequel.connect 'postgres:///mackey'

mysql.tables.each do |t|
  puts t
end
