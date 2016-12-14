require 'csv'
require 'sort_alphabetical'
require 'unicode_utils'

idhm = CSV.read('idhm.csv')
idhm = idhm.sort_alphabetical_by{|x| x[4] }
i = 0

idhm.each do |u|
  puts "UPDATE cidades SET idh=#{UnicodeUtils.downcase(u[233].inspect.gsub("\"","").gsub(".","").gsub(",","."), nil)} WHERE codestado=#{u[1].inspect.gsub("\"","")} AND nome = #{u[4].upcase.inspect.gsub("\'","").gsub("\"","\'")};"
  i = i + 1
end

puts i

