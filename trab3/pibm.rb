require 'csv'
require 'sort_alphabetical'
require 'unicode_utils'

pibm = CSV.read('pibm.csv',{ force_quotes: false })
pibm = pibm.sort_alphabetical_by{|x| x[4] }

i = 0

pibm.each do |u|
  if u[0].eql? "2010"
    puts "INSERT INTO CIDADES (codestado, nome, pib) VALUES(#{u[1].inspect.gsub("\"","")}, #{UnicodeUtils.upcase(u[4].upcase.inspect.gsub("'","").gsub("\"","\'"))}, #{u[18].inspect.gsub("\"","").gsub(".","").gsub(",",".")});"
    i = i + 1
  end
end
