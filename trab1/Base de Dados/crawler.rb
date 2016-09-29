# crawler criado para retirar informações do site do uri
# e assim ter uma base de dados de benchmark dado um problema

require 'nokogiri'
require 'open-uri'

i = 1
puts "ranking, submission_id, user, language, time, submission_date"
while i <= 303 do
  doc = Nokogiri::HTML(open("https://www.urionlinejudge.com.br/judge/en/ranks/problem/1160?page=#{i}"), nil, 'EU')
  labs = doc.css('table tbody tr')

  labs.css('td').each do |el|
    if el['class'] == 'center'
      puts el.text.strip
    else
      print el.text.strip
      print ','
    end
  end
  i+=1
end

