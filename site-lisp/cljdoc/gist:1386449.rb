require 'nokogiri'
require 'open-uri'

open("http://clojure.github.com/clojure/clojure.core-api.html", 'r') do |f|
  page = Nokogiri(f.read)

  funcs = []
  page.search("div#var-entry").each do |e|
    next if e.search("pre#var-usage").children.text.empty?
    usage = e.search("pre#var-usage").children.text.split("\n").map{|f| f.strip}.last.sub('Usage: ', '')
    funcs << [usage.split(' ').first[1..-1], usage,
      e.search("pre#var-docstr").children.text.gsub("\n", ' ')]
  end

  funcs.each do |r|
    puts %Q[("#{r[0]}" . "#{r[1].gsub('"', '\"')} #{r[2].gsub('"', '\"')}")]
  end
end
