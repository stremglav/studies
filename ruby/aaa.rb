#!/usr/bin/ruby

ss = [{:q => 4, :p => {:id => 1, :s => 1}},
      {:q => 5, :p => {:id => 2, :s => 2}},
      {:q => 6, :p => {:id => 3, :s => 1}}]

items = {}
ss.each do |item|
  p = item[:p]
  items[p[:s]] ||= []
  items[p[:s]] << item
end

dd = { 1 => "dddd",
       2 => "ssss",
       3 => "ffff"}

dd.each_pair do |k,v|
  print "key: "+k.to_s+" val: "+ v + "\n"
end
