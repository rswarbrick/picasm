#!/usr/bin/ruby
# Process CSV files from Microchip's website to produce XML
# descriptions of all chips.

require 'rubygems'
require 'rexml/document'
require 'csv-hash'
require 'fileutils'

csvfile = ARGV[0]

# Pre-process broken CSV from Microchip
csv = File.open(csvfile, "r")
out = File.open(csvfile + "_copy", "w")
csv.each_line do |line|
  if(line.match(/Product Family/))  # Header
    out.print line
  elsif(md = line.match(/^(PIC(?:10|12|16|18)F[A-Za-z0-9]+) ,(.*)/))
    out.print "\"#{md[1]}\",#{md[2]}\n"
  end
end
out.close
csv.close

$chips = CSVHash(csvfile + "_copy")
FileUtils.rm(csvfile + "_copy")

doc = REXML::Document.new
top = REXML::Element.new("Chips")

$chips[0].each do |chip|
  c = REXML::Element.new("Chip")
  c.add_attributes( {"Name"=>chip['Product Family'], "PinCount"=>chip['Pin count']} )
  p = REXML::Element.new("Peripherals")
  if(chip['# of A/D Ch.'] != '0')
    a = REXML::Element.new("ADCChannels")
    a.add_text(chip['# of A/D Ch.'])
    p.add_element(a)
  end
  if(chip['Digital Communication'])
    if(md = chip['Digital Communication'].match(/([1-9]) -UART/))
      u = REXML::Element.new("UARTs")
      u.add_text(md[1])
      p.add_element(u)
    end
    if(md = chip['Digital Communication'].match(/([1-9]) -A\/E\/USART/))
      u = REXML::Element.new("USARTs")
      u.add_text(md[1])
      p.add_element(u)
    end
    if(md = chip['Digital Communication'].match(/([1-9]) -SPI/))
      u = REXML::Element.new("SPI")
      u.add_text(md[1])
      p.add_element(u)
    end    
    if(md = chip['Digital Communication'].match(/([1-9]) -I2C/))
      u = REXML::Element.new("I2C")
      u.add_text(md[1])
      p.add_element(u)
    end
    if(md = chip['Digital Communication'].match(/([1-9]) -SSP/))
      u = REXML::Element.new("SSP")
      u.add_text(md[1])
      p.add_element(u)
    end
    if(md = chip['Digital Communication'].match(/([1-9]) -MSSP/))
      u = REXML::Element.new("MSSP")
      u.add_text(md[1])
      p.add_element(u)
    end
  end
  tmr8 = chip['Timers'].match(/(\d+)[- ]+8-bit/)
  if(tmr8 && tmr8[1] != '0')
    t = REXML::Element.new("Timer")
    t.add_attributes({"Count"=>tmr8[1], "Bits"=>8})
    p.add_element(t)
  end
  tmr16 = chip['Timers'].match(/(\d+)[- ]+16-bit/)
  if(tmr16 && tmr16[1] != '0')
    t = REXML::Element.new("Timer")
    t.add_attributes({"Count"=>tmr16[1], "Bits"=>16})
    p.add_element(t)
  end
  tmr32 = chip['Timers'].match(/(\d+)[- ]+32-bit/)
  if(tmr32 && tmr32[1] != '0')
    t = REXML::Element.new("Timer")
    t.add_attributes({"Count"=>tmr32[1], "Bits"=>32})
    p.add_element(t)
  end
  c.add_element(p)
  m = REXML::Element.new("Memory")
  m.add_text(chip['RAM'])
  c.add_element(m)
  if(chip['Internal Oscillator'])
    chip['Internal Oscillator'].split(',').map { |s| s.strip }.each do |o|
      osc = REXML::Element.new("Oscillator")
      osc.add_attributes({"Speed"=>o})
      c.add_element(osc)
    end
  end
  top.add_element(c)
end


doc.add_element(top)
formatter = REXML::Formatters::Pretty.new()
out = String.new
formatter.write(doc, out)
puts out
