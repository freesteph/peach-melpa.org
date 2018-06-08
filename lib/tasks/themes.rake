require 'timeout'
require 'open-uri'
require 'json'
require "#{Rails.root}/lib/critic"

namespace :themes do
  MELPA_ARCHIVE = 'https://melpa.org/archive.json'
  ARCHIVE_PATH = "#{Rails.root}/tmp/archive.json"
  SCREENSHOT_FOLDER = "#{Rails.root}/tmp/screenshots/"

  desc "grabs MELPA archives.json and put it in tmp"
  task refresh: :environment do
    puts "grabbing archive file from MELPA"
    open(MELPA_ARCHIVE) do |fresh|
      puts "writing MELPA file down."
      File.open(ARCHIVE_PATH, "w+") do |tmp|
        tmp.write(fresh.read)
      end
    end
  end

  desc "grabs tmp JSON file and store themes"
  task parse: :environment do
    data = JSON.parse(IO.read(ARCHIVE_PATH))

    themes = data.select { |name,| name.end_with? '-theme' }

    themes.each do |theme|
      name = theme.first.sub('-theme', '')
      info = theme.last
      version = info['ver'].join('.')

      puts "parsing #{name}"
      t = Theme.find_or_create_by(name: name)

      if t.version == version
        puts "not updating as version (#{t.version}) is similar"
      else
        t.version = version
        t.url = info['props']['url']
        t.description = info['desc']

        t.save

        puts "creating screenshot for #{name}"
        Rake::Task["themes:screenshot"].reenable

        begin
          Timeout::timeout(10) do
            Rake::Task["themes:screenshot"].invoke t.name

            result = SCREENSHOT_FOLDER + t.name + ".png"

            t.screenshot.attach(
              io: File.open(result),
              filename: "#{t.name}.png"
            )
            puts "done with screenshot"

            t.brightness =
              PeachHelpers::Critic::get_brightness_index result
          end
        rescue Timeout::Error
          puts "could not capture screenshot"
        end

        t.save
      end
    end
  end

  desc "update screenshot for THEME_NAME"
  task :screenshot, [:name] => :environment do |t, args|
    theme = Theme.find_by(name: args[:name])

    if not Dir.exists? SCREENSHOT_FOLDER
      Dir.mkdir SCREENSHOT_FOLDER
    end

    %x[emacs -Q -l lib/take-screenshot.el -eval '(fetch-and-load-theme "#{theme[:name]}" "#{theme[:version]}")']
  end
end
