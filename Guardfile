# A sample Guardfile
# More info at https://github.com/guard/guard#readme

notification :off

watch(%r{elm/([^.].*\.elm)}) do |m|
  Dir.chdir "elm" do
    `elm --make --set-runtime="static/js/elm-runtime.js" -b ../static/js -c ../static/tmp --only-js #{m[1]}`
  end
end
