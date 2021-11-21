# let foo = "ls -l"
# tell foo to -a "." end

with LANG = "fr" and BAR = "bar"
  # Foo
  tell "echo" to -e "$BAR$FOO" end
end

tell "echo $LANG"

for i in [ 1, 2, 3 ] do
  let error_code = ((tell "echo" to (i as string) end) as string)

  tell "echo" to "code d\'erreur:" error_code end
end
