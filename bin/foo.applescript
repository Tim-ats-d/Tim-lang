# let foo = "ls -l"
# tell foo to -a "." end

for i in [ 1, 2, 3 ] do
  let error_code = ((tell "echo" to (i as string) end) as string)

  if true then
    tell "echo" to "Sucess: " error_code end
  else
    with MSG = "Error"
      tell "echo" to "$MSG" end
    end
  end
end
