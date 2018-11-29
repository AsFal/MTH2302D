
file = open("data.txt", "r")
fileContent = file.read()
file.close()

newFileContent = fileContent.replace("$", "")
print(newFileContent)


file = open("data.txt", "w")
file.write(newFileContent)
file.close()
