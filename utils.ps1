# Run BEVENT.EXE against all files in a dir
Get-ChildItem -File | Foreach {.\BEVENT.EXE $_ -y 1979 -f 0-96 > C:\Retrosheet\1979eve\$_.CSV}
