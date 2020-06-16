# Run BEVENT.EXE against all event files 

$Files = Get-ChildItem -File -Filter *.ev*

Foreach ($File in $Files) {
	$Year = $File.Name.SubString(0,4)
	.\BEVENT.EXE -y $Year -f 0-96 $File > C:\{PATH_TO_EXTRACTED_FILES}\$Year\$File.CSV
}
