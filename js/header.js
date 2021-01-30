function addHeader(){
	var rawFile = new XMLHttpRequest();
	rawFile.open("GET", "components/header.txt", false);
	rawFile.onreadystatechange = function () {
		var allText = rawFile.responseText;
		document.getElementById("header").innerHTML = allText;		
	}
	rawFile.send(null);

}
