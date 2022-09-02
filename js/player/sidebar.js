function addSideBar(){
	var rawFile = new XMLHttpRequest();
	rawFile.open("GET", "js/player/sidebar.txt", false);
	rawFile.onreadystatechange = function () {
		var allText = rawFile.responseText;
		document.getElementById("sidebar").innerHTML = allText;
	}
	rawFile.send(null);

}
