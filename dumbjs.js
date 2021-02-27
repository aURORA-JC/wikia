const tail = ([, ...t]) => t;

var util = {
    ifFileExists: function(file){
	var http = new XMLHttpRequest();

	http.open('HEAD', file, false);
	http.send();
	return (http.status != 404 && http.status != 403);
    },
    clearAllChildren: function(element){
	var myNode = document.getElementById(element);
	while (myNode.firstChild) {
	    myNode.removeChild(myNode.firstChild);
	}
    },
    loadShipList: function(callback){
	var xobj = new XMLHttpRequest();
	xobj.overrideMimeType("application/json");
	xobj.open('GET', 'json/shiplist.json', true); // Replace 'my_data' with the path to your file
	xobj.onreadystatechange = function () {
	    if (xobj.readyState == 4 && xobj.status == "200") {
		// Required use of an anonymous callback as .open will NOT return a value but simply returns undefined in asynchronous mode
		callback(xobj.responseText);
	    }
	};
	xobj.send(null);
    },
    loadShipJSON: function(ship, callback){
	var xobj = new XMLHttpRequest();
	xobj.overrideMimeType("application/json");
	xobj.open('GET', 'Ships/'+ship+'.json', true); // Replace 'my_data' with the path to your file
	xobj.onreadystatechange = function () {
	    if (xobj.readyState == 4 && xobj.status == "200") {
		// Required use of an anonymous callback as .open will NOT return a value but simply returns undefined in asynchronous mode
		callback(xobj.responseText, ship);
	    }
	};
	xobj.send(null);
    },
    guidGenerator: function(){
	var S4 = function() {
	    return (((1+Math.random())*0x10000)|0).toString(16).substring(1);
	};
	return (S4()+S4()+"-"+S4()+"-"+S4()+"-"+S4()+"-"+S4()+S4()+S4());
    }
}

function SD(basePath) {
    this.basePath = basePath;
    this.loader = new PIXI.loaders.Loader(this.basePath);
}

SD.prototype = {
    spineData : {},
    load: function(name, v) {
        if (!this.spineData[name]) {
            var skelpath = name+'.skel';
            var atlaspath = name+'.atlas';
            var texpath = name+'.png';

            this.loader.add(name+'_atlas', atlaspath, { "type" : "atlas" })
            this.loader.add(name+'_skel', skelpath, { "xhrType" : "arraybuffer" })
            this.loader.add(name+'_tex', texpath)

            this.loader.load((loader, resources) => {
                var dec = new TextDecoder("utf-8");
                var head = dec.decode(resources[name+'_skel'].data.slice(2, 10));
                var rawSkeletonData;
                if (head == "skeleton") {
                    rawSkeletonData = JSON.parse(dec.decode(resources[name+'_skel'].data));
                } else {
                    var skelBin = new SkeletonBinary();
                    skelBin.data = new Uint8Array(resources[name+'_skel'].data);
                    skelBin.initJson();

                    rawSkeletonData = skelBin.json;
                }
                var rawAtlasData = resources[name+'_atlas'].data;

                var spineAtlas = new PIXI.spine.core.TextureAtlas(rawAtlasData, function(line, callback) {
                    callback(PIXI.BaseTexture.from(name+'_tex'));
                });
                var spineAtlasLoader = new PIXI.spine.core.AtlasAttachmentLoader(spineAtlas);
                var spineJsonParser = new PIXI.spine.core.SkeletonJson(spineAtlasLoader);
                var skeletonData = spineJsonParser.readSkeletonData(rawSkeletonData);

                this.spineData[name] = skeletonData;
                v.changeCanvas(skeletonData);
            });
        } else {
            v.changeCanvas(this.spineData[name]);
        }
    }
}

var viewer = {
    init2: function() {
        viewer.canvas = document.getElementById("canvasSD-" + current);
        viewer.selectAnimation = document.getElementById("selectAnimation-" + current);
        viewer.selectAnimation.addEventListener('change', function() {
            viewer.changeAnimation(this.selectedIndex);
        });
        viewer.canvas.appendChild(viewer.app.view);
    },
    init: function(basePath) {
        viewer.sd = new SD(basePath);
        viewer.app = new PIXI.Application(512, 512, { transparent: true });
	viewer.init2();
    },
    changeCanvas : function(skeletonData) {
        viewer.app.stage.removeChildren();

        viewer.spine = new PIXI.spine.Spine(skeletonData);
        var animations = viewer.spine.spineData.animations;
        var stringAnimations = "";
        for(var i = 0; i < animations.length; i++) {
            if (animations[i].name == "stand")
                stringAnimations += "<option value=\"" + animations[i].name + "\" selected>" + animations[i].name + "</option>";
            else
                stringAnimations += "<option value=\"" + animations[i].name + "\">" + animations[i].name + "</option>";
        }
        viewer.selectAnimation.innerHTML = stringAnimations;
        viewer.spine.state.setAnimation(0, "stand", true);
        viewer.app.stage.addChild(viewer.spine);
        viewer.spine.position.set(viewer.app.view.width * 0.5 , viewer.app.view.height * 0.8);
        centerSD();
    },
    changeAnimation : function(num) {
        var name = viewer.spine.spineData.animations[num].name;
        viewer.spine.state.setAnimation(0, name, true);
    },
    onResize: function() {
        var element = document.getElementById("containerSD-" + current);
        var positionInfo = element.getBoundingClientRect();
        var width = positionInfo.width;
        if (width > 512){
            width = 512;
        }
        var height = width;
        viewer.app.view.style.width = width + "px";
        viewer.app.view.style.height = height + "px";
        viewer.app.renderer.resize(width, height);
        viewer.spine.position.set(width * 0.5 , height * 0.8);
    },
    removeSd : function() {
        viewer.app.stage.removeChildren();
    },
    loadSd: function(sd) {
        viewer.sd.load(sd, viewer);
    },
    sdWidth: function (){
        return viewer.app.view.width;
    }
};

function centerSD(){
    var containerWidth = document.getElementById("containerSD-" + current).getBoundingClientRect().width;
    var sdWidth = viewer.sdWidth();
    document.getElementById("canvasSD-" + current).style.left = Math.floor((containerWidth / 2) - (sdWidth / 2)) + "px";
}

window.onresize = function(){
    viewer.onResize();
    centerSD();
}

function loadShipSkinElements(skinID){
    try {
	viewer.removeSd();
    } catch (err){
    }

    if (util.ifFileExists("https://algwiki.moe/assets/char/" + skinID + ".png")){
	viewer.loadSd(skinID);
    }

    util.clearAllChildren("shipSkinExpressions-" + current);
}

function setShipExpressionNav(skin){
    const expressions = tail(skin);
    var firstIteration = true;
    var initial = "";
    var id = "";
    for (var x in expressions) {
	var container = document.createElement("img");
	container.id = "expr-" + current + "_" + x;
	container.setAttribute("onclick", "onExpressionNavButtonClick(\"" + x + "\",\"" + skin[0] + "\")");
	if (firstIteration){
	    container.src = expressions.length == 1 ? "https://algwiki.moe/assets/squareicon/" + skin[0] + ".png" : "https://algwiki.moe/assets/paintingface/" + skin[0] + "/" + x + ".png";
	    container.className = "active expression";
	    firstIteration = false;
	    initial = x;
	    id = x;
	} else {
	    container.src = "https://algwiki.moe/assets/paintingface/" + skin[0] + "/" + x + ".png";
	    container.className = "expression";
	}

	document.getElementById("shipSkinExpressions-" + current).appendChild(container);
    }
}

function onExpressionNavButtonClick(exprID, skinID){
    var id = "expr-" + current + "_" + exprID;
    var obj = document.getElementById('shipSkinExpressions-' + current).children;

    for (var i = 0; i < obj.length; ++i){
	if (obj[i].id == id) {
	    document.getElementById(id).classList.add('active');
	    document.getElementById("painting-" + current).src = "https://algwiki.moe/assets/painting/" + skinID + (exprID == 0 ? "" : "-" + exprID) + ".png";
	} else
	    document.getElementById(obj[i].id).classList.remove('active');
    }
}

var current = skins[0][0];

function skinChange(i, skinID) {
    current = skins[i][0];
    viewer.init2();
    loadShipSkinElements(skinID);
    setShipExpressionNav(skins[i]);
    onExpressionNavButtonClick(skins[i][1], skinID);
}

viewer.init("https://algwiki.moe/assets/char");
loadShipSkinElements(skins[0][0]);
setShipExpressionNav(skins[0]);
onExpressionNavButtonClick(skins[0][1], skins[0][0]);
