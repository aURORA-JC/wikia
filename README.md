# ALG wiki

Azur Lane © is owned by Shanghai Manjuu, Xiamen Yongshi, Shanghai Yostar | All logos and trademarks are property of their respective owners. Special thanks to /alg/, English Koumakan Wiki, Chinese Wikis, Japanese Wikis, and to all our contributors. This is a non-profit website.


/alg/ wiki

Copyright (C) 2021  alg-wiki

Contact at botebreeder@gmail.com

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.


Copied from the previous wiki source at https://github.com/alg-wiki/wikia due to inactivity.

# Contribute

## Ship JSON files

If you want to help contribute in building ship pages, use this json template https://gitgud.io/alg-wiki/wikia/-/blob/master/Ships/aurora.json and use a json online editor like this https://jsoneditoronline.org/. Just replace the values with the ff:

Some are self explanatory so I did not include them here


```json
"name_reference": "",
```
Generally should match the name of the json without the extension, e.g. ark_royal - all lowercase and use underscores not spaces, for collab ships the collab is usally appended, e.g. honoka_doa, META ships can be appended with _meta

```json
"cn_reference": "",
```
The romanised Chinese name for the ship, for new ships you'll need to find this in the game files

```json
"internal_id": "",
```
The ID for the voice files, in the cue assets each ship has a unique number e.g. cv-10137 - NOTE: a 1 is added to the end of this number, so the for that example the number put in the .json file would be 101371

Generally these follow a pattern:

The first digit is the Nation - 1=EagleUnion, 2=RoyalNavy, 3=SakuraEmpire, 4=Ironblood, 5=Dragon Empery, 6=SardegnaEmpire, 7=NorthernParliament, 8=IrisLibre, 9=VichyaDominion

The second and third digit is the ship class - 01=DD, 02=CL, 03=CA, 04=BC, 05=BB, 06=CVL, 07=CV, 08=SS, 12=AR, 17=SSV, 19=AE, 99=PR/DR

The fourth and fifth digit is the number of the ship, this is somewhat arbitrary, sometimes numbers are skipped and sometimes ships are numbered in order in which they were added to the game

Collab ships have a 7 digit number, e.g. 1060007

The second and third digit are the collab number, e.g. 06 for DOA collab

The last digit is the number of the ship, usually 1-7 in no particular order

META ships seem to have a 6 digit number, Hiryuu is 970701, as there is only one so far I can't be sure of a pattern for these but I suspect they'll all be a 6 digit number beginning with 9

```json
"icon": "",
```
For skill icons this one refers to the file name of the skill icon in assets/skillicon_new/
  
```json
"1":{},
"2":{},
```
This usually applies to items where you can have more than one of it like skins, expressions, skills, lines. If you want to add one, just increment the number from the last one while keeping format. eg. "3":{},

```json
"stats":{}
```
If the bote does not have a retrofit, do not remove the "100retrofit" and "120retrofit" part, just leave the values blank. eg. ""

```json
"equipmentLoadout": {
    "1": {
      "type": "CL Main Gun",
      "efficiency": "125%/127%/130%/135%",
      "amount": "1/1/1/1",
      "preload": "0/0/0/0"
    },
```
Efficiency, amount, and preload values are based on limit break ranks, being rank0/rank1/rank2/rank3. Enclose retrofit values on parenthesis. eg. retrofit has +5% MG on 2nd rank, "125%/127%/130%(135%)/135%(140%)

```json
"drop": {
    "droppable": "false"
```
Set to false if it doesn't drop to any map, world or events. 

```json
"list": {
      "1": {
        "event": "World",
 ```
The map or "event" that contains chapters and nodes. For example if the bote also drops on Crimson Echoes, add: "2": { "event": "Crimson Echoes",

```json
"chapter": {
          "1": {
            "label": "Ch.2",
```
The chapters in the "event". For "World", the label should be the chapter names, eg. "Ch.2". For events, the label should be "AX" or "BX" 

```json
"node": {
              "1": {
                "drop": "✓",
                "note": "JP Only"
              }
```
The nodes pertain to the playable stages inside the chapter. YOU SHOULD INCLUDE ALL NODES of a chapter in the json file even though the bote does not drop on that node. Just mark the "drop" as "-" instead of "✓"

```json
"skin": {
    "1": {
      "id": "ouruola",
      "name": "Aurora",
      "description": "Arethusa-class light cruiser - Aurora, pennant number 12",
      "expression": {
        "0": {
          "id": "0"
        },
        "1": {  
          "id": "1"
        },
        "2": {  
          "id": "2"
        },
        "3": {  
          "id": "3"
        }
      }
    }
},
```
Here "id" is the romanised Chinese name of the ship for each skin, appending _2, _3, _4, on the end for each skin after the default, e.g. ouruola, ouruola_2, ouruola_3. Expressions point to each of the expressions for the ship in assets/paintingface/<skin_name>, you'll need to enter one for each of the expressions there, matching the numbers, e.g. hiryuu_meta doesn't have expression number 4, it skips from 3 to 5 so that is not present in the .json file either. There should always be a 0 as that is the base skin, even if no expression file exists with that number.

```json
"lines": {
    "skin": {
      "1": {
        "skin_id": "",
        "id": "0",
        "label": "Default",
        "dialogue": {
          "1": {
            "event": "Ship Description",
            "media": "",
            "chinese": "",
            "chineseTL": "",
            "chineseNote": "",
            "japanese": "",
            "japaneseTL": "",
            "japaneseNote": "",
            "english": "",
            "englishNote": ""
          }
```
Each skin has its own different set of lines. If you notice the template, aurora has 3 skins so on the "skin": {"1"} part, you see this repeated 3 times. The "skin_id" is the romanised Chinese name of the skin, usually just the Chinese name and the number of the skin if not the default, appending _2, _3, _4 etc. depending on the number of the skin, e.g. ouruola, ouruola_2, ouruola_3. The "label" is the skin name but this doesn't appear on the page yet but make sure to fill it up with the correct details. the "media" corresponds to the "event" so this needs to be left as it is but if you are willing to help with the media files (in the section below) you can actually edit this to the filename of the sound file. Notes are to be implemented soon but there are already fields for it in the json file.

### Retrofit Section
![Retrofit Image](https://i.imgur.com/0JNgi0T.png)
```json
"1": {
	"nodeSettings": "retronode, a1, n n y n",
	"stat": "evasion, base, Evasion +5",
	"stage": "1",
	"desc": "Evasion Enhancement I",
	"level": "1",
	"limitBreak": "★★☆☆☆",
	"material": "bpt1 3, gold 600"
}
```
#### nodeSettings
nodeSettings has 3 fields, "nodetype, node, nodeconnections"
- "node" is the cell id if the node in the retrofit table in game. Like in microsoft excel or google sheets, the rows are defined by numbers while the columns are defined by letters
- "nodetype" is the type of node that fills the cell. Can only be of two, a "retronode" which is a1 in the example image and "line" which is c1 in the image
	- For "line" nodetype, you can omit the rest below like the ff example below:
```json
"5": {
	"nodeSettings": "line, c1, n y n y"
}
```
- "nodeconnections" is the directions on where the node connects to. y means yes, n means no. The format is top right bottom left. In the sample image, a1 connects to the lower cell a2, so the connections are "n n y n" which means: top = no, left = no, bottom = yes, right = no.

#### stat
stat has 3 fields, "stat, modifier, description"
- "stat" is basically the stat of the ship that is increased. Can be one of the following values: aviation, asw, divebomber, firepower, evasion, fighter, hit, hp, maingun, modernization, reload, auxgun, skill, speed, torpedobomber, torpedo.
- "modifier" is which type of stat increase it does. It can be of the two values: "base" is flat increase, "efficiency" is percentage increase. There is an exception for stats tagged as "skill" and "modernization", in this case, the "modifier" can be: offensive, defensive or support.
- "description" is the general description of the stat increase. eg. "Evasion +5"

#### stage
Up to how many times the node can be reinforced. In the case of d1 in the sample image, it's 2

#### desc
This is the description you see in the node in the sample image, in the case of a1, it's "Evasion Enhancement I"

#### level
The level where the node can be unlocked

#### limitBreak
The limit break rank allowed for the node to be unlocked

#### material
Can be of multiple field entries, but the format is generally
````
item amount, item amount, item amount
````
- "item" can only have fixed values like the following:
	- For blueprints, the format is bpt1 where t1 is the tier of the blueprint up to t3
	- For upgrade plates, the format is plt1_aux where t1 is the tier of the upgrade plate up to t3 and the value after the underscore can be of any of the ff:
		- aux = auxilliary plates
		- torp = torpedo plates
		- gun = gun plates
		- air = aircraft plates
	- For gold, just type "gold"
	- For limitbreak material (bullins/dupe) just use "duplicate"
	- For special items like san diego and warspite's items, use "special"
- "amount" is the number of needed items
## Media files

If you have extra time to assemble the media files as well, you can check the file structure from this link https://mega.nz/#!1Q9mWSZR!qpC2DluKhYgfq1LrT3ofW_9Tm0LhpaSV7zB9dqk5Sd4. It contains the format of the file names as well as the image resolutions. 

Also a few notes
- For sprites, get full resolution as much as possible, if you're ripping it from the current english wiki, be sure to get the original resolution since the one posted in the pages are scaled. 
- If you try and compress an image for size, be sure to compare the pixel quality to the original.

## Patch Notes

Compose a markdown using this website 
- http://demo.showdownjs.com/

The left pane is where you input the code, the right is the visuals it shows. After you are finised, copy the code from the left and paste it in a notepad then save it as:
````
"<yyyy>_<mm>_<dd>_patch_<server>.txt" 
````
and put it on: 
````
Patchnotes/<server>/
````
Then update the json file of the corresponding server at the json directory adding a line to the new file 
```json
{
	"1": {
		"filename": "2019_02_14_patch_jp",
		"title": "Valentines Patch",
		"date": "2019/02/14"
	}
}
```
to
```json
{
	"1": {
		"filename": "2019_02_14_patch_jp",
		"title": "Valentines Patch",
		"date": "2019/02/14"
	},
  "2": {
		"filename": "2019_02_21_patch_jp",
		"title": "x3 Hard mode",
		"date": "2019/02/21"
	}
}
```
  

## Submission
Just reply to the current OP post in /alg/ if you have a submission. 

# Updates

Adding the following json files provided by anons below:

- -

Also
- Add jukebox section [https://mega.nz/#F!SaZkHAIL!v2vRbqpbeOEjz9-tTDe94A](https://mega.nz/#F!SaZkHAIL!v2vRbqpbeOEjz9-tTDe94A)
- Filters on ship list
- Equipment page

