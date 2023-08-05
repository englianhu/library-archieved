<img src="screenshots/logo.png" align="right" width="100" height="100"/>

# 文言 wenyan-lang

### [wy-lang.org](https://wy-lang.org) | [Wiki](https://github.com/wenyan-lang/wenyan/wiki)

[English](./README.md) | [简体中文](./README.zh-Hans.md) | 繁體中文

文言文程式語言。 [線上體驗](http://wenyan-lang.lingdong.works/ide.html)

> 🚧 請註意，中文版文件並非即時更新，僅供參考。如有歧義，請以英文版文件為主。

![](screenshots/screenshot01.png)

## 序

> 夫[唐](https://en.wikipedia.org/wiki/Emperor_Yao)、[虞](https://en.wikipedia.org/wiki/Emperor_Shun)之世，[結繩而足治](https://ctext.org/book-of-changes/xi-ci-xia/zh)，屈指而足算。是時豈料百代之後，計算機械之巧，精於[公輸](https://en.wikipedia.org/wiki/Lu_Ban)之[木鳶](https://en.wikipedia.org/wiki/Lu_Ban#Inventions)，善於[武侯](https://en.wikipedia.org/wiki/Zhuge_Liang)之[流馬](https://en.wikipedia.org/wiki/Wooden_ox)；程式語言之多，繁若[《天官》](https://ctext.org/shiji/tian-guan-shu/zh)之星宿，奇勝[《山經》](https://en.wikipedia.org/wiki/Classic_of_Mountains_and_Seas)之走獸。[鼠](https://golang.org/)、[蟹](https://www.rust-lang.org/)、[鑽](http://ruby-lang.org/)、[魚](https://fishshell.com/)，或以速稱。[蛇](https://www.python.org/)、[象](https://www.php.net/)、[駱](https://www.perl.org/)、[犀](http://shop.oreilly.com/product/9780596805531.do)，各爭文采。方知[鬼之所以夜哭，天之所以雨粟](https://ctext.org/huainanzi/ben-jing-xun/zh)。然以文言編程者 ，似所未有。此誠非文脈之所以傳，文心之所以保。[嗟予小子](https://zh.wikisource.org/zh-hant/%E6%A6%AE%E6%9C%A8_(%E9%99%B6%E6%B7%B5%E6%98%8E))，遂有斯志。然則數寸之烏絲猶覆於頭，[萬卷之素書未破於手](https://zh.wikisource.org/zh-hant/%E5%A5%89%E8%B4%88%E9%9F%8B%E5%B7%A6%E4%B8%9E%E4%B8%88%E4%BA%8C%E5%8D%81%E4%BA%8C%E9%9F%BB)；一身長羁于远邦，兩耳久旷于[雅言](https://zh.wikipedia.org/wiki/%E9%9B%85%E9%9F%B3)。然夫文章者吾之所宿好，程式者偶承時人之謬譽。故[希孟](https://en.wikipedia.org/wiki/Wang_Ximeng)不慚年少，[莊生](https://en.wikipedia.org/wiki/Zhuang_Zhou)不望[無涯](https://ctext.org/zhuangzi/nourishing-the-lord-of-life/zh)。乃作斯言。誠未能嘔瀝[長吉](https://en.wikipedia.org/wiki/Li_He)之[心血](https://zh.wikisource.org/zh-hant/%E6%9D%8E%E8%B3%80%E5%B0%8F%E5%82%B3)，亦庶幾免於[義山](https://en.wikipedia.org/wiki/Li_Shangyin)之[流沫](https://zh.wikisource.org/zh-hant/%E9%9F%93%E7%A2%91)。既成之後，復學[干將鑄劍](https://zh.wikisource.org/wiki/%E9%91%84%E5%8A%8D)而自飼，[越王嚐糞](https://ctext.org/wu-yue-chun-qiu/yue-wang-gou-jian-wu-nian/zh)而當先。自謂偶追[《十書》](https://en.wikipedia.org/wiki/Ten_Computational_Canons)之筆意，但恨少[八家](https://en.wikipedia.org/wiki/Eight_Masters_of_the_Tang_and_Song)之淋漓。此[子山](https://en.wikipedia.org/wiki/Yu_Xin)所謂[士衡抚掌而甘心，平子見陋而固宜](https://zh.wikisource.org/zh-hant/%E5%93%80%E6%B1%9F%E5%8D%97%E8%B3%A6)。然則雖實[覆甕](https://zh.wikisource.org/zh-hant/%E6%99%89%E6%9B%B8/%E5%8D%B7092#%E5%B7%A6%E6%80%9D)之質，尚存斧正之望；雖乏[呂相](https://en.wikipedia.org/wiki/L%C3%BC_Buwei)之金，[易字](https://zh.wikisource.org/zh/%E5%8F%B2%E8%A8%98/%E5%8D%B7085)之渴蓋同。此亦開源之大義，吾輩之所以勉勵也。一笑。


## Helloworld

文言:

```
吾有一數。曰三。名之曰「甲」。
為是「甲」遍。
吾有一言。曰「「問天地好在。」」。書之。
云云。
```
等同於以下 JavaScript:

```JavaScript
var n = 3;
for (var i = 0; i < n; i++) {
console.log("問天地好在。");
}
```

輸出:

```
問天地好在。
問天地好在。
問天地好在。
```

標點符號和換行都是可選的（就像古漢語中文字是連續的），所以上面的程式碼等同於：


```
吾有一數曰三名之曰「甲」為是「甲」遍吾有一言曰「「問天地好在」」書之云云
```

更多複雜的例子，可以在 `./examples` 中找到。比如，埃拉托斯特尼篩法、快速排序、曼德博集合、河諾塔。


## 特點
- 符合[古漢語](https://en.wikipedia.org/wiki/Classical_Chinese)語法的自然語言處理程式
- 可以編譯成[JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript)，[Python](https://python.org)，或者[Ruby](http://ruby-lang.org)
- [圖靈完備](https://github.com/wenyan-lang/wenyan/blob/master/examples/turing.wy)
- [線上 IDE](http://wenyan-lang.lingdong.works/ide.html)，即刻體驗
- 通過[幾個例子](https://github.com/wenyan-lang/wenyan/tree/master/examples)快速入門


## 安裝

### 編譯器

使用以下命令安裝本編譯器：

```bash
npm install -g @wenyan/cli
```

嘗試運行內建的例子，例如：

```bash
wenyan examples/helloworld.wy -o helloworld.js
```


### [線上 IDE](http://wenyan-lang.lingdong.works/ide.html)

![](screenshots/screenshot02.png)


### 編輯器外掛：

- 由[antfu](https://github.com/antfu)提供的[適用於VSCode的外掛](https://github.com/antfu/wenyan-lang-vscode)
- 由[voldikss](https://github.com/voldikss)提供的[適用於Vim的外掛](https://github.com/voldikss/vim-wenyan)
- 由[absop](https://github.com/absop)提供的[適用於Sublime Text的外掛](https://github.com/absop/SublimeWenyan)


## 語法表

上下文無關文法的語法描述還在構建中。同時，請查閱下面的語法表，或者在 `src/parser.js` 中學習其他更多語法。當然你也可以從線上 IDE 現有的例子中學習更多語法知識！

### 變量

| wenyan | JavaScript |
|---|---|
|`吾有一數。曰三。名之曰「甲」。 ` | `var a = 3;` |
|`有數五十。名之曰「大衍」。 ` | `var dayan = 50;` |
|`昔之「甲」者。今「大衍」是也。 ` | `a = dayan;` |
|`吾有一言。曰「「噫吁戲」」。名之曰「乙」。 ` | `var b = "alas!";` |
|`吾有一爻。曰陰。名之曰「丙」。 ` | `var c = false;` |
|`吾有一列。名之曰「丁」。 ` | `var d = [];` |
|`吾有三數。曰一。曰三。曰五。名之曰「甲」曰「乙」曰「丙」。 ` | `var a=1,b=3,c=5;` |


### 流程控制

| wenyan | JavaScript |
|---|---|
|`若三大於二者。乃得「「想當然耳」」也。 ` | `if (3>2){ return "of course"; }` |
|`若三不大於五者。乃得「「想當然耳」」。若非。乃得「「怪哉」」也。 ` | `if(3<=5){return "of course"}else{return "no way"}` |
|`為是百遍。 ⋯⋯ 云云。 ` | `for (var i = 0; i < 100; i++){ ... }` |
|`恆為是。 ⋯⋯ 云云。 ` | `while (true) { ... }` |
|`凡「天地」中之「人」。 ⋯⋯ 云云。 ` | `for (var human of world){ ... }` |
|`乃止。 ` | `break;` |


### 運算

| wenyan | JavaScript |
|---|---|
|`加一以二。 ` | `1+2` |
|`加一於二。 ` | `2+1` |
|`加一以二。乘其以三。 ` | `(1+2)*3` |
|`除十以三。所餘幾何。 ` | `10%3` |
|`減七百五十六以四百三十三。名之曰「甲」。 ` | `var a = 756-433;` |
|`夫「甲」「乙」中有陽乎。 ` | `a \|\| b` |
|`夫「甲」「乙」中無陰乎。 ` | `a && b` |


### 容器
數組下標從一開始。而不是零。

| wenyan | JavaScript |
|---|---|
|`吾有一列。名之曰「甲」。充「甲」以四。以二。 ` | `var a = []; a.push(4, 2);` |
|`銜「甲」以「乙」。以「丙」` | `a.concat(b).concat(c);` |
|`夫「甲」之一。 ` | `a[0]` |
|`夫「甲」之其餘。 ` | `a.slice(1);` |
|`夫「玫瑰」之「「名」」。 ` | `rose["name"]` |
|`夫「寶劍」之長。 ` | `sword.length;` |



### 對象

| wenyan | JavaScript |
|---|---|
|`吾有一物。名之曰「甲」。 ` | `var a = {};` |
|`吾有一物。名之曰「甲」。其物如是。物之「「乙」」者。數曰三。物之「「丙」」者。言曰「「丁」」。是謂「甲」之物也。 ` | `var a = {b:3, c:"d"}` |


### 函數

| wenyan | JavaScript |
|---|---|
|`吾有一術。名之曰「吸星大法」。是術曰。 ⋯⋯是謂「吸星大法」之術也。 `|`function f(){...}`|
|`吾有一術。名之曰「六脈神劍」。欲行是術。必先得六數。曰「甲」。曰「乙」。曰「丙」。曰「丁」。曰「戊」。曰「己」乃行是術曰。 ⋯⋯是謂「六脈神劍」之術也。 `|`function f(a,b,c,d,e,f){...}`|
|`吾有一術。名之曰「翻倍」。欲行是術。必先得一數。曰「甲」。乃行是術曰。乘「甲」以二。名之曰「乙」。乃得「乙」。是謂「翻倍」之術也。 `|`function double(a){var b = a * 2; return b;}`|
|`施「翻倍」於「大衍」。 `|`double(dayan);`|
|`吾有一術。名之曰「甲」。欲行是術。必先得一數曰「乙」。二言。曰「丙」。曰「丁」`|`function a(float b, string c, string d)`|
|`夫「甲」。夫「乙」。夫「丙」。取二以施「丁」。取二以施「戊」。名之曰「己」。 ` | `var f = e(a,d(b,c))`|
|`夫「甲」。夫「乙」。夫「丙」。取二以施「丁」。取二以施「戊」。取一以施「己」。夫「庚」。夫「辛」。取三以施「壬」。名之曰「癸」。 ` | `var j = i(f(e(a,d(b,c))),g,h)`|

### 導入

| wenyan | JavaScript |
|---|---|
|`吾嘗觀「「算經」」之書。方悟「正弦」「餘弦」之義。 ` | `var {sin,cos} = require("math");` |

### 雜項

| wenyan | JavaScript |
|---|---|
|`吾有一數。曰五。書之。 `|`console.log(5);`|

### 註釋

| wenyan | JavaScript |
|---|---|
|`批曰。 「「文氣淋灕。字句切實」」。 ` | `/*文氣淋灕。字句切實*/` |
|`注曰。 「「文言備矣」」。 ` | `/*文言備矣*/` |
|`疏曰。 「「居第一之位故稱初。以其陽爻故稱九」」。 ` | `/*居第一之位故稱初。以其陽爻故稱九*/` |


## 渲染器

```bash
wenyan examples/turing.wy --render 圖靈機 --output .
```

`src/render.js` 腳本文件能把 `wy` 程式碼格式化輸出成為古漢語書籍樣式（歷史印刷書籍）的可縮放向量圖形（SVG）。同時它還可以將產生的SVG文件解析回原始文言程式碼。需要獲取更詳細的訊息，請使用`wenyan -h`命令來獲取幫助。

下圖是用 wenyan 編寫的 通用圖靈機程序 渲染而成。

![](screenshots/screenshot03.png)

## 功能請求

在你打開一個新Issue之前，請先檢查你的需求是否屬於以下類別：

|名稱|優先級|需要幫助|狀態|
|---|---|---|---|
|[語言規範](https://github.com/wenyan-lang/wenyan/issues/1)| ***** | | 正在進行中 |
|[類](https://github.com/wenyan-lang/wenyan/issues/31) / [對象文法](https://github.com/wenyan-lang/wenyan/issues/20) | *** | | 對象文法已經添加 |
|[導入語句](https://github.com/wenyan-lang/wenyan/issues/100) | *** | | 導入語句已經添加 |
|標準庫([Math(數學)](https://github.com/wenyan-lang/wenyan/issues/55)/[Bitwise ops(位運算)](https://github.com/wenyan-lang/wenyan/issues/2)/[Random(隨機)](https://github.com/wenyan-lang/wenyan/issues/87)) | ***** | √ | 正在進行中 |
|[測試套件](https://github.com/wenyan-lang/wenyan/issues/38)| **** | √ | 正在進行中 |
|[Switch語句](https://github.com/wenyan-lang/wenyan/issues/53)| *** | | |
|[函數式程式設計](https://github.com/wenyan-lang/wenyan/issues/99) | *** | | |
|更嚴格的編譯器 | **** | | |
|其他語言的編譯器 | ** | √ | |
|[編輯器的附加元件](https://github.com/wenyan-lang/wenyan/issues/77) | ** | √ | 適用於VSCode, Vim, Sublime的附加元件已添加 |
|將[js](https://github.com/wenyan-lang/wenyan/issues/47)/[py](https://github.com/wenyan-lang/wenyan/issues/67)/ [anything](https://github.com/wenyan-lang/wenyan/issues/73) 轉換回wenyan(文言) | * | √ | |
|[轉義](https://github.com/wenyan-lang/wenyan/issues/84)/[產生](https://github.com/wenyan-lang/wenyan/issues/128) 特殊符號| *** | | |
|[對「「」」的替換語法](https://github.com/wenyan-lang/wenyan/issues/81)| ** | | |
|[對 。的替換語法](https://github.com/wenyan-lang/wenyan/issues/93)| ** | | |
|線上IDE 的[字體](https://github.com/wenyan-lang/wenyan/issues/5) 和[垂直文本](https://github.com/wenyan-lang/wenyan/issues/9) | ** | | |
|[將註釋呈現為小型內聯文本](https://github.com/wenyan-lang/wenyan/issues/148) | ** | | |
|更多示範 | ** | √ | |

如果你可以幫助我們實現一個在`需要幫助`下有`√`的功能，請放心提交一個PR。我們將非常感謝你！如果你想幫助我們實現其他功能，也請讓我們知道！謝謝。

## 已知問題
|名稱|優先級|需要幫助|狀態|
|---|---|---|---|
|[漢字到數字的轉換問題](https://github.com/wenyan-lang/wenyan/issues/114)| ***** | | |
|[漢字到數字轉換中多字符數字沒有被加入支援](https://github.com/wenyan-lang/wenyan/issues/130) | *** | | |
