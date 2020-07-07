(function() {
    /**
     * Check and set a global guard variable.
     * If this content script is injected into the same page again,
     * it will do nothing next time.
     */
    if (window.hasRun) {
      return;
    }
    //webRequest.onHeadersReceived.addListener()

    // A function that extracts relevant features from a weblink
    function extractFeatures(webLink) {
        /*
            Array is as follows:
            URL_Length
            having_At_Symbol
            double_slash_redirecting
            Prefix_Suffix
            having_Sub_domain
        */
       var featureArray = new Array(5);
       
       // URL_Length
       if (webLink.length < 54) {
           featureArray[0] = 1;
       } else if (webLink.length > 75) {
           featureArray[0] = -1;
       } else {
           featureArray[0] = 0;
       }

       // having_At_Symbol
       if (webLink.includes("@")) {
           featureArray[1] = -1;
       } else {
           featureArray[1] = 1;
       }

       // double_slash_redirecting
       if (webLink.replace("http://","").replace("https://","").includes("//")) {
           featureArray[2] = -1;
       } else {
           featureArray[2] = 1;
       }

       link_split = webLink.split(".")
       // Prefix_Suffix
       if (link_split.length >= 2) {
        if (link_split[0].includes("-") || link_split[2].includes("-")) {
            featureArray[3] = -1;
        } else {
            featureArray[3] = 1;
        }
       } else {
            featureArray[3] = 1;
       }

       // sub domain and multi sub domain
       dotCount = 3;
       if (link_split[0].includes("www")) {
        dotCount = 4;
       }
       if (link_split.length < dotCount) {
           featureArray[4] = 1;
       } else if (link_split.length == dotCount) {
           featureArray[4] = 0;
       } else {
           featureArray[4] = -1;
       }
       
       return(featureArray)
    }

    function isLegit_Linear(array) {

        var model_total = 0.40569;
        model_total += 0.05772 * array[0];
        model_total += 0.10261 * array[1];
        model_total += -0.01059 * array[2];
        model_total += 0.4598 * array[3];
        model_total += 0.33674 * array[4];
        if (model_total >= 0) {
            return true;
        } else {
            return false
        }
    }

    function isLegit_Tree(array) {
        if (array[4] < 1) {
            if (array[3] < 1) {
                return false;
            } else {
                return true;
            }
        } else {
            return false;
        }
    }

    var state = 0;

    var links = document.getElementsByTagName('a');
    var notSketch = "padding: 3px; background-color: green; color: black";
    var sketch = "padding: 3px; background-color: red; color: black";


    function gottaShow () {
        state = 1;
        for (var i = 0; i < links.length; i++) {
            var div = document.createElement("div");
            div.classList.add("sketchRating");
            if (isLegit_Tree(extractFeatures(links[i].href))) {
                links[i].style = notSketch;
                div.style = notSketch;
            } else {
                links[i].style = sketch;
                div.style = sketch;
            }
            
            links[i].insertBefore(div, links[i].childNodes[0]);
            extractFeatures(links[i].href);
        }
    }

    function notShow () {
        state = 0;
        for (var i = 0; i < links.length; i++) {
            links[i].style = "";
        }
        sketchRatings = document.getElementsByClassName("sketchRating")
        while(sketchRatings) {
            sketchRatings[0].remove();
        }
    }

    window.hasRun = true;
    browser.runtime.onMessage.addListener((message) => {
        if (message.command == "phish" && state == 0) {
            gottaShow();
        } else if (message.command == "reset" && state == 1) {
            notShow();
        }
      });
    
})();