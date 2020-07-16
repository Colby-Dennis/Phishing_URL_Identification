(function() {
    /**
     * Check and set a global guard variable.
     * If this content script is injected into the same page again,
     * it will do nothing next time.
     */
    if (window.hasRun) {
      return;
    }

    // A quick function to extract the domain of a weblink
    function getDomain(link) {
        return ((link.split("://")[1]).split("/")[0]);
    }

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Use this section to cause interactions on the webpage itself.

    // Getting the header and domain for the page.
    var pageURL = window.location.href;
    var pageDomain = getDomain(pageURL);

    // URL of Anchor
    // Checking if domain in link matches site domain. 
    // if less than 20% of the links on the page are leaving the page
    // then the page is classified as legit. If between 20 and 50
    // it is suspecious. Greater than 50 it is phishing.

    function url_of_anchor () {

        // Storing all of the links on the page in an array. (links include full address)
        l = document.links;
        var doc_links = new Array(l.length);
        for (i = 0; i < l.length; i++) {
            doc_links[i] = l[i].href;
        }
        
        // Counting number of times the url leaves the page or isn't a url. (only counting https)
        var legit_count = 0;
        for (i = 0; i < doc_links.length; i++) {
            if (doc_links[i].includes("https://")) {
                legit_count++;
            }
        }

        // Determining the percentages of links that are NOT legit (assuming https)
        var percent_anchor = Math.floor(100*(doc_links.length - legit_count)/(doc_links.length));
        
        if (percent_anchor < 20) {
            return 1;
        } else if (percent_anchor > 50) {
            return -1;
        } else {
            return 0;
        }
    }

    // Checking if the image sources are loaded from the same webpage simlar to the above method
    // If the percent of externally loaded images is less than 20 then legit, between 20 and 50
    // suspecious, greather than 50 then phishing.

    function request_url() {
        // storing all of the image sources in an array.
        var img = document.getElementsByTagName("img");
        var img_src = new Array(img.length);

        for (i = 0; i < img.length; i++) {
            img_src[i] = img[i].src;
        }

        // Counting the number of times an image is pulled from outside the site.
        var outsideRequest = 0;
        for (i = 0; i < img_src.length; i++) {
            if (pageDomain != getDomain(img_src[i])) {
                outsideRequest++;
            }
        }

        // determining the percentage of outside equests
        var requestPercent = Math.floor(100*(outsideRequest / img_src.length));

        // Returning the prediction of phishing.
        if (requestPercent < 20) {
            return 1;
        } else if (requestPercent > 50) {
            return -1;
        } else {
            return 0;
        }

    }


    // A function that extracts relevant features from a standalone weblink.
    function extractFeatures(webLink) {
        /*
            Array is as follows:
            URL_Length
            having_At_Symbol
            double_slash_redirecting
            Prefix_Suffix
            having_Sub_domain
            having_IP_address
        */
       var featureArray = new Array(6);
       
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

       
       // Having ip address
       var domainParts = getDomain(webLink.split("."));
       if (domainParts.length >= 4) {
            // If 4 consecutive parts of the domain contain numbers then it is
            // considered to have an IP address and is phishing.
            var numberCount = 0;
            var i = 0;
            // only loop while the number count is less than zero and there are more parts to consider
            while (i < domainParts.length && numberCount < 4) {
                // sectionNumber turns to true if section contains a number
                var sectionNumber = false;
                for (j = 0; j < domainParts[i].length; j++) {
                    if (!isNaN(domainParts[i][j])) {
                        sectionNumber = true;
                    }
                }

                if (sectionNumber) {
                    numberCount++;
                } else {
                    numberCount = 0;
                }

                i++;
            }

            if (numberCount >= 4) {
                featureArray[5] = -1;
            } else {
                featureArray[5] = 1;
            }
       } else {
           featureArray[5] = 1;
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
    var images = document.getElementsByTagName("img");


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