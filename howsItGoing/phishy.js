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
        if (link.includes("://")) {
            return ((link.split("://")[1]).split("/")[0]);
        } else {
            return (link.split("/")[0]);
        }
        
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
            Prefix_Suffix
            having_Sub_domain
        */
       var featureArray = new Array(3);
       
       // URL_Length
       if (webLink.length < 54) {
           featureArray[0] = 1;
       } else if (webLink.length > 75) {
           featureArray[0] = -1;
       } else {
           featureArray[0] = 0;
       }

       var linkDomain = getDomain(webLink);

       // Prefix_Suffix
       if (linkDomain.includes("-")) {
           featureArray[1] = -1;
       } else {
           featureArray[1] = 1;
       }

       // sub domain and multi sub domain
       // removing www. from domain if present.
       if (linkDomain.includes("www.")) {
           linkDomain = linkDomain.split("www.");
           linkDomain = linkDomain.join("");
       }

       linkDomain = linkDomain.split(".");
       if (linkDomain.length < 4) {
           featureArray[2] = 1;
       } else if (linkDomain.length == 4) {
           featureArray[2] = 0;
       } else {
           featureArray[2] = -1;
       }

      // console.log("got domain sub domain");

            
       return(featureArray)

    }

    var state = 0;

    // Combining everything to check for phishing
    // The function loops through each link on the page. If the link is considered phishing
    // it will put a read border around the link.
    function goPhishing() {
        l = document.links;
        state = 1;
        var sketchLinkCount = 0;

        console.log("Total number of links on the page: "+l.length);

        var urlAnchor = url_of_anchor();
        var requestUrl = request_url();

        // Using the c.50 model which gave a 85.3% accuracy on our dataset.
        // looping through each link on the page.
        for (var i = 0; i < l.length; i++) {
            var isPhishing = false; // a place to store the result of this link.
            var features = extractFeatures(l[i].href);
            /*
            features array is as follows:
            [0] URL_Length
            [1] Prefix_Suffix
            [2] having_Sub_domain
        */
            if (urlAnchor <= -1) {
                isPhishing = true;
            } else {
                if (features[1] > 0) {
                    isPhishing = false;
                } else {
                    if(features[2] > 0) {
                        isPhishing = false;
                    } else {
                        if (features[0] > 0) {
                            isPhishing = false;
                        } else {
                            if (urlAnchor > 0) {
                                isPhishing = false;
                            } else {
                                if (requestUrl <= -1) {
                                    isPhishing = true;
                                } else {
                                    isPhishing = false;
                                }
                            }
                        }
                    }
                }
            }


            if (isPhishing) {
                l[i].style.border = "thick solid red";
                sketchLinkCount++;
            }
        }

        console.log("I predected there were "+sketchLinkCount+" phishing links on this page.");
         
    }

    function removePhishingDetection() {
        l = document.links;
        state = 0;
        for (i = 0; i < l.length; i++) {
            l[i].style.border = "";
        }
        
    }


    window.hasRun = true;
    browser.runtime.onMessage.addListener((message) => {
        if (message.command == "phish" && state == 0) {
            goPhishing();
        } else if (message.command == "reset" && state == 1) {
            removePhishingDetection();
        }
      });

    
})();