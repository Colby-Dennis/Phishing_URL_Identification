const showLinks = `a {
    border: "5px solid red";
}`;

function listenForClicks() {
    document.addEventListener("click", (e) => {

        function showAllLinks(tabs) {
            browser.tabs.sendMessage(tabs[0].id, {
                command: "phish",
            });
        }

        function reset(tabs) {
            browser.tabs.sendMessage(tabs[0].id, {
                command: "reset",
            })
        }

        if (e.target.id === "phish") {
            browser.tabs.query({active: true, currentWindow: true})
            .then(showAllLinks);
        }

        if (e.target.id === "reset") {
            browser.tabs.query({active: true, currentWindow: true})
            .then(reset);
        }


    });
}

browser.tabs.executeScript({file: "phishy.js"})
.then(listenForClicks)