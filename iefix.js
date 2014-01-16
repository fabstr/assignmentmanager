// this code snippet fixes responsivness on windows mobile 8
// found on http://geekswithblogs.net/dotgeek/archive/2013/08/06/how-to-fix-the-bootstrap-issue-on-windows-phone-8.aspx
(function() {
    if ("-ms-user-select" in document.documentElement.style && navigator.userAgent.match(/IEMobile\/10\.0/)) {
        var msViewportStyle = document.createElement("style");
        msViewportStyle.appendChild(
            document.createTextNode("@-ms-viewport{width:auto!important}")
        );
        document.getElementsByTagName("head")[0].appendChild(msViewportStyle);
    }
})();
