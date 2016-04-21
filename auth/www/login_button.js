$(document).keyup(function(event) {
    if ($("#userpass").is(":focus") && (event.keyCode == 13)) {
        $("#login_button").click();
    }
});