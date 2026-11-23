
function() {
  var now = new Date();
  Shiny.setInputValue('userTime', {
    iso: now.toISOString(), // UTC
    tz: Intl.DateTimeFormat().resolvedOptions().timeZone
  }, {priority: 'event'});
}

