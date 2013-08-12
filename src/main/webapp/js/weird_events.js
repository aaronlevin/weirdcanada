function cityAjaxCall(chosenCity){
	if(chosenCity == 'all' || !chosenCity){
		urlSubString = 'featured_events/';
	}
	else{
		urlSubString = 'featured_events/city/' + chosenCity + '/';
	}
	var items = [];
	jQuery("ul#featuredEvents").html('<li class="loading"><img src="http://dc591.4shared.com/img/gDNn5xoH/loading.gif" class="loading-icon" /></li>');
  jQuery.ajax({
    dataType: "json",
    url: 'http://api.weirdcanada.com/api/' + urlSubString,
    success: function(data) {

      jQuery.each(data, function(key, val) {
        eventItem = '';
        eventItem += '<ul class="featuredEvent">';
        eventItem += '<li class="eventDate">'+ val.eventDateTime + '</li>';
        eventItem += '<li class="eventInfo">';
        eventItem += '<span class="eventPresenter">' + val.presenter +'</span>: ';
        eventItem += '<span class="eventDescription">' + val.description + '</span> @ ';
        eventItem += '<span class="eventVenue">' + val.venueName +'</span> ';
        eventItem += '(<span class="eventCity">' + val.city + '</span>)';
        eventItem += '</li>';
        eventItem += '<li class="eventDetails">(<a href="'+ val.url + '" target="_blank">more details</a>)</li>';
        eventItem += '</ul>';
        items.push('<li class="event">' + eventItem + '</li>');
      });
      if(items.length == 0){
			items.push('<li class="event">There are no featured events for this location at this time.</li>');
      }
    }})
	.done(function(){jQuery("ul#featuredEvents").html(items.join(''));})
	.fail(function(){jQuery("ul#featuredEvents").html('<li class="event">An error occured while getting this info.</li>');});
}

jQuery(document).ready(function(){
	cityAjaxCall();
	jQuery("select#featured_events").change(function(){
		cityAjaxCall(jQuery(this)[0].value);
	});
});
