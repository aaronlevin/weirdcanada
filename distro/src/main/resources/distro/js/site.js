wc = {
  typeaheadWrapper: function(inputId, handleDatum, remoteUrl) {
    $( document ).ready(function() {
      $(inputId).typeahead({
        name: 'artists',
        remote: remoteUrl
        //remote: '/api/artist/%QUERY'
      });
      $(inputId + '.input-sm').siblings('input.tt-hint').addClass('hint-small');
      $(inputId + '.input-lg').siblings('input.tt-hint').addClass('hint-large');
      $(inputId).on('typeahead:selected', function (object, datum) {
        handleDatum(datum);
      });
    });
  }
}

