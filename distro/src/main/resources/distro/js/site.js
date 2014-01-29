wc = {
  typeaheadWrapper: function(inputId, handleDatum, remoteUrl) {
    $( document ).ready(function() {
      $(inputId).typeahead({
        name: inputId,
        remote: remoteUrl
      });
      $(inputId + '.input-sm').siblings('input.tt-hint').addClass('hint-small');
      $(inputId + '.input-lg').siblings('input.tt-hint').addClass('hint-large');
      $(inputId).on('typeahead:selected', function (object, datum) {
        handleDatum(datum);
      });
    });
  },
  setProgress: function (percent, statusLabel, progressBarId, progressStatusId, progressPercentId) {
    $( document ).ready(function() {
      var progress = document.getElementById(progressBarId);
      progress.style.width = percent + '%';
      progress.textContent = percent + '%';
      //document.getElementById(progressBarId).className = 'loading';
      document.getElementById(progressBarId).style.width = progress.style.width;
      document.getElementById(progressStatusId).innerText = statusLabel;
    });
  },
  s3Upload: function (signUrl, nameParam, mimeParam, progressBarId, progressStatusId, progressPercentId, blurId, setProgress) {

      function createCORSRequest(method, url) {
        var xhr = new XMLHttpRequest();
          if ("withCredentials" in xhr) {
            xhr.open(method, url, true);
          }
          else if (typeof XDomainRequest != "undefined") {
            xhr = new XDomainRequest();
            xhr.open(method, url);
          }
          else {
            xhr = null;
          }
          return xhr;
      }

      /**
        * Execute the given callback with the signed response.
        */
      function executeOnSignedUrl(file, callback) {
        var xhr = new XMLHttpRequest();
        xhr.open('GET', signUrl + '?' + nameParam + '=' + file.name + '&' + mimeParam + '=' + file.type, true);

        // Hack to pass bytes through unprocessed.
        xhr.overrideMimeType('text/plain; charset=x-user-defined');

        xhr.onreadystatechange = function(e) {

          if (this.readyState == 4 && this.status == 200) {
            callback(decodeURIComponent(this.responseText));
          }
          else if(this.readyState == 4 && this.status != 200) {
            setProgress(0, 'Could not contact signing script. Status = ' + this.status, progressBarId, progressStatusId, progressPercentId);
          }
        };
        xhr.send();
      }

      function uploadFile(file) {
        executeOnSignedUrl(file, function(signedURL) {
          uploadToS3(file, signedURL);
        });
      }

      /**
        * Use a CORS call to upload the given file to S3. Assumes the url
        * parameter has been signed and is accessable for upload.
        */
      function uploadToS3(file, url) {
        var xhr = createCORSRequest('PUT', url);
        if (!xhr) {
          setProgress(0, 'CORS not supported', progressBarId, progressStatusId, progressPercentId);
        }
        else{
          xhr.onload = function() {
            if(xhr.status == 200) {
              setProgress(100, 'Upload completed.', progressBarId, progressStatusId, progressPercentId);
              /**
               * parse url and blur field
               */
              var p = document.createElement('a');
              p.href = url;
              var sansQuery = p.protocol + '//' + p.host + p.pathname;
              document.getElementById(blurId).value = sansQuery;
              $('#' + blurId).blur();
            }
            else {
              setProgress(0, 'Upload error: ' + xhr.status, progressBarId, progressStatusId, progressPercentId);
            }
          };
          xhr.onerror = function() {
            setProgress(0, 'XHR error.', progressBarId, progressStatusId, progressPercentId);
          };

          xhr.upload.onprogress = function(e) {
            if (e.lengthComputable) {
              var percentLoaded = Math.round((e.loaded / e.total) * 100);
              setProgress(percentLoaded, percentLoaded == 100 ? 'Finalizing.' : 'Uploading.', progressBarId, progressStatusId, progressPercentId);
            }
          };

          xhr.setRequestHeader('Content-Type', file.type);
          xhr.setRequestHeader('x-amz-acl', 'public-read');
          xhr.send(file);
        }
      }

      function handleFileSelect(evt) {
        setProgress(0, 'Upload started.', progressBarId, progressStatusId, progressPercentId);

        var files = evt.target.files;
        var output = [];

        for (var i = 0, f; f = files[i]; i++) {
          uploadFile(f);
        }
      }

      /**
       * return the function
       */
      return handleFileSelect;
  }

}

