$(document).ready ->
  
  $('header h1 a').on('click', (event) ->
    event.preventDefault()
    $('header').toggleClass('active')
  )
  
  $(window).on('scroll', ->
    $('header').removeClass('active')
  )

  
  if $("body").hasClass("fullImage")
    theWindow = $(window)
    $postMedia = $(".postMedia")
    $postMediaImg = $(".postMedia img")
    postMediaImgAspectRatio = 1
    
    img = $postMediaImg[0]
    $("<img/>").attr("src", $(img).attr("src")).load( ->
      pic_real_width = this.width
      pic_real_height = this.height
      postMediaImgAspectRatio = pic_real_width / pic_real_height
    )
    
    resizeBg = ->
      if ( (theWindow.width() / theWindow.height()) < postMediaImgAspectRatio*0.9 )
        $postMedia.css("height", "auto")
        console.log "we're good"
      else
        console.log "what up"
        
        $postMedia.css("height", theWindow.height() * 0.9)
        
        # $postMediaImg.css("top", -topOffset)
    
        # $postMedia
        #   .removeClass()
        #   .addClass('bgwidth')
    
    theWindow.resize(resizeBg).trigger("resize")
  
  
