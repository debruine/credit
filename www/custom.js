$(document).on("shiny:connected", function() {
  // send window width to shiny
  shiny_size = function() {
    Shiny.setInputValue("window_width", window.innerWidth);
    Shiny.setInputValue("window_height", window.innerHeight);
  }

  window.onresize = shiny_size;
  shiny_size(); // trigger once at start

  // collapse box by ID
  closeBox = function(boxid) {
    var box = $('#' + boxid).closest('.box');
    if (!box.hasClass('collapsed-box')) {
      box.find('[data-widget=collapse]').click();
    }
  };

  // uncollapse box by ID
  openBox = function(boxid) {
    var box = $('#' + boxid).closest('.box');
    if (box.hasClass('collapsed-box')) {
      box.find('[data-widget=collapse]').click();
    }
  };

  // toggle box on click
  $('.box').on('click', '.box-header h3', function() {
    $(this).closest('.box')
           .find('[data-widget=collapse]')
           .click();
  });

  // add to author list from orcid_matches
  $('#orcid_options').on('click', 'button', function() {
    console.log("button ", this.id)
    Shiny.setInputValue("orcid_to_add", this.id);
  });

});
