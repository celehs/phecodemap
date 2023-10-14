
function setObservation(objectId){
  if(domDoExist(objectId)){
    setObservationOp(objectId)
  }
}

function domDoExist(objectId){
  if(document.getElementById(objectId)){
    if(document.getElementById(objectId)){
      return true;
    }
  }
  return false;
}

function setObservationOp(objectId){
  // Appear the first time: draw a circle
  setHeight();
  // set a listener: redraw on resize
  let observingDom = document.getElementById(objectId);
  let observe = new MutationObserver(function (mu, ob) {
    // console.log("DOM updated");
    setHeight();
  })
  observe.observe(observingDom, { attributes: true, childList: true, subtree: true });
}

// Store all insertedDoms
setHeight = function() {
  // var window_height = $(window).height();
  // var header_height = $(".main-header").height();
  // var boxHeight = window_height - header_height - 30;
  var boxHeight = $("#box_table").height();
  
  $("#box_legend").height(boxHeight);
  $("#ui_legend").height(boxHeight - 20);
};

setInterval("setObservation('box_table')", 500)
