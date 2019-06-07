shinyjs.log = function(msg){
  console.log(msg);
};

shinyjs.setStyle = function(params){
  var defaultParams = {
    group: 'censusblocks',
    layerId: [],
    style: []
  };

  params = shinyjs.getParams(params, defaultParams);
  var group = params.group;
  var layerId = params.layerId;
  var style = params.style;

  //convert columnstore to row store
  style = HTMLWidgets.dataframeToD3(style);

  var styleMap = {};
  for (var i = 0; i < style.length; i++) {
    styleMap[layerId[i]] = style[i];
  }

  console.log({styleMap: styleMap});

  map.layerManager.getLayerGroup(group).eachLayer(function(layer) {
    var s = styleMap[layer.options.layerId];
    layer.setStyle(s);
  });
};
