LeafletWidget.methods.setStyle = function(group, layerId, style) {
  var map = this;
  //convert columnstore to row store
  style = HTMLWidgets.dataframeToD3(style);

  var styleMap = {};
  for (var i = 0; i < style.length; i++) {
    styleMap[layerId[i]] = style[i];
  }

  map.layerManager.getLayerGroup(group).eachLayer(function(layer) {
    var s = styleMap[layer.options.layerId];
    layer.setStyle(s);
  });
};
