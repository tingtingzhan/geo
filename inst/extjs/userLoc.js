function(el, x) {
  navigator.geolocation.getCurrentPosition(function(pos) {
    Plotly.addTraces(el, {
      type: 'scattergeo',
      mode: 'markers',
      lat: [pos.coords.latitude],
      lon: [pos.coords.longitude],
      name: 'You'
    });
  });
}
