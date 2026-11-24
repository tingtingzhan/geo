function(el, x) {
  navigator.geolocation.getCurrentPosition(function(pos) {
    Plotly.addTraces(el, {
      type: 'scattergeo',
      mode: 'markers',
      lat: [pos.coords.latitude],
      lon: [pos.coords.longitude],
      name: 'You',
      hovertemplate: '(%{lat:.3f}\u00b0, %{lon:.3f}\u00b0)'
    });
  });
}
