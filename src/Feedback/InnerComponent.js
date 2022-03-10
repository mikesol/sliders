exports.sliderValue = function(e) {
  console.log(e);
  return parseInt(e.target.value) / 100.0;
}