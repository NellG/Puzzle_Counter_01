if(annyang) {
  var value = 0;
  var commands = {
    'biscuit': function() {
      value += 1;
      Shiny.onInputChange('counter', value);
    }
  };
  annyang.addCommands(commands);
  annyang.start();
}