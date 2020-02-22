if(annyang) {
  var value = 0;
  var commands = {
    'ding': function() {
      value += 1;
      Shiny.onInputChange('counter', value);
    }
  };
  annyang.addCommands(commands);
  annyang.start();
}