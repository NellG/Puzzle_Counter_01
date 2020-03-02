if(annyang) {
  var value = 0;
  var commands = {
    '*val': function(val) {
      value = val;
      Shiny.onInputChange('spoken', value);
    }
  };
  annyang.addCommands(commands);
  annyang.start();
}