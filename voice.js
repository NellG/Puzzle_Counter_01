if(annyang) {
  var value = 0;
  var commands = {
    '*val': function(val) {
      value = val;
      buffer = 'chartruese';
      Shiny.onInputChange('spoken', value, {priority: 'event'});
    }
  };
  annyang.addCommands(commands);
  annyang.start();
}