$(function(){main();});

function main() {
  getTemplates(function(templates){
    var validator = templateValidator();
    var selector = templateSelector( templates, function(template) {
      validator.setTemplate( template );
    });
    $("div#template-validator").append(selector.element);
    $("div#template-validator").append(validator.element);
  });
}

function templateSelector( templates, templateCallback ) {
  var names = Object.keys(templates);
  names.sort;
  var orderedTemplates = $.map(names, function( name ) { return templates[name]; });
  var selector = {};
  selector.element = $("<select>");
  $.each( orderedTemplates, function( idx, template) {
    var option = $("<option>");
    option.text(template.name);
    option.val(idx);
    selector.element.append(option);
  });
  selector.element.change(function() { templateCallback(orderedTemplates[this.value]); });
  if(names.length > 0) {
    selector.element.val(0);
    templateCallback(orderedTemplates[0]);

  }
  return selector;
}

function templateValidator() {
  var validator = {};
  validator.element = $("<div>");
  validator.setTemplate = function( template ) {
    validator.element.children().remove();
    var keys = Object.keys( template.record );
    var templateForm = {};
    $.each( keys, function( hucars, key ) {
      console.log(key);
      templateForm[key] = {};
      templateForm[key].input = inputByType(templateTypes[key]);
      templateForm[key].statusButton = $("<button type=\"button\" disabled=\"true\">");
      templateForm[key].statusButton.html("&nbsp;");
      var formDiv = $("<div>");
      formDiv.text(key + ": ");
      formDiv.append(templateForm[key].input.element);
      formDiv.append(templateForm[key].statusButton);
      validator.element.append(formDiv);
    });
    checkButton = $("<button type=\"button\">Check</button>");
    checkButton.click(function() {
      var candidate = {};
      var url = "/validate/" + encodeURIComponent(template.name) + "/";
      $.each(keys, function(hucars, key){
        candidate[key] = templateForm[key].input.getValue();
      });
      $.ajax({"url": url,
              "method": "POST",
              "contentType": "application/json",
              "data": JSON.stringify(candidate),
              "success": function( data ) {
                           var allValid = true;
                           $.each(Object.keys(data), function( hucars, key) {
                             if(data[key]){ templateForm[key].statusButton.css("background-color", "green");  }
                             else { templateForm[key].statusButton.css("background-color", "red"); }
                             allValid = allValid && data[key]
                           });
                           if(allValid) {
                             checkButton.css("background-color", "green");
                           } else {
                             checkButton.css("background-color", "red"); 
                           }
                         },
              "dataType": "json"});
    });
    validator.element.append(checkButton);
  };
  return validator;
}


