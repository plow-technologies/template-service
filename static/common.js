function getTemplates( templateCallback ) {
  $.get( "/templates/",
         function( data ) {
           data.reverse();
           var templates = {};
           $.each(data,function(hucars,template){
             templates[template.name] = template;
           });
           templateCallback(templates);
         });
}

function inputByType(type) {
  var input = {};
  input.element = $("<span>");
  if(type == "int") {
    var numberInput = $("<input type=\"text\">");
    input.setValue = function( num ) { numberInput.val("" + num); };
    input.getValue = function() { return parseInt( numberInput.val() ); };
    input.element.append(numberInput);
  } else if (type == "text" || type == "time") {
    var textInput = $("<input type=\"text\">");
    input.setValue = function( text ) { textInput.val(text); };
    input.getValue = function() { return textInput.val(); };
    input.element.append(textInput)
  } else if (type == "location") {
    input.setValue = function(nope){};
    input.getValue = function() { return {"location": "location"}; };
  } else if (typeof(type["maybe"] !== undefined)) {
    var maybeInput=$("<select>");
    maybeInput.append($("<option value=\"just\">Just</option>"));
    maybeInput.append($("<option value=\"nothing\">Nothing</option>"));
    var recInput = inputByType(type["maybe"]);
    input.setValue = function( val ) { if( val == null ) { maybeInput.val("nothing"); } else { maybeInput.val("just"); recInput.setValue( val ); }};
    input.getValue = function() { if( maybeInput.val() == "nothing" ) { return null; } else { return recInput.getValue(); }};
    input.element.append(maybeInput);
    input.element.append(recInput.element);
  }
  return input;
}

templateTypes = {
  "location_id": {"maybe": "int"},
  "slave_parameter_id": {"maybe": "int"},
  "parameter_tag_id": {"maybe": "int"},
  "description": {"maybe": "text"},
  "unit_id": {"maybe": "int"},
  "status_active": {"maybe": "int"},
  "status_writable": {"maybe": "int"},
  "last_update": {"maybe": "time"},
  "result": {"maybe": "text"},
  "validation_code": {"maybe": "text"},
  "permissions": {"maybe": "int"},
  "delete": {"maybe": "int"},
  "companyIdRef": {"maybe": "int"},
  "siteIdRef": {"maybe": "int"},
  "location": {"maybe": "location"},
  "pid": {"maybe": "int"}
};

