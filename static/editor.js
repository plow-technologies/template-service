$(function(){main();});

function main() {
  getTemplates(function(templates){
    var editor = templateEditor();
    var selector = templateSelector( templates, function(template) {
      editor.setTemplate( template );
    }, editor);
    var adder = templateAdder( selector );
    $("div#template-editor").append(selector.element);
    $("div#template-editor").append(adder.element);
    $("div#template-editor").append(editor.element);
  });
}

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

function templateSelector( templates, selectCallback, editor ) {
  var selector = {};
  var templatesList = {};
  templatesList.templates = templates;
  selector.element = $("<select>");

  selector.setTemplates = function( templatesFunction, selectedTemplate ) {
    selector.element.html("");
    templatesList.templates = templatesFunction( templatesList.templates );
    var names = Object.keys(templatesList.templates);
    names.sort();
    templatesList.list = $.map(names, function( templateName ) { return templatesList.templates[templateName]; });
    $.each(templatesList.list, function(idx, template ) { 
      var option = $("<option>");
      option.val(idx);
      option.text(template.name);
      selector.element.append(option);
    });
    selector.element.change( function () { selectCallback( templatesList.list[this.value] ); });
    if(templatesList.list.length > 0) {
      if(typeof(selectedTemplate) !== 'undefined' && $.inArray(selectedTemplate, names) >= 0) {
        selector.element.val("" + $.inArray(selectedTemplate, names));
        selectCallback(templatesList.list[$.inArray(selectedTemplate, names)]);
      } else {
        selector.element.val('0');
        selectCallback(templatesList.list[0]);
      }
    }
  };

  selector.setTemplates( function(x){return x;} );
  editor.setSelector( selector );
  return selector;
}

function templateEditor() {
  var editor = {};
  editor.selector = undefined;
  editor.setSelector = function( selector ){ editor.selector = selector };
  editor.element = $("<div class=\"editor-widget\">");
  var titleDiv = $("<div>");
  var titleH1 = $("<h1>");
  titleDiv.append(titleH1);
  var editorDiv = $("<div>");
  editor.element.append(titleDiv);
  editor.element.append(editorDiv);
  editor.setTemplate = function( template ) {
    titleH1.text(template.name);
    editorDiv.children().remove();
    $.each(Object.keys(template.record), function(hucars, key) {
      var keyDiv = $("<div>");
      var keyH2 = $("<h2>");
      keyH2.text(key);
      editorDiv.append(keyDiv);
      keyDiv.append(keyH2);
      var editTable = templateEditTable( template, key, editor );
      editorDiv.append(editTable.element);
    }); 
  };
  return editor;
}

function templateEditTable( template, key, editor ) {
  var matrix = [];
  if($.inArray("Meh", Object.keys(template.record[key])) >= 0) {
  } else {// Or
    $.each(template.record[key]["Or"], function(orIdx, disjTemplate) {
      matrix[orIdx] = [];
      if($.inArray("Meh", Object.keys(disjTemplate)) >= 0) {
      } else {//And
        $.each(disjTemplate["And"], function(andIdx, conjTemplate) {
          matrix[orIdx][andIdx] = $("<td>"); 
          var constrSelector = constraintSelector(template, key, orIdx, andIdx, editor);
          matrix[orIdx][andIdx].append(constrSelector.element);
        });
      } 
    });
  }
  var templateEditTable = {};
  templateEditTable.element = $("<table>");
  $.each(matrix, function(orIdx, andArray) {
    var orRow = $("<tr>");
    $.each(andArray, function(andIdx, constrTr) {
      orRow.append(constrTr);
    });
    var andAddCell = $("<td>");
    var andAddButton = $("<button type=\"button\">");
    andAddButton.text("+ AND");
    andAddButton.click(function() {
      if($.inArray("Meh", Object.keys(template.record[key]["Or"][orIdx])) >= 0) {
        template.record[key]["Or"][orIdx] = {"And": [{"Meh": []}]};
      } else {
        template.record[key]["Or"][orIdx]["And"].push({"Meh":[]});
      }
      postTemplate( template, editor.selector );
    });
    andAddCell.append(andAddButton);
    orRow.css("border-bottom: 1px dotted black");
    orRow.append(andAddCell);
    templateEditTable.element.append(orRow);
  });
  var orAddRow = $("<tr>");
  var orAddCell = $("<td>");
  var orAddButton = $("<button type=\"button\">");
  orAddButton.text("+ OR");
  orAddButton.click(function() {
    if($.inArray("Meh", Object.keys(template.record[key])) >= 0) {
      template.record[key] = {"Or": [{"And": []}]};
    } else {
      template.record[key]["Or"].push({"And": []});
    }
    postTemplate( template, editor.selector);
  });
  orAddCell.append(orAddButton);
  orAddRow.append(orAddCell);
  templateEditTable.element.append(orAddRow);
  return templateEditTable
}

function constraintSelector( template, key, orIdx, andIdx, editor ){
  var cSelector = {};
  cSelector.element = $("<div>");
  var vInput = valInput( template, key, orIdx, andIdx, editor );
  var typeSelector = $("<select>");
  var types = {"=": "Eq", "<": "Lt", ">" : "Gt", " " : "Meh"};
  $.each(Object.keys(types), function(hucars, typeKey) {
    var typeOption = $("<option>");
    typeOption.text(typeKey);
    typeOption.val(types[typeKey]);
    typeSelector.append(typeOption);
  });
  typeSelector.val(Object.keys(template.record[key]["Or"][orIdx]["And"][andIdx])[0]);
  cSelector.element.append(typeSelector);
  cSelector.element.append(vInput.element);
  saveButton = $("<button type=\"button\">✓</button>");
  saveButton.click( function() {
    if(typeSelector.val() == "Meh") {
      template.record[key]["Or"][orIdx]["And"][andIdx] = {"Meh": []};
    } else {
      var newTemplate = {};
      newTemplate[typeSelector.val()] = vInput.getValue();
      template.record[key]["Or"][orIdx]["And"][andIdx] = newTemplate;
    }
    postTemplate( template, editor.selector);
  });
  cSelector.element.append(saveButton);
  return cSelector;
}

function getValueFromTemplate( template, key, orIdx, andIdx ) {
  constr = template.record[key]["Or"][orIdx]["And"][andIdx];
  if(constr["Eq"] !== undefined) {
    return constr["Eq"];
  } else if(constr["Lt"] !== undefined) {
    return constr["Lt"];
  } else if(constr["Gt"] !== undefined) {
    return constr["Gt"];
  } else {
    return null;
  } 
}

function valInput( template, key, orIdx, andIdx, editor ) {
  var vInput = inputByType(templateTypes[key]);
  vInput.setValue( getValueFromTemplate( template, key, orIdx, andIdx ));
  return vInput;
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

function templateAdder( selector ) {
  var adder = {};
  adder.element = $("<div class=\"adder-widget\">");
  nameField = $("<input type=\"text\">");
  addButton = $("<button type=\"button\">");
  addButton.text("Add");
  addButton.click(function() {
    postTemplate({"name": nameField.val(), "record": defaultTemplate}, selector);
    nameField.val("");
  });
  adder.element.append(nameField);
  adder.element.append(addButton);
  return adder;
}

function postTemplate( template, selector ) {
  var url = "/templates/" + encodeURIComponent(template.name) + "/";
  $.ajax({"url": "/templates/",
           "method": "POST",
           "contentType": "application/json",
           "data": JSON.stringify(template),
           "success": function( ){
                        $.get(url, null, function( newTemplate ) {
                          selector.setTemplates ( function( templates ) { templates[template.name] =
                            {
                              "name": template.name, 
                              "record": newTemplate
                            };
                            return templates;
                          }, template.name );
                        },"json");
                      },
           "dataType": "json"});
}

defaultTemplate = {"unit_id":{"Meh":[]},"status_active":{"Meh":[]},"location":{"Meh":[]},"parameter_tag_id":{"Meh":[]},"result":{"Meh":[]},"slave_parameter_id":{"Meh":[]},"validation_code":{"Meh":[]},"last_update":{"Meh":[]},"location_id":{"Meh":[]},"siteIdRef":{"Meh":[]},"pid":{"Meh":[]},"permissions":{"Meh":[]},"companyIdRef":{"Meh":[]},"description":{"Meh":[]},"delete":{"Meh":[]},"status_writable":{"Meh":[]}};

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
  "delete": {"maybe": "int"},
  "companyIdRef": {"maybe": "int"},
  "siteIdRef": {"maybe": "int"},
  "location": {"maybe": "location"},
  "pid": {"maybe": "int"}
};

