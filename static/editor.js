$(function(){main();});

function main() {
  getTemplates(function(templates){
    var editor = templateEditor();
    var selector = templateSelector( templates, function(template) {
      editor.setTemplate( template );
    });
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

function templateSelector( templates, selectCallback ) {
  var selector = {};
  var templatesList = {};
  templatesList.templates = templates;
  selector.element = $("<select>");

  selector.setTemplates = function( templatesFunction ) {
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
      selectCallback(templatesList.list[0]);
    }
  };

  selector.setTemplates( function(x){return x;} )
  return selector;
}

function templateEditor() {
  var editor = {};
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
      var editTable = templateEditTable( template.record[key] );
      editorDiv.append(editTable.element);
    }); 
  };
  return editor;
}

function templateEditTable( template ) {
  var matrix = [];
  if($.inArray("Meh", Object.keys(template)) >= 0) {
  } else {// Or
    $.each(template.record["Or"], function(orIdx, disjTemplate) {
      matrix[orIdx] = [];
      if($.inArray("Meh", Object.keys(disjTemplate)) >= 0) {
      } else {//And
        $.each(disjTemplate["And"], function(andIdx, conjTemplate) {
          matrix[orIdx][andIdx] = $("<td>"); 
          matrix[orIdx][andIdx].text(JSON.encode(conjTemplate));
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
    templateEditTable.element.append(orRow);
  });
  return templateEditTable
}

function templateAdder( selector ) {
  var adder = {};
  adder.element = $("<div class=\"adder-widget\">");
  nameField = $("<input type=\"text\">");
  addButton = $("<button type=\"button\">");
  addButton.text("Add");
  addButton.click(function() {
    var addUrl = "/templates/" + encodeURIComponent(nameField.val()) + "/";
    $.ajax({"url": "/templates/",
             "method": "POST",
             "contentType": "application/json",
             "data": JSON.stringify({"name": nameField.val(), "record": defaultTemplate}),
             "success": function( ){
                          $.get(addUrl, null, function( newTemplate ) {
                            selector.setTemplates ( function( templates ) { templates[nameField.val()] =
                              {
                                "name": nameField.val(),
                                "record": newTemplate
                              };
                              return templates;
                            });
                            nameField.val("");
                          },"json");
                        },
             "dataType": "json"});

  });
  adder.element.append(nameField);
  adder.element.append(addButton);
  return adder;
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






