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
    var list = $.map(names, function( templateName ) { return templatesList.templates[templateName]; });
    $.each(list, function(idx, template ) { 
      var option = $("<option>");
      option.val(idx);
      option.text(template.name);
      selector.element.append(option);
    });
    selector.element.change( function () { console.log(this.value); console.log(list); selectCallback( list[this.value] ); });
    if(list.length > 0) {
      selectCallback(list[0]);
      console.log(list[0]);
    }
  };

  selector.setTemplates( function(x){return x;} )
  return selector;
}

function templateEditor() {
  var editor = {};
  editor.element = $("<div class=\"editor-widget\">");
  titleDiv = $("<div>");
  titleH1 = $("<h1>");
  titleDiv.append(titleH1);
  editor.element.append(titleDiv);
  editor.setTemplate = function( template ) {
    titleH1.text(template.name);
  };
  return editor;
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
                              console.log(templates);
                              return templates;
                            });
                          },"json");
                        },
             "dataType": "json"});

  });
  adder.element.append(nameField);
  adder.element.append(addButton);
  return adder;
}

defaultTemplate = {"unit_id":{"Meh":[]},"status_active":{"Meh":[]},"location":{"Meh":[]},"parameter_tag_id":{"Meh":[]},"result":{"Meh":[]},"slave_parameter_id":{"Meh":[]},"validation_code":{"Meh":[]},"last_update":{"Meh":[]},"location_id":{"Or":[]},"siteIdRef":{"Meh":[]},"pid":{"Meh":[]},"permissions":{"Meh":[]},"companyIdRef":{"Meh":[]},"description":{"Meh":[]},"delete":{"Meh":[]},"status_writable":{"Meh":[]}};

