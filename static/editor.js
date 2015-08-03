$(function(){main();});

function main() {
  getTemplates(function(templates){
    var selector = templateSelector( templates, function(template) { console.log(template); } );
    $("div#template-editor").append(selector);
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
  var templateNames = Object.keys(templates);
  templateNames.sort();
  templatesList = $.map(templateNames, function( templateName ) { return templates[templateName]; });
  var selector = $("<select>");
  $.each(templatesList, function(idx, template ) { 
    var option = $("<option>");
    option.val(idx);
    option.text(template.name);
    selector.append(option);
  });
  selector.change( function () { selectCallback( templatesList[this.value] ); });
  if(templatesList.length > 0) {
    selectCallback(templatesList[0]);
  }
  return selector;
}
         
