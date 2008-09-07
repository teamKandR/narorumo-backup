function addtag() {
  meta = document.createElement("meta");
  meta.name = "gwt:property";
  meta.content = "locale={{locale}}"
  
  document.getElementsByTagName('head')[0].appendChild(meta);
}

addtag();
