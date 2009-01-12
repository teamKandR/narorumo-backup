// File: bidynodes.js
// Author: Lindsey Kuper
// Created: April 24, 2006
// Last modified: May 30, 2006
// http://www.rockstargirl.org/sandbox/bidynodes

// Largely borrowed from:

// dynodes.js
// Kent Brewster, 2005
// http://www.mindsack.com/uxe/dynodes/
// feel free to use or distribute this code
// but please retain this notice

// Gotcha: onLoad takes a camelCase L, or IE won't work
// Gotcha: need init(), not init, or Opera won't work
window.onLoad=init();
var el;
var ind=document.getElementById('LoadingIndicator');

// capture onmousedown, onmouseover, onmouseout
function init() {
	document.onmouseup = k;
	document.onmouseover = h;
	document.onmouseout = o;
}

// hover: prepare the remote script
function h(v) {
	el = getEl(v);
	if (el.className == 'loadScript') {
		// create a new node to host the remote script
		var remoteScript=document.createElement('script');
		// Format the query string in such a way that the remote script will be able to understand it
		var remoteScriptName = 'http://shoeboxfulloftapes.org/sandbox/bidynodes/formprocessor?';
		for(var i=0; i<document.theForm.elements.length; i++) {
			// We don't need the button in there
			if (document.theForm.elements[i].getAttribute('type') != 'button') {
				// Validation check
				if (document.theForm.elements[i].getAttribute('type') != 'hidden'
					&& document.getElementsByTagName('label')[i].getAttribute('class') == "required"
					&& !isValid(document.theForm.elements[i])) {
					if (!document.getElementsByTagName('label')[i].innerHTML.match(errorMessage)) {
						document.getElementsByTagName('label')[i].innerHTML += ' ' + errorMessage;
					}
					document.getElementsByTagName('label')[i].style.color = '#f00';
					return;
				}
				// Append each field's name and value to the query string
				if (document.theForm.elements[i].getAttribute('type') == 'text' ||
						document.theForm.elements[i].getAttribute('type') == 'hidden') {
					// Don't bother appending a text or hidden field unless it has a value
					if (document.theForm.elements[i].value.length > 0) {
						remoteScriptName += document.theForm.elements[i].getAttribute('name') 
						+ '=' + document.theForm.elements[i].value + '&';
					}
				}
				if (document.theForm.elements[i].getAttribute('type') == 'checkbox') {
					// Don't bother appending a checkbox field unless it's checked
					if (document.theForm.elements[i].checked) {
						remoteScriptName += document.theForm.elements[i].getAttribute('name') 
						+ '=' + document.theForm.elements[i].value + '&';
					}
				}
			}
		}
		
		remoteScript.id = 'rs';
		remoteScript.setAttribute('type','text/javascript');
		remoteScript.setAttribute('src',remoteScriptName);
		var hd=document.getElementsByTagName('head')[0];
		// Gotcha: set attribute and src BEFORE appending, or Safari won't work
		hd.appendChild(remoteScript);
		el.className = 'runScript'; 
	}
}

// klick: execute remote function, remove the remote script
function k(v) {
	el = getEl(v);
	if (el.className == 'runScript') {
		// Try out our remote script
		tryGetStuff(v);
		// Remove the remote script
		removeScript('rs');		
		// and restyle the element for next time
		el.className = 'loadScript';
	}
}

function tryGetStuff(v) {
	if (window.getStuff) {
		getStuff();
	} else {
		ind.style.visibility = 'visible';
		setTimeout(function() { o(v);h(v);k(v); }, 1000);
	}
}

// outtahere: did not click, remove the remote script
function o(v) {
	el = getEl(v);
	if (el.className == 'runScript') {
		// remote script is loaded; remove it	
		removeScript('rs');
		// and restyle the link for next time	
		el.className = 'loadScript';
	}
}

// remove script node
function removeScript(id) {
	var hd=document.getElementsByTagName('head')[0];
	hd.removeChild(document.getElementById(id));
}

// figure out which element the event is pointing to
function getEl(v) {
	var tg = (v) ? v : event;
	if (tg.target) {
		el = (tg.target.nodeType == 3) ? tg.target.parentNode : tg.target;
	}
	else {
		el = tg.srcElement;
	}
	return el;
}

// called by the remote script upon successful execution
function doStuffWith(theStuffIGotSomewhereElse) {
	document.getElementById('TheForm').innerHTML = theStuffIGotSomewhereElse.json;
}

// Form validation 
function isValid(el) {
	// Might want to use something like this for email addresses
	if (el.getAttribute('name') == 'confirmation_sent_to') {
		if (invalidEmailAddress(el.value)) {
			errorMessage = '(Please provide a valid email address, such as \'cheese@example.com\')';
			return false;
		}
		return true;
	}
	if (withoutContent(el.value)) {
		errorMessage = '(Required)';
		return false;
	}
	return true;
}

// Regexp for email address formatting
function invalidEmailAddress(ss) {
	var reg1 = /(@.*@)|(\.\.)|(@\.)|(\.@)|(^\.)/;
	var reg2 = /^.+\@(\[?)[a-zA-Z0-9\-\.]+\.([a-zA-Z]{2,3}|[0-9]{1,3})(\]?)$/;
	if(reg1.test(ss) || !reg2.test(ss)) { return true; }
	return false;	
}

// Simple check for empty fields
function withoutContent(ss) {
	if(ss.length > 0) { return false; }
	return true;
}