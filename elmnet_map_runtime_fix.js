(function () {
  "use strict";

  var MAX_ATTEMPTS = 120;
  var attempts = 0;

  function getMapWidgetElement() {
    return document.querySelector(
      ".leaflet.html-widget, .leaflet.html-widget-static-bound, .leaflet"
    );
  }

  function getLeafletMap() {
    var element = getMapWidgetElement();

    if (!element || !element.id || !window.HTMLWidgets) {
      return null;
    }

    var widget = window.HTMLWidgets.find("#" + element.id);

    if (!widget || typeof widget.getMap !== "function") {
      return null;
    }

    return widget.getMap();
  }

  function getEnvironmentalSection() {
    var sections = Array.from(
      document.querySelectorAll(
        "#elmnet-layer-panel .elmnet-layer-section"
      )
    );

    return sections.find(function (section) {
      var heading = section.querySelector("h3");

      return heading &&
        heading.textContent.trim() === "Environmental zones";
    }) || null;
  }

  function getEnvironmentalInputs(section) {
    if (!section) {
      return [];
    }

    return Array.from(
      section.querySelectorAll(
        "label.elmnet-layer-option input[type='checkbox']"
      )
    ).filter(function (input) {
      return !input.classList.contains("elmnet-zone-master-input");
    });
  }

  function updateMasterState(master, zoneInputs) {
    var checkedCount = zoneInputs.filter(function (input) {
      return input.checked;
    }).length;

    master.checked =
      zoneInputs.length > 0 &&
      checkedCount === zoneInputs.length;

    master.indeterminate =
      checkedCount > 0 &&
      checkedCount < zoneInputs.length;
  }

  function addEnvironmentalMasterSwitch() {
    var section = getEnvironmentalSection();

    if (!section) {
      return false;
    }

    if (section.querySelector(".elmnet-zone-master")) {
      return true;
    }

    var zoneInputs = getEnvironmentalInputs(section);

    if (zoneInputs.length === 0) {
      return false;
    }

    var row = document.createElement("label");
    row.className = "elmnet-layer-option elmnet-zone-master";

    row.style.fontWeight = "700";
    row.style.marginBottom = "0.35rem";
    row.style.paddingBottom = "0.45rem";
    row.style.borderBottom = "1px solid #d9dee3";

    var master = document.createElement("input");
    master.type = "checkbox";
    master.className = "elmnet-zone-master-input";
    master.setAttribute(
      "aria-label",
      "Show or hide all environmental zones"
    );

    var symbol = document.createElement("span");
    symbol.className =
      "elmnet-layer-symbol elmnet-symbol-square";
    symbol.style.setProperty(
      "--elmnet-symbol-color",
      "#7f8c8d"
    );

    var text = document.createElement("span");
    text.className = "elmnet-layer-label";
    text.textContent = "All environmental zones";

    row.appendChild(master);
    row.appendChild(symbol);
    row.appendChild(text);

    var firstOption = section.querySelector(
      "label.elmnet-layer-option"
    );

    section.insertBefore(row, firstOption);

    master.addEventListener("change", function () {
      var desiredState = master.checked;

      zoneInputs.forEach(function (input) {
        if (input.checked !== desiredState) {
          input.click();
        }
      });

      updateMasterState(master, zoneInputs);
    });

    zoneInputs.forEach(function (input) {
      input.addEventListener("change", function () {
        updateMasterState(master, zoneInputs);
      });
    });

    updateMasterState(master, zoneInputs);

    return true;
  }

  function refreshRasterDisplay() {
    var map = getLeafletMap();

    if (!map) {
      return false;
    }

    map.invalidateSize({
      pan: false,
      animate: false,
      debounceMoveend: true
    });

    map.eachLayer(function (layer) {
      if (typeof layer._reset === "function") {
        layer._reset();
      } else if (typeof layer.redraw === "function") {
        layer.redraw();
      }
    });

    map.fire("viewreset");
    map.fire("moveend");

    return true;
  }

  function runFixes() {
    attempts += 1;

    var switchReady = addEnvironmentalMasterSwitch();
    var mapReady = refreshRasterDisplay();

    if ((!switchReady || !mapReady) && attempts < MAX_ATTEMPTS) {
      window.setTimeout(runFixes, 100);
      return;
    }

    [150, 400, 900, 1600].forEach(function (delay) {
      window.setTimeout(refreshRasterDisplay, delay);
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", runFixes);
  } else {
    runFixes();
  }

  window.addEventListener("load", function () {
    window.setTimeout(runFixes, 100);
  });
})();
