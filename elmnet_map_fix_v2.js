(function () {
  "use strict";

  var attempts = 0;
  var maxAttempts = 150;
  var refreshDone = false;

  function findEnvironmentalSection() {
    var sections = Array.from(
      document.querySelectorAll(
        "#elmnet-layer-panel .elmnet-layer-section"
      )
    );

    return sections.find(function (section) {
      var heading = section.querySelector("h3");

      return heading &&
        heading.textContent.trim().toLowerCase() ===
          "environmental zones";
    }) || null;
  }

  function getZoneInputs(section) {
    if (!section) {
      return [];
    }

    return Array.from(
      section.querySelectorAll(
        "label.elmnet-layer-option input[type='checkbox']"
      )
    ).filter(function (input) {
      return input.id !== "elmnet-all-environmental-zones";
    });
  }

  function updateMasterState(master, inputs) {
    var checked = inputs.filter(function (input) {
      return input.checked;
    }).length;

    master.checked =
      inputs.length > 0 &&
      checked === inputs.length;

    master.indeterminate =
      checked > 0 &&
      checked < inputs.length;
  }

  function addMasterSwitch(section, inputs) {
    var existing = section.querySelector(
      "#elmnet-all-environmental-zones"
    );

    if (existing) {
      updateMasterState(existing, inputs);
      return existing;
    }

    var row = document.createElement("label");
    row.className =
      "elmnet-layer-option elmnet-zone-master";

    row.style.fontWeight = "700";
    row.style.marginBottom = "0.35rem";
    row.style.paddingBottom = "0.45rem";
    row.style.borderBottom = "1px solid #d9dee3";

    var master = document.createElement("input");
    master.type = "checkbox";
    master.id = "elmnet-all-environmental-zones";
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
      var targetState = master.checked;

      inputs.forEach(function (input) {
        if (input.checked !== targetState) {
          input.click();
        }
      });

      window.setTimeout(function () {
        updateMasterState(master, inputs);
        refreshMap();
      }, 50);
    });

    inputs.forEach(function (input) {
      input.addEventListener("change", function () {
        updateMasterState(master, inputs);
      });
    });

    updateMasterState(master, inputs);

    return master;
  }

  function ensureZonesInitiallyVisible(inputs) {
    inputs.forEach(function (input) {
      if (!input.checked) {
        input.click();
      }
    });
  }

  function nudgeZoom() {
    var zoomIn = document.querySelector(
      ".leaflet-control-zoom-in"
    );

    var zoomOut = document.querySelector(
      ".leaflet-control-zoom-out"
    );

    if (!zoomIn || !zoomOut) {
      return;
    }

    zoomIn.click();

    window.setTimeout(function () {
      zoomOut.click();
    }, 250);
  }

  function refreshMap() {
    window.dispatchEvent(new Event("resize"));

    /*
      The raster overlays in this rendered Leaflet widget are
      recalculated reliably after a zoom event. A quick zoom in
      and back out forces the initial raster display without
      changing the final map extent.
    */
    window.setTimeout(nudgeZoom, 100);
  }

  function installFix() {
    attempts += 1;

    var section = findEnvironmentalSection();
    var inputs = getZoneInputs(section);

    if (!section || inputs.length === 0) {
      if (attempts < maxAttempts) {
        window.setTimeout(installFix, 100);
      }
      return;
    }

    ensureZonesInitiallyVisible(inputs);
    addMasterSwitch(section, inputs);

    if (!refreshDone) {
      refreshDone = true;

      window.setTimeout(refreshMap, 100);
      window.setTimeout(refreshMap, 700);
      window.setTimeout(refreshMap, 1600);
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener(
      "DOMContentLoaded",
      installFix
    );
  } else {
    installFix();
  }

  window.addEventListener("load", function () {
    window.setTimeout(installFix, 100);
  });

  var observer = new MutationObserver(function () {
    installFix();
  });

  observer.observe(document.documentElement, {
    childList: true,
    subtree: true
  });
})();
