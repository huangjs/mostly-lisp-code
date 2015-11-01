
(in-package :cg-user)

;;; Here is a Google Maps API key that was generated for the franz.com domain.
;;; Apparently any valid key will work when reading a local HTML file as this
;;; example does, but if you adapt this code to serve the HTML on the web
;;; then you will need to replace this value with a Google Maps API key that
;;; you generate for the web server that is used.  Generate your own key here:
;;; http://code.google.com/apis/maps/signup.html

(defparameter *google-maps-api-key*
  "ABQIAAAAb4zVQYoi1pjhn-cgIfxB8xTIvawiYadrDEtGjIWCNEOmar7GXhR5GQqhs8xqs7rKUvYMLVDfGjiarg")

;;; ------------------------------
;;; Beginning of Web Page Skeleton

;;; This is a large format string that provides the skeleton of a
;;; web page with JavaScript that will be loaded into the HTML browser.
;;; This web page skeleton comprises most of this example code.

(defparameter *google-maps-javascript-skeleton*
  "<html><head><title>Google Maps Script</title>

<!-- This small script loads the Google Maps code and validates the key.
     The lisp code will fill in the actual key.  -->
<script src=\"http://maps.google.com/maps?file=api&amp;v=2.x&amp;~
              key=~a\"
        type=\"text/javascript\"></script>

<!-- Beginning of Google Maps JavaScript.  Most of this web page is
     JavaScript that uses the Google Maps API to display a map. -->
<script type=\"text/javascript\">

// A couple of objects to create once at load time.
var defaultIcon = new GIcon (G_DEFAULT_ICON);
var geocoder = new GClientGeocoder();

// Some global variables.
var minLat
var maxLat
var minLng
var maxLng
var markerCount

// This JavaScript function is called when this HTML page is
// loaded, due to the onload attribute of the body element below.
function initialize() {
    if (GBrowserIsCompatible()) {

        // Create a map and show it in the map_canvas HTML element
        // that's in the static HTML below.
        var map = new GMap2 (document.getElementById ('map_canvas'));

        // Google Maps requires that we set the map's center position
        // right off the bat, though later we will change the center
        // to be the midpoint of all marked locations.
        map.setCenter (new GLatLng (0, 0));

        // Set the map type (street, satellite, or hybrid).
        // The lisp code will fill in the requested type.
        map.setMapType (~a);

        // Enable zooming with the mouse's scroll wheel.
        map.enableScrollWheelZoom();

        // Add the standard Google Maps navigation control.
        map.addControl (new GSmallMapControl());

        // Add buttons for changing the type of map that is viewed.
        map.addControl (new GMapTypeControl());

        // Add markers for a set of locations.
        addAddresses (map, 12);
    };
};

function addAddresses (map) {
  minLat = 90.0
  maxLat = -90.0
  minLng = 180.0
  maxLng = -180

  // The lisp code will add a call here to addAddress for
  // each address that is requested in lisp, or a call to
  // addPlace for each explicit latitude and longitude.
  ~a
  }

function addAddress (map, address, label) {

  // Ask Google for the latitude and longitude of an address,
  // and then call addPlace to mark that location.
  geocoder.getLatLng (
    address,

    // Google will return the coordinates asynchronously, so
    // we must pass this callback function that performs anything
    // that must be done after the coordinates are known.
    function (point) {
      if (!point) {
        alert (address + \" not found\");
      }
      else {

        addPlace (map, point.lat(), point.lng(),
                       (label + \"<br>\" + address));

      }
    }
  );
}

function addPlace (map, lat, lng, label) {

    // Create a marker for a particular lattitude and longitude.
    var point = new GLatLng (lat, lng);
    var marker = new GMarker (point, { defaultIcon : defaultIcon });

    // Show an info window whenever the user moves the mouse cursor
    // over a location marker, and close the info window when they
    // move off the map entirely.
    GEvent.addListener (marker, \"mouseover\", function () {
        map.openInfoWindowHtml (point, (label + \"<br>\"
                                        + \"Lat \" + lat
                                        + \"&nbsp&nbsp&nbsp&nbsp Lng \" + lng));
    });
    GEvent.addListener (map, \"mouseout\", function () {
        map.closeInfoWindow ();
    });

    // Add the new marker to the map.
    map.addOverlay (marker);

    // Remember the range of coordinates that have been marked
    // so that we can make the map encompass all of them.
    minLat = Math.min (minLat, lat);
    maxLat = Math.max (maxLat, lat);
    minLng = Math.min (minLng, lng);
    maxLng = Math.max (maxLng, lng);

    // Recenter the map to the middle of all locations that
    // we have marked so far.
    map.setCenter (new GLatLng ((minLat + maxLat) / 2,
                                (minLng + maxLng) / 2));

    // Find the maximum zoom level at which all of the requested addresses
    // will fit into the map (with a bit of margin), and zoom the map to
    // that level.  Note that it doesn't seem to work to specify the
    // corners of the bounding box when creating the GLatLngBounds
    // object, so we need to use the alternate approach of calling the
    // extend method instead.
    var bounds = new GLatLngBounds;
    bounds.extend (new GLatLng (minLat - ((maxLat - minLat) / 12),
                                minLng - ((maxLng - minLng) / 12)));
    bounds.extend (new GLatLng (maxLat + ((maxLat - minLat) / 12),
                                maxLng + ((maxLng - minLng) / 12)));
    map.setZoom (map.getBoundsZoomLevel (bounds));
};

</script></head>

<!-- End of JavaScript -->

<!-- When this web page is loaded, call our initialize function. -->
<body onload=\"initialize()\" onunload=\"GUnload()\">

<!-- This HTML element will be filled in with a map object that
     the JavaScript above creates. -->
<div id=\"map_canvas\" style=\"width: 100%; height: 100%\"></div>

</body></html>
")

;;; End of Web Page Skeleton
;;; ------------------------
;;; Beginning of Lisp Code

(defun display-addresses 
    (locations &key
               (map-type :street) ;; or :satellite or :hybrid
               
               ;; If you have an html-widget on a custom dialog,
               ;; you could pass the widget here.
               #-gtk
               (html-browser-or-widget
                (html-browser :owner (ide:main-ide-window)
                              :exterior (make-box 0 0 900 700))))
  
  ;; The is the user-callable entry-point function.
  ;; It takes a list of location descriptors and adds hardcoded calls
  ;; into a JavaScript file to mark each location on a map.
  
  ;; Each entry in the locations list should be a list
  ;; containing a label to display for location followed by either
  ;; an address string or a latitude number and a longitude number.
  
  ;; Generate a JavaScript program into an HTML file.
  (let* ((javascript-path (merge-pathnames "temp-google-maps-script.html"
                                           (sys:temporary-directory))))
    (with-open-file (out javascript-path
                         :direction :output :if-exists :supersede)
      (format out *google-maps-javascript-skeleton*
        
        ;; Insert the Google Maps API key into the JavaScript skeleton.
        *google-maps-api-key*
        
        ;; Insert the requested map type into the JavaScript skeleton.
        (case map-type
          (:street "G_NORMAL_MAP")
          (:satellite "G_SATELLITE_MAP")
          (:hybrid "G_HYBRID_MAP"))
        
        ;; Insert calls to addAddress and addPlace into the JavaScript skeleton.
        (with-output-to-string (string-out)
          (dolist (entry locations)
            
            ;; Make sure each entry is of one of the valid forms.
            (unless (and (consp entry)
                         (cdr entry)
                         (not (cdddr entry))
                         (stringp (first entry))
                         (if* (cddr entry)
                            then (and (realp (second entry))
                                      (realp (third entry)))
                            else (stringp (second entry))))
              (pop-up-message-dialog
               (screen *system*) "Bad Location Value"
               (format nil "Each location must be a list consisting of either ~
                            a label string and an address string or else ~
                            a label string, a latitude number, and a ~
                            longitude number.  The following entry does ~
                            not match:~%~%~s"
                 entry)
               error-icon "~OK")
              (return-from display-addresses))
            
            ;; If an entry has a third member, then it specifies
            ;; a latitude and longitude directly.
            (if* (cddr entry)
               then (format string-out "  addPlace (map, ~a, ~a, ~s);~%"
                      (second entry)(third entry)(first entry))
                    
                    ;; Otherwise it specifies an address.
               else (format string-out "  addAddress (map, ~s, ~s);~%"
                      (second entry)(first entry)))))))
    
    ;; Maybe display the generated JavaScript file in the editor
    ;; so you can see the code that was generated.
    ;; Warning:  If this file was already in the editor, this will
    ;; not update it, so you'll need to use File | Revert to Saved.
    #+maybe
    (ed javascript-path)
    
    ;; Display the Google map in an HTML widget.  This requires
    ;; Common Graphics (and the IDE if the owner of the browser
    ;; is an IDE window, as it is with the default argument value).
    #-gtk
    (progn
      (display-html html-browser-or-widget javascript-path)
      (select-window (typecase html-browser-or-widget
                       (html-browser html-browser-or-widget)
                       (html-widget (owner html-browser-or-widget)))))
    
    ;; On GTK the Mozilla GTK widget is likely not installed,
    ;; so display the map in Firefox if available, and otherwise
    ;; simply post the pathname on the clipboard for the user to
    ;; paste into some third-party HTML browser.
    #+gtk
    (invoke-private-html-browser javascript-path)))

;;; End of Lisp Code to be loaded
;;; ------------------------------

#|

;;  These forms will display maps:

(display-addresses '(("Franz Headquarters"
                      "555 12th Street, Oakland, CA 94607")
                     ("Berkeley Bowl Marketplace"
                      "2020 Oregon Street, Berkeley, CA 94703")
                     ("Nifty Trails on the Hill"
                      37.8643 -122.241)))

(display-addresses '(("Franz Headquarters"
                      "555 12th Street, Oakland, CA 94607")
                     ("Berkeley Bowl Marketplace"
                      "2020 Oregon Street, Berkeley, CA 94703")
                     ("The City" "San Francisco")
                     ("Nifty Trails on the Hill"
                      37.8643 -122.241))
                   :map-type :hybrid)

(display-addresses '(("Nifty Trails on the Hill"
                      37.8643 -122.241))
                   :map-type :satellite)

 |#

