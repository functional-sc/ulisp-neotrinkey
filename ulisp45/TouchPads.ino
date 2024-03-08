/*
 * User Extension for the AdaFruit NeoPixel to read the capacitive touch device.
 * 
 * Requires the board Adafruit NeoPixel Trinkey M0 (SAMD21) with  
 * wire and serial defs copied from the Adafruit Gemma M0 (SAMD21)
 * 
 * Requires the following libraries from Adafruit
 *   Adafruit NeoPixel
 *   Adafruit FreeTouch
 *   
 * Generally a reading over 500 means it is being touched.
 *   
 * uLisp usage:
 * 
 * 
 * ;; no args it returns a list of both cap touch devices
 * > (touchpads)
 * (547 789)
 * 
 * ;; an argument it returns the value of the respective device
 * > (touchpads 1)
 * 547
 *
 * ;; an argument it returns the value of the respective device
 * > (touchpads 2)
 * 789
 * 
 * ;; are either being touched?
 * (apply max (touchpads))
 * 
 * ;; test loop
 * (loop (format t "QT1: ~a  QT2:~a~%" (touchpads 1) (touchpads 2)) (delay 100))
 * 
 * ;; on error it rerturns 0
 * > (touchpads)
 * ERROR: Failed to start QT
 * (0 0)
 * 
 * > (touchpads 89)
 * 0
 * 
 * > (touchpads 1 2 3)
 * 0
 * 
 */

#include <Adafruit_NeoPixel.h>
#include "Adafruit_FreeTouch.h"

// Create the two touch pads on pins 1 and 2:
Adafruit_FreeTouch qt_1 = Adafruit_FreeTouch(1, OVERSAMPLE_4, RESISTOR_50K, FREQ_MODE_NONE);
Adafruit_FreeTouch qt_2 = Adafruit_FreeTouch(2, OVERSAMPLE_4, RESISTOR_50K, FREQ_MODE_NONE);

bool freetouch_initialized = false;

// Definitions
object *fn_readtouch (object *args, object *env) {
  (void) env;
 
  if (! freetouch_initialized) {
     if (! qt_1.begin() || ! qt_2.begin()) {
        Serial.println("ERROR: Failed to start QT");
        return cons(number(0), cons(number (0), NULL));
     }
     freetouch_initialized = true;
  }

  // too many args?
  if (1 < listlength(args)) return number(0);

  // no args, get both as list
  if (0 == listlength(args)) {
    return cons(number(qt_1.measure()), cons(number(qt_2.measure()), NULL));
  }

  // 1 arg, which pin 1 or 2?
  switch ((unsigned long)(checkinteger(first(args)))) {
    case 1:  return number(qt_1.measure());
    case 2:  return number(qt_2.measure());
  }

  // fall through to error, arg is too big or small
  return number(0);
}

// Symbol names
const char stringreadtouch[] PROGMEM = "touchpads";

// Documentation strings
const char doctouch[] PROGMEM = "(touchpads [number])\n"
"Returns the capacitive touch reading.\n"
"With no arguments it returns a list of all readings, otherwise device number 1 or 2.\n";

// Symbol lookup table
const tbl_entry_t lookup_table2[] PROGMEM = {
  { stringreadtouch, fn_readtouch, 0203, doctouch },

  // NeoPixel defs here
  { stringPixelsBegin, fn_PixelsBegin, 0200, docPixelsBegin },
  { stringPixelsClear, fn_PixelsClear, 0200, docPixelsClear },
  { stringPixelsFill, fn_PixelsFill, 0203, docPixelsFill },
  { stringPixelsSetPixelColor, fn_PixelsSetPixelColor, 0225, docPixelsSetPixelColor },
  { stringPixelsColor, fn_PixelsColor, 0234, docPixelsColor },
  { stringPixelsColorHSV, fn_PixelsColorHSV, 0233, docPixelsColorHSV },
  { stringPixelsShow, fn_PixelsShow, 0200, docPixelsShow },
  { stringPixelsRainbow, fn_PixelsRainbow, 0205, docPixelsRainbow },

};

// Table cross-reference functions

/*
 * HACK alert: oddly only 2 lookup tables work, combined NeoPixel with this
 */

tbl_entry_t *tables[] = { lookup_table, lookup_table2 };
const unsigned int tablesizes[] = { arraysize(lookup_table), arraysize(lookup_table2) };

const tbl_entry_t *table (int n) {
  return tables[n];
}

unsigned int tablesize (int n) {
  return tablesizes[n];
}
